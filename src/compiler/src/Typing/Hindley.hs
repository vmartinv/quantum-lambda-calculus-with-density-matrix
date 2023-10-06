{-# LANGUAGE TupleSections #-}
module Typing.Hindley where

import           CompilerError
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Coerce
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Data.Tuple.Extra
import           Parsing.LamRhoExp
import           Typing.GateChecker
import           Typing.MatrixChecker
import           Typing.QType
import           Typing.Robinson
import           Typing.Subst
import           Typing.TypeEq
import           Utils

type TypeState = Int

-- | Hindley monad
type HindleyM a = ReaderT
                  TypeEnv             -- Typing environment
                  (StateT         -- Inference state
                  TypeState
                  ExceptInfer)
                  a              -- Result

initTypeState :: TypeState
initTypeState = 0

-- | Run the inference monad
runHindleyM :: TypeEnv -> HindleyM a -> ExceptInfer a
runHindleyM env m = evalStateT (runReaderT m env) initTypeState

runHindley :: TypeEnv -> LamRhoExp -> ExceptInfer (QType, [TypeEq])
runHindley env ex = runHindleyM env (hindley ex)

ftvExp :: LamRhoExp -> S.Set T.Text
ftvExp (PVar v)         = S.singleton v
ftvExp (PLambda v e)    = v `S.delete` (ftvExp e)
ftvExp (PFunApp t r)    = (ftvExp t) `S.union` (ftvExp r)
ftvExp (PQubits _)      = S.empty
ftvExp (PGateApp _ e)   = ftvExp e
ftvExp (PProjector _ e) = ftvExp e
ftvExp (POtimesExp t r) = (ftvExp t) `S.union` (ftvExp r)
ftvExp (PLetCase v e _) = v `S.delete` (ftvExp e)

fresh :: HindleyM QType
fresh = do
  s <- get
  put (s+1)
  return $ QTVar s

equalTypes :: [QType] -> HindleyM (QType, [TypeEq])
equalTypes (t1:ts) = return (t1, map (EqualTypeEq t1) ts)
equalTypes []      = throwError $ InvalidLetCaseNumCases 0

-- | Extend type environment
addToEnv :: (T.Text, QType) -> HindleyM a -> HindleyM a
addToEnv (x, t) = local scope
  where scope (TypeEnv e) = TypeEnv $ M.insert x t e

-- | Lookup type in the environment
lookupEnv :: T.Text -> HindleyM (Maybe QType)
lookupEnv x = do
  (TypeEnv env) <- ask
  return (M.lookup x env)

-- this function is used to partition envs when required
-- to maintain the system affine
partitionEnv :: S.Set T.Text -> S.Set T.Text -> HindleyM (TypeEnv, TypeEnv)
partitionEnv s1 s2 = do
  (TypeEnv env) <- ask
  let
    common = s1 `S.intersection` s2
    inE1 v _ = v `S.member` s1
    (env1, env2) = M.partitionWithKey inE1 env
  when (not (S.null common)) (throwError $ VariablesUsedMoreThanOnce common)
  return (TypeEnv env1, TypeEnv env2)

hindley :: LamRhoExp -> HindleyM (QType, [TypeEq])
hindley ex = case ex of
  PVar x -> do
    tv <- lookupEnv x
    tv' <- maybe (throwError $ UnboundVariable x) return tv
    return (tv', [])

  PLambda x e -> do
    tv <- fresh
    inCtx <- lookupEnv x
    when (isJust inCtx) (throwError $ VariableAlreadyInScope x)
    (t1, eq1) <- addToEnv (x, tv) (hindley e)
    return (tv `QTFun` t1, eq1)

  PFunApp e1 e2 -> do
    tv <- fresh
    (env1, env2) <- partitionEnv (ftvExp e1) (ftvExp e2)
    (t1, eq1) <- local (const env1) $ hindley e1
    (t2, eq2) <- local (const env2) $ hindley e2
    return (tv, eq1 ++ eq2 ++ [EqualTypeEq t1 (QTFun t2 tv)])

  PLetCase x e es -> do
    -- validate number is cases is a power of two
    -- also infers the number of qubits of the conditional
    let q = log2 (length es)
    when (q<0 || (2^q) /= (length es)) (throwError $ InvalidLetCaseNumCases (length es))
    xt <- fresh
    (t1, eq1) <- hindley e
    -- the cases should have a context with only the conditional variable defined
    let
      newEnv = TypeEnv (M.singleton x xt)
      hindley' e = local (const newEnv) (hindley e)
    (tts, eqcases) <- unzip <$> sequenceA (hindley' <$> es)
    (tv, eqcasesr) <- equalTypes tts
    return (tv, eq1++concat eqcases ++ eqcasesr ++ [EqualTypeEq t1 (QTMeasuredQubits q xt), AtLeastSizeEq [xt] q])

  POtimesExp e1 e2 -> do
    tv <- fresh
    (env1, env2) <- partitionEnv (ftvExp e1) (ftvExp e2)
    (t1, eq1) <- local (const env1) $ hindley e1
    (t2, eq2) <- local (const env2) $ hindley e2
    return (tv, eq1++eq2++[SumSizeEq [t1, t2] tv])

  PProjector d e -> do
    (t, eq) <- hindley e
    when (d<=0) (throwError $ InvalidProjectorSize d)
    return (QTMeasuredQubits d t, eq++[AtLeastSizeEq [t] d])

  PPair b m rho -> do
    n <- (lift . lift) (getMatrixSize rho)
    when (b<0 || m<1 || b>=2^m || m>n) (throwError $ InvalidPair b m rho)
    return (QTMeasuredQubits n (QTQubits m), [])

  PGateApp gate@(PGate _n _a offset) e -> do
    (t, eq) <- hindley e
    sz <- (lift . lift) (getGateSize gate)
    return (t, eq++[AtLeastSizeEq [t] (sz+offset)])

  PQubits q -> return (QTQubits (T.length q), [])

  PMatrix m -> do
    q <- (lift . lift) (getMatrixSize m)
    return (QTQubits q, [])
