{-# LANGUAGE TupleSections #-}
module Typing.Hidley where

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
import           Parser
import           Typing.QType
import           Typing.Robinson
import           Typing.Subst
import           Typing.TypeEq
import           Typing.TypeError

type TypeState = Int

-- | Hidley monad
type HidleyM a = (ReaderT
                  TypeEnv             -- Typing environment
                  (StateT         -- Inference state
                  TypeState
                  ExceptInfer)
                  a)              -- Result

initTypeState :: TypeState
initTypeState = 0

-- | Run the inference monad
runHidleyM :: TypeEnv -> HidleyM a -> ExceptInfer a
runHidleyM env m = evalStateT (runReaderT m env) initTypeState

runHidley :: TypeEnv -> PExp -> ExceptInfer (QType, [TypeEq])
runHidley env ex = runHidleyM env (hindley ex)

ftvExp :: PExp -> S.Set T.Text
ftvExp (PVar v)         = S.singleton v
ftvExp (PLambda v e)    = v `S.delete` (ftvExp e)
ftvExp (PFunApp t r)    = (ftvExp t) `S.union` (ftvExp r)
ftvExp (PQubits _)      = S.empty
ftvExp (PGate _ _ e)    = ftvExp e
ftvExp (PProjector _ e) = ftvExp e
ftvExp (POtimes t r)    = (ftvExp t) `S.union` (ftvExp r)
ftvExp (PLetCase v e _) = v `S.delete` (ftvExp e)

letters :: [T.Text]
letters = T.pack <$> ([1..] >>= flip replicateM ['a'..'z'])

fresh :: HidleyM QType
fresh = do
  s <- get
  put (s+1)
  return $ QTVar s

equalTypes :: [QType] -> HidleyM (QType, [TypeEq])
equalTypes (t1:ts) = return (t1, map (TypeEq t1) ts)
equalTypes []      = throwError $ InvalidLetCaseNumCases 0

-- | Extend type environment
addToEnv :: (T.Text, QType) -> HidleyM a -> HidleyM a
addToEnv (x, t) = local scope
  where scope (TypeEnv e) = TypeEnv $ M.insert x t e

-- | Lookup type in the environment
lookupEnv :: T.Text -> HidleyM (Maybe QType)
lookupEnv x = do
  (TypeEnv env) <- ask
  return (M.lookup x env)

-- this function is used to partition envs when required
-- to maintain the system affine
partitionEnv :: S.Set T.Text -> S.Set T.Text -> HidleyM (TypeEnv, TypeEnv)
partitionEnv s1 s2 = do
  (TypeEnv env) <- ask
  let
    common = s1 `S.intersection` s2
    inE1 v _ = v `S.member` s1
    (env1, env2) = M.partitionWithKey inE1 env
  when (not (S.null common)) (throwError $ VariablesUsedMoreThanOnce common)
  return (TypeEnv env1, TypeEnv env2)

hindley :: PExp -> HidleyM (QType, [TypeEq])
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
    return (tv, eq1 ++ eq2 ++ [TypeEq t1 (QTFun t2 tv)])

  PLetCase x e es -> do
    -- validate number is cases is a power of two
    -- also infers the number of qubits of the conditional
    let log2 = floor . logBase 2.0 . fromIntegral
        q = log2 (length es)
    when ((2^q) /= (length es)) (throwError $ InvalidLetCaseNumCases (length es))
    tcase <- fresh
    (t1, eq1) <- hindley e
    -- the cases should have a context with only the conditional variable defined
    let
      newEnv = TypeEnv (M.singleton x tcase)
      hindley' e = local (const newEnv) (hindley e)
    (tts, eqcases) <- unzip <$> sequenceA (hindley' <$> es)
    (tv, eqcasesr) <- equalTypes tts
    return (tv, eq1++concat eqcases ++ eqcasesr ++ [TypeEq t1 (QTMeasuredQubits q)])

  POtimes e1 e2 -> do
    tv <- fresh
    (env1, env2) <- partitionEnv (ftvExp e1) (ftvExp e2)
    (t1, eq1) <- local (const env1) $ hindley e1
    (t2, eq2) <- local (const env2) $ hindley e2
    return (tv, eq1++eq2++[IsQubits t1, IsQubits t2, IsQubits tv, SumSizeEq [t1, t2] tv])

  PProjector d e -> do
    tv <- fresh
    (t, eq) <- hindley e
    return (tv, eq++[TypeEq tv (QTMeasuredQubits d), IsQubits t, IsMeasuredQubits tv, AtLeastSizeEq [t] tv])

  PGate g _params e -> do
    tv <- fresh
    (t, eq) <- hindley e
    return (tv, eq++[TypeEq tv t, TypeEq tv (QTQubits 2)])

  PQubits q -> return (QTQubits (T.length q), [])
