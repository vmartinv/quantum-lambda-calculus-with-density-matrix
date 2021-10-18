{-# LANGUAGE TupleSections #-}
module Typing.TypeChecker where

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
import           Typing.Utils

-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

type TypeState = Int

data TypeError  = UnificationFail QType QType
              | InfiniteType VariableId QType
              | UnboundVariable T.Text
              | InvalidLetCaseNumCases Int
              | TypeNotQubits QType
              | TypeNotMeasuredQubits QType
              | UnificationMismatch [QType] [QType]
              | InvalidOperatorSizes
              | VariableAlreadyInScope T.Text
              | VariablesUsedMoreThanOnce (S.Set T.Text)
              deriving (Show,Eq)

newtype TypeEnv = TypeEnv (M.Map T.Text QType)
type ExceptInfer = Except TypeError
-- | Hidley monad
type HidleyM a = (ReaderT
                  TypeEnv             -- Typing environment
                  (StateT         -- Inference state
                  TypeState
                  ExceptInfer)
                  a)              -- Result
type Subst = M.Map VariableId QType

data TEq = SumSizeEq [QType] QType
        | IsQubits QType
        | IsMeasuredQubits QType
        | TypeEq QType QType
      deriving (Show)

initTypeState = 0

-- | Run the inference monad
runHidleyM :: TypeEnv -> HidleyM a -> ExceptInfer a
runHidleyM env m = evalStateT (runReaderT m env) initTypeState

-- | Solve for the toplevel type of an expression
typeCheck :: PExp -> Except String QType
typeCheck = (withExcept show).(fullTypeCheck (TypeEnv M.empty))

fullTypeCheck :: TypeEnv -> PExp -> ExceptInfer QType
fullTypeCheck env ex = do
  (t, eqs) <- runHidleyM env (hindley ex)
  subst <- robinson eqs
  closeOver (apply subst t)

closeOver :: QType -> ExceptInfer QType
closeOver t = foldr apply t <$> mapM assign1 (S.toList (ftv t))
  where
    assign1 v = v `bind` (QTQubits 1)

emptySubst :: Subst
emptySubst = M.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

ftvExp :: PExp -> S.Set T.Text
ftvExp (PVar v)         = S.singleton v
ftvExp (PLambda v e)    = v `S.delete` (ftvExp e)
ftvExp (PFunApp t r)    = (ftvExp t) `S.union` (ftvExp r)
ftvExp (PQubits _)      = S.empty
ftvExp (PGate _ e)      = ftvExp e
ftvExp (PProjector e)   = ftvExp e
ftvExp (PTimes t r)     = (ftvExp t) `S.union` (ftvExp r)
ftvExp (PLetCase v e _) = v `S.delete` (ftvExp e)

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> S.Set VariableId

instance Substitutable QType where
  apply _ t@(QTQubits _)         = t
  apply _ t@(QTMeasuredQubits _) = t
  apply s t@(QTVar a)            = M.findWithDefault t a s
  apply s (t1 `QTFun` t2)        = apply s t1 `QTFun` apply s t2

  ftv (QTVar v)     = S.singleton v
  ftv (QTFun t1 t2) = ftv t1 `S.union` ftv t2
  ftv _             = S.empty

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (S.union . ftv) S.empty

instance Substitutable TEq where
  apply s (SumSizeEq ts tr)    = SumSizeEq (map (apply s) ts) (apply s tr)
  apply s (IsQubits t)         = IsQubits $ apply s t
  apply s (IsMeasuredQubits t) = IsMeasuredQubits $ apply s t
  apply s (TypeEq t1 t2)       = TypeEq (apply s t1) (apply s t2)
  ftv (SumSizeEq ts tr)    = ftv (tr:ts)
  ftv (IsQubits t)         = ftv t
  ftv (IsMeasuredQubits t) = ftv t
  ftv (TypeEq t1 t2)       = ftv [t1, t2]

letters :: [T.Text]
letters = T.pack <$> ([1..] >>= flip replicateM ['a'..'z'])

fresh :: HidleyM QType
fresh = do
  s <- get
  put (s+1)
  return $ QTVar s

equalTypes :: [QType] -> HidleyM (QType, [TEq])
equalTypes (t1:ts) = return (t1, map (TypeEq t1) ts)
equalTypes []      = throwError $ InvalidLetCaseNumCases 0

-- | Extend type environment
inEnv :: (T.Text, QType) -> HidleyM a -> HidleyM a
inEnv (x, t) = local scope
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

hindley :: PExp -> HidleyM (QType, [TEq])
hindley ex = case ex of
  PVar x -> do
    tv <- lookupEnv x
    tv' <- maybe (throwError $ UnboundVariable x) return tv
    return (tv', [])

  PLambda x e -> do
    tv <- fresh
    inCtx <- lookupEnv x
    when (isJust inCtx) (throwError $ VariableAlreadyInScope x)
    (t1, eq1) <- inEnv (x, tv) (hindley e)
    return (tv `QTFun` t1, eq1)

  PFunApp e1 e2 -> do
    tv <- fresh
    (env1, env2) <- partitionEnv (ftvExp e1) (ftvExp e2)
    (t1, eq1) <- local (const env1) $ hindley e1
    (t2, eq2) <- local (const env2) $ hindley e2
    return (tv, eq1 ++ eq2 ++ [TypeEq t1 (QTFun t2 tv)])

  PLetCase x e es -> do
    tcase <- fresh
    (t1, eq1) <- hindley e
    -- the cases should have a context with only the conditional variable defined
    let
      newEnv = TypeEnv (M.singleton x tcase)
      hindley' e = local (const newEnv) (hindley e)
    (tts, eqcases) <- unzip <$> sequenceA (hindley' <$> es)
    (tv, eqcasesr) <- equalTypes tts
    -- validate number is cases is a power of two
    -- also infers the number of qubits of the conditional
    let log2 = floor . logBase 2.0 . fromIntegral
        q = log2 (length es)
    when ((2^q) /= (length es)) (throwError $ InvalidLetCaseNumCases (length es))
    return (tv, eq1++concat eqcases ++ eqcasesr ++ [TypeEq t1 (QTMeasuredQubits q)])

  PTimes e1 e2 -> do
    tv <- fresh
    (env1, env2) <- partitionEnv (ftvExp e1) (ftvExp e2)
    (t1, eq1) <- local (const env1) $ hindley e1
    (t2, eq2) <- local (const env2) $ hindley e2
    return (tv, eq1++eq2++[IsQubits t1, IsQubits t2, IsQubits tv, SumSizeEq [t1, t2] tv])

  PProjector e -> do
    tv <- fresh
    (t, eq) <- hindley e
    return (tv, eq++[IsQubits t, IsMeasuredQubits tv, SumSizeEq [t] tv])

  PGate g e -> do
    tv <- fresh
    (t, eq) <- hindley e
    return (tv, eq++[TypeEq tv t, TypeEq tv (QTQubits 2)])

  PQubits q -> return (QTQubits (T.length q), [])

occursCheck :: Substitutable a => VariableId -> a -> Bool
occursCheck a t = a `S.member` ftv t

bind :: VariableId -> QType -> ExceptInfer Subst
bind a t | t == QTVar a     = return emptySubst
       | occursCheck a t = throwError $ InfiniteType a t
       | otherwise       = return $ M.singleton a t

unifies :: QType -> QType -> ExceptInfer Subst
unifies t1 t2                       | t1 == t2 = return emptySubst
unifies (QTVar v) t                 = v `bind` t
unifies t (QTVar v)                 = v `bind` t
unifies (QTFun t1 t2) (QTFun t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2                       = throwError $ UnificationFail t1 t2

unifyMany :: [QType] -> [QType] -> ExceptInfer Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  su1 <- unifies t1 t2
  su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
  return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ UnificationMismatch t1 t2

classCheck :: TEq -> ExceptInfer ()
classCheck (IsQubits (QTQubits _)) = return ()
classCheck (IsQubits (QTVar _)) = return ()
classCheck (IsQubits t) = throwError $ TypeNotQubits t
classCheck (IsMeasuredQubits (QTMeasuredQubits _)) = return ()
classCheck (IsMeasuredQubits (QTVar _)) = return ()
classCheck (IsMeasuredQubits t) = throwError $ TypeNotMeasuredQubits t
classCheck _ = return ()

robinson :: [TEq] -> ExceptInfer Subst
robinson eqs = do
  su1 <- uncurry unifyMany $ unzip [ (t1, t2) | TypeEq t1 t2 <- eqs]
  let eqs' = apply su1 eqs
      sumEqs = dprint "sumEqs" $ [(ls, r) | SumSizeEq ls r <- eqs']
  sol <- solveSums sumEqs
  su2 <- assignValues eqs' sol
  sequence_ $ classCheck <$> apply su2 eqs'
  return $ su1 `compose` su2

assignValues :: [TEq] -> M.Map VariableId Int -> ExceptInfer Subst
assignValues eqs sol = foldr compose emptySubst <$> sequence (bindv <$> M.toList sol)
  where
    measureds = dprint "measureds" $ S.fromList [ v | IsMeasuredQubits (QTVar v) <- eqs]
    cons v | S.member v measureds = QTMeasuredQubits
           | otherwise = QTQubits
    bindv (v, q) = v `bind` cons v q

solveSums :: [([QType], QType)] -> ExceptInfer (M.Map VariableId Int)
solveSums sumEqs = sndPass $ foldr fstPass init_state (reverse sumEqs)
  where
    ftvEqs (ls, r) = foldr S.union (ftv r) $ ftv <$> ls
    set1 = M.fromList ((,1) <$> S.toList (foldr S.union S.empty $ ftvEqs <$> sumEqs))
    init_state = ([], return set1)
    getVar m (QTVar v)            = maybe (0, S.singleton v) snd (find ((==v).fst) m)
    getVar m (QTQubits q)         = (q, S.empty)
    getVar m (QTMeasuredQubits q) = (q, S.empty)
    combine (q1, xs1) (q2, xs2) = (q1+q2, xs1 `S.union` xs2)
    combineLst = foldr combine (0, S.empty)
    num (QTQubits q)         = q
    num (QTMeasuredQubits q) = q
    fstPass (ls, (QTVar v)) (vars, su) = (vars', su)
      where
        ls' = combineLst $ getVar vars <$> ls
        vars' = (v, ls'):vars
    fstPass (ls, q) (vars, su) = (vars, su')
      where
        ls' = combineLst $ getVar vars <$> ls
        su' = do
          (v, q) <- solveSingle ls' (num q)
          M.insert v q <$> su
    sndPass (vars, su) = fold <$> su
      where
        fold su = (foldr f su (reverse vars))::M.Map VariableId Int
        f (v, (q, vs)) su = M.insert v (q'::Int) su
          where
            q' = q + (foldr (+) 0 ((flip (M.findWithDefault 1)) su <$> S.toList vs))


solveSingle :: (Int, S.Set VariableId) -> Int -> ExceptInfer (VariableId, Int)
solveSingle (qf, vs) qt | qf + S.size vs > qt = throwError InvalidOperatorSizes
                        | qf < qt && S.null vs = throwError InvalidOperatorSizes
                        | otherwise = return sol
  where
    v = head $ S.toList vs
    sol = (v, qt - qf - S.size vs + 1)
