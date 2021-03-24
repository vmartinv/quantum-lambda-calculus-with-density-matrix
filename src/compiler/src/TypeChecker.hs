module TypeChecker where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Either.Combinators
import           Data.List
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Text               as T
import           Parser
import           QType


-------------------------------------------------------------------------------
-- Classes
-------------------------------------------------------------------------------

type TypeState = Int

data TypeError  = UnificationFail QType QType
              | InfiniteType VariableId QType
              | UnboundVariable T.Text
              | InvalidType QType
              | InvalidLetCaseVar QType
              | InvalidLetCaseNumCases Int
              | TypeNotQubits QType
              | TypeNotMeasuredQubits QType
              | UnificationMismatch [QType] [QType]
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

initTypeState = 0

-- | Run the inference monad
runHidleyM :: TypeEnv -> HidleyM a -> ExceptInfer a
runHidleyM env m = evalStateT (runReaderT m env) initTypeState

-- | Solve for the toplevel type of an expression
typeCheck :: PExp -> Either TypeError QType
typeCheck ex = runExcept $ do
  (eqs, subst, t, closeOver) <- fullTypeCheck (TypeEnv M.empty) ex
  return closeOver

-- | Return the internal constraints besides type result
fullTypeCheck :: TypeEnv -> PExp -> ExceptInfer ([TEq], Subst, QType, QType)
fullTypeCheck env ex = do
  (t, eqs) <- runHidleyM env (hindley ex)
  subst <- robinson eqs
  -- Todo instantiate vars with random type
  return (eqs, subst, t, apply subst t)

emptySubst :: Subst
emptySubst = M.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

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
lookupEnv :: T.Text -> HidleyM QType
lookupEnv x = do
  (TypeEnv env) <- ask
  case M.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s  -> return s

hindley :: PExp -> HidleyM (QType, [TEq])
hindley ex = case ex of
  PVar x -> do
    tv <- lookupEnv x
    return (tv, [])

  PLambda x e -> do
    tv <- fresh
    (t1, eq1) <- inEnv (x, tv) (hindley e)
    return (tv `QTFun` t1, eq1)

  PFunApp e1 e2 -> do
    tv <- fresh
    (t1, eq1) <- hindley e1
    (t2, eq2) <- hindley e2
    return (tv, eq1 ++ eq2 ++ [TypeEq t1 (QTFun t2 tv)])

  PLetCase x e es -> do
    tcase <- fresh
    let hindley' e = inEnv (x, tcase) (hindley e)
    (t1, eq1) <- hindley' e
    (tts, eqcases) <- unzip <$> sequenceA (hindley' <$> es)
    (tv, eqcasesr) <- equalTypes tts
    let log2 = floor . logBase 2.0 . fromIntegral
        q = log2 (length es)
    when ((2^q) /= (length es)) (throwError $ InvalidLetCaseNumCases (length es))
    return (tv, eq1++concat eqcases ++ eqcasesr ++ [IsMeasuredQubits t1, SumSizeEq [t1] (QTQubits q)])

  PTimes e1 e2 -> do
    tv <- fresh
    (t1, eq1) <- hindley e1
    (t2, eq2) <- hindley e2
    return (tv, eq1++eq2++[IsQubits t1, IsQubits t2, IsQubits tv, SumSizeEq [t1, t2] tv])

  PProjector e -> do
    tv <- fresh
    (t, eq) <- hindley e
    return (tv, eq++[IsQubits t, IsMeasuredQubits tv, SumSizeEq [t] tv])

  PGate g e -> do
    tv <- fresh
    (t, eq) <- hindley e
    return (tv, eq++[TypeEq tv t, IsQubits t, IsQubits tv, SumSizeEq [t] tv])

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
  sequenceA $ classCheck <$> eqs'
  -- SumSizeEq = gauss
  return su1
