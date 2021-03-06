module TypeChecker where


import           Control.Monad.Except
import           Control.Monad.State
import           Data.Either.Combinators
import           Data.List
import qualified Data.Map                as M
import qualified Data.Set                as S
import qualified Data.Text               as T
import           Parser
import           QType


type TypeState = Int

data TypeError  = UnificationFail QType QType
              | InfiniteType T.Text QType
              | UnboundVariable T.Text
              | InvalidType QType
              | InvalidLetCaseVar QType
              | InvalidLetCaseNumCases Int Int
              deriving (Show,Eq)

newtype TypeEnv = TypeEnv (M.Map T.Text Scheme)
type Infer a = ExceptT TypeError (State TypeState) a
type Subst = M.Map T.Text QType

extend :: TypeEnv -> (T.Text, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ M.insert x s env

initTypeState = 0

closeOver (s, t) = Forall ftvs' t''
  where t' = apply s t
        ftvs = S.toList (ftv t')
        ftvs' = fst <$> zip letters ftvs
        convert = M.fromList $ zip ftvs (QTVar <$> ftvs')
        t'' = apply convert t'

runInfer :: Infer (Subst, QType) -> Either TypeError Scheme
runInfer m = case evalState (runExceptT m) initTypeState of
  Left err  -> Left err
  Right res -> Right $ closeOver res

typeCheck :: PExp -> Either TypeError Scheme
typeCheck = runInfer . (infer (TypeEnv M.empty))

nullSubst :: Subst
nullSubst = M.empty

compose :: Subst -> Subst -> Subst
s1 `compose` s2 = M.map (apply s1) s2 `M.union` s1

class Substitutable a where
  apply :: Subst -> a -> a
  ftv   :: a -> S.Set T.Text

instance Substitutable QType where
  apply _ t@(QTQubits _)         = t
  apply _ t@(QTMeasuredQubits _) = t
  apply s t@(QTVar a)            = M.findWithDefault t a s
  apply s (t1 `QTFun` t2)        = apply s t1 `QTFun` apply s t2

  ftv (QTVar v)     = S.singleton v
  ftv (QTFun t1 t2) = ftv t1 `S.union` ftv t2
  ftv _             = S.empty

instance Substitutable Scheme where
  apply s (Forall as t)   = Forall as $ apply s' t
                            where s' = foldr M.delete s as
  ftv (Forall as t) = ftv t `S.difference` S.fromList as

instance Substitutable a => Substitutable [a] where
  apply = fmap . apply
  ftv   = foldr (S.union . ftv) S.empty

instance Substitutable TypeEnv where
  apply s (TypeEnv env) =  TypeEnv $ M.map (apply s) env
  ftv (TypeEnv env) = ftv $ M.elems env

letters :: [T.Text]
letters = T.pack <$> ([1..] >>= flip replicateM ['a'..'z'])

fresh :: Infer QType
fresh = do
  s <- get
  put (s+1)
  return $ QTVar (letters !! s)

occursCheck ::  Substitutable a => T.Text -> a -> Bool
occursCheck a t = a `S.member` ftv t

unify ::  QType -> QType -> Infer Subst
unify (l `QTFun` r) (l' `QTFun` r')  = do
    s1 <- unify l l'
    s2 <- unify (apply s1 r) (apply s1 r')
    return (s2 `compose` s1)

unify (QTVar a) t = bind a t
unify t (QTVar a) = bind a t
unify (QTQubits a) (QTQubits b) | a == b = return nullSubst
unify (QTMeasuredQubits a) (QTMeasuredQubits b) | a == b = return nullSubst
unify t1 t2 = throwError $ UnificationFail t1 t2

bind :: T.Text -> QType -> Infer Subst
bind a t | t == QTVar a     = return nullSubst
         | occursCheck a t = throwError $ InfiniteType a t
         | otherwise       = return $ M.singleton a t

instantiate ::  Scheme -> Infer QType
instantiate (Forall as t) = do
  as' <- mapM (const fresh) as
  let s = M.fromList $ zip as as'
  return $ apply s t

generalize :: TypeEnv -> QType -> Scheme
generalize env t  = Forall as t
  where as = S.toList $ ftv t `S.difference` ftv env

unifyCompose :: [(Subst, QType)] -> Infer (Subst, QType)
unifyCompose ts = foldr foldFun start ts
  where foldFun (s', t') m = do
          (s, t) <- m
          sn <- unify t t'
          return (s `compose` s' `compose` sn, apply sn t')
        start = do
          tv <- fresh
          return (nullSubst, tv)

infer :: TypeEnv -> PExp -> Infer (Subst, QType)
infer env ex = case ex of
  PVar x -> lookupEnv env x

  PLambda x e -> do
    tv <- fresh
    let env' = env `extend` (x, Forall [] tv)
    (s1, t1) <- infer env' e
    return (s1, apply s1 tv `QTFun` t1)

  PFunApp e1 e2 -> do
    tv <- fresh
    (s1, t1) <- infer env e1
    (s2, t2) <- infer (apply s1 env) e2
    s3       <- unify (apply s2 t1) (QTFun t2 tv)
    return (s3 `compose` s2 `compose` s1, apply s3 tv)

  PLetCase x e es -> do
    (s1, t1) <- infer env e
    let env' = apply s1 env
        t'   = generalize env' t1
    q <- case t' of
      Forall [] (QTMeasuredQubits q) -> return q
      otherwise                      -> throwError $ InvalidLetCaseVar t1
    when ((2^q) /= (length es)) (throwError $ InvalidLetCaseNumCases (length es) (2^q))
    res <- sequenceA (infer (env' `extend` (x, t')) <$> es)
    (ss, ts) <- unifyCompose res
    return (s1 `compose` ss, ts)

  PTimes e1 e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer env e2
    tl <- fresh
    tr <- fresh
    tv <- fresh
    s3 <- case (t1, t2) of
      (QTQubits q1, QTQubits q2) -> unify (QTQubits (q1+q2)) tv
      otherwise                  -> return nullSubst
    s4 <- unify (QTFun t1 (QTFun t2 tv)) (QTFun tl (QTFun tr tv))
    return (s1 `compose` s2 `compose` s3 `compose` s4, apply s4 $ apply s3 tv)

  PProjector e -> do
    (s, t) <- infer env e
    tv <- fresh
    s'<- case t of
      QTQubits q -> unify (QTMeasuredQubits q) tv
      otherwise  -> return nullSubst
    return (s `compose` s', apply s' tv)

  PGate g e -> do
    (s, t) <- infer env e
    tv <- fresh
    s' <- unify t tv
    return (s `compose` s', apply s' tv)

  PQubits q -> return (nullSubst, QTQubits (T.length q))

lookupEnv :: TypeEnv -> T.Text -> Infer (Subst, QType)
lookupEnv (TypeEnv env) x = do
  case M.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)
