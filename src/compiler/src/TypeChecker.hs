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

data TypeError  = UnificationFail QType QType | InfiniteType T.Text QType | UnboundVariable T.Text
            deriving (Show,Eq)

newtype TypeEnv = TypeEnv (M.Map T.Text Scheme)
type Infer a = ExceptT TypeError (State TypeState) a
type Subst = M.Map T.Text QType

extend :: TypeEnv -> (T.Text, Scheme) -> TypeEnv
extend (TypeEnv env) (x, s) = TypeEnv $ M.insert x s env

initTypeState = 0

closeOver (s, t) = Forall (S.toList (ftv t')) t'
  where t' = apply s t

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

  ftv (QTQubits _)         = S.empty
  ftv (QTMeasuredQubits _) = S.empty
  ftv (QTVar a)            = S.singleton a
  ftv (t1 `QTFun` t2)      = ftv t1 `S.union` ftv t2

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
  return.QTVar $ letters !! s

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

bind ::  T.Text -> QType -> Infer Subst
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

  -- data PExp = PVar Text
  --          | PLambda Text PExp
  --          | PFunApp PExp PExp
  --          | PQubits Text
  --          | PGate Text PExp
  --          | PProjector PExp
  --          | PTimes PExp PExp
  --          | PLetCase Text PExp [PExp]
  --          deriving (Show,Eq)

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
    (ss, ts) <- unzip <$> sequenceA (infer (env' `extend` (x, t')) <$> es)

    return (foldr compose nullSubst ss, ts !! 0)

  PTimes e1 e2 -> do
    (s1, t1) <- infer env e1
    (s2, t2) <- infer env e2
    tv <- fresh
    s3 <- unify (QTFun t1 (QTFun t2 tv)) (QTFun (QTQubits 1) (QTFun (QTQubits 1) tv))
    return (s1 `compose` s2 `compose` s3, apply s3 tv)

  PProjector e -> do
    (s, t) <- infer env e
    tv <- fresh
    s' <- unify (QTFun t tv) (QTFun (QTQubits 1) (QTMeasuredQubits 1))
    return (s `compose` s', apply s' tv)

  PGate g e -> do
    (s, t) <- infer env e
    tv <- fresh
    s' <- unify (QTFun t tv) (QTFun (QTQubits 1) (QTQubits 1))
    return (s `compose` s', apply s' tv)

  PQubits q -> return (nullSubst, QTQubits 1) -- TODO set number

lookupEnv :: TypeEnv -> T.Text -> Infer (Subst, QType)
lookupEnv (TypeEnv env) x = do
  case M.lookup x env of
    Nothing -> throwError $ UnboundVariable x
    Just s  -> do t <- instantiate s
                  return (nullSubst, t)
    -- data QType = QTQubits Int
    --         | QTMeasuredQubits Int
    --         | QTFun QType QType
    --         | QTVar Text
    --         deriving (Eq, Ord)
--
-- typeCheck :: PExp -> Either T.Text QType
-- typeCheck = typeCheckEnv M.empty
--
-- typeCheckEnv :: QTEnv -> PExp -> Either T.Text QType
-- typeCheckEnv env (PVar v) = maybeToRight "Undefined var" (M.lookup v env)
-- typeCheckEnv env (PLambda v exp) = Left "Not finished1" --TODO
-- typeCheckEnv env (PFunApp exp1 exp2) = Left "Not finished2" --TODO
-- typeCheckEnv env (PQubits qubits) = Right $ QTQubits (T.length qubits)
-- typeCheckEnv env (PGate g exp) = whenRight (typeCheckEnv env exp) validate
--   where validate t@(QTQubits _) = Right t
--         validate _              = Left "invalid gate"
-- typeCheckEnv env (PProjector exp) = whenRight (typeCheckEnv env exp) validate
--   where validate (QTQubits n) = Right (QTMeasuredQubits n)
--         validate _            = Left "invalid projector"
-- typeCheckEnv env (PTimes exp1 exp2) = do
--   texp1 <- typeCheckEnv env exp1
--   texp2 <- typeCheckEnv env exp2
--   validate texp1 texp2
--   where
--       validate (QTQubits n1) (QTQubits n2) = Right (QTQubits (n1+n2))
--       validate _ _                         = Left "times invalid type"
-- typeCheckEnv env (PLetCase v exp exps) = do
--   texp <- typeCheckEnv env exp
--   szVar <- validateVar texp
--   texps <- sequenceA (typeCheckEnv (M.insert v (QTQubits szVar) env) <$> exps)
--   validateCases texps
--   where
--       validateVar (QTMeasuredQubits n) = Right n
--       validateVar _                    = Left "invalid letcase var exp"
--       validateCases texps = Left "Not finished3"  --TODO
