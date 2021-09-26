module Typing.TypeChecker where

import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Coerce
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Text            as T
import           Data.Tuple.Extra
import           Debug.Trace
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
              | InvalidType QType
              | InvalidLetCaseVar QType
              | InvalidLetCaseNumCases Int
              | TypeNotQubits QType Int
              | TypeNotMeasuredQubits QType Int
              | UnificationMismatch [QType] [QType]
              | BadSumEqSystem QType
              | InvalidOperatorSizes
              deriving (Show,Eq)

newtype TypeEnv = TypeEnv (M.Map T.Text QType)
newtype MatrixEnv = MatrixEnv (M.Map VariableId Int)
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
    return (tv, eq++[TypeEq tv t, IsQubits t, IsQubits tv, TypeEq tv (QTQubits 2), SumSizeEq [t] tv])

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

classCheck :: Int -> TEq -> ExceptInfer ()
classCheck _ (IsQubits (QTQubits _)) = return ()
classCheck _ (IsQubits (QTVar _)) = return ()
classCheck step (IsQubits t) = throwError $ TypeNotQubits t step
classCheck _ (IsMeasuredQubits (QTMeasuredQubits _)) = return ()
classCheck _ (IsMeasuredQubits (QTVar _)) = return ()
classCheck step (IsMeasuredQubits t) = throwError $ TypeNotMeasuredQubits t step
classCheck _ _ = return ()

robinson :: [TEq] -> ExceptInfer Subst
robinson eqs = do
  su1 <- uncurry unifyMany $ unzip [ (t1, t2) | TypeEq t1 t2 <- eqs]
  let eqs' = apply su1 eqs
  sequenceA $ classCheck 1 <$> eqs'
  let sumEqs = dprint "sumEqs" $ [(ls, r) | SumSizeEq ls r <- eqs']
      measured = S.fromList [ v | IsMeasuredQubits (QTVar v) <- eqs']
  su2 <- solveSums sumEqs measured
  let su = su1 `compose` su2
  sequenceA $ classCheck 2 <$> apply su eqs
  return su

dprint s v = traceShow (s, v) v

solveSums :: [([QType], QType)] -> S.Set VariableId -> ExceptInfer Subst
solveSums sumEqs measureds = do
    let flatten (ls, r) = reverse (r:ls)
        varList = [v | QTVar v <- concat (flatten <$> sumEqs)]
        (vars, childs) = dprint "getChildren" $ getChildren sumEqs
        parents = dprint "getParents" $ getParents childs
        unknowns = dprint "unknowns" $ M.keys parents
        solM = dprint "solveRec" $ solveRec unknowns childs parents
    solution <- maybe (throwError $ InvalidOperatorSizes) return solM
    let set1 x = (x, 1)
        preSol = (M.fromList (zip unknowns solution)) `M.union` (M.fromList (set1 <$> varList))
        calc (v, (q, xs)) m = M.insert v (q + (foldr (+) 0 (getValue <$> S.toList xs))) m
          where
            getValue x = M.findWithDefault 0 x m
        fullSol = foldr calc preSol (reverse vars)
    substSolution measureds fullSol

substSolution :: S.Set VariableId -> M.Map VariableId Int -> ExceptInfer Subst
substSolution measureds solution = foldr compose emptySubst <$> (mapM f (M.toList solution))
  where
      f (var, sol) = var `bind` (kind sol)
        where kind = if var `S.member` measureds then QTMeasuredQubits else QTQubits

solveRec :: [VariableId] -> M.Map Int (Int, S.Set VariableId) ->  M.Map VariableId (S.Set Int) -> Maybe [Int]
solveRec (x:unknowns) childs parents = tryVal l
  where
    getBounds p | S.size xs == 1 = (1 `max` q, q)
                | otherwise = (1, q - S.size xs + 1)
      where
          (q, xs) = M.findWithDefault (0, S.empty) p childs
    maxV = foldr max 1 (fst.snd <$> M.toList childs)
    combineBound (l1, h1) (l2, h2) = (l1 `max` l2, h1 `min` h2)
    combineBounds = foldr combineBound (1, maxV)
    xparents = S.toList (M.findWithDefault S.empty x parents)
    (l, h) = combineBounds $ getBounds <$> xparents
    tryVal v | v>h = Nothing
             | otherwise = ((v:) <$> subsol) <|> (tryVal (v+1))
      where
        updater (q, xs) = (q-v, S.delete x xs)
        mapUpdater p = M.adjust updater p
        childs' = foldr mapUpdater childs xparents
        subsol = solveRec unknowns childs' parents
solveRec [] childs parents | all (==(0, S.empty)) (M.elems childs) = Just []
                           | otherwise = Nothing

getChildren :: [([QType], QType)] -> ([(VariableId, (Int, S.Set VariableId))], M.Map Int (Int, S.Set VariableId))
getChildren sumEqs = convert $ foldr eqToAdj (M.empty, [], []) (zip [1..] sumEqs)
  where
        convert (a, a', b) = (a', M.fromList b)
        getVar m (QTVar v) = M.findWithDefault (0, S.singleton v) v m
        getVar m (QTQubits q) = (q, S.empty)
        getVar m (QTMeasuredQubits q) = (q, S.empty)
        combine (q1, xs1) (q2, xs2) = (q1+q2, xs1 `S.union` xs2)
        combineLst = foldr combine (0, S.empty)
        eqToAdj (idx, (ls, (QTVar v))) (vars, varsL, children) = (vars', varsL', children)
          where
            ls' = combineLst $ getVar vars <$> ls
            vars' = M.insert v ls' vars
            varsL' = (v, ls'):varsL
        eqToAdj (idx, (ls, r)) (vars, varsL, children) = (vars, varsL, children')
          where
            (q, ls') = combineLst $ getVar vars <$> ls
            (rq, _) = getVar vars r
            children' = (idx, (rq-q, ls')):children

getParents :: M.Map Int (Int, S.Set VariableId) -> M.Map VariableId (S.Set Int)
getParents children = foldr processEdge M.empty edgeList
  where
    edge idx x = (x, idx)
    edges (idx, (_q, xs)) = edge idx <$> (S.toList xs)
    edgeList = concat $ edges <$> M.toList children
    processEdge (a, b) = M.insertWith (S.union) a (S.singleton b)
