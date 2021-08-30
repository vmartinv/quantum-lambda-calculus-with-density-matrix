module Typing.TypeChecker where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Coerce
import           Data.Either.Combinators
import qualified Data.Map                as M
import qualified Data.Matrix             as Matrix
import qualified Data.Set                as S
import qualified Data.Text               as T
import           Data.Tuple.Extra
import qualified Data.Vector             as V
import qualified Data.Vector.Mutable     as MV
import           Debug.Trace
import           Parser
import           Typing.QType
import           Typing.Solver
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
  let sumEqs = [(ls, r) | SumSizeEq ls r <- eqs']
      measured = S.fromList [ v | IsMeasuredQubits (QTVar v) <- eqs']
  su2 <- solveSums sumEqs measured
  let su = su1 `compose` su2
  sequenceA $ classCheck 2 <$> (apply su eqs)
  return su

solveSums :: [([QType], QType)] -> S.Set VariableId -> ExceptInfer Subst
solveSums sumEqs measured = do
    let flatten (ls, r) = reverse (r:ls)
        varList = concat (flatten <$> sumEqs)
    let edgeList = createEdgeList sumEqs
        invertEdge (p, q) = (q, p)
        adjacencyList = createAdj edgeList
        invertedAdj = createAdj (invertEdge <$> edgeList)
        nodes = M.keys adjacencyList
        lowerBounds = calcLowerBound adjacencyList <$> nodes
        upperBounds = calcUpperBound adjacencyList <$> nodes
        leafs = M.keys $ M.filter (==[]) invertedAdj
        solveRec leafs lowerBounds upperBounds adjacencyList
INFINITE :: Int
INIFINITE = 1e9

solveRec leafs lowerBounds upperBounds adjacencyList =
  maybe tryAnotherValue adaptSolution subProblem
  where
    problem v = 
    tryAnotherValue = problem (l+1) h
    subProblem = solveRec (tail leafs) lowerBounds upperBounds adjacencyList
    adaptSolution

calcUpperBound :: M.Map Int Int -> Int -> Int
calcUpperBound v m | v<0 = -v
                   -- | (size (m M.! v)) == 0 = INFINITE
                   | otherwise = maximum（calcUpperBound m <$> (m M.! v))

 calcLowerBound :: M.Map Int Int -> Int -> Int
 calcLowerBound v m | v<0 = -v
                    | (size (m M.! v)) == 1 = head (calcLowerBound m <$> (m M.! v))
                    | otherwise = 1

createEdgeList :: [([QType], QType)] -> (Int, Int)
createEdgeList sumEqs = concat (eqToEdges <$> sumEqs)
  where
        getVar (QTVar v) = v
        getVar (QtQubits q) = -q
        getEdge p q = (getVar p, getVar q)
        eqToEdges (ls, r) = (flip getEdge) r <$> ls

createAdj :: [(Int, Int)] -> M.Map Int [Int]
createAdj edgeList = M.fromListWith (++) (conv <$> edgeList)
  where
        conv (p, q) = (p, [q])



addSolution :: Subst -> MatrixEnv -> S.Set VariableId -> V.Vector Int -> ExceptInfer Subst
addSolution (MatrixEnv var2Ints) measureds solution = foldr compose emptySubst (mapM f var2Cols)
  where
      f var = var `bind` (kind sol)
        where sol = solution V.! (M.findWithDefault (-1) var var2Cols)
              kind = if var `S.member` measureds then QTMeasuredQubits else QTQubits


              -- var2Ints <- execStateT (mapM createMap varList) (MatrixEnv M.empty)
              -- let rows = createRow var2Ints <$> sumEqs
              --     system = (fromListsFixed *** V.fromList) (unzip rows)
              --     solutionM = traceShow ("solving system", "sumEqs=", sumEqs, "system=", system, "su1=", su1) $ solve system
              -- solution <- maybe (throwError $ InvalidOperatorSizes) return solutionM
              -- addSolution var2Ints measured solution

createRow :: MatrixEnv -> ([QType], QType) -> ([Int], Int)
createRow (MatrixEnv var2Cols) (ls, r) = (V.toList $ V.slice 0 numCols processed, processed V.! numCols)
  where
        numCols = (M.size var2Cols):: Int
        addFalse t = (False, t)
        flattened = (True, r):(addFalse <$> ls)
        processed = V.accum (+) (V.replicate (numCols+1) 0) (processItem <$> flattened)
        processItem :: (Bool, QType) -> (Int, Int)
        processItem (False, (QTQubits q)) = (numCols, -q)
        processItem (True, (QTQubits q)) = (numCols, q)
        processItem (False, (QTVar v)) = (M.findWithDefault (-1) v var2Cols, 1)
        processItem (True, (QTVar v)) = (M.findWithDefault (-1) v var2Cols, -1)

createMap :: QType -> StateT MatrixEnv ExceptInfer ()
createMap (QTQubits _) = return ()
createMap (QTVar v)    = modify addVar
  where
      dontOverride = curry snd
      addVar (MatrixEnv m) = MatrixEnv $ M.insertWith dontOverride v (M.size m) m
createMap t            = throwError $ BadSumEqSystem t
