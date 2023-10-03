{-# LANGUAGE TupleSections #-}
module Typing.Robinson where

import           CompilerError
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.Map             as M
import           Data.Maybe
import qualified Data.Set             as S
import           Typing.QType
import           Typing.Subst
import           Typing.TypeEq
import           Utils

-- returns a susbstitution to replace a variable with a given type
bind :: VariableId -> QType -> ExceptInfer Subst
        -- unreachable vecause we cannot reuse variables
bind a t | t == QTVar a     = return emptySubst
        -- unreachable because is not possible to create infinite types
       | a `S.member` ftv t = throwError $ InfiniteType a t
       | otherwise       = return $ M.singleton a t

-- returns a substitutions that unifies the given two types
unifies :: TypeEq -> ExceptInfer Subst
unifies (EqualTypeEq t1 t2)                       | t1 == t2 = return emptySubst
unifies (EqualTypeEq (QTVar v) t)                 = v `bind` t
unifies (EqualTypeEq t (QTVar v))                 = v `bind` t
unifies (EqualTypeEq (QTFun t1 t2) (QTFun t3 t4)) = unifyMany [EqualTypeEq t1 t3, EqualTypeEq t2 t4]
unifies (EqualTypeEq t1 t2)                       = throwError $ UnificationFail t1 t2
unifies (SumSizeEq ls q)                       =
  assertQubitOrVar q >> sequence_ (assertQubitOrVar <$> ls) >> return emptySubst
unifies (AtLeastSizeEq ls q)                       =
  assertQubitOrVar q >> sequence_ (assertQubitOrVar <$> ls) >> return emptySubst

assertQubitOrVar :: QType -> ExceptInfer ()
assertQubitOrVar (QTVar _)    = return ()
assertQubitOrVar (QTQubits _) = return ()
assertQubitOrVar t            = throwError $ TypeNotQubits t

-- like unifies but for list, composes the substitutions
unifyMany :: [TypeEq] -> ExceptInfer Subst
unifyMany typeEqs = foldM f emptySubst (reverse typeEqs)
  where f su eq = compose su <$> unifies (apply su eq)

-- core algorithm
robinson :: [TypeEq] -> ExceptInfer Subst
robinson eqs = do
  su1 <- unifyMany $ dprint "robinsonEqs" $ eqs
  let eqs' = apply su1 eqs
  sol <- solveSums eqs'
  su2 <- assignValues $ dprint "robinsonSol" $ sol
  return $ su1 `compose` su2

-- given the solution to the sum of equations
-- it creates the corresponding substition based on them
assignValues :: M.Map VariableId Int -> ExceptInfer Subst
assignValues sol = foldr compose emptySubst <$> sequence (bindv <$> M.toList sol)
  where
    bindv (v, q) = v `bind` QTQubits q

-- Given that the sum equations form a tree and are given in topological order
-- this function transforms the equations into a flat tree
-- it does a map over the list of equations while keeping a state
flattenTree :: [TypeEq] -> [TypeEq]
flattenTree eqs = evalState (sequence (f <$> eqs)) M.empty
  where
    expand m (QTVar v) = fromMaybe [QTVar v] (M.lookup v m)
    expand m q         = [q]
    getVars ls m = concat $ expand m <$> ls
    f :: TypeEq -> State (M.Map VariableId [QType]) TypeEq
    f (SumSizeEq ls (QTVar v)) = do
      ls' <- gets (getVars ls)
      modify (M.insert v ls')
      return $ SumSizeEq ls' (QTVar v)
    f (AtLeastSizeEq ls q) = do
      ls' <- gets (getVars ls)
      return $ AtLeastSizeEq ls' q
    f eq = return eq

-- this function should fail for other types
typeSize :: QType -> Int
typeSize (QTQubits q)         = q

-- given the sum equations returns a map from variables to their assigned size
solveSums :: [TypeEq] -> ExceptInfer (M.Map VariableId Int)
solveSums eqs = do
  let eqs' = flattenTree $ dprint "solveSums" $ eqs
      -- by default all variables are set to 1
      all1 =
        M.fromList ((,1) <$> S.toList (ftv eqs))
  su <- foldM fstPass all1  $ dprint "solveSums'" $ eqs'
  return (foldl sndPass su eqs')
  where
    -- First pass solves the equations
    fstPass :: M.Map VariableId Int -> TypeEq -> ExceptInfer (M.Map VariableId Int)
    fstPass su (SumSizeEq ls (QTVar v)) = return su
    fstPass su (AtLeastSizeEq ls q) = do
      maybeSol <- solveSingleIneq ls (typeSize q)
      let addSol = (flip (uncurry M.insert)) su
      return $ maybe su addSol maybeSol
    fstPass su _ = return su

    -- Second pass sets the dependent variables
    sndPass :: M.Map VariableId Int -> TypeEq -> M.Map VariableId Int
    sndPass su (SumSizeEq ls (QTVar v)) = M.insert v q su
      where
        -- we use withDefault to avoid complaining, but it should never happen
        -- because all variables are set to 1 already
        getVal (QTVar v) = M.findWithDefault 1 v su
        getVal q         = typeSize q
        q = sum (getVal <$> ls)
    sndPass su _ = su

-- given a set of variables adds all the defined sizes together
-- and puts the variables apart
simplify :: [QType] -> (Int, [VariableId])
simplify eqs = (q, [v | QTVar v <- eqs])
  where
    q = sum [q | QTQubits q <- eqs]

-- finds v_1,...v_n / v_1+...+v_n+q_f>=q_t, if it exists
-- by fixing v_2,..., v_n to 1 and returning (v_1, value)
solveSingleIneq :: [QType] -> Int -> ExceptInfer (Maybe (VariableId, Int))
solveSingleIneq vars qt | qf < qt && null vs = throwError InvalidOperatorSizes
                        | null vs = return Nothing --solution=no vars
                        | otherwise = return (Just sol)
  where
    (qf, vs) = simplify vars
    v = head vs
    sol = (v, max 1 (qt - qf - length vs + 1))