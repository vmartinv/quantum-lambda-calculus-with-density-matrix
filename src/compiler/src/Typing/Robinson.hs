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
import qualified Data.Vector as V
import Numeric.LinearProgramming




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
unifies (EqualTypeEq (QTMeasuredQubits n1 t1) (QTMeasuredQubits n2 t2)) | n1==n2 =
      unifies (EqualTypeEq t1 t2)
unifies (EqualTypeEq t1 t2)                       = throwError $ UnificationFail t1 t2
unifies (SumSizeEq ls q)                       =
  assertQubitOrVar q >> sequence_ (assertQubitOrVar <$> ls) >> return emptySubst
unifies (AtLeastSizeEq ls q)                       =
  sequence_ (assertQubitOrVar <$> ls) >> return emptySubst

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
  su1 <- dprint "unifyMany" $ unifyMany $ dprint "robinsonEqs" $ eqs
  let eqs' = apply su1 eqs
  su2 <- dprint "solveSumsAndInequalities" $ solveSumsAndInequalities eqs'
  verifyEqsAfterSubst eqs' su2
  return $ dprint "robinsonRet" $ su1 `compose` su2

-- given a set of variables adds all the defined sizes together
-- and puts the variables apart
simplify :: [QType] -> (Int, [VariableId])
simplify eqs = (q, [v | QTVar v <- eqs])
  where
    q = sum [q | QTQubits q <- eqs]


getLinearEquations :: [TypeEq] -> Constraints
getLinearEquations eqs = Sparse $ catMaybes $ makeRow <$> eqs
  where
    setValue value var = value#(1+var)
    makeRow :: TypeEq -> Maybe (Bound [(Double, VariableId)])
    makeRow (SumSizeEq ls r) =
        Just (row :==: fromIntegral (qr-ql))
      where (ql, vl) = simplify ls
            (qr, vr) = simplify [r]
            row = (setValue 1 <$> vl) ++ (setValue (-1) <$> vr)
    makeRow (AtLeastSizeEq ls qr) =
        Just (row :>=: fromIntegral (qr-ql))
      where (ql, vl) = simplify ls
            row = (setValue 1 <$> vl)
    makeRow _eq = Nothing

-- given the sum equations returns a map from variables to their assigned size
solveSumsAndInequalities :: [TypeEq] -> ExceptInfer Subst
solveSumsAndInequalities eqs | not (S.null (ftv eqs)) = do
  let numCols = S.findMax (ftv eqs) + 1
      linEq = getLinearEquations $ dprint "solveSums" $ eqs
  sol <- solveLinear numCols linEq eqs
  unifyMany [EqualTypeEq (QTVar v) (QTQubits q) | (v, q) <- (zip [0..] sol), v `S.member` (ftv eqs)]
solveSumsAndInequalities _ = return M.empty

getQubitSize :: QType -> Maybe Int
getQubitSize (QTQubits q) = return q
getQubitSize t            = Nothing

verifyEq :: TypeEq -> Maybe ()
verifyEq (AtLeastSizeEq ls right) = do
  left <- sum <$> sequence (getQubitSize <$> ls)
  if (left>=right)
    then Just ()
    else Nothing
verifyEq (SumSizeEq ls q) = do
  left <- sum <$> sequence (getQubitSize <$> ls)
  right <- getQubitSize q
  if (left==right)
    then Just ()
    else Nothing
verifyEq _ = Just ()

verifyEqsAfterSubst :: [TypeEq] -> Subst -> ExceptInfer ()
verifyEqsAfterSubst eqs su =
  maybe err return verifyMaybe
  where
      err = throwError $ InvalidOperatorSizesCheckFailed eqs su
      verifyMaybe = sequence_ $ verifyEq <$> (apply su eqs)

solveLinear :: Int -> Constraints -> [TypeEq] -> ExceptInfer [Int]
solveLinear numCols constraints eqs = convert (simplex optimization constraints varBounds) >>= roundsOrFails
  where
    varBounds = [i :>=: 1 | i <- [1..numCols]]
    optimization = Minimize (replicate numCols 1)
    convert :: Solution -> ExceptInfer [Double]
    convert (Feasible (_, s)) = return s
    convert (Optimal (_, s))  = return s
    convert _                 = throwError $ InvalidOperatorSizesNoSolution eqs
    roundOrFail :: Double -> ExceptInfer Int
    roundOrFail x = when (x /= fromInteger (round x)) (throwError $ InvalidOperatorSizesNotIntegerSolution x eqs) >> return (round x)
    roundsOrFails :: [Double] -> ExceptInfer [Int]
    roundsOrFails xs = sequence (roundOrFail <$> xs)







