module Typing.Solver where

import           Control.Monad.Extra
import           Data.Matrix               as M
import           Data.Ratio
import           Data.Vector               as V
import           Numeric.LinearProgramming
import           Typing.Smith

data VarSol a = Value a | AnyValue
  deriving (Eq, Show)

unVarSol :: VarSol a -> Maybe a
unVarSol AnyValue  = Nothing
unVarSol (Value x) = Just x

-- solves AX=B, X>=1 if there's a solution
solve :: Integral a => Matrix a -> Vector a -> Maybe (Vector a)
solve a b = do
  let (d, (p, q))  = getSmithForm a
      pb = getMatrixAsVector (multStrassenMixed p (M.colVector b))
  y <- solveForDiagInt d pb
  y1 <- unValueList (V.take (nonFreeVarCount y) y)
  if nonFreeVarCount y == V.length y then
    checkSol (getMatrixAsVector (multStrassenMixed q (M.colVector y1)))
    else do
      let q2 = submatrix 1 (nrows q) (1 + V.length y1) (ncols q) q
          q1y1' = if V.length y1 > 0 then q1y1 else M.zero (nrows q2) 1
            where q1 = submatrix 1 (nrows q) 1 (V.length y1) q
                  q1y1 = multStrassenMixed q1 (M.colVector y1)
          m1q1y1 = getMatrixAsVector $ (\x -> 1-x) <$> q1y1'
          minimize = V.fromList $ V.foldl (+) 0 <$> ((flip getCol) q2) <$> [1..(ncols q2)]
      y2 <- solveInequality q2 m1q1y1 minimize
      let q2y2 = multStrassenMixed (fromIntegral <$> q2) (ceiling <$> M.colVector y2)
          x = getMatrixAsVector $ q1y1'+q2y2
      checkSol x

checkSol :: Integral a => Vector a -> Maybe (Vector a)
checkSol x = do
  when (not (V.all (>=1) x)) (fail "No solution")
  return x

-- solves AX>=B if there's a solution
solveInequality ::Integral a => Matrix a -> Vector a -> Vector a -> Maybe (Vector Double)
solveInequality a b m = V.fromList <$> convert (simplex prob constraints bounds)
  where
    bounds =  Free <$> [1..ncols a]
    a' = M.toLists (fromIntegral <$> a)
    b' = V.toList (fromIntegral <$> b)
    makeEq (row_i, b_i) = row_i :>=: b_i
    constraints = Dense $ makeEq <$> Prelude.zip a' b'
    prob = Minimize (V.toList $ fromIntegral <$> m)
    convert (Feasible (_, s)) = Just s
    convert (Optimal (_, s))  = Just s
    convert _                 = Nothing

unValueList :: Vector (VarSol a) -> Maybe (Vector a)
unValueList = V.sequence . (V.map unValue)
  where
    unValue (Value a) = Just a
    unValue AnyValue  = Nothing

nonFreeVarCount :: Eq a => Vector (VarSol a) -> Int
nonFreeVarCount = V.length . V.filter (/=AnyValue)

-- solves DX=B with D diagonal, X in Z^n if there's a solution
solveForDiagInt :: Integral a => Matrix a -> Vector a -> Maybe (Vector (VarSol a))
solveForDiagInt d b = do
  when (not solveBottom) (fail "No solution")
  V.sequence $ (V.map solveDiag (V.zip (getDiag d) b) V.++ solveRight)
  where
    solveDiag (d_i, b_i) | d_i==0 && b_i/=0 = Nothing
                        | d_i==0 && b_i==0 = Just $ AnyValue
                        | d_i `divides` b_i = Just $ Value (b_i `div` d_i)
                        | otherwise = Nothing
    solveRight = V.replicate (max 0 (ncols d - nrows d)) (Just AnyValue)
    solveBottom = ncols d >= nrows d || V.all (==0) (V.slice (ncols d) (nrows d - ncols d) b)
