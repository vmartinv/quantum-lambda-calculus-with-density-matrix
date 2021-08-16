
module Typing.SolverOld where

import           Control.Monad.Extra
import           Data.Matrix               as M
import           Data.Ratio
import           Data.Vector               as V
import           Debug.Trace
import           Numeric.LinearProgramming
import           Typing.Smith
import Data.Maybe
import Control.Exception
import Typing.Utils

-- solves AX=B, X>=1 if there's a solution
solve :: Show a => Integral a => Matrix a -> V.Vector a -> Maybe (V.Vector a)
solve a b = do
  let (d, (p, q))  = traceShow ("solving", "A=", a, "B=", b) $ getSmithForm a
      pb = traceShow ("D=", d, ("P=", p, "Q=", q)) $ (getMatrixAsVector (multStrassenMixed p (M.colVector b)))
  y <- traceShow pb $ solveForDiagInt d pb
  y1 <- traceShow ("y=", y, "nonFreeCount y=", nonFreeVarCount y, "Dy=", multStrassenMixed d (M.colVector (fromMaybe 0 <$> unVarSol <$> y)), "AQY=", multStrassenMixed a (multStrassenMixed q (M.colVector (fromMaybe 0 <$> unVarSol <$> y)))) $ unValueList (V.take (nonFreeVarCount y) y)
  if nonFreeVarCount y == V.length y then
    checkSol (getMatrixAsVector (multStrassenMixed q (M.colVector y1)))
    else do
      let q2 = submatrix 1 (nrows q) (1 + V.length y1) (ncols q) q
          q1y1' = if V.length y1 > 0 then q1y1 else M.zero (nrows q2) 1
            where q1 = submatrix 1 (nrows q) 1 (V.length y1) q
                  q1y1 = multStrassenMixed q1 (M.colVector y1)
          m1q1y1 = traceShow ("Q2=", q2) $ getMatrixAsVector $ (\x -> 1-x) <$> q1y1'
          minimize = traceShow ("1-Q1Y1=", m1q1y1) $ V.fromList $ V.foldl (+) 0 <$> ((flip getCol) q2) <$> [1..(ncols q2)]
      y2 <- traceShow minimize $ solveInequality q2 m1q1y1 minimize
      let q2y2 = multStrassenMixed (fromIntegral <$> q2) (ceiling <$> M.colVector y2)
          q2y2Ceil = multStrassenMixed (fromIntegral <$> q2) ((\x->fromIntegral (ceiling x) - x) <$> M.colVector y2)
          x =  traceShow ("Y1=", y1, "Q1Y1'=", q1y1', "A Q1Y1'=", multStrassenMixed a q1y1', q2y2) $ getMatrixAsVector $ q1y1'+q2y2
      -- if (not (V.all (>=0) (getMatrixAsVector q2y2Ceil))) then
      -- if (not ((Prelude.all (>=0) . (Prelude.map (Prelude.foldl (+) 0)) . M.toLists . M.transpose) q)) then
      --   return (V.fromList [0])
      --   else
        -- let x = traceShow (multStrassenMixed (fromIntegral <$> q2) (M.colVector y2), multStrassenMixed qDouble yDouble, multStrassenMixed (fromIntegral <$> a) (multStrassenMixed qDouble yDouble)) $
        --   ceiling <$> getMatrixAsVector (multStrassenMixed qDouble yDouble)
      return x

checkSol :: Integral a => V.Vector a -> Maybe (V.Vector a)
checkSol x = do
  when (not (V.all (>=1) x)) (fail "No solution")
  return x

-- solves AX>=B if there's a solution
solveInequality :: Show a => Integral a => Matrix a -> V.Vector a -> V.Vector a -> Maybe (Vector Double)
solveInequality a b m = V.fromList <$> convert (simplex prob constraints bounds)
  where
    bounds =  Free <$> [1..ncols a]
    a' = M.toLists (fromIntegral <$> a)
    b' = V.toList (fromIntegral <$> b)
    makeEq (row_i, b_i) = row_i :>=: b_i
    constraints = traceShow ("solving simplex", "a=", a, "b=", b, "m=", m, "bounds=", bounds) $  Dense $ makeEq <$> Prelude.zip a' b'
    prob = Minimize (V.toList $ fromIntegral <$> m)
    convert (Feasible (_, s)) = Just s
    convert (Optimal (_, s))  = Just s
    convert _                 = Nothing

unValueList :: Vector (VarSol a) -> Maybe (V.Vector a)
unValueList = V.sequence . (V.map unValue)
  where
    unValue (Value a) = Just a
    unValue AnyValue  = Nothing

nonFreeVarCount :: Eq a => Vector (VarSol a) -> Int
nonFreeVarCount = V.length . V.filter (/=AnyValue)

-- solves DX=B with D diagonal, X in Z^n if there's a solution
solveForDiagInt :: Integral a => Matrix a -> V.Vector a -> Maybe (Vector (VarSol a))
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
