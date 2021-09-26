module Typing.SolverBad where

import Data.Matrix as M
import qualified Data.Vector as V
import Debug.Trace
import Numeric.LinearProgramming
import Typing.Utils

-- solves max x, Ax=b, x>=1, A totally unimodular, if there's a solution
solve :: Show a => Integral a => Num a => LinSystem a -> Maybe (V.Vector a)
solve (a, b) | ncols a == 0 = return V.empty
             | otherwise = V.map (+1) <$> solveInequality (a, b')
  where
      delta = V.fromList $ (foldr (+) 0) <$> M.toLists a
      b' = V.zipWith (-) b delta

-- solves max x, Ax>=b, x>=0, A totally unimodular, if there's a solution
solveInequality :: Show a => Integral a => LinSystem a -> Maybe (V.Vector a)
solveInequality (a, b) = V.fromList <$> map (fromIntegral. ceiling) <$> (traceShowId $ convert (simplex prob constraints bounds))
  where
    bounds = []-- Free <$> [1..ncols a]
    a' = M.toLists (fromIntegral <$> a)
    b' = V.toList (fromIntegral <$> b)
    makeEq (row_i, b_i) = row_i :==: b_i
    constraints = traceShow ("solving simplex", "a=", a, "b=", b) $  Dense $ makeEq <$> zip a' b'
    prob = Minimize (replicate (ncols a) 1)
    convert (Feasible (_, s)) = Just s
    convert (Optimal (_, s))  = Just s
    convert _                 = Nothing
