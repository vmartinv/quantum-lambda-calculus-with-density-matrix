module Typing.DensMatrixChecker where

import           Control.Monad.Except
import           Data.Complex
import           Typing.TypeError
import           Utils

getMatrixSize :: [[Complex Double]] -> ExceptInfer Int
getMatrixSize m = do
  let n = length m
  -- the parser already guarantees that is not empty
  when (not $ all (==n) (length <$> m)) (throwError $ MatrixIsNotSquare m)
  -- validate number of rows is a power of two
  let q = log2 n
  when ((2^q) /= n) (throwError $ MatrixIsNotAPowerOfTwo m)
  when (q==0) (throwError $ MatrixHasZeroQubits m)
  when (q>=64) (throwError $ MatrixExceedsMaxSize 64 m)
  return q
