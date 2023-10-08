module Typing.MatrixChecker where

import           CompilerError
import           Control.Monad.Except
import           Data.Complex
import           Utils
import Data.Matrix
import qualified Numeric.LinearAlgebra.HMatrix as HM

getMatrixSize :: [[Complex Double]] -> ExceptInfer Int
getMatrixSize m = do
  let n = length m
  -- the parser already guarantees that is not empty
  when (not $ all (==n) (length <$> m)) (throwError $ MatrixIsNotSquare m)
  -- validate number of rows is a power of two
  let q = log2 n
  when ((2^q) /= n) (throwError $ MatrixIsNotAPowerOfTwo m)
  when (q==0) (throwError $ MatrixHasZeroQubits m)
  let tr = trace (fromLists m)
  when (magnitude (tr-1.0) >= 1e-9) (throwError $ MatrixTraceNot1 m tr)
  let (eigvals, eigvec) = HM.eig (HM.fromLists m)
      eigvalsL = HM.toList eigvals
      isNonNegative eigval = abs (imagPart eigval) <= 1e-9  && realPart eigval >= -1e-9
  when (not $ all isNonNegative eigvalsL) (throwError $ MatrixNotPositive m eigvalsL)
  return q
