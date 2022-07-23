module Translation.Purification where
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HMatrix
import           Utils

-- purifies a density matrix
-- (assumes the input is a square matrix with a power of 2 size)
-- given a matrix of 2^q x 2^q size, it returns a vector of 2^(q+1) size
purify :: [[Double]] -> [Complex Double]
purify mList = HMatrix.toList $ HMatrix.flatten $
  sum (map f (zip3 [0..] (HMatrix.toList eigvals) (HMatrix.toRows eigvec)))
  where
    m = HMatrix.fromLists (fmap (:+ 0.0) <$> mList)
    (n, _) = HMatrix.size m
    (eigvals, eigvec) = HMatrix.eig m
    e i = HMatrix.asRow $ HMatrix.fromList $ [if i==j then (1.0 :+ 0.0) else (0.0 :+ 0.0) | j <- [0..n-1]] :: HMatrix.Matrix (Complex Double)
    f (i, eigval, eigvec) = HMatrix.scale (sqrt eigval) (HMatrix.kronecker (HMatrix.asRow eigvec) (e i)) :: HMatrix.Matrix (Complex Double)
