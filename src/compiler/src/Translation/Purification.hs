module Translation.Purification where
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Utils

-- purifies a density matrix
-- (assumes the input is a square matrix with a power of 2 size)
-- given a matrix of 2^q x 2^q size, it returns a vector of 2^(q+1) size
purify :: HM.Matrix (Complex Double) -> HM.Vector (Complex Double)
purify m = HM.flatten $
  sum (map f (zip3 [0..] (HM.toList eigvals) (HM.toRows eigvec)))
  where
    (n, _) = HM.size m
    (eigvals, eigvec) = HM.eig m
    e i = HM.asRow $ HM.fromList $ [if i==j then (1.0 :+ 0.0) else (0.0 :+ 0.0) | j <- [0..n-1]] :: HM.Matrix (Complex Double)
    f (i, eigval, eigvec) = HM.scale (sqrt eigval) (HM.kronecker (HM.asRow eigvec) (e i)) :: HM.Matrix (Complex Double)
