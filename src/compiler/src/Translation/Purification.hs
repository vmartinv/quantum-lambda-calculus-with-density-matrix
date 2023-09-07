module Translation.Purification where
import           Data.Bits
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Utils

-- interleavePerm :: Int -> Int -> Int
-- interleavePerm q x | x<q = 2*x
--                    | x>=q = 2*(x-q)+1
interleavePerm :: Int -> Int -> Int
interleavePerm q x | x`mod`2==0 = x`div`2
                   | x`mod`2==1 = q+(x-1) `div` 2

makePermutationGate :: Int -> (Int -> Int) -> HM.Matrix (Complex Double)
makePermutationGate q perm = HM.complex (dprint "makePermutationGate" $ sum (f <$> [0..2^(2*q)-1]))
  where
      f :: Int -> HM.Matrix Double
      f ib = foldr HM.kronecker (HM.ident 1) $ g ib <$> [0..2*q-1]
      g ib j =  (boolToQubit (testBit ib (perm j))) `HM.outer` (boolToQubit (testBit ib j))
      boolToQubit False = 2 HM.|> [1, 0]
      boolToQubit True  = 2 HM.|> [0, 1]

-- purifies a density matrix
-- (assumes the input is a square matrix with a power of 2 size)
-- given a matrix of 2^q x 2^q size, it returns a vector of 2^2q size
purify :: HM.Matrix (Complex Double) -> HM.Vector (Complex Double)
purify m = dprint "purify" $ HM.app swapQubitGate purifiedSimple
  where
    (n, _) = dprint "size" $ HM.size m
    q = dprint "q" $ log2 n
    (eigvals, eigvec) = dprint "eig" $ HM.eig m
    e i = HM.asRow $ HM.fromList $ [if i==j then (1.0 :+ 0.0) else (0.0 :+ 0.0) | j <- [0..n-1]] :: HM.Matrix (Complex Double)
    f (i, eigval, eigvec) = HM.scale (sqrt eigval) (HM.kronecker (HM.asRow eigvec) (e i)) :: HM.Matrix (Complex Double)
    purifiedSimple = dprint "purifiedSimple" $ HM.flatten $
      sum (map f (zip3 [0..] (HM.toList eigvals) (HM.toRows eigvec)))
    swapQubitGate = makePermutationGate q (interleavePerm q)
