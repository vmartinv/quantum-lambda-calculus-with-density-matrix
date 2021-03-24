module SmithTests(smithTests) where
import           Data.Matrix
import           Data.Vector           as V
import           Smith
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC

smithTests :: TestTree
smithTests = testGroup "Smith tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  []

-- | a `divides` b <=> a | b
divides :: Integral a => a -> a -> Bool
divides a b = b `rem` a == 0

validateSmith :: Integral a => Matrix a -> (Matrix a, (Matrix a, Matrix a)) -> Bool
validateSmith a (d, (p, q)) = multStrassenMixed p (multStrassenMixed d q) == a
  && isDiagonal && V.all (>=0) diag && zeroesAtTheEnd (V.toList diag) && diagDivides
  && isInvertible p && isInvertible q
  where
      diag = getDiag d
      isInvertible m = detLU (fromIntegral <$> m) /=0
      isDiagonal = Prelude.and [(getElem i j d) == 0| i <- [1..nrows d], j <- [1..ncols d], i/=j]
      zeroesAtTheEnd (x:xs) | x/=0 = zeroesAtTheEnd xs
                            | otherwise = Prelude.all (==0) xs
      zeroesAtTheEnd [] = True
      diagDivides = fst $ V.foldl check (True, V.head diag) diag
        where checkConsecutives x1 x2 = (x1 <= x2 || x2==0) &&  (x1==0 || x1 `divides` x2)
              check (b, x1) x2 = (b && checkConsecutives x1 x2, x2)

instance (Arbitrary a) => Arbitrary (Matrix a) where
  arbitrary = do
    r <- choose (1, 10)
    c <- choose (1, 10)
    vals <- vectorOf r (vector c)
    return (fromLists vals)

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "Test random matrices" $
      \x -> validateSmith (x :: Matrix Int) (getSmithForm x)
  ]


unitTests = testGroup "Unit tests"
  []
