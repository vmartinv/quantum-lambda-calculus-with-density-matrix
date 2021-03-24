module SmithTests where
import           Data.Matrix
import           Data.Vector           as V
import           Smith
import           Test.QuickCheck
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

validateSmithAll :: Integral a => Matrix a -> (Matrix a, (Matrix a, Matrix a)) -> Bool
validateSmithAll a r@(d, (p, q)) = validateSmith a r
  && (isDiagonal d) && V.all (>=0) diag && zeroesAtTheEnd (V.toList diag) && (diagDivides diag)
  && isInvertible p && isInvertible q
  where
      diag = getDiag d

instance (Arbitrary a) => Arbitrary (Matrix a) where
  arbitrary = do
    r <- choose (1, 5)
    c <- choose (1, 5)
    vals <- vectorOf r (vector c)
    return (fromLists vals)

isDiagonal :: Integral a => Matrix a -> Bool
isDiagonal d = Prelude.and [(getElem i j d) == 0| i <- [1..nrows d], j <- [1..ncols d], i/=j]

smithFormInt :: Matrix Int -> (Matrix Int, (Matrix Int, Matrix Int))
smithFormInt = getSmithForm

zeroesAtTheEnd (x:xs) | x/=0 = zeroesAtTheEnd xs
                      | otherwise = Prelude.all (==0) xs
zeroesAtTheEnd [] = True

isInvertible m = detLU (fromIntegral <$> m) /=0

validateSmith a (d, (p, q)) = multStrassenMixed p (multStrassenMixed a q) == d


diagDivides diag = fst $ V.foldl check (True, V.head diag) diag
  where checkConsecutives x1 x2 = (x1 <= x2 || x2==0) &&  (x1==0 || x1 `SmithTests.divides` x2)
        check (b, x1) x2 = (b && checkConsecutives x1 x2, x2)

isSquare m = nrows m == ncols m

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "Generates diagonal matrices" $
      isDiagonal . fst . smithFormInt,
    QC.testProperty "Diagonal is all non-negatives" $
      V.all (>=0) . getDiag . fst . smithFormInt,
    QC.testProperty "Diagonal is increasing & divides" $
      diagDivides . getDiag . fst . smithFormInt,
    QC.testProperty "Zeroes are only at the end of the diagonal" $
      zeroesAtTheEnd . V.toList . getDiag . fst . smithFormInt,
    QC.testProperty "D=PAQ" $
      \x -> validateSmith x (smithFormInt x),
    QC.testProperty "P is square" $
      isSquare . fst . snd . smithFormInt,
    QC.testProperty "Q is square" $
      isSquare . snd . snd . smithFormInt,
    QC.testProperty "P is invertible" $
      isInvertible . fst . snd . smithFormInt,
    QC.testProperty "Q is invertible" $
      isInvertible . snd . snd . smithFormInt
  ]


unitTests = testGroup "Unit tests"
  []
