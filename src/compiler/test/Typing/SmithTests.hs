module Typing.SmithTests where
import           Data.Matrix
import           Data.Vector           as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC
import           Typing.Smith

smithTests :: TestTree
smithTests = testGroup "Smith tests" [qcProps, unitTests]


-- | a `divides` b <=> a | b
divides :: Integral a => a -> a -> Bool
divides a b = b `rem` a == 0

validateSmithAll :: Integral a => Matrix a -> (Matrix a, (Matrix a, Matrix a)) -> Bool
validateSmithAll a r@(d, (p, q)) = validateSmith a r
  && (isDiagonal d) && V.all (>=0) diag && zeroesAtTheEnd (V.toList diag) && (diagDivides diag)
  && isInvertible p && isInvertible q
  where
      diag = getDiag d

genElem :: Gen Int
genElem = frequency [
    (3, return 0),
    (1, return 1)
  ]

genMat :: Gen (Matrix Int)
genMat =  do
  r <- choose (1, 10)
  c <- choose (1, 10)
  vals <- vectorOf r (vectorOf c genElem)
  return (fromLists vals)

genMatBig :: Gen (Matrix Int)
genMatBig =  do
  r <- choose (1, 20)
  c <- choose (1, 20)
  vals <- vectorOf r (vectorOf c genElem)
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
  where checkConsecutives x1 x2 = (x1 <= x2 || x2==0) &&  (x1==0 || x1 `Typing.SmithTests.divides` x2)
        check (b, x1) x2 = (b && checkConsecutives x1 x2, x2)

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "Generates diagonal matrices" $ QC.forAll genMatBig $
      isDiagonal . fst . smithFormInt,
    QC.testProperty "Diagonal is all non-negatives" $ QC.forAll genMatBig $
      V.all (>=0) . getDiag . fst . getSmithForm,
    QC.testProperty "Diagonal is increasing & divides" $ QC.forAll genMatBig $
      diagDivides . getDiag . fst . getSmithForm,
    QC.testProperty "Zeroes are only at the end of the diagonal" $ QC.forAll genMatBig $
      zeroesAtTheEnd . V.toList . getDiag . fst . getSmithForm,
    QC.testProperty "D=PAQ" $ withMaxSuccess 10000 $  QC.forAll genMat $
      \x -> validateSmith x (getSmithForm x),
    QC.testProperty "P is invertible" $ withMaxSuccess 10000 $  QC.forAll genMat $
      isInvertible . fst . snd . getSmithForm,
    QC.testProperty "Q is invertible"  $ withMaxSuccess 10000 $  QC.forAll genMat $
      isInvertible . snd . snd . getSmithForm
  ]


unitTests = testGroup "Unit tests"
  []
