module Typing.SolverTests where
import           Data.Matrix           as M
import           Data.Vector           as V
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC
import           Typing.Solver         as Solver

solverTests :: TestTree
solverTests = testGroup "SolverTests"
  [ QC.testProperty "AX=B" $ withMaxSuccess 10000 $ QC.forAll genLinearSys $
      \(a, b) -> validateSolution a (Solver.solve (a, b)) b,
    QC.testProperty "XbiggerThan1" $ withMaxSuccess 10000 $ QC.forAll genLinearSys $
      \(a, b) -> maybe True (V.all (>=1)) (Solver.solve (a, b))
  ]

genElem :: Gen Int
genElem = frequency [
    (3, return 0),
    (1, return 1)
  ]

genMat :: Gen (Matrix Int)
genMat =  do
  r <- choose (1, 8)
  c <- choose (1, 8)
  vals <- vectorOf r (vectorOf c genElem)
  return (fromLists vals)

genVec :: Int -> Gen (Vector Int)
genVec n = V.fromList <$> vectorOf n (choose (0, 10))

genLinearSys :: Gen (Matrix Int, Vector Int)
genLinearSys = do
  a <- genMat
  b <- genVec (nrows a)
  return (a, b)

validateSolution :: Matrix Int -> Maybe (Vector Int) -> Vector Int -> Bool
validateSolution _ Nothing _ = True
validateSolution a (Just x) b = multStrassenMixed a (M.colVector x) == (M.colVector b)
