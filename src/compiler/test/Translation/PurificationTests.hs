module Translation.PurificationTests(purificationTests) where
import           Control.Monad.Except
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HMatrix
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
import           Test.Tasty.SmallCheck         as SC
import           Translation.Purification

purificationTests :: TestTree
purificationTests = testGroup "Purification tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  []

qcProps = testGroup "(checked by QuickCheck)"
  []

approxEqual :: [Complex Double] -> [Complex Double] -> Assertion
approxEqual v1 v2 = HMatrix.norm_2 ((HMatrix.fromList v1) - (HMatrix.fromList v2)) `compare` 1e5 @?= LT

unitTests = testGroup "Unit tests"
  [ testCase "Simple state" $
      approxEqual (purify [[0.5,0],[0,0.5]]) [1/(sqrt 2) :+ 0, 0:+ 0, 0:+ 0, 1/(sqrt 2):+ 0]
  ]
