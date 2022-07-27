module Translation.PurificationTests(purificationTests) where
import           Control.Monad.Except
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HMatrix
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
import           Test.Tasty.SmallCheck         as SC
import           TestUtils
import           Translation.Purification


purificationTests = testGroup "purificationTests"
  [ testCase "Simple state" $
      approxEqualA (purify [[0.5,0],[0,0.5]]) [1/(sqrt 2) :+ 0, 0:+ 0, 0:+ 0, 1/(sqrt 2):+ 0]
  ]
