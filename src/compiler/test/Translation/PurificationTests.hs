module Translation.PurificationTests(purificationTests) where
import           Control.Monad.Except
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
import           Test.Tasty.SmallCheck         as SC
import           TestUtils
import           Translation.Purification


purificationTests = testGroup "purificationTests"
  [ testCase "Simple state" $
      st $ approxEqualV (HM.fromList $ purify [[0.5,0],[0,0.5]]) (HM.fromList [1/(sqrt 2) :+ 0, 0:+ 0, 0:+ 0, 1/(sqrt 2):+ 0])
  ]
