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
      st $ approxEqualV (purify $ HM.fromLists [[0.5,0],[0,0.5]]) (HM.fromList [1/(sqrt 2) :+ 0, 0:+ 0, 0:+ 0, 1/(sqrt 2):+ 0])
  , testCase "zero" $
      st $ approxEqualV (purify $ HM.fromLists [[1.0,0],[0,0]]) (HM.fromList [1 :+ 0, 0:+ 0, 0:+ 0, 0:+ 0])
  , testCase "one" $
      st $ approxEqualV (purify $ HM.fromLists [[0,0],[0,1.0]]) (HM.fromList [0 :+ 0, 0:+ 0, 0:+ 0, 1:+ 0])
  , testCase "interleavePerm 2" $
      (interleavePerm 2 <$> [0..2*2-1]) @?= [0, 2, 1, 3]
  , testCase "interleavePerm 3" $
      (interleavePerm 3 <$> [0..2*3-1]) @?= [0,3,1,4,2,5]
  , testCase "interleavePerm 4" $
      (interleavePerm 4 <$> [0..2*4-1]) @?= [0,4,1,5,2,6,3,7]
  , testCase "01" $
      st $ approxEqualV (purify $ HM.fromLists [[0,0,0,0],[0,1,0,0],[0,0,0,0],[0,0,0,0]]) (HM.fromList [
        0 :+ 0, 0:+ 0, 0:+ 0, 1:+ 0,
        0 :+ 0, 0:+ 0, 0:+ 0, 0:+ 0,
        0 :+ 0, 0:+ 0, 0:+ 0, 0:+ 0,
        0 :+ 0, 0:+ 0, 0:+ 0, 0:+ 0])
  ]
