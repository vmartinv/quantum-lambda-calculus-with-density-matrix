module Translation.StateBuilderTests(stateBuilderTests) where
import           Control.Monad.Except
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
import           Test.Tasty.SmallCheck         as SC
import           Translation.StateBuilder

stateBuilderTests :: TestTree
stateBuilderTests = testGroup "StateBuilder tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [scProps, qcProps]

scProps = testGroup "(checked by SmallCheck)"
  []

qcProps = testGroup "(checked by QuickCheck)"
  []

unitTests = testGroup "Bit manipulation"
  [ testCase "binaryToGray vals" $
      (binaryToGray <$> [0..15]) @?= [0, 1, 3, 2, 6, 7, 5, 4, 12, 13, 15, 14, 10, 11, 9, 8]
  , testCase "grayDiffNext vals" $
      (grayDiffNext <$> [0..15]) @?= [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4]
  , testCase "getBit first vals" $
      ((flip getBit) 0 <$> [0..33]) @?= [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1]
  , testCase "getBit second vals" $
      ((flip getBit) 1 <$> [0..33]) @?= [0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0]
  , testCase "getBit 10th vals" $
      ((flip getBit) 10 <$> [128..140]) @?= [0,0,0,0,0,0,0,0,0,0,0,0,0]
  ]
