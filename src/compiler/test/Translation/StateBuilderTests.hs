module Translation.StateBuilderTests(stateBuilderTests) where
import           Control.Monad.Except
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
import           Test.Tasty.SmallCheck         as SC
import           Translation.StateBuilder

stateBuilderTests = testGroup "stateBuilderTests" [bitManipulation]

bitManipulation = testGroup "bitManipulation"
  [ testCase "binaryToGray vals" $
      (binaryToGray <$> [0..15]) @?= [0, 1, 3, 2, 6, 7, 5, 4, 12, 13, 15, 14, 10, 11, 9, 8]
  , testCase "grayDiffNext vals" $
      (grayDiffNext <$> [0..15]) @?= [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4]
  , QC.testProperty "|(gray x+1)-(gray x)|==2^(grayDiffNext x)" $
      (\x -> abs (binaryToGray (x+1) - binaryToGray (x :: Int)) ==  2^(grayDiffNext x)) . QC.getNonZero
  , testCase "getBit first vals" $
      ((flip getBit) 0 <$> [0..33]) @?= [0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1]
  , testCase "getBit second vals" $
      ((flip getBit) 1 <$> [0..33]) @?= [0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0,1,1,0,0]
  , testCase "getBit 10th vals" $
      ((flip getBit) 10 <$> [128..140]) @?= [0,0,0,0,0,0,0,0,0,0,0,0,0]
  ]
