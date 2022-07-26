module UtilsTests(utilsTests) where
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.SmallCheck as SC
import           Utils

utilsTests = testGroup "utilsTests"
  [ QC.testProperty "2^(log2 x) <= x" $
      (\x -> 2^(log2 x) <= (x :: Int) ) . QC.getNonZero
  , QC.testProperty "2^(log2 x + 1) > x" $
      (\x -> 2^(log2 x + 1) > (x :: Int) ) . QC.getNonZero
  , testCase "log2 (2^x) = x" $
      (log2.(2^) <$> [0..63]) @?= [0..63]
  , testCase "log2 vals" $
      (log2 <$> [0..32]) @?= [-1,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5]
  ]
