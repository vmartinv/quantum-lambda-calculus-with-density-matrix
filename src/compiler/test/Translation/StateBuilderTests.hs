module Translation.StateBuilderTests(stateBuilderTests) where
import           Control.Monad.Except
import           Data.Complex
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Parsing.PExp
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
import           Test.Tasty.SmallCheck         as SC
import           TestUtils
import           Translation.StateBuilder

stateBuilderTests = testGroup "stateBuilderTests" [bitManipulation, getThetaAnglesTests, uniformlyContRotTests]

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

getThetaAnglesTests = testGroup "getThetaAnglesTests"
  [ QC.testProperty "k=0" $
      \x -> getThetaAngles (HM.fromList [x]) == (HM.fromList [x])
  , QC.testProperty "k=1" $
      \x y -> getThetaAngles (HM.fromList [x, y]) == (HM.scale (1/2) $ HM.fromList [x+y, x-y])
  , QC.testProperty "k=2" $
      \a1 a2 a3 a4 -> approxEqualV (getThetaAngles (HM.fromList [a1, a2, a3, a4])) (HM.scale (1/4) $ HM.fromList [a1+a2+a3+a4, a1-a2+a3-a4, a1-a2+a3-a4, a1+a2+a3+a4])
  , testCase "k=2'" $
      getThetaAngles (HM.fromList [0.1, 0.3, 0.5, 0.7]) @?= HM.fromList [0.4,-0.1,-0.1,0.4]
  ]

uniformlyContRotTests = testGroup "uniformlyContRotTests"
  [ testCase "k=0" $
    uniformlyContRot 0 0 ZAxis (HM.fromList [0.5]) @?= [PGate "U" [0.0,0.0,0.5]]
  , testCase "k=1" $
    uniformlyContRot 1 1 ZAxis (HM.fromList [0.5, 0.1]) @?= [PGateOtimes (PGate "I" [1.0]) (PGate "U" [0.0,0.0,0.3]),PGate "UC" [pi,0.0,pi],PGateOtimes (PGate "I" [1.0]) (PGate "U" [0.0,0.0,0.2]),PGate "UC" [pi,0.0,pi]]
  , testCase "k=2" $
    uniformlyContRot 2 2 ZAxis (HM.fromList [0.1, 0.3, 0.5, 0.7]) @?= [
        PGateOtimes (PGate "I" [2.0]) (PGate "U" [0.0,0.0,0.4]),
        PGateOtimes (PGate "I" [1.0]) (PGate "SWAP" []),
        PGateOtimes (PGate "UC" [pi,0.0,pi]) (PGate "I" [1.0]),
        PGateOtimes (PGate "I" [1.0]) (PGate "SWAP" []),

        PGateOtimes (PGate "I" [2.0]) (PGate "U" [0.0,0.0,-0.1]),
        PGateOtimes (PGate "SWAP" []) (PGate "I" [1.0]),
        PGateOtimes (PGate "I" [1.0]) (PGate "SWAP" []),
        PGateOtimes (PGate "I" [1.0]) (PGate "UC" [pi,0.0,pi]),
        PGateOtimes (PGate "I" [1.0]) (PGate "SWAP" []),
        PGateOtimes (PGate "SWAP" []) (PGate "I" [1.0]),

        PGateOtimes (PGate "I" [2.0]) (PGate "U" [0.0,0.0,-0.1]),
        PGateOtimes (PGate "I" [1.0]) (PGate "SWAP" []),
        PGateOtimes (PGate "UC" [pi,0.0,pi]) (PGate "I" [1.0]),
        PGateOtimes (PGate "I" [1.0]) (PGate "SWAP" []),

        PGateOtimes (PGate "I" [2.0]) (PGate "U" [0.0,0.0,0.4]),
        PGateOtimes (PGate "SWAP" []) (PGate "I" [1.0]),
        PGateOtimes (PGate "I" [1.0]) (PGate "SWAP" []),
        PGateOtimes (PGate "I" [1.0]) (PGate "UC" [pi,0.0,pi]),
        PGateOtimes (PGate "I" [1.0]) (PGate "SWAP" []),
        PGateOtimes (PGate "SWAP" []) (PGate "I" [1.0])
      ]
  ]
