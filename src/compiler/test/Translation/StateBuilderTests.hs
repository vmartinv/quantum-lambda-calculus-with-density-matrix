module Translation.StateBuilderTests where
import           Control.Monad.Except
import           Data.Complex
import           Data.Maybe
import           Data.Tuple.Extra
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Parsing.LamRhoExp
import qualified Test.QuickCheck.Property      as QCP
import qualified Test.SmallCheck               as SCP
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck         as QC
import qualified Test.Tasty.SmallCheck         as SC
import           TestUtils
import           Translation.StateBuilder

stateBuilderTests = testGroup "stateBuilderTests" [bitManipulation, utilFunctionTests, getThetaAnglesTests, uniformlyContRotTests, calcAlphasTests, stateToZeroGatesTests, stateToZeroGatesSimTests]

getMatrix :: PGate -> HM.Matrix (Complex Double)
getMatrix (PGate "I" [n]) = HM.ident (2^(round n))
getMatrix (PGate "U" [theta, phi, lambda]) = (2 HM.>< 2)
  [ cos (theta / 2) :+ 0, -exp (0 :+ lambda) * (sin(theta/2):+0)
  , exp (0 :+ phi) * (sin(theta/2):+0), exp (0 :+ (phi+lambda)) * (cos(theta/2):+0)
  ]
getMatrix (PGate "UC" [theta, phi, lambda]) = (4 HM.>< 4)
  [ 1:+0, 0:+0, 0:+0, 0:+0
  , 0:+0, 1:+0, 0:+0, 0:+0
  , 0:+0, 0:+0, u HM.! 0 HM.! 0, u HM.! 0 HM.! 1
  , 0:+0, 0:+0, u HM.! 1 HM.! 0, u HM.! 1 HM.! 1
  ]
    where
      u = getMatrix (PGate "U" [theta, phi, lambda])
getMatrix (PGate "SWAP" []) = (4 HM.>< 4) $ (:+0) <$>
  [ 1, 0, 0, 0
  , 0, 0, 1, 0
  , 0, 1, 0, 0
  , 0, 0, 0, 1
  ]
getMatrix (PGateOtimes g1 g2) = (getMatrix g1) `HM.kronecker` (getMatrix g2)

applyGatesV :: [PGate] -> HM.Vector (Complex Double) -> HM.Vector (Complex Double)
applyGatesV ms st = foldl applyGate st ms
  where
    applyGate :: HM.Vector (Complex Double) -> PGate -> HM.Vector (Complex Double)
    applyGate s g = HM.app (getMatrix g) s

-- https://en.wikipedia.org/wiki/3-sphere#Hopf_coordinates
hopf :: Double -> Double -> Double -> [Complex Double]
hopf t p d = [exp (0 :+ abs d) * (cos(t /2) :+ 0), exp (0 :+ (abs d+abs p)) * (sin(t/2) :+ 0)]

boch :: Double -> Double -> [Complex Double]
boch t p = hopf t p 0

otimes :: [Complex Double] -> [Complex Double] -> [Complex Double]
otimes q1 q2 = [a1 * a2 | a1 <- q1, a2 <- q2]

sameUpToGPhaseB :: HM.Vector (Complex Double) -> HM.Vector (Complex Double) -> Bool
sameUpToGPhaseB v1 v2 = HM.ranksv 1e-5 mxdim sv == 1
  where
    mxdim = max 2 (HM.size v1)
    sv = HM.toList $ HM.singularValues $ HM.fromColumns [v1, v2]

sameUpToGPhase :: HM.Vector (Complex Double) -> HM.Vector (Complex Double) -> QCP.Result
sameUpToGPhase v1 v2 = if  sameUpToGPhaseB v1 v2 then QCP.succeeded else QCP.failed { QCP.reason = errorMsg }
  where
    errorMsg = "following two vectors were expected to be linearly dependent:\n"<>show v1<>"\n"<>show v2


utilFunctionTests = testGroup "utilFunctionTests"
  [ testCase "U(0, 0, 0)=I^1" $
      getMatrix (PGate "U" [0, 0, 0]) @?= (2 HM.>< 2) [1,0,0,1]
  , QC.testProperty "hopf generates norm 1" $
      \t p d -> HM.norm_2 (HM.fromList (hopf t p d)) - 1 < 1e-5
  , testCase "|0> otimes |0>" $
      [1,0] `otimes` [1,0] @?= [1,0,0,0]
  , testCase "|1> otimes |0>" $
      [0,1] `otimes` [1,0] @?= [0,0,1,0]
  , testCase "boch (-pi/2) (pi/3) state" $
      st $ approxEqualV (HM.fromList $ boch (-pi/2) (pi/3)) (HM.fromList [1/sqrt 2 :+ 0, exp (0:+ pi/3) * ((-1/sqrt 2) :+ 0)])
  , testCase "boch (-pi/2) (pi/3) to polar" $
      (((roundDec <$>) *** (roundDec <$>)) <$> unzip $ polar <$> boch (-pi/2) (pi/3)) @?= (roundDec <$> [1/sqrt 2, 1/sqrt 2], [0, roundDec $ -2*pi/3])
  ]

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
  [ SC.testProperty "k=0" $
      \x -> sct $ getThetaAngles (HM.fromList [x]) `tequal` (HM.fromList [x])
  , SC.testProperty "k=1" $
      \x y -> sct $ getThetaAngles (HM.fromList [x, y]) `tequal` (HM.scale (1/2) $ HM.fromList [x+y, x-y])
  , QC.testProperty "k=2" $
      \a1 a2 a3 a4 -> approxEqualV (getThetaAngles (HM.fromList [a1, a2, a3, a4])) (HM.scale (1/4) $ HM.fromList [a1+a2+a3+a4, a1-a2+a3-a4, a1-a2+a3-a4, a1+a2+a3+a4])
  , testCase "k=2'" $
      st $ approxEqualV (getThetaAngles (HM.fromList [0.1, 0.3, 0.5, 0.7])) (HM.fromList [0.4,-0.1,-0.1,0.4])
  ]

calcAlphasTests = testGroup "calcAlphasTests"
  [ testCase "|0> ZAxis" $
      st $ approxEqualV ((calcAlphas . HM.fromList) [1.0, 0.0] ZAxis 1) (HM.fromList [0.0])
  , testCase "|0> YAxis" $
      st $ approxEqualV ((calcAlphas . HM.fromList) [1.0, 0.0] YAxis 1) (HM.fromList [0.0])
  , testCase "|1> ZAxis" $
      st $ approxEqualV ((calcAlphas . HM.fromList) [0.0, 1.0] ZAxis 1) (HM.fromList [0.0])
  , testCase "|1> YAxis" $
      st $ approxEqualV ((calcAlphas . HM.fromList) [0.0, 1.0] YAxis 1) (HM.fromList [-pi])
  , testCase "boch (-pi/2) (pi/3) ZAxis" $
      st $ approxEqualV ((calcAlphas . HM.fromList) (boch (-pi/2) (pi/3)) ZAxis 1) (HM.fromList [2*pi/3])
  , testCase "boch (-pi/2) (pi/3) YAxis" $
      st $ approxEqualV ((calcAlphas . HM.fromList) (boch (-pi/2) (pi/3)) YAxis 1) (HM.fromList [-pi/2])
  ]

compareOp (PGate g1 args1) (PGate g2 args2) = (g1 @?= g2) >> (st $ approxEqualV (HM.fromList args1) (HM.fromList args2))
compareOp (PGateOtimes g1 g2) (PGateOtimes g3 g4) = compareOp g1 g3 >> compareOp g2 g4
compareOp op1 op2 = op1 @?= op2

compareOps v1 v2 = sequence_ $ uncurry compareOp <$> zip v1 v2

uniformlyContRotTests = testGroup "uniformlyContRotTests"
  [ testCase "k=0" $
    uniformlyContRot 0 0 ZAxis (HM.fromList [0.5]) @?= [PGate "U" [0.0,0.0,0.5]]
  , testCase "k=1" $
    uniformlyContRot 1 1 ZAxis (HM.fromList [0.5, 0.1]) @?= [PGateOtimes (PGate "I" [1.0]) (PGate "U" [0.0,0.0,0.3]),PGate "UC" [pi,0.0,pi],PGateOtimes (PGate "I" [1.0]) (PGate "U" [0.0,0.0,0.2]),PGate "UC" [pi,0.0,pi]]
  , testCase "k=2" $
    compareOps (uniformlyContRot 2 2 ZAxis (HM.fromList [0.1, 0.3, 0.5, 0.7])) [
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

stateToZeroGatesTests = testGroup "stateToZeroGatesTests"
  [ testCase "|0>" $
      (stateToZeroGates . HM.fromList) [1, 0] @?=
        []
  , testCase "|1>" $
      (stateToZeroGates . HM.fromList) [0, 1] @?=
        [ PGate "U" [-pi,0.0,0.0]
        ]
  , testCase "(boch (-pi/2) (pi/3))" $
      (stateToZeroGates . HM.fromList) (boch (-pi/2) (pi/3)) @?=
        [ PGate "U" [0.0,0.0,2*pi/3+4e-16]
        , PGate "U" [-pi/2-2e-16,0,0]
        ]
  ]

stateToZeroGatesSimTests = testGroup "stateToZeroGatesSimTests"
  [ testCase "building |0>" $
      st $ approxEqualV (applyGatesV ((stateToZeroGates . HM.fromList) [1, 0]) (HM.fromList [1, 0])) (HM.fromList [1, 0])
  , testCase "building |1>" $
      st $ approxEqualV (applyGatesV ((stateToZeroGates . HM.fromList) [0, 1]) (HM.fromList [0, 1])) (HM.fromList [1, 0])
  , testCase "building |+>" $
      st $ approxEqualV (applyGatesV ((stateToZeroGates . HM.fromList) [1/sqrt(2), 1/sqrt(2)]) (HM.fromList [1/sqrt(2), 1/sqrt(2)])) (HM.fromList [1, 0])
  , testCase "building |->" $
      st $ approxEqualV (applyGatesV ((stateToZeroGates . HM.fromList) [1/sqrt(2), -1/sqrt(2)]) (HM.fromList [1/sqrt(2), -1/sqrt(2)])) (HM.fromList [1, 0])
  , testCase "building |0> phase in snd" $
      st $ approxEqualV (applyGatesV ((stateToZeroGates . HM.fromList) [0, exp (0 :+ pi)]) (HM.fromList [0, exp (0 :+ pi)])) (HM.fromList [1, 0])
  , testCase "building (boch -pi/2 pi/3)" $
      st $ approxEqualV (applyGatesV ((stateToZeroGates . HM.fromList) (boch (-pi/2) (pi/3))) (HM.fromList (boch (-pi/2) (pi/3)))) (HM.fromList [1, 0])
  , SC.testProperty "building boch n=1" $
      \t p -> sct $ sameUpToGPhase (applyGatesV ((stateToZeroGates . HM.fromList) (boch t p)) (HM.fromList (boch t p))) (HM.fromList [1, 0])
  , testCase "building |0> phase in fst" $
      st $ sameUpToGPhase (applyGatesV ((stateToZeroGates . HM.fromList) [exp (0 :+ pi), 0]) (HM.fromList [exp (0 :+ pi), 0])) (HM.fromList [1, 0])
  , QC.testProperty "building hopf n=1" $
      QC.withMaxSuccess 1000 $ \t p d -> qct $ sameUpToGPhase (applyGatesV ((stateToZeroGates . HM.fromList) (hopf t p d)) (HM.fromList (hopf t p d))) (HM.fromList [1, 0])
  , testCase "building |0> \\otimes |1>" $
      st $ sameUpToGPhase (applyGatesV ((stateToZeroGates . HM.fromList) ((boch 0 0) `otimes` [0,1])) (HM.fromList ((boch 0 0) `otimes` [0,1]))) (HM.fromList [1, 0, 0, 0])
  , QC.testProperty "building boch n=2" $
      QC.withMaxSuccess 1000 $ \t p t2 p2 -> sameUpToGPhase (applyGatesV ((stateToZeroGates . HM.fromList) ((boch t p) `otimes` (boch t2 p2))) (HM.fromList ((boch t p) `otimes` (boch t2 p2)))) (HM.fromList [1, 0, 0, 0])
  ]
