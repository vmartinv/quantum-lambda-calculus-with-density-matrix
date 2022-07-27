module Translation.StateBuilder where
import           Data.Bits
import           Data.Complex
import qualified Data.Text                     as T
import           Data.Tuple.Extra
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Parsing.PExp
import           Typing.GateChecker
import           Utils

data Axis = XAxis | YAxis | ZAxis
  deriving (Show,Eq)

rot :: Axis -> Double -> PGate
rot XAxis angle = PGate "U" [angle, -pi/2, pi/2]
rot YAxis angle = PGate "U" [angle, 0, 0]
rot ZAxis angle = PGate "U" [0, 0, angle]

ident :: Int -> PGate
ident n = PGate "I" [fromIntegral n]

-- targetQubits :: total # of qubits -> targeted qubit -> gate -> transformed gate
targetQubits :: Int -> Int -> PGate -> PGate
targetQubits n 0 g | n==gq = g
                   | n>gq  = g `PGateOtimes` (ident (n-gq))
                   | otherwise = error errorMsg
  where
    gq = getGateSizeNoCheck g
    errorMsg = "Unexpected arguments to targetQubits: "<>show n<>" "<>show 0<>" "<>show gq
targetQubits n i g | 0<= i && i==n-gq = (ident i) `PGateOtimes` g
                   | 0<= i && i<n-gq = (ident i) `PGateOtimes` g `PGateOtimes` (ident (n-i-gq))
                   | otherwise = error errorMsg
  where
    gq = getGateSizeNoCheck g
    errorMsg = "Unexpected arguments to targetQubits: "<>show n<>" "<>show i<>" "<>show gq

swap2 :: Int -> Int -> Int -> [PGate]
swap2 n i j | n<2 = error errorMsg
            | j==i = []
            | j==i+1 = swapNext <$> [i]
            | i>j = swap2 n j i
            | otherwise = swapNext <$> ([i..j-1] ++ [j-2..i])
    where
      -- swaps qubit i and i+1
      swapNext i = targetQubits n i (PGate "SWAP" [])
      errorMsg = "Unexpected arguments to swap2: "<>show n<>" "<>show i<>" "<>show j

cnot :: PGate
cnot = PGate "UC" [pi, 0, pi]

-- c is control, t is target
cnot2 :: Int -> Int -> Int -> [PGate]
cnot2 n c t = swaps ++ [targetQubits n c cnot] ++ (reverse swaps)
  where
    -- control goes to qubit 0
    -- target goes to qubit 1
    swaps = swap2 n 0 c ++ swap2 n 1 t

binaryToGray :: Int -> Int
binaryToGray m = fromIntegral $ mU `xor` (shiftR mU 1)
  where
    mU = fromIntegral m :: Word

grayDiffNext :: Int -> Int
grayDiffNext i = log2 $ (binaryToGray i) `xor` (binaryToGray (i+1))

getBit :: Int -> Int -> Int
getBit m j = (shiftR m j) .&. 1

getThetaAngles :: HM.Vector Double -> HM.Vector Double
getThetaAngles alphas = HM.app bigM alphas
  where
    k = log2 (HM.size alphas)
    b j = (round j)
    g i = binaryToGray (round i)
    bigM_elem :: Double -> Double -> Double
    bigM_elem i j = 2**(-fromIntegral k) * (fromIntegral $ (-1)^(b j * g i))
    bigM = HM.build (2^k, 2^k) bigM_elem :: HM.Matrix Double

uniformlyContRot :: Int -> Int -> Axis -> HM.Vector Double -> [PGate]
uniformlyContRot 0 m ax alphas = rotGates
  where
    thetas = getThetaAngles alphas
    rotGates = rot ax <$> HM.toList thetas
uniformlyContRot k m ax alphas | m>=k && HM.size alphas == 2^k =
  concat [rot:cnot | (rot, cnot) <- zip rotGates cnotGates]
                               | otherwise = error errorMsg
  where
    thetas = getThetaAngles alphas
    rotGates = targetQubits (k + 1) m <$> rot ax <$> HM.toList thetas
    cnotGates = [cnot2 (k + 1) (controlPos i) m | i <- [0..2^k-1]]
    controlPos i | i==(2^k - 1) = grayDiffNext i - 1
                 | otherwise = grayDiffNext i
    errorMsg = "Unexpected arguments to uniformlyContRot: "<>show k<>" "<>show m<>" "<>show ax<>" "<>show alphas

applyGates :: [PGate] -> PExp -> PExp
applyGates gates e = foldl (flip PGateApp) e gates

hermConjugate :: PGate -> PGate
hermConjugate g@(PGate "I" _)       = g
hermConjugate g@(PGate "SWAP" _)    = g
hermConjugate (PGate "U" [theta, phi, lambda]) = PGate "U" [theta, pi - lambda, -(phi+pi)]
hermConjugate (PGate "UC" [theta, phi, lambda]) = PGate "UC" [theta, pi - lambda, -(phi+pi)]

invertGates :: [PGate] -> [PGate]
invertGates gs = reverse (hermConjugate <$> gs)

circuitForState :: [Complex Double] -> PExp
circuitForState st = applyGates (invertGates (firstPhase++secondPhase)) start
  where
    n = log2 (length st) :: Int -- Number of qubits
    start = PQubits (T.replicate n "0") -- Initial state
    anorms :: HM.Vector Double
    ws :: HM.Vector Double
    (anorms, ws) = (HM.fromList *** HM.fromList) $ unzip (polar <$> st)
    alpha :: Axis -> Int -> HM.Vector Double
    alpha ZAxis k = HM.fromList [ sum [
           ws HM.! (2*(j+1)*2^k + l) - ws HM.! (j*2^(k+1) + l)
          | l <- [0..2^(k-1) :: Int]
        ] / 2^k :: Double
        | j <- [0..2^(n-k)-1]
      ]
    alpha YAxis k = HM.fromList [ (2*).asin <$> uncurry (/) $ sqrt.sum *** sqrt.sum $ unzip [
           ( (anorms HM.! (2*(j+1)*2^k + l)) ** 2
           , (anorms HM.! (j*2^(k+1) + l)) ** 2
           )
          | l <- [0..2^(k-1) :: Int]
        ] :: Double
        | j <- [0..2^(n-k)-1]
      ]
    firstPhase :: [PGate]
    firstPhase = concat [
        (`PGateOtimes` ident (n-j-1)) <$> uniformlyContRot j j ZAxis (alpha ZAxis (n-j-1))
        | j <- [n-1..0]
      ]
    secondPhase :: [PGate]
    secondPhase =  concat [
        (`PGateOtimes` ident (n-j-1)) <$> uniformlyContRot j j YAxis (alpha YAxis (n-j-1))
        | j <- [n-1..0]
      ]
