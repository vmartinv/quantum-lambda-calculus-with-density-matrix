module Translation.StateBuilder where
import           Data.Bits
import           Data.Complex
import qualified Data.Text                     as T
import           Data.Tuple.Extra
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Parsing.PExp
import           Utils

data Axis = XAxis | YAxis | ZAxis

rot :: Axis -> Double -> PGate
rot XAxis angle = PGate "U" [angle, -pi/2, pi/2]
rot YAxis angle = PGate "U" [angle, 0, 0]
rot ZAxis angle = PGate "U" [0, 0, angle]

ident :: Int -> PGate
ident n = PGate "I" [fromIntegral n]

targetSingleQubit :: Int -> Int -> PGate -> PGate
targetSingleQubit n i g = (ident i) `PGateOtimes` g `PGateOtimes` (ident (n-i-1))

swap2 :: Int -> Int -> Int -> [PGate]
swap2 n i j | j==i = []
            | i>j = swap2 n j i
            | otherwise = swapNext <$> ([i..j-1] ++ [j-2..i])
    where
      -- swaps qubit i and i+1
      swapNext i = targetSingleQubit n i (PGate "SWAP" [])

cnot :: PGate
cnot = PGate "UC" [pi, 0, pi]

-- c is control, t is target
cnot2 :: Int -> Int -> Int -> [PGate]
cnot2 n c t = swaps ++ [cnot] ++ swaps
  where
    swaps = swap2 n 0 c ++ swap2 n 1 t

binaryToGray :: Int -> Int
binaryToGray m = fromIntegral $ mU `xor` (shiftR mU 1)
  where
    mU = fromIntegral m :: Word

grayDiffNext :: Int -> Int
grayDiffNext i = log2 $ (binaryToGray i) `xor` (binaryToGray (i+1))

getBit :: Int -> Int -> Int
getBit m j = (shiftR m j) .&. 1

getThetaAngles :: HM.Vector Double -> Int -> HM.Vector Double
getThetaAngles alphas m = HM.app bigM alphas
  where
    k = log2 (HM.size alphas)
    b j = getBit m (round j)
    g i = getBit (binaryToGray m) (round i)
    bigM_elem :: Double -> Double -> Double
    bigM_elem i j = 2^(-k) * (-1)^(b j * g i)
    bigM = HM.build (2^k, 2^k) bigM_elem :: HM.Matrix Double

uniformlyContRot :: Int -> Int -> Axis -> HM.Vector Double -> [PGate]
uniformlyContRot k m ax alphas =
  concat [rot:cnot | (rot, cnot) <- zip rotGates cnotGates]
  where
    thetas = getThetaAngles alphas m
    rotGates = rot ax <$> HM.toList thetas
    cnotGates = [cnot2 (2^k + 1) (controlPos i) m | i <- [0..2^k-1]]
    controlPos i | i==(2^k - 1) = grayDiffNext i - 1
                 | otherwise = grayDiffNext i

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
