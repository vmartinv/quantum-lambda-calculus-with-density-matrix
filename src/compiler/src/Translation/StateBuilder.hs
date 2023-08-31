module Translation.StateBuilder where
import           Data.Bits
import           Data.Complex
import           Data.Fixed
import qualified Data.Text                     as T
import           Data.Tuple.Extra
import qualified Numeric.LinearAlgebra.HMatrix as HM
import           Parsing.LamRhoExp
import           Typing.GateChecker
import           Utils

data Axis = YAxis | ZAxis
  deriving (Show,Eq)

rot :: Axis -> Double -> PGate
-- rot XAxis angle = PGate "U" [angle, -pi/2, pi/2]
rot YAxis angle = PGate "U" [angle, 0, 0] 0
rot ZAxis angle = PGate "U" [0, 0, angle] 0

ident :: Int -> PGate
ident n = PGate "I" [fromIntegral n] 0

offsetL :: Int -> PGate -> PGate
offsetL 0 gate               = gate
offsetL offset (PGate n a p)=PGate n a (p+offset)

offsetR :: Int -> PGate -> PGate
offsetR _p = id

-- targetQubits :: total # of qubits -> targeted qubit -> gate -> transformed gate
targetQubits :: Int -> Int -> PGate -> PGate
targetQubits n i g | 0<= i && i<=n-gq = offsetL i (offsetR (n-i-gq) g)
                   | otherwise = error errorMsg
  where
    gq = getGateSizeNoCheck g
    errorMsg = "Unexpected arguments to targetQubits: "<>show n<>" "<>show i<>" "<>show gq

swap2 :: Int -> Int -> Int -> [PGate]
swap2 n i j | n<2 = error errorMsg
            | j==i = []
            | j==i+1 = swapNext <$> [i]
            | i>j = swap2 n j i
            | otherwise = swapNext <$> ([i..j-1] ++ reverse [i..j-2])
    where
      -- swaps qubit i and i+1
      swapNext i = targetQubits n i (PGate "SWAP" [] 0)
      errorMsg = "Unexpected arguments to swap2: "<>show n<>" "<>show i<>" "<>show j

cnot :: PGate
cnot = PGate "CU" [pi, 0, pi, 0] 0

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

applyGates :: [PGate] -> LamRhoExp -> LamRhoExp
applyGates gates e = foldl (flip PGateApp) e gates

normRad :: Double -> Double
normRad a = (2*pi+a) `mod'` (2*pi) - pi

hermConjugate :: PGate -> PGate
hermConjugate g@(PGate "I" _ _)       = g
hermConjugate g@(PGate "SWAP" _ _)    = g
hermConjugate (PGate "U" [theta, phi, lambda] p) = PGate "U" [theta, normRad $ pi - lambda , normRad $ -(phi+pi)] p
hermConjugate (PGate "CU" [theta, phi, lambda, gamma] p) = PGate "CU" [theta, normRad $ pi - lambda, normRad $ -(phi+pi), gamma] p

invertCircuit :: [PGate] -> [PGate]
invertCircuit gs = reverse (hermConjugate <$> gs)

calcAlphas :: HM.Vector (Complex Double) -> Axis -> Int -> HM.Vector Double
calcAlphas st ax k | ok && ax==ZAxis =
  HM.fromList [ sum [
       ws HM.! (j*2^k + l) - ws HM.! ((2*j+1)*2^(k-1) + l)
      | l <- [0..2^(k-1)-1 :: Int]
    ] / 2^(k-1) :: Double
    | j <- [0..2^(n-k)-1]
  ]
                   | ok && ax==YAxis =
  HM.fromList [ ((-2)*).asin <$> uncurry (divFix) $
    ( sqrt $ sum [
        (anorms HM.! ((2*j+1)*2^(k-1) + l)) ** 2
        | l <- [0..2^(k-1)-1 :: Int]
      ]
    , sqrt $ sum [
        (anorms HM.! (j*2^k + l)) ** 2
        | l <- [0..2^k-1 :: Int]
      ]
    ) :: Double
    | j <- [0..2^(n-k)-1]
  ]
                   | otherwise = error errorMsg
  where
    n = log2 (HM.size anorms) :: Int -- Number of qubits
    anorms :: HM.Vector Double
    ws :: HM.Vector Double
    (anorms, ws) = (HM.fromList *** HM.fromList) $ unzip (polar <$> HM.toList st)
    ok = 0 < k && k < HM.size st
    a `divFix` b | b<1e-8 = 0
                 | otherwise = a/b
    errorMsg = "Unexpected arguments to calcAlphas: "<>show (HM.size st)<>" "<>show ax<>" "<>show k

isIdentGate :: PGate -> Bool
isIdentGate (PGate "I" [_] _)           = True
isIdentGate (PGate "U" [0, 0, 0] _)     = True
isIdentGate (PGate "CU" [0, 0, 0, 0] _) = True
isIdentGate _                           = False

stateToZeroGates :: HM.Vector (Complex Double) -> [PGate]
stateToZeroGates st = filter (not . isIdentGate) $ firstPhase++secondPhase
  where
    n = log2 (HM.size st) :: Int -- Number of qubits
    firstPhase :: [PGate]
    firstPhase = concat [
        offsetR (n-j-1) <$> uniformlyContRot j j ZAxis (calcAlphas st ZAxis (n-j))
        | j <- reverse [0..n-1]
      ]
    secondPhase :: [PGate]
    secondPhase =  concat [
        offsetR (n-j-1) <$> uniformlyContRot j j YAxis (calcAlphas st YAxis (n-j))
        | j <- reverse [0..n-1]
      ]

circuitForState :: HM.Vector (Complex Double) -> LamRhoExp
circuitForState st = applyGates (invertCircuit $ stateToZeroGates st) start
  where
    n = log2 (HM.size st) :: Int -- Number of qubits
    start = PQubits (T.replicate n "0") -- Initial state
