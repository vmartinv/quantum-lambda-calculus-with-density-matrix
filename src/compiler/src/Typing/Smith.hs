module Typing.Smith where

import           Control.Monad.Extra
import           Control.Monad.State  as S
import           Control.Monad.Writer
import           Data.List            as L
import           Data.Matrix          as M
import           Data.Maybe
import           Data.Vector          as V

-- Given a Matrix A, the smith normal form is a diagonal matrix in the form PAQ
-- Complete definition https://en.wikipedia.org/wiki/Smith_normal_form
-- The algorithm recursively performs operations on A which are encoded
-- in the matrixes P and Q
type SmithState a = (Matrix a, (Matrix a, Matrix a)) -- Matrixes A, (P, Q)
type SmithM a b = (State
                  (SmithState a)
                  b)

getSmithForm :: Integral a => Matrix a -> SmithState a
getSmithForm a = execSmith smith a

execSmith :: Integral a => SmithM a b -> Matrix a -> SmithState a
execSmith m a = execState m (startState a)

runSmith :: Integral a => SmithM a b -> Matrix a -> (b, SmithState a)
runSmith m a = runState m (startState a)

startState :: Integral a => Matrix a -> SmithState a
startState a = (a, (p, q))
  where
    p = identity (nrows a)
    q = identity (ncols a)

-- | a `divides` b <=> a | b
divides :: Integral a => a -> a -> Bool
divides a b = b `rem` a == 0

-- | checks whether a divides every element
dividesAll :: (Foldable f, Integral a) => a -> f a -> Bool
dividesAll a = L.all (divides a)

isZero :: Integral a => Matrix a -> Bool
isZero = L.all (==0)

smith :: Integral a => SmithM a ()
smith = getA >>= smith'
  where smith' a  | isZero a                      = return ()
                  | nrows a == 1 || ncols a == 1  = pivot
                  | otherwise = do
                    pivot
                    S.modify solveSmallerAndCombine

solveSmallerAndCombine :: Integral a => SmithState a -> SmithState a
solveSmallerAndCombine (aBig, (pBig, qBig)) = (aRes, (pRes, qRes))
  where
      (tl,tr,bl,br) = splitBlocks 1 1 aBig
      (aSmall, (pSmall, qSmall)) = execSmith smith br
      aRes = M.joinBlocks (tl,tr,bl,aSmall)
      pRes = multStrassenMixed (extendElementary pSmall) pBig
      qRes = multStrassenMixed qBig (extendElementary qSmall)

extendElementary :: Integral a => Matrix a -> Matrix a
extendElementary a = M.joinBlocks (identity 1, zero 1 (ncols a), zero (nrows a) 1, a)

pivot :: Integral a => SmithM a ()
pivot = moveSmallest >> (whileM $ makeFstRowAndColZero >> performCase3) >> makePositive

makePositive :: Integral a => SmithM a ()
makePositive = do
  a <- getA
  when (getElem 1 1 a < 0) (Typing.Smith.swapSignRow 1)

performCase3 :: Integral a => SmithM a Bool
performCase3 = do
  a <- getA
  let a_11 = getElem 1 1 a
      nonDivisible = L.find (\j -> not $ a_11 `dividesAll` (getRow j a))
                 [2..nrows a]
      fix i = Typing.Smith.combineRows 1 1 i
  whenJust nonDivisible fix
  return (isJust nonDivisible)

-- performs iteratively case 1 and case 2 until elements are divisables
makeFstRowAndColZero :: Integral a => SmithM a ()
makeFstRowAndColZero = whileM $ do
  makeFstRowZero
  Typing.Smith.transpose
  makeFstRowZero
  Typing.Smith.transpose
  not <$> isFstRowZero

isFstRowZero :: Integral a => SmithM a Bool
isFstRowZero = ((V.all (==0)) . V.tail . (M.getRow 1)) <$> getA

makeFstRowZero :: Integral a => SmithM a ()
makeFstRowZero = do
    makeFstRowDivisible
    a <- getA
    let a_11     = getElem 1 1 a
        fix j = when (a_1j/=0) (Typing.Smith.combineCols j (- a_1j `div` a_11) 1)
            where a_1j = getElem 1 j a
    sequenceA (fix <$> [2..ncols a])
    return ()

makeFstRowDivisible :: Integral a => SmithM a ()
makeFstRowDivisible = whileM $ do
    a <- getA
    let a_11     = getElem 1 1 a
        fstRow = V.tail $ getRow 1 a
        nonDivisibleFound = (+2) <$> V.findIndex (not . (a_11 `divides`)) fstRow
        fix j = do
          let a_1j = getElem 1 j a
              (s, t) = extendedEuNeg a_11 a_1j
              b = s*a_11+t*a_1j
              (alpha, gamma) = (a_11 `div` b, a_1j `div` b)
          Typing.Smith.l0Transform 1 j s t alpha gamma
    whenJust nonDivisibleFound fix
    return (isJust nonDivisibleFound)

l0Transform :: Integral a => Int -> Int -> a -> a -> a -> a -> SmithM a ()
l0Transform i j s t alpha gamma = S.modify transformer
  where transformer (a, (p, q)) = (a', (p, q'))
          where
            coeffs = [(s, (i, i))
                ,(t, (j, i))
                ,(-gamma, (i, j))
                ,(alpha, (j, j))
                ]
            eleM = L.foldr (uncurry M.setElem) (identity (ncols a)) coeffs
            a' = multStrassenMixed a eleM
            q' = multStrassenMixed q eleM

-- Given a,b return s,t such that as + bt = gcd(a,b).
extendedEu :: Integral a => a -> a -> (a, a)
extendedEu a 0 = (1, 0)
extendedEu a b = (t, s - q * t)
  where
    (q, r) = quotRem a b
    (s, t) = extendedEu b r

extendedEuNeg :: Integral a => a -> a-> (a, a)
extendedEuNeg a b = ((signum a)*s, (signum b)*t)
  where (s, t) = extendedEu (abs a) (abs b)

-- | moves the smallest non-zero element to (1,1), assumes a is non-zero.
moveSmallest :: Integral a => SmithM a ()
moveSmallest = do
  a <- getA
  swap (1,1) (smallestNonZeroIndex a)

getA :: SmithM a (Matrix a)
getA = fst <$> get

swap :: Integral a => (Int,Int) -> (Int,Int) -> SmithM a ()
swap (i,j) (k,l) = do
  Typing.Smith.switchCols j l
  Typing.Smith.switchRows i k

transpose :: SmithM a ()
transpose = S.modify transposer
  where
    transposer (a, (p, q)) = (M.transpose a, (M.transpose q, M.transpose p))

switchCols :: Integral a => Int -> Int -> SmithM a ()
switchCols i j = when (i/=j) $ do
  Typing.Smith.transpose
  Typing.Smith.switchRows i j
  Typing.Smith.transpose

combineCols :: Integral a => Int -> a -> Int -> SmithM a ()
combineCols i s j = do
  Typing.Smith.transpose
  Typing.Smith.combineRows i s j
  Typing.Smith.transpose

swapSignRow :: Integral a => Int -> SmithM a ()
swapSignRow i = S.modify scaler
  where scaler (a, (p, q)) = (a', (p', q))
          where
            a' = M.scaleRow (-1) i a
            eleM = M.setElem (-1) (i, i) (identity (nrows a))
            p' = multStrassenMixed eleM p

combineRows :: Integral a => Int -> a -> Int -> SmithM a ()
combineRows i s j = S.modify combiner
  where combiner (a, (p, q)) = (a', (p', q))
          where
            a' = M.combineRows i s j a
            eleM = M.setElem s (i, j) (identity (nrows a))
            p' = multStrassenMixed eleM p

switchRows :: Integral a => Int -> Int -> SmithM a ()
switchRows i j = when (i/=j) $ S.modify switcher
  where switcher (a, (p, q)) = (a', (p', q))
          where
            a' = M.switchRows i j a
            eleM = M.permMatrix (nrows a) i j
            p' = multStrassenMixed eleM p


-- | returns the index of the entry with the smallest (non-zero) magnitude.
--   will error if passed the zero matrix.
smallestNonZeroIndex :: Integral a => Matrix a -> (Int,Int)
smallestNonZeroIndex a = snd $ L.minimum $
          [(abs $ getElem i j a, (i,j)) | i <- [1..m], j <- [1..n]
                                   , getElem i j a /= 0]
       where m = nrows a
             n = ncols a
