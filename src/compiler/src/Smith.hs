module Smith
( getSmithForm
) where

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
getSmithForm a = runSmith smith a

runSmith :: Integral a => SmithM a () -> Matrix a -> SmithState a
runSmith m a = execState m (startState a)

startState :: Integral a => Matrix a -> SmithState a
startState a = (a, (p, q))
  where
    p = identity (ncols a)
    q = identity (nrows a)

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
solveSmallerAndCombine (aBig, (pBig, qBig)) =  (aRes, (pRes, qRes))
  where
      (tl,tr,bl,br) = splitBlocks 1 1 aBig
      (aSmall, (pSmall, qSmall)) = runSmith smith br
      aRes = M.joinBlocks (tl,tr,bl,aSmall)
      pRes = multStrassenMixed (extendElementary pSmall) pBig
      qRes = multStrassenMixed (extendElementary qSmall) qBig

extendElementary :: Integral a => Matrix a -> Matrix a
extendElementary a = M.joinBlocks (identity 1, zero 1 (ncols a), zero (nrows a) 1, a)

pivot :: Integral a => SmithM a ()
pivot = whileM $ do
  makeFstRowAndColDivisible
  zeroOutFstRow
  Smith.transpose
  zeroOutFstRow
  Smith.transpose
  performCase3

performCase3 :: Integral a => SmithM a Bool
performCase3 = do
  a <- getA
  let a_11 = getElem 1 1 a
      nonDivisible = L.find (\j -> not $ a_11 `dividesAll` (getRow j a))
                 [2..nrows a]
      fix i = Smith.combineRows 1 1 i
  whenJust nonDivisible fix
  return (isJust nonDivisible)


zeroOutFstRow :: Integral a => SmithM a ()
zeroOutFstRow = do
  a <- getA
  let a_11 = getElem 1 1 a
      fstRow = L.tail $ L.zip [1..] $ V.toList $ (getCol 1 a)
      clearRow (j, a_1j) = Smith.combineRows j (- a_1j `div` a_11) 1
  sequenceA (L.map clearRow fstRow)
  return ()

-- performs iteratively case 1 and case 2 until elements are divisables
makeFstRowAndColDivisible :: Integral a => SmithM a ()
makeFstRowAndColDivisible = whileM $ do
  whileM makeFstRowDivisibleStep
  Smith.transpose
  whileM makeFstRowDivisibleStep
  Smith.transpose
  makeFstRowDivisibleStep

-- makes one step as described in case 1, returns true if it made changes
makeFstRowDivisibleStep :: Integral a => SmithM a Bool
makeFstRowDivisibleStep = do
    a <- getA
    let a_11     = getElem 1 1 a
        fstRow = (getRow 1 a)
        nonDivisable = (+1) <$> V.findIndex (not . (a_11 `divides`)) fstRow
        fix j = do
          let a_1j = getElem 1 j a
              q = a_11 `div` a_1j
          Smith.combineRows j q 1
          Smith.switchCols 1 j
    whenJust nonDivisable fix
    return (isJust nonDivisable)


-- | moves the smallest non-zero element to (1,1), assumes a is non-zero.
moveSmallest :: Integral a => SmithM a ()
moveSmallest = do
  a <- getA
  swap (1,1) (smallestNonZeroIndex a)

getA :: SmithM a (Matrix a)
getA = fst <$> get

swap :: Integral a => (Int,Int) -> (Int,Int) -> SmithM a ()
swap (i,j) (k,l) = do
  Smith.switchCols j l
  Smith.switchRows i k

combineRows :: Integral a => Int -> a -> Int -> SmithM a ()
combineRows i s j = S.modify combiner
  where combiner (a, (p, q)) = (a', (p', q))
          where
            a' = M.combineRows i s j a
            eleM = M.setElem s (i, j) (identity (ncols a'))
            p' = multStrassenMixed eleM p

transpose :: SmithM a ()
transpose = S.modify transposer
  where
    transposer (a, (p, q)) = (M.transpose a, (M.transpose q, M.transpose p))

switchCols :: Integral a => Int -> Int -> SmithM a ()
switchCols i j = do
  Smith.transpose
  Smith.switchRows i j
  Smith.transpose

switchRows :: Integral a => Int -> Int -> SmithM a ()
switchRows i j = S.modify switcher
  where switcher (a, (p, q)) = (a', (p', q))
          where
            a' = M.switchRows i j a
            eleM = M.switchRows i j (identity (ncols a'))
            p' = multStrassenMixed eleM p


-- | returns the index of the entry with the smallest (non-zero) magnitude.
--   will error if passed the zero matrix.
smallestNonZeroIndex :: Integral a => Matrix a -> (Int,Int)
smallestNonZeroIndex = snd . V.minimum . filterZeroes . M.getMatrixAsVector . (M.mapPos mkPair)
  where
      mkPair p v = (abs v, p)
      filterZeroes = V.filter ((/=0) . fst)
