module Typing.Litchman where

import           Control.Monad.Extra
import           Data.Matrix               as M
import           Data.Ratio
import qualified Data.Vector               as V
import           Debug.Trace
import           Numeric.LinearProgramming
import           Typing.Smith
import Data.Maybe
import Control.Exception
import Data.Tuple.Extra
import Typing.Utils

joinSystems :: LinSystem a -> LinSystem a -> LinSystem a
joinSystems (a, b) (a', b') = (a <-> a', b <> b')

-- solves AX>B, X in Z^n, if there's a solution
solveIneq :: Show a => Integral a => LinSystem a -> Maybe (V.Vector (VarSol a))
solveIneq ab@(a, b) --TODO: | isJust (solveIDefinite ab) = solveIDefinite ab
                    | ncols a == 1 = V.singleton <$> solveSingleVarIneq ab
                    | otherwise = Nothing --TODO: combineSol ab <$> solveIneq subsystem
      where ((pa, pb), (na, nb), (za, zb)) = mkPNZPartition ab
            zeqs = (M.submatrix 1 (nrows za) 2 (ncols za) za, zb)
            npeqs = [mkDetEq (M.getRow i pa, pb V.! (i-1)) (M.getRow j na, nb V.! (j-1)) | i <- [1..(nrows pa)], j <- [1..(nrows na)]]
            subsystem = foldr joinSystems emptySystem (zeqs:(systemSingleton <$> npeqs))

-- combineSol :: Integral a => LineSystem a -> Vector (VarSol a) -> Vector (VarSol a)
-- combineSol (a, b) x =

-- solves AX>B, X in Z^n, rows(A)=1, if there's a solution
solveSingleVarIneq :: Integral a => LinSystem a -> Maybe (VarSol a)
solveSingleVarIneq (a, b) | any isNothing solutions = Nothing
                          | not (allTheSame intSolutions) = Nothing
                          | intSolutions == [] = Just AnyValue
                          | otherwise = Just (head intSolutions)
      where c = M.getCol 1 a
            solveEq 0 0 = Nothing
            solveEq 0 b = Just AnyValue
            solveEq a b | b `divides` a = Just (Value (b `div` a + 1))
                        | otherwise = Just (Value (b `div` a))
            solutions = V.toList $ (uncurry solveEq) <$> V.zip c b
            intSolutions = filter (/=AnyValue) (values solutions)


systemSingleton :: (V.Vector a, a) -> LinSystem a
systemSingleton (a, b) = (M.fromLists [V.toList a], V.fromList [b])

emptySystem :: LinSystem a
emptySystem = (M.fromLists [[]], V.empty)

mkDetEq :: Integral a => (V.Vector a, a) -> (V.Vector a, a) -> (V.Vector a, a)
mkDetEq (pa, pb) (na, nb) = ((uncurry detWithCol1) <$> V.zip pa na, detWithCol1 pb nb)
  where
    det a b c d =  (a*c)-(b*d)
    detWithCol1 ai aj = det (pa V.! 0) ai (na V.!0) aj


mkPNZPartition :: Integral a => LinSystem a -> (LinSystem a, LinSystem a, LinSystem a)
mkPNZPartition (a, b) = (partition (>0), partition (<0), partition (==0))
       where m = nrows a
             partition criteria = (fromListsFixed *** V.fromList) $
                unzip [
                  (V.toList (M.getRow i a), b V.! (i-1))
                    | i <- [1..m], criteria (getElem i 1 a)
                ]

isIDefinite :: Integral a => Matrix a -> Bool
isIDefinite a = and [V.all (/= 0) (M.getCol j a) | j <- [1..n]]
       where n = ncols a
