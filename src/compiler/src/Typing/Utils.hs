module Typing.Utils where

import           Data.Matrix as M
import           Data.Maybe
import qualified Data.Vector as V
import           Debug.Trace

type LinSystem a = (Matrix a, V.Vector a)

data VarSol a = Value a | AnyValue
  deriving (Eq, Show)

dprint s v = traceShow (s, v) v

unVarSol :: VarSol a -> Maybe a
unVarSol AnyValue  = Nothing
unVarSol (Value x) = Just x

allTheSame :: (Eq a) => [a] -> Bool
allTheSame [] = True
allTheSame xs = and $ map (== head xs) (tail xs)

-- | a `divides` b <=> a | b
divides :: Integral a => a -> a -> Bool
divides a b = b `rem` a == 0

-- | checks whether a divides every element
dividesAll :: (Foldable f, Integral a) => a -> f a -> Bool
dividesAll a = all (divides a)

isZero :: Integral a => Matrix a -> Bool
isZero = all (==0)

fromListsFixed :: [[a]] -> Matrix a
fromListsFixed [] = M.fromLists [[]]
fromListsFixed a  = M.fromLists a

values :: [Maybe a] -> [a]
values []           = []
values (Just x:xs)  = x:(values xs)
values (Nothing:xs) = values xs
