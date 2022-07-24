module Utils where

import           Data.Bits
import           Debug.Trace

dprint s v = traceShow (s, v) v

-- logarithm in base 2 (floor)
log2 :: Int -> Int
log2 x = finiteBitSize x - 1 - countLeadingZeros x
