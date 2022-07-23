module Utils where

import           Debug.Trace

dprint s v = traceShow (s, v) v

-- logarithm in base 2, rounded
log2 :: Int -> Int
log2 = floor . logBase 2.0 . fromIntegral
