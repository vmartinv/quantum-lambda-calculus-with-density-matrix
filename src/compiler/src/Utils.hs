{-# LANGUAGE CPP #-}
module Utils where

import           Data.Bits
import           Debug.Trace


-- stack test --ghc-options="-DDEBUG" --test-arguments="-p tests.translationTests.prefix"
#ifdef DEBUG
dprint s v = traceShow (s, v) v
#else
dprint s v = v
#endif

-- logarithm in base 2 (floor)
log2 :: Int -> Int
log2 x = finiteBitSize x - 1 - countLeadingZeros x
