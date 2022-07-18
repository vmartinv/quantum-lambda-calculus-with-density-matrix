module Utils where

import           Debug.Trace

dprint s v = traceShow (s, v) v
