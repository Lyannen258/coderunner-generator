module Helper where

import Debug.Trace

debug :: Show a => a -> a
debug a = trace (show a) a