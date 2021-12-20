module Helper where

import qualified Data.List as L (find)
import Debug.Trace

debug :: Show a => a -> a
debug a = trace (show a) a

maybeToEither :: Maybe a -> String -> Either String a
maybeToEither (Just a) _ = Right a
maybeToEither Nothing s = Left s