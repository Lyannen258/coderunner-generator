module Helper where

import Debug.Trace

debug :: Show a => a -> a
debug a = trace (show a) a

maybeToEither :: Maybe a -> String -> Either String a
maybeToEither (Just a) _ = Right a
maybeToEither Nothing s = Left s