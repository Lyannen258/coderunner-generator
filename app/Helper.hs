module Helper where

import qualified Data.List as L (find)
import Debug.Pretty.Simple

debug :: Show a => a -> a
debug a = pTrace (show a) a

maybeToEither :: Maybe a -> String -> Either String a
maybeToEither (Just a) _ = Right a
maybeToEither Nothing s = Left s

toTuple2 :: [a] -> (a, a)
toTuple2 (x : y : l) = (x, y)
toTuple2 l = error "Cannot convert a list with size < 2 to tuple of size 2"