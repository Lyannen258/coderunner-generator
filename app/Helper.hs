module Helper where

import Debug.Trace
import qualified Data.Graph.Inductive as G
import qualified Data.List as L (find)

debug :: Show a => a -> a
debug a = trace (show a) a

maybeToEither :: Maybe a -> String -> Either String a
maybeToEither (Just a) _ = Right a
maybeToEither Nothing s = Left s

index :: (G.DynGraph gr, Eq a) => a -> gr a b -> Maybe G.Node
index lbl g = do
  fst <$> maybeNode
  where
    nodes = G.labNodes g
    predicate (n, curLbl) = lbl == curLbl
    maybeNode = L.find predicate nodes