{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module ConstraintGraph
  ( Value (..),
    ConstraintGraph,
    Edge,
    values,
    edges,
    merge,
    empty,
    node,
    node',
    edge,
    edge',
    (#>),
    (##>),
    parameters,
    parameter,
    configs,
    addMissingEdges,
    rmNonReciprocEdges,
  )
where

import Data.List (intercalate)
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Helper
import Lens.Micro (each, (^.), (^..), _2)
import Lens.Micro.TH (makeLenses)

-- | Data type for a node in the constraint graph
data Value = Value
  { _parameter :: String,
    _value :: [String]
  }
  deriving (Eq, Ord)

makeLenses ''Value

instance Show Value where
  show (Value p v) = p ++ " " ++ intercalate "," v

-- | Data type for an edge in the constraint graph
type Edge = (Value, Value)

-- | The constraint graph
--
-- Holds the dependencies between parameter values
data ConstraintGraph = ConstraintGraph (Set Value) (Set Edge)
  deriving (Show)

-- Getters
----------

-- | Returns a set of all values in the constraint graph.
values :: ConstraintGraph -> Set Value
values (ConstraintGraph vs _) = vs

-- | Returns a set of all edges in the constraint graph.
edges :: ConstraintGraph -> Set Edge
edges (ConstraintGraph _ es) = es

-- Construction
---------------

-- | Merges multiple graphs
--
-- Takes a list of graphs and merges them into one.
-- Drops already contained edges/nodes.
merge :: [ConstraintGraph] -> ConstraintGraph
merge l = ConstraintGraph vs es
  where
    vs = S.unions $ map values l
    es = S.unions $ map edges l

-- | Empty graph
empty :: ConstraintGraph
empty = ConstraintGraph S.empty S.empty

-- | Merge two graphs
(#>) :: ConstraintGraph -> ConstraintGraph -> ConstraintGraph
a #> b = merge [a, b]

infixl 1 #>

-- | Merge single graph in a list of grahs
(##>) :: ConstraintGraph -> [ConstraintGraph] -> ConstraintGraph
g ##> gs = merge (g : gs)

infixl 1 ##>

-- | Node constructor
--
-- Constructs a constraint graph from a value
node :: Value -> ConstraintGraph
node v = ConstraintGraph (S.singleton v) S.empty

-- | Edge constructor
--
-- Constructs a constraint graph from an Edge
edge :: Edge -> ConstraintGraph
edge (src, dst) = ConstraintGraph vs es
  where
    vs = S.fromList [src, dst]
    es = S.singleton (src, dst)

-- | Node constructor
--
-- Takes two strings and constructs a constraint graph.
-- First string is parameter, second is value.
node' :: (String, String) -> ConstraintGraph
node' (p, v) = node $ Value p [v]

-- | Edge constructor
--
-- Constructs a constraint graph containing just an edge.
edge' :: (String, String) -> (String, String) -> ConstraintGraph
edge' (p1, v1) (p2, v2) = edge (n1, n2)
  where
    n1 = Value p1 [v1]
    n2 = Value p2 [v2]

-- Destruction
--------------

removeEdge :: Edge -> ConstraintGraph -> ConstraintGraph
removeEdge e g = ConstraintGraph vs es
  where
    vs = values g
    es = S.delete e (edges g)

removeEdges :: Set Edge -> ConstraintGraph -> ConstraintGraph
removeEdges es g = ConstraintGraph vs newEdges
  where
    vs = values g
    newEdges = edges g \\ es

-- Graph Information
--------------------

-- | Returns a set of all contained parameters
parameters :: ConstraintGraph -> Set String
parameters g = S.map _parameter (values g)

-- | Returns all Values for the given parameter
valuesFor :: String -> ConstraintGraph -> Set Value
valuesFor param g = S.filter f (values g)
  where
    f v = v ^. parameter == param

-- | Find all cliques of size k in the constraint graph
--
-- Cliques are a concept of graph theory.
--
-- Algorithm based on https://iq.opengenus.org/algorithm-to-find-cliques-of-a-given-size-k/
kCliques :: Int -> ConstraintGraph -> Set (Set Value)
kCliques k g
  | k < 2 = S.empty
  | k == 2 = S.map (\(v1, v2) -> S.insert v2 $ S.singleton v1) $ edges g
  | otherwise = cliques
  where
    kminus1 = kCliques (k - 1) g
    cp = S.cartesianProduct kminus1 kminus1
    cliques = foldr findCliques S.empty cp
    findCliques (s1, s2) acc =
      if isEdgeExists
        then S.insert u acc
        else acc
      where
        is = S.intersection s1 s2
        u = S.union s1 s2
        symDiff = u \\ is -- symmetric difference
        size_sd = S.size symDiff
        isTuple = toTuple2 $ S.toList symDiff
        isEdgeExists = size_sd == 2 && (isTuple `S.member` edges g)

-- | Finds a set of all valid configurations
configs :: ConstraintGraph -> Set (Set Value)
configs g = kCliques k g
  where
    k = S.size $ parameters g

-- Specific Adjustements
------------------------

addMissingEdges :: ConstraintGraph -> ConstraintGraph
addMissingEdges g = ConstraintGraph (values g) (edges g `S.union` additionalEdges)
  where
    additionalEdges = S.unions $ S.map addPerNode (values g)

    addPerNode :: Value -> Set Edge
    addPerNode v = newEdges
      where
        outEdges = S.filter (\(src, _) -> src == v) (edges g)
        reachableNodes = S.map snd outEdges
        coveredParams = S.insert (v ^. parameter) (S.map _parameter reachableNodes)
        missingParams = parameters g \\ coveredParams
        missingValues = S.filter (\(Value p _) -> p `S.member` missingParams) (values g)
        newEdges = S.map (v,) missingValues

-- | Remove non-reciproc edges
rmNonReciprocEdges :: ConstraintGraph -> ConstraintGraph
rmNonReciprocEdges g = removeEdges esToDelete g
  where
    es = edges g
    esToDelete = S.filter (f es) es
      where
        f es (v1, v2) = (v2, v1) `notElem` es