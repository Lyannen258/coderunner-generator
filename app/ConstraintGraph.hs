{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module ConstraintGraph
  ( Value (..),
    Constraint (..),
    ConstraintGraph,
    Edge,
    Context(..),
    nodes,
    edges,
    merge,
    merge2,
    empty,
    node,
    edge,
    (#>),
    (##>),
    parameters,
    gmap,
    parameter,
    configs,
    addOneOfEdges,
    rmNonReciprocEdges
  )
where

import Data.Graph.Inductive (Gr)
import qualified Data.Graph.Inductive as G
import Data.List (nub, union, (\\))
import Data.Maybe (catMaybes, fromJust)
import Lens.Micro (each, (^.), (^..), _2)
import Lens.Micro.TH (makeLenses)
import Data.Tree (Tree)

-- | Data type for a node in the constraint graph
data Value = Value
  { _parameter :: String,
    _value :: String
  }
  deriving (Eq, Ord, Show)

makeLenses ''Value

-- | Data type for an edge in the constraint graph
data Constraint
  = Exact
  | OneOf
  | AllOf
  deriving (Eq, Ord, Show)

-- | The constraint graph
--
-- Holds the dependencies between parameter values
type ConstraintGraph = Gr Value Constraint

-- | Overwrites the standard edge.
-- Additionally contains node labels
type Edge = (Value, Value, Constraint)

-- | Context inspired by fgl context
data Context = Context
  { _self :: Value,
    _to :: [Edge],
    _from :: [Edge]
  }

context :: Value -> ConstraintGraph -> Context
context v g = Context v to from
  where
    to = filter predTo (edges g)
    predTo (_, dst, _) = dst == v
    from = filter predFrom (edges g)
    predFrom (src, _, _) = src == v

contextToGraph :: Context -> ConstraintGraph
contextToGraph (Context self to from) =
  node (self ^. parameter, self ^. value)
    ##> map edge' to
    ##> map edge' from

-- Getters
----------

-- | Overwrites default.
-- Returns a list of all values in the constraint graph.
nodes :: ConstraintGraph -> [Value]
nodes g = map snd (G.labNodes g)

-- | Overwrites default.
-- Returns a list of all constraints in the constraint graph.
edges :: ConstraintGraph -> [Edge]
edges g = catMaybes maybeNodeMapEdges
  where
    maybeNodeMapEdges = map (getEdge g) (G.labEdges g)

-- | Gets the values (node labels) for the src and dst of an edge.
getEdge :: ConstraintGraph -> (Int, Int, Constraint) -> Maybe Edge
getEdge g (a, b, lbl) = do
  labelA <- G.lab g a
  labelB <- G.lab g b
  return (labelA, labelB, lbl)

-- Construction
---------------

-- | Merges two graphs
--
-- Takes two graphs and merges them into one.
-- Drops already contained edges/nodes.
merge2 :: ConstraintGraph -> ConstraintGraph -> ConstraintGraph
merge2 a b = G.run_ b mergeA
  where
    labelUnion = nub $ nodes a `union` nodes b
    edgeUnion = nub $ edges a `union` edges b
    mergeA = do
      -- using the NodeMap feature of fgl
      G.insMapNodesM labelUnion
      G.insMapEdgesM edgeUnion
-- | Merges multiple graphs
--
-- Takes a list of graph and merges them into one.
-- Drops already contained edges/nodes.
merge :: [ConstraintGraph] -> ConstraintGraph
merge l
  | length l >= 2 = foldr1 merge2 l
  | length l == 1 = head l
  | otherwise = empty

-- | Empty graph
empty :: ConstraintGraph
empty = G.empty

-- | Operator alias for merge2
(#>) :: ConstraintGraph -> ConstraintGraph -> ConstraintGraph
(#>) = merge2

infixl 1 #>

-- | Merge single graph in a list of grahs
(##>) :: ConstraintGraph -> [ConstraintGraph] -> ConstraintGraph
g ##> gs = merge (g : gs)

infixl 1 ##>

-- | Node constructor
--
-- Takes two strings and constructs a constraint graph.
-- First string is parameter, second is value.
node :: (String, String) -> ConstraintGraph
node (p, v) = G.insNode (0, Value p v) G.empty

-- | Edge constructor
--
-- Constructs a constraint graph containing just an edge.
edge :: (String, String) -> (String, String) -> Constraint -> ConstraintGraph
edge v1p v2p e = G.mkGraph [(0, v1), (1, v2)] [(0, 1, e)]
  where
    v1 = uncurry Value v1p
    v2 = uncurry Value v2p

-- | Edge constructor
--
-- Constructs a graph from an Edge value
edge' :: Edge -> ConstraintGraph
edge' (src, dst, c) = G.mkGraph [(0, src), (1, dst)] [(0, 1, c)]

-- | Node constructor
--
-- Same as node but, with Value instead of tuple
node' :: Value -> ConstraintGraph
node' (Value p v) = node (p, v)

-- Graph Information
--------------------

-- | Gives a list of all contained parameters
parameters :: ConstraintGraph -> [String]
parameters g = nub (nodes g ^.. each . parameter)

-- | Returns all Values for the given parameter
valuesFor :: String -> ConstraintGraph -> [Value]
valuesFor param g = filter f (nodes g)
  where
    f v = (v ^. parameter) == param

-- | Gives a list of all valid configurations
configs :: ConstraintGraph -> [[Value]]
configs g = [[]]
  where
    valuesFor' = flip valuesFor
    valuesByParam = map (valuesFor' g) (parameters g)
    --allConfigs = 

    combine :: [[a]] -> [[a]]
    combine [] = []
    combine (x:[]) = map (: []) x
    combine (x1:x2:xs) = [[x,y] | x<-x1, y<-x2]

-- Map
------

-- | Maps a function over every node in a constraint graph
--
-- The function takes a value as a parameter and returns a new partial
-- constraint graph. The set of resulting constraint graph is then merged.
gmap :: (Context -> Context) -> ConstraintGraph -> ConstraintGraph
gmap f g = merge partialGraphs
  where
    ns = nodes g
    contexts = map (`context` g) ns
    partialContexts = map f contexts
    partialGraphs = map contextToGraph partialContexts

-- Specific Adjustements
------------------------

-- | Add One-Of edges to constraint graph
addOneOfEdges :: ConstraintGraph -> ConstraintGraph
addOneOfEdges g = gmap perNode g
  where
    perNode :: Context -> Context
    perNode (Context self to from) =
      let edgesWithoutAllOf = filter (\(_, _, e) -> e /= AllOf) from
          coveredParams = edgesWithoutAllOf ^.. each . _2 . parameter
          coveredParams' = nub $ self ^. parameter : coveredParams
          missingParams = parameters g \\ coveredParams'
          missingValues = filter (\(Value p _) -> p `elem` missingParams) (nodes g)
          -- The weird tuple in the following expression is the same as following lambda:
          -- (\newValue -> (self, newValue, OneOf))
          -- See tuple sections for further information
          newEdges = map (self,,OneOf) missingValues
       in Context self to (from ++ newEdges)

-- | Remove non-reciproc edges
rmNonReciprocEdges :: ConstraintGraph -> ConstraintGraph
rmNonReciprocEdges g = G.delEdges esToDelete g
  where
    es = G.edges g
    esToDelete = filter (f es) es
      where
        f es (v1, v2) = (v2, v1) `notElem` es