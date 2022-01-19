{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module CoderunnerGenerator.SemanticAnalyzer where

import Brick (getContext)
import qualified CoderunnerGenerator.Parser as P
import CoderunnerGenerator.Types.AbstractSyntaxTree as AST
import CoderunnerGenerator.Types.ConstraintGraph ((##>), (#>))
import qualified CoderunnerGenerator.Types.ConstraintGraph as CG
import CoderunnerGenerator.Types.SymbolTable
  ( Enumeration,
    EnumerationValue,
    Property,
    RequiresRule,
    SymbolInformation,
    SymbolTable,
  )
import qualified CoderunnerGenerator.Types.SymbolTable as ST
import Control.Monad (join)
import Control.Monad.Trans.Reader
import Data.List
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Set (fromList)
import Debug.Trace
import Lens.Micro (each, (^.), (^..), _2)
import Lens.Micro.Extras (view)

-- * Semantic Result

type SemanticResult = Either String (SymbolTable, CG.ConstraintGraph)

liftSymbolTable :: SymbolTable -> SemanticResult
liftSymbolTable st = Right (st, CG.empty)

-- * Semantic Analysis

semanticAnalysis :: Template -> SemanticResult
semanticAnalysis ast = do
  result <- analyzeTemplate ast
  let symbolTableRaw = fst result
  let graph = (CG.rmNonReciprocEdges . CG.addImplicitEdges) (snd result)
  symbolTable <- getBPUsageProperties symbolTableRaw
  return (symbolTable, graph)

analyzeTemplate :: Template -> SemanticResult
analyzeTemplate template = do
  let statements = view parameterStatements template
  partialResults <- mapM analyzeStatement statements
  let (tables, graphs) = unzip partialResults
  mergedTables <- ST.mergeMany tables
  let mergedGraphs = CG.merge graphs
  return (mergedTables, mergedGraphs)

analyzeStatement :: ParameterStatement -> SemanticResult
analyzeStatement (EnumerationStatement e) =
  analyzeEnumeration e
analyzeStatement (GenerationStatement g) =
  liftSymbolTable $ analyzeGeneration g
analyzeStatement (BlueprintStatement bp) =
  liftSymbolTable $ analyzeBlueprint bp
analyzeStatement (BlueprintUsageStatement bpu) =
  liftSymbolTable $ analyzeBlueprintUsage bpu

-- Analyze Enumeration Parameter Statement

analyzeEnumeration :: AST.Enumeration -> SemanticResult
analyzeEnumeration (Enumeration _ part1 mPart2) = do
  let (id1, enumInfo1) = analyzeEnumerationPart part1
  let maybeReqPart = analyzeRequiresPart mPart2
  enumInfoWithRules <- (case maybeReqPart of
        Nothing -> Right enumInfo1
        Just (id, enumInfo) -> enrichWithRules enumInfo1 id enumInfo)
  let evList = (id1, enumInfo1) : maybeToList maybeReqPart
  let evMap = Map.fromList evList
  let m = Map.map ST.EnumerationSymbol evMap
  let g = buildGraphFromEnumInfo id1 enumInfoWithRules
  return (m, g)

analyzeEnumerationPart :: EnumerationPart -> (Identifier, [EnumerationValue])
analyzeEnumerationPart part =
  let id = view identifier part
      vs = view values part
      enumValues = map (`ST.EnumerationValue` []) vs
   in (id, enumValues)

analyzeRequiresPart :: Maybe EnumerationPart -> Maybe (Identifier, [EnumerationValue])
analyzeRequiresPart (Just enumPart) =
  Just $ analyzeEnumerationPart enumPart
analyzeRequiresPart Nothing = Nothing

buildGraphFromEnumInfo :: String -> ST.Enumeration -> CG.ConstraintGraph
buildGraphFromEnumInfo param vs = CG.merge partialGraphs
  where
    partialGraphs = map enumValueGraph vs

    enumValueGraph :: EnumerationValue -> CG.ConstraintGraph
    enumValueGraph ev =
      CG.node' (param, ST.enumValue ev)
        ##> map (reqRuleGraph $ ST.enumValue ev) (ST.rules ev)

    reqRuleGraph :: String -> RequiresRule -> CG.ConstraintGraph
    reqRuleGraph srcValue (ST.RequiresValue dstParam dstValue) =
      CG.edge' (param, srcValue) (dstParam, dstValue)
    reqRuleGraph srcValue (ST.SetsValueArea dstParam dstValues) =
      CG.edge (CG.Value param [srcValue], CG.Value dstParam dstValues)

-- Requires Rule Functions

enrichWithRules :: ST.Enumeration -> String -> ST.Enumeration -> Either String ST.Enumeration
enrichWithRules base reqId requires
  | length base == length requires =
    enrichWithValueRule base reqId requires
  | length base == 1 && length requires > 1 =
    enrichWithValueAreaRule base reqId requires
  | otherwise =
    Left "Amount of enumeration values on the left and right side of 'Requires' must be equal or amount on the left side must be 1"

enrichWithValueRule :: ST.Enumeration -> String -> ST.Enumeration -> Either String ST.Enumeration
enrichWithValueRule base reqId requires =
  Right $ oneRulePerEnumValue base (getRequiresValueRules reqId requires)

getRequiresValueRule :: String -> EnumerationValue -> RequiresRule
getRequiresValueRule id b = ST.RequiresValue id (ST.enumValue b)

getRequiresValueRules :: String -> ST.Enumeration -> [RequiresRule]
getRequiresValueRules reqId =
  map (getRequiresValueRule reqId)

oneRulePerEnumValue :: [EnumerationValue] -> [RequiresRule] -> ST.Enumeration
oneRulePerEnumValue = zipWith addRuleToEnumValue

addRuleToEnumValue :: EnumerationValue -> RequiresRule -> EnumerationValue
addRuleToEnumValue enum rule =
  ST.EnumerationValue (ST.enumValue enum) (rule : ST.rules enum)

addRulesToEnumValue :: EnumerationValue -> [RequiresRule] -> EnumerationValue
addRulesToEnumValue enum ruleList =
  ST.EnumerationValue (ST.enumValue enum) (ruleList ++ ST.rules enum)

enrichWithValueAreaRule :: ST.Enumeration -> String -> ST.Enumeration -> Either String ST.Enumeration
enrichWithValueAreaRule base id requires =
  Right
    [ addRuleToEnumValue
        (head base)
        (getRequiresValueAreaRule id requires)
    ]

getRequiresValueAreaRule :: String -> ST.Enumeration -> RequiresRule
getRequiresValueAreaRule reqId requires =
  ST.SetsValueArea reqId (map ST.enumValue requires)

-- Analyze Generation Statements

analyzeGeneration :: Generation -> SymbolTable
analyzeGeneration (Generation _ id mixed) =
  Map.singleton id (ST.GenerationSymbol mixed)

-- Analyze Blueprint Statements

analyzeBlueprint :: Blueprint -> SymbolTable
analyzeBlueprint (Blueprint _ id props ellipse) =
  Map.singleton id $ ST.BlueprintSymbol $ ST.Blueprint props ellipse

-- Analyze BlueprintUsage Statements

analyzeBlueprintUsage :: BlueprintUsage -> SymbolTable
analyzeBlueprintUsage (BlueprintUsage _ id bp vs) =
  Map.singleton id $ ST.BlueprintUsagePreSymbol $ ST.BlueprintUsagePre bp vs

-- BlueprintUsage add properties and values

getBPUsageProperties :: SymbolTable -> Either String SymbolTable
getBPUsageProperties table =
  do
    let onlyBUPre = ST.onlyBlueprintUsagePre table
    bpuMap <- mapM (addBlueprintUsageProperties table) onlyBUPre
    let tablePart = Map.map ST.BlueprintUsageSymbol bpuMap
    return $ Map.union tablePart table -- union prefers left map in case of duplicate keys

findBlueprint :: String -> SymbolTable -> Either String ST.Blueprint
findBlueprint k table =
  let maybeBP = Map.lookup k table
   in case maybeBP of
        Just (ST.BlueprintSymbol bp) -> Right bp
        _ -> Left ("Blueprint " ++ k ++ " does not exist.")

addBlueprintUsageProperties :: SymbolTable -> ST.BlueprintUsagePre -> Either String ST.BlueprintUsage
addBlueprintUsageProperties table bpuPre = do
  bp <- findBlueprint (ST.preBlueprint bpuPre) table
  propValueZip <- zipPropsToValues (ST.properties bp) (ST.preValues bpuPre)
  avs <- additionalValues bp (ST.preValues bpuPre)
  return $
    ST.BlueprintUsage
      (ST.preBlueprint bpuPre)
      propValueZip
      avs

zipPropsToValues :: [ST.Property] -> [String] -> Either String (Map.Map String String)
zipPropsToValues ps ss
  | length ps <= length ss = Right $ Map.fromList $ zip ps ss
  | otherwise = Left "Not enough values for properties in blueprint usage."

additionalValues :: ST.Blueprint -> [String] -> Either String [String]
additionalValues bp ss
  | length (ST.properties bp) == length ss = Right []
  | length (ST.properties bp) < length ss && ST.hasEllipse bp =
    Right $ drop (length (ST.properties bp)) ss
  | otherwise = Left "More values as properties in blueprint usage. Blueprint has no ellipse"
