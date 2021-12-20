{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module SemanticAnalyzer where

import Brick (getContext)
import ConstraintGraph ((##>), (#>))
import qualified ConstraintGraph as CG
import Control.Monad (join)
import Data.List
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Set (fromList)
import Data.Tree (drawTree)
import Debug.Trace
import Lens.Micro (each, (^.), (^..), _2)
import Lens.Micro.TH (makeLenses)
import Parser
  ( AST (AST, children, label),
    Label
      ( Blueprint,
        BlueprintUsage,
        Enumeration,
        Generation,
        Identifier,
        ParameterDefinition,
        ParameterInformation,
        ParameterStatement,
        Requires,
        Value
      ),
  )
import qualified Parser as P

-- Problem: siehe $INPUT im ersten Beispiel. Da brauchen wir eine Liste.
-- Das wird aber schwierig, wenn mehrere Parameter Listen sind, eine konsistente
-- Konfiguration zu behalten. Möglicherweise doch auch die Kanten im Graph be-
-- schriften mit ONE-OF oder ALL-OF. Dann muss allerdings auch die Syntax angepasst
-- werden, da ONE-OF und ALL-OF syntaktisch identisch wären. Gutes Keyword für ALL-OF
-- statt Required wäre Set

newtype SemanticResult = SemanticResult SymbolTable

type SymbolTable = Map.Map String SymbolInformation

showSymbolTable :: SymbolTable -> String
showSymbolTable symbolTable =
  let tableBody = intercalate "\n" (map tupleToString (Map.toList symbolTable))
   in "Identifier          Type\n\n" ++ tableBody

tupleToString :: (String, SymbolInformation) -> String
tupleToString (a, b) = fillToTwenty a ++ show b

fillToTwenty :: String -> String
fillToTwenty s =
  if length s < 20
    then fillToTwenty (s ++ " ")
    else s

data SymbolInformation
  = EnumerationSymbol {possibleValues :: [EnumerationValue]}
  | GenerationSymbol [AST]
  | BlueprintSymbol {properties :: [Property]}
  | BlueprintUsageSymbol {blueprint :: String, propertyValues :: Map.Map String [String]}
  | BlueprintUsageSymbolValuesOnly {blueprint :: String, values :: [String]}

instance Show SymbolInformation where
  show (EnumerationSymbol a) = "Enumeration (" ++ intercalate "," (map show a) ++ ")"
  show (GenerationSymbol a) = "Generation"
  show (BlueprintSymbol a) = "Blueprint (" ++ intercalate "," [show a] ++ ")"
  show (BlueprintUsageSymbol a b) =
    "BlueprintUsage BP="
      ++ a
      ++ ", PropertyValues: "
      ++ intercalate ", " (map (\(a, b) -> a ++ show b) (Map.toList b))
  show (BlueprintUsageSymbolValuesOnly a b) =
    "BlueprintUsage Values Only BP="
      ++ a
      ++ ", Values: "
      ++ intercalate ", " [show b]

data EnumerationValue = EnumerationValue
  { enumValue :: String,
    rules :: [RequiresRule]
  }
  deriving (Show)

data RequiresRule
  = RequiresValue String String -- Identifier, Value
  | SetsValueArea String [String] -- Identifier, ValueArea
  deriving (Show)

data Property
  = Property {name :: String}
  | Ellipse
  deriving (Show, Eq)

semanticAnalysis :: AST -> Either String (SymbolTable, CG.ConstraintGraph)
semanticAnalysis ast = do
  result <- semanticAnalysisMain ast
  let symbolTableRaw = fst result
  let graph = (CG.rmNonReciprocEdges . CG.addOneOfEdges) (snd result)
  symbolTable <- getBPUsageProperties symbolTableRaw
  return (symbolTable, graph)

semanticAnalysisMain :: AST -> Either String (SymbolTable, CG.ConstraintGraph)
semanticAnalysisMain ast@(AST ParameterStatement _ children) = do
  case statementType ast of
    Right Enumeration -> analyzeEnumerationStatement ast
    Right Generation -> analyzeGenerationStatement ast >>= symbolTableToTuple
    Right Blueprint -> analyzeBlueprintStatement ast >>= symbolTableToTuple
    Right BlueprintUsage -> analyzeBlueprintUsageStatement ast >>= symbolTableToTuple
    Left x -> Left x
    Right _ -> Left "Unknown statement type"
semanticAnalysisMain (AST _ _ children) = do
  list <- mapM semanticAnalysisMain children
  let symbolTable = foldl mergeTableParts Map.empty (map fst list)
  let graph = CG.merge (map snd list)
  return (symbolTable, graph)

mergeTableParts :: SymbolTable -> SymbolTable -> SymbolTable
mergeTableParts t1 t2 =
  let keys1 = Map.keys t1
      keys2 = Map.keys t2
      overlappingKeys = intersect keys1 keys2
      allKeys = union keys1 keys2
      uniqueKeys = allKeys \\ overlappingKeys
   in Map.fromList $
        map (\k -> (k, mergeSymbolInfos (t1 Map.! k) (t2 Map.! k))) overlappingKeys
          ++ map (\k -> (k, Map.union t1 t2 Map.! k)) uniqueKeys

-- Analyze ParameterStatement

statementType :: AST -> Either String Label
statementType (AST Enumeration _ _) = Right Enumeration
statementType (AST Generation _ _) = Right Generation
statementType (AST Blueprint _ _) = Right Blueprint
statementType (AST BlueprintUsage _ _) = Right BlueprintUsage
statementType (AST ParameterStatement _ children) = statementType (head children)
statementType (AST ParameterDefinition _ children) = statementType $ children !! 1
statementType (AST ParameterInformation _ children) = statementType $ head children
statementType _ =
  Left "ParameterStatement does not contain a parameter definition"

symbolTableToTuple :: SymbolTable -> Either String (SymbolTable, CG.ConstraintGraph)
symbolTableToTuple st = Right (st, CG.empty)

-- Analyze Enumeration Parameter Statement

analyzeEnumerationStatement :: AST -> Either String (SymbolTable, CG.ConstraintGraph)
analyzeEnumerationStatement
  ( AST
      ParameterStatement
      _
      [ def1@(AST ParameterDefinition _ _),
        req@(AST Requires _ _),
        def2@(AST ParameterDefinition _ _)
        ]
    ) = do
    id1 <- getIdentifier def1
    enumInfo1 <- getEnumerationInfo def1
    id2 <- getIdentifier def2
    enumInfo2 <- getEnumerationInfo def2
    ruleResult <- enrichWithRules enumInfo1 id2 enumInfo2
    case ruleResult of
      ("RequiresValue", x) ->
        return
          ( Map.fromList
              [ (id1, x),
                (id2, enumInfo2)
              ],
            buildGraphFromEnumInfo id1 x
          )
      ("SetsValueArea", y) -> return (Map.singleton id1 y, CG.empty) -- TODO G.empty
      _ -> return (Map.empty, CG.empty)
analyzeEnumerationStatement
  ( AST
      ParameterStatement
      _
      [ def@(AST ParameterDefinition _ _)
        ]
    ) = do
    id <- getIdentifier def
    enumInfo <- getEnumerationInfo def
    return
      ( Map.singleton id enumInfo,
        buildGraphFromEnumInfo id enumInfo
      )
analyzeEnumerationStatement _ = Left "Malformed enumeration statement"

getEnumerationInfo :: AST -> Either String SymbolInformation
getEnumerationInfo ast@(AST ParameterDefinition _ _) = do
  values <- getEnumerationValues ast
  return $ EnumerationSymbol values
getEnumerationInfo ast = Left "Expected AST with label ParameterInformation"

getEnumerationValue :: AST -> Either String EnumerationValue
getEnumerationValue (AST Value v []) = Right $ EnumerationValue v []
getEnumerationValue _ = Left "Called getEnumerationValue on an AST that is not of type Value"

getEnumerationValues :: AST -> Either String [EnumerationValue]
getEnumerationValues (AST ParameterDefinition _ children) =
  getEnumerationValues (children !! 1)
getEnumerationValues (AST ParameterInformation _ children) =
  getEnumerationValues (head children)
getEnumerationValues (AST Enumeration _ children) =
  mapM getEnumerationValue children
getEnumerationValues ast = Left $ "Unexpected Node in Enumeration ParameterDefinition: " ++ show (label ast)

mergeSymbolInfos :: SymbolInformation -> SymbolInformation -> SymbolInformation
mergeSymbolInfos (EnumerationSymbol vs1) (EnumerationSymbol vs2) =
  EnumerationSymbol $ mergeEnumerationValues vs1 vs2

mergeSymbolInfos' a b =
  trace
    ("\nmergeSymbolInfos:\na: " ++ show a ++ "\nb: " ++ show b ++ "\nresult: " ++ show (mergeSymbolInfos a b))
    (mergeSymbolInfos a b)

mergeEnumerationValues :: [EnumerationValue] -> [EnumerationValue] -> [EnumerationValue]
mergeEnumerationValues vs1 vs2 =
  let mergedValues = concatMap mergeEnumerationValue [(a, b) | a <- vs1, b <- vs2]
      singleValues = filter filterPredicate (vs1 ++ vs2)
      filterPredicate x = enumValue x `elem` getDuplicateKeys (vs1 ++ vs2)
   in mergedValues ++ singleValues

getDuplicateKeys :: [EnumerationValue] -> [String]
getDuplicateKeys vs = Map.keys multiOccurences
  where
    occurences = countOccurences (map enumValue vs) Map.empty
    multiOccurences = Map.filter (<= 1) occurences

countOccurences :: Ord a => [a] -> Map.Map a Int -> Map.Map a Int
countOccurences (v : vs) counted =
  if Map.member v counted
    then countOccurences vs $ Map.insert v ((counted Map.! v) + 1) counted
    else countOccurences vs $ Map.insert v 1 counted
countOccurences [] counted = counted

mergeEnumerationValue :: (EnumerationValue, EnumerationValue) -> [EnumerationValue]
mergeEnumerationValue (v1, v2)
  | enumValue v1 == enumValue v2 =
    [ EnumerationValue
        (enumValue v1)
        (rules v1 ++ rules v2)
    ]
  | otherwise = []

mergeEnumerationValue' (a, b) =
  trace
    ( "\nmergeEnumerationValue:\na: "
        ++ show a
        ++ "\nb: "
        ++ show b
        ++ "\nresult: "
        ++ show (mergeEnumerationValue (a, b))
        ++ "\n"
    )
    (mergeEnumerationValue (a, b))

buildGraphFromEnumInfo :: String -> SymbolInformation -> CG.ConstraintGraph
buildGraphFromEnumInfo param (EnumerationSymbol vs) = CG.merge partialGraphs
  where
    partialGraphs = map enumValueGraph vs

    enumValueGraph :: EnumerationValue -> CG.ConstraintGraph
    enumValueGraph ev =
      CG.node (param, enumValue ev)
        ##> map (reqRuleGraph $ enumValue ev) (rules ev)

    reqRuleGraph :: String -> RequiresRule -> CG.ConstraintGraph
    reqRuleGraph srcValue (RequiresValue dstParam dstValue) =
      CG.edge (param, srcValue) (dstParam, dstValue) CG.Exact
    reqRuleGraph srcValue (SetsValueArea dstParam dstValues) =
      CG.merge $
        map valAreaEdge dstValues
      where
        valAreaEdge dstV = CG.edge (param, srcValue) (dstParam, dstV) CG.AllOf
buildGraphFromEnumInfo _ _ = CG.empty

-- Requires Rule Functions

enrichWithRules :: SymbolInformation -> String -> SymbolInformation -> Either String (String, SymbolInformation)
enrichWithRules base reqId requires
  | length (possibleValues base) == length (possibleValues requires) = do
    symbolInfo <- enrichWithValueRule base reqId requires
    return ("RequiresValue", symbolInfo)
  | length (possibleValues base) == 1 && length (possibleValues requires) > 1 = do
    symbolInfo <- enrichWithValueAreaRule base reqId requires
    return ("SetsValueArea", symbolInfo)
  | otherwise =
    Left "Amount of enumeration values on the left and right side of 'Requires' must be equal or amount on the left side must be 1"

enrichWithValueRule :: SymbolInformation -> String -> SymbolInformation -> Either String SymbolInformation
enrichWithValueRule base reqId requires =
  Right $ oneRulePerEnumValue (possibleValues base) (getRequiresValueRules reqId requires)

getRequiresValueRule :: String -> EnumerationValue -> RequiresRule
getRequiresValueRule id b = RequiresValue id (enumValue b)

getRequiresValueRules :: String -> SymbolInformation -> [RequiresRule]
getRequiresValueRules reqId requires =
  map (getRequiresValueRule reqId) (possibleValues requires)

oneRulePerEnumValue :: [EnumerationValue] -> [RequiresRule] -> SymbolInformation
oneRulePerEnumValue enums requires =
  EnumerationSymbol $ zipWith addRuleToEnumValue enums requires

addRuleToEnumValue :: EnumerationValue -> RequiresRule -> EnumerationValue
addRuleToEnumValue enum rule =
  EnumerationValue (enumValue enum) (rule : rules enum)

addRulesToEnumValue :: EnumerationValue -> [RequiresRule] -> EnumerationValue
addRulesToEnumValue enum ruleList =
  EnumerationValue (enumValue enum) (ruleList ++ rules enum)

enrichWithValueAreaRule :: SymbolInformation -> String -> SymbolInformation -> Either String SymbolInformation
enrichWithValueAreaRule base id requires =
  Right $
    EnumerationSymbol
      [ addRuleToEnumValue
          (head (possibleValues base))
          (getRequiresValueAreaRule id requires)
      ]

getRequiresValueAreaRule :: String -> SymbolInformation -> RequiresRule
getRequiresValueAreaRule reqId requires =
  SetsValueArea reqId (map enumValue (possibleValues requires))

getIdentifier :: AST -> Either String String
getIdentifier ast =
  let identifiers = getIdentifierList ast
   in case length identifiers of
        0 -> Left $ "No identifier found in following AST: \n" ++ show ast
        1 -> Right $ head identifiers
        x ->
          Left $
            "Ambiguous Result: Found more than one identifier in AST: \n"
              ++ show ast

getIdentifierList :: AST -> [String]
getIdentifierList (AST ParameterDefinition _ (c : cs)) = do
  if label c == Identifier
    then return $ P.value c
    else return []
getIdentifierList (AST _ _ cs) = do
  join $ mapM getIdentifierList cs

getSymbolInformation :: [AST] -> Either String SymbolInformation
getSymbolInformation (ast : asts) =
  if label ast == ParameterInformation
    then case label $ head $ children ast of
      Enumeration -> Right $ EnumerationSymbol []
      Generation -> Right $ GenerationSymbol []
      Blueprint -> Right $ BlueprintSymbol []
      BlueprintUsage -> Right $ BlueprintUsageSymbol "test" Map.empty
      x -> Left $ "Label '" ++ show x ++ "' invalid"
    else getSymbolInformation asts
getSymbolInformation [] = Left "No ParameterInformation found"

-- Analyze Generation Statements

analyzeGenerationStatement :: AST -> Either String SymbolTable
analyzeGenerationStatement ast = do
  id <- getIdentifier ast
  parts <- leftIfEmpty message $ getGenerationParts ast
  return $ Map.singleton id $ GenerationSymbol parts
  where
    message = "No parts for the generation found in AST:\n" ++ show ast

getGenerationParts :: AST -> [AST]
getGenerationParts (AST Generation _ cs) = cs
getGenerationParts (AST _ _ cs) = concatMap getGenerationParts cs

-- Analyze Blueprint Statements

analyzeBlueprintStatement :: AST -> Either String SymbolTable
analyzeBlueprintStatement ast = do
  identifier <- getIdentifier ast
  props <- leftIfEmpty "No properties found" (getProperties ast)
  return $ Map.singleton identifier (BlueprintSymbol props)

getProperties :: AST -> [Property]
getProperties (AST P.Property v _) = [Property v]
getProperties (AST P.Ellipse v _) = [Ellipse]
getProperties (AST _ _ children) = concatMap getProperties children

-- Analyze BlueprintUsage Statements

analyzeBlueprintUsageStatement :: AST -> Either String SymbolTable
analyzeBlueprintUsageStatement ast = do
  identifier <- getIdentifier ast
  blueprint <- getBlueprint ast
  values <- leftIfEmpty ("No value for BlueprintUsage found:\n" ++ show ast) (getValues ast)
  return $ Map.singleton identifier $ BlueprintUsageSymbolValuesOnly blueprint values

getBlueprint :: AST -> Either String String
getBlueprint ast = case getBlueprints ast of
  [bp] -> Right bp
  bps -> Left $ "Expected exactly one identifier for BlueprintUsage in subtree: " ++ show ast ++ "\nfound: " ++ show bps
  where
    getBlueprints :: AST -> [String]
    getBlueprints (AST BlueprintUsage _ (c : cs)) =
      [P.value c | label c == Identifier]
    getBlueprints (AST _ _ cs) = concatMap getBlueprints cs

getValues :: AST -> [String]
getValues (AST Value v _) = [v]
getValues (AST _ _ cs) = concatMap getValues cs

-- Helper

leftIfEmpty :: a -> [b] -> Either a [b]
leftIfEmpty a b
  | null b = Left a
  | otherwise = Right b

-- BlueprintUsage add properties and values

getBPUsageProperties :: SymbolTable -> Either String SymbolTable
getBPUsageProperties table =
  let predicate (BlueprintUsageSymbolValuesOnly _ _) = True
      predicate _ = False
      onlyBUs = Map.filter predicate table
      addProperties a = do
        let symbolInfoMaybe = Map.lookup (blueprint a) table
        symbolInfo <- case symbolInfoMaybe of
          Nothing -> Left $ "No entry for key '" ++ blueprint a ++ "# in Map:\n" ++ show table
          Just si -> Right si
        pvs <- valuesAndPropertiesToMap (properties symbolInfo) (values a)
        return (BlueprintUsageSymbol (blueprint a) pvs)
      onlyBUsDone = do
        symbolInfos <- mapM addProperties (Map.elems onlyBUs)
        return $ Map.fromList $ zip (Map.keys onlyBUs) symbolInfos
   in do
        busDone <- onlyBUsDone
        Right $ Map.union busDone table -- union prefers left map in case of duplicate keys

zipExtend :: [a] -> [b] -> [(a, b)]
zipExtend a b
  | length a == length b = zip a b
  | length a > length b = zip a (extend b (length a))
  | length a < length b = zip (extend a (length b)) b
  where
    extend l i =
      if length l < i
        then extend (l ++ [last l]) i
        else l

valuesAndPropertiesToMap :: [Property] -> [String] -> Either String (Map.Map String [String])
valuesAndPropertiesToMap properties values =
  let propertiesWOEllipse =
        if hasEllipse
          then init properties
          else properties
      hasEllipse = case last properties of
        Ellipse -> True
        Property _ -> False
      isValid
        | length propertiesWOEllipse == length values = True
        | length propertiesWOEllipse < length values && hasEllipse = True
        | otherwise = False
      zipped = zipExtend (map name propertiesWOEllipse) values
      folder :: Map.Map String [String] -> (String, String) -> Map.Map String [String]
      folder m (prop, val) =
        if prop `Map.member` m
          then Map.insert prop ((m Map.! prop) ++ [val]) m
          else Map.insert prop [val] m
   in if isValid
        then Right $ foldl folder Map.empty zipped
        else Left $ "Amount of properties and amount of values does not match.\nProperties: " ++ show properties ++ "\nValues: " ++ show values

