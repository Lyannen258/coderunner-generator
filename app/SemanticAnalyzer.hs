module SemanticAnalyzer where

import qualified Data.Map as Map
import Parser
import Control.Monad (join)
import Data.List
import Debug.Trace



newtype SemanticResult = SemanticResult SymbolTable

type SymbolTable = Map.Map String SymbolInformation

showSymbolTable :: SymbolTable -> String
showSymbolTable symbolTable =
    let tableBody = intercalate "\n" (map tupleToString (Map.toList symbolTable))
    in "Identifier          Type\n\n" ++ tableBody


tupleToString :: (String, SymbolInformation) -> String
tupleToString (a, b) = fillToTwenty a ++ show b

fillToTwenty :: String -> String
fillToTwenty s = if length s < 20
    then fillToTwenty (s ++ " ")
    else s


data SymbolInformation =
    EnumerationSymbol {possibleValues :: [EnumerationValue]}
    | GenerationSymbol
    | BlueprintSymbol {properties :: [Property]}
    | BlueprintUsageSymbol {blueprint :: String, propertyValues :: Map.Map String String}

instance Show SymbolInformation where
    show (EnumerationSymbol a)      = "Enumeration (" ++ intercalate "," (map show a) ++ ")"
    show GenerationSymbol           = "Generation"
    show (BlueprintSymbol a)        = "Blueprint (" ++ intercalate "," [show a] ++ ")"
    show (BlueprintUsageSymbol a b) = "BlueprintUsage BP="
        ++ a
        ++ ", PropertyValues: "
        ++ intercalate ", " (map (\(a,b) -> a ++ b) $ Map.toList b)

data EnumerationValue = EnumerationValue {
    enumValue :: String,
    rules :: [RequiresRule] }
    deriving (Show)

data RequiresRule =
    RequiresValue String String | -- Identifier, Value 
    SetsValueArea String [String] -- Identifier, ValueArea
    deriving (Show)

data Property = Property String
    | Ellipse
    deriving (Show)


semanticAnalysis :: AST -> Either String SymbolTable
semanticAnalysis ast@(AST ParameterStatement _ children) =
    case statementType ast of
        Right Enumeration    -> analyzeEnumerationStatement ast
        Right Generation     -> analyzeGenerationStatement ast
        Right Blueprint      -> analyzeBlueprintStatement ast
        --Right BlueprintUsage -> analyzeBlueprintUsageStatement ast
        Left x               -> Left x
semanticAnalysis (AST _ _ children) = do
    list <- mapM semanticAnalysis children
    return $ foldl mergeTableParts Map.empty list

mergeTableParts :: SymbolTable -> SymbolTable -> SymbolTable
mergeTableParts t1 t2 =
    let keys1 = Map.keys t1
        keys2 = Map.keys t2
        overlappingKeys = intersect keys1 keys2
        allKeys = union keys1 keys2
        uniqueKeys = allKeys \\ overlappingKeys
    in Map.fromList $
        map (\k -> (k, mergeSymbolInfos (t1 Map.! k) (t2 Map.! k)))overlappingKeys
        ++ map (\k -> (k, Map.union t1 t2 Map.! k)) uniqueKeys



-- Analyze ParameterStatement

statementType :: AST -> Either String Label
statementType (AST Enumeration _ _) = Right Enumeration
statementType (AST Generation _ _) = Right Generation
statementType (AST Blueprint _ _) = Right Blueprint
statementType (AST BlueprintUsage _ _) = Right BlueprintUsage
statementType (AST ParameterStatement _ children) = statementType (head children)
statementType (AST ParameterDefinition _ children) = statementType $ children!!1
statementType (AST ParameterInformation _ children) = statementType $ head children
statementType _ =
    Left "ParameterStatement does not contain a parameter definition"


-- Analyze Enumeration Parameter Statement

analyzeEnumerationStatement :: AST -> Either String SymbolTable
analyzeEnumerationStatement (AST ParameterStatement _
    [
        def1@(AST ParameterDefinition _ _),
        req@(AST Requires _ _),
        def2@(AST ParameterDefinition _ _)
    ]) = do
    id1 <- getIdentifier def1
    enumInfo1 <- getEnumerationInfo def1
    id2 <- getIdentifier def2
    enumInfo2 <- getEnumerationInfo def2
    enumInfo1WRules <- enrichWithRules enumInfo1 id2 enumInfo2

    return $ Map.fromList
        [
            (id1, enumInfo1WRules),
            (id2, enumInfo2)
        ]

analyzeEnumerationStatement (AST ParameterStatement _
    [
        def@(AST ParameterDefinition _ _)
    ]) = do
    id <- getIdentifier def
    enumInfo <- getEnumerationInfo def
    return $ Map.singleton id enumInfo

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
    getEnumerationValues (children!!1)
getEnumerationValues (AST ParameterInformation _ children) =
    getEnumerationValues (head children)
getEnumerationValues (AST Enumeration _ children) =
    mapM getEnumerationValue children
getEnumerationValues ast = Left $ "Unexpected Node in Enumeration ParameterDefinition: " ++ show (label ast)



mergeSymbolInfos :: SymbolInformation -> SymbolInformation -> SymbolInformation
mergeSymbolInfos (EnumerationSymbol vs1) (EnumerationSymbol vs2) =
    EnumerationSymbol $ mergeEnumerationValues vs1 vs2

mergeSymbolInfos' a b = trace
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
    where occurences = countOccurences (map enumValue vs) Map.empty
          multiOccurences = Map.filter (<= 1) occurences

countOccurences :: Ord a => [a] -> Map.Map a Int -> Map.Map a Int
countOccurences (v:vs) counted =
    if Map.member v counted
    then countOccurences vs $ Map.insert v ((counted Map.! v) + 1) counted
    else countOccurences vs $ Map.insert v 1 counted
countOccurences [] counted = counted


mergeEnumerationValue :: (EnumerationValue, EnumerationValue) -> [EnumerationValue]
mergeEnumerationValue (v1,v2)
    | enumValue v1 == enumValue v2 = [EnumerationValue
        (enumValue v1)
        (rules v1 ++ rules v2)]
    | otherwise = []

mergeEnumerationValue' (a, b) = trace
    ("\nmergeEnumerationValue:\na: "
    ++ show a
    ++ "\nb: "
    ++ show b
    ++ "\nresult: "
    ++ show (mergeEnumerationValue (a, b))
    ++ "\n")
    (mergeEnumerationValue (a, b))



-- Requires Rule Functions

enrichWithRules :: SymbolInformation -> String -> SymbolInformation -> Either String SymbolInformation
enrichWithRules base reqId requires
    | length (possibleValues base) == length (possibleValues requires) =
        enrichWithValueRule base reqId requires
    | length (possibleValues base) == 1 && length (possibleValues requires) > 1 =
        enrichWithValueAreaRule base reqId requires
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
    Right $ EnumerationSymbol [addRuleToEnumValue
        (head (possibleValues base))
        (getRequiresValueAreaRule id requires)]

getRequiresValueAreaRule :: String -> SymbolInformation -> RequiresRule
getRequiresValueAreaRule reqId requires =
    SetsValueArea reqId (map enumValue (possibleValues requires))





getIdentifier :: AST -> Either String String
getIdentifier ast =
    let identifiers = getIdentifierList ast
    in case length identifiers of
        0 -> Left $ "No identifier found in following AST: \n" ++ show ast
        1 -> Right $ head identifiers
        x -> Left $
            "Ambiguous Result: Found more than one identifier in AST: \n"
            ++ show ast

getIdentifierList :: AST -> [String]
getIdentifierList (AST ParameterDefinition _ (c:cs)) = do
    if label c == Identifier
        then return $ value c
        else return []
getIdentifierList (AST _ _ cs) = do
    join $ mapM getIdentifierList cs


getSymbolInformation :: [AST] -> Either String SymbolInformation
getSymbolInformation (ast:asts) =
    if label ast == ParameterInformation
        then case label $ head $ children ast of
            Enumeration    -> Right $ EnumerationSymbol []
            Generation      -> Right GenerationSymbol
            Blueprint      -> Right $ BlueprintSymbol []
            BlueprintUsage -> Right $ BlueprintUsageSymbol "test" Map.empty
            x                -> Left $ "Label '" ++ show x ++ "' invalid"
        else getSymbolInformation asts
getSymbolInformation [] = Left "No ParameterInformation found"


-- Analyze Generation Statements

analyzeGenerationStatement :: AST -> Either String SymbolTable
analyzeGenerationStatement ast = do
    id <- getIdentifier ast
    return $ Map.singleton id GenerationSymbol


-- Analyze Blueprint Statements

analyzeBlueprintStatement :: AST -> Either String SymbolTable
analyzeBlueprintStatement ast = do
    identifier <- getIdentifier ast
    props <- leftIfEmpty "No properties found" (getProperties ast)
    return $ Map.singleton identifier (BlueprintSymbol props)

getProperties :: AST -> [Property]
getProperties (AST Parser.Property v _) = [SemanticAnalyzer.Property v]
getProperties (AST Parser.Ellipse v _) = [SemanticAnalyzer.Ellipse]
getProperties (AST _ _ children) = concatMap getProperties children


-- Analyze BlueprintUsage Statements

{- analyzeBlueprintUsageStatement :: AST -> Either String SymbolTable
analyzeBlueprintUsageStatement = do -}


-- Helper

leftIfEmpty :: a -> [b] -> Either a [b]
leftIfEmpty a b
    | null b    = Left a
    | otherwise = Right b