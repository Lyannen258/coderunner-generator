module SemanticAnalyzer where

import qualified Data.Map as Map
import Parser
import Data.List (intercalate)
import Data.ByteString.Builder.Prim (emptyB)



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
    | BlueprintSymbol {properties :: [String]}
    | BlueprintUsageSymbol {blueprint :: String, propertyValues :: Map.Map String String}

instance Show SymbolInformation where
    show (EnumerationSymbol a)      = "Enumeration (" ++ intercalate "," (map show a) ++ ")"
    show GenerationSymbol           = "Generation"
    show (BlueprintSymbol a)        = "Blueprint (" ++ intercalate "," a ++ ")"
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





semanticAnalysis :: AST -> Either String SymbolTable
semanticAnalysis (AST ParameterDefinition _ children) = do
    identifier <- getIdentifier children
    symbolInfo <- getSymbolInformation children
    return $ Map.singleton identifier symbolInfo

semanticAnalysis ast@(AST ParameterStatement _ children) =
    case statementType ast of
        Right Enumeration    -> analyzeEnumerationStatement ast
        Right Generation     -> analyzeGenerationStatement ast
        --Right Blueprint      -> analyzeBlueprintStatement ast
        --Right BlueprintUsage -> analyzeBlueprintUsageStatement ast
        Left x               -> Left x

semanticAnalysis (AST _ _ children)  = do
    list <- mapM semanticAnalysis children
    return $ foldl Map.union Map.empty list






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
    id1 <- getIdentifier $ children def1
    enumInfo1 <- getEnumerationInfo def1
    id2 <- getIdentifier $ children def2
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
    id <- getIdentifier $ children def
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





getIdentifier :: [AST] -> Either String String
getIdentifier (ast:asts) = if label ast == Identifier
    then Right $ value ast
    else getIdentifier asts
getIdentifier [] = Left "No identifier found"

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


-- Analyze Generation Symbol

analyzeGenerationStatement :: AST -> Either String SymbolTable
analyzeGenerationStatement ast = do
    id <- getIdentifier $ children ast
    return $ Map.singleton id GenerationSymbol