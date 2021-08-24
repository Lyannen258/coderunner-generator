module SemanticAnalyzer where

import qualified Data.Map as Map
import Parser
import Data.List (intercalate)



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


data SymbolInformation = Enumeration {possibleValues :: [String]}
    | Generation
    | Blueprint {properties :: [String]}
    | BlueprintUsage {blueprint :: String, propertyValues :: Map.Map String String}

instance Show SymbolInformation where
    show (Enumeration a)      = "Enumeration (" ++ intercalate "," a ++ ")"
    show Generation           = "Generation"
    show (Blueprint a)        = "Blueprint (" ++ intercalate "," a ++ ")"
    show (BlueprintUsage a b) = "BlueprintUsage BP="
        ++ a
        ++ ", PropertyValues: "
        ++ intercalate ", " (map (\(a,b) -> a ++ b) $ Map.toList b)





semanticAnalysis :: AST -> Either String SymbolTable
{- semanticAnalysis (AST "ParameterDefinition" _ children) = 
    case getIdentifier children of
        Nothing -> Left "No identifier found"
        Just identifier -> (case getSymbolInformation children of
            Nothing -> Left "No parameter information found"
            Just symbolInfo -> Map.singleton identifier symbolInfo) -}
semanticAnalysis (AST "ParameterDefinition" _ children) = do
    identifier <- getIdentifier children
    symbolInfo <- getSymbolInformation children
    return $ Map.singleton identifier symbolInfo

semanticAnalysis (AST _ _ children)  = do
    list <- mapM semanticAnalysis children
    return $ foldl Map.union Map.empty list






-- Analyze ParameterDefinition

getIdentifier :: [AST] -> Either String String
getIdentifier (ast:asts) = if label ast == "Identifier"
    then Right $ value ast
    else getIdentifier asts
getIdentifier [] = Left "No identifier found"

getSymbolInformation :: [AST] -> Either String SymbolInformation
getSymbolInformation (ast:asts) = 
    if label ast == "ParameterInformation"
        then case label $ head $ children ast of
            "Enumeration"    -> Right $ Enumeration []
            "Generator"      -> Right Generation
            "Blueprint"      -> Right $ Blueprint []
            "BlueprintUsage" -> Right $ BlueprintUsage "test" Map.empty
            x                -> Left $ "Label '" ++ x ++ "' invalid"
        else getSymbolInformation asts
getSymbolInformation [] = Left "No ParameterInformation found"