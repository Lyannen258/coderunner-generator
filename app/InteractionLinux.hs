module InteractionLinux where

import Control.Monad (foldM, join)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Show (Show)
import Helper
import Parser
import SemanticAnalyzer
import Text.Read (readMaybe)

-- Types and associated functions

data UsageDecision
  = ChooseValues
  | ChooseAmount
  deriving (Show)

data InteractionResult
  = ValueResult {valueTable :: Map String [String]}
  | AmountResult Int
  deriving (Show)

getIrValues :: String -> InteractionResult -> Either String [String]
getIrValues k vt = do
  let maybeValue = Map.lookup k $ valueTable vt
  maybeToEither maybeValue $ "No matching value for Identifier '" ++ k ++ "' found"

getIrSingleValue :: String -> InteractionResult -> Either String String
getIrSingleValue k vt = do
  values <- getIrValues k vt
  return $ head values

-- Functions

questionUser :: SymbolTable -> IO (Either String InteractionResult)
questionUser table = do
  decision <- selfOrAmount
  case decision of
    ChooseValues -> chooseValues table

--ChooseAmount -> chooseAmount table

selfOrAmount :: IO UsageDecision
selfOrAmount = do
  putStrLn message
  decision <- getLine
  case decision of
    "self" -> return ChooseValues
    "amount" -> return ChooseAmount
    x -> failed
  where
    message = "Do you want to choose values for the parameters yourself (self) or do you want to specify an amount of exercises (amount) ?"
    failed = do putStr "Not a valid choice"; selfOrAmount

chooseValues :: SymbolTable -> IO (Either String InteractionResult)
chooseValues table = do
  enumMap <- Map.foldrWithKey enumFolder (return Map.empty) table
  let enumAndGenMap = do myMap <- Map.foldrWithKey genFolder (Right enumMap) table; return $ ValueResult myMap
  return enumAndGenMap
  where
    enumFolder :: String -> SymbolInformation -> IO (Map String [String]) -> IO (Map String [String])
    enumFolder key value@(EnumerationSymbol values) accum = do
      accumRaw <- accum
      if key `Map.member` accumRaw
        then accum
        else addValues accumRaw
      where
        addValues accumRaw = do
          decision <- chooseFromValueArea key value
          let newAccum1 = Map.insert key [decision] accumRaw
          let maybeEV = find (\x -> enumValue x == decision) values
          let newAccum2 = case maybeEV of
                Just ev -> evaluateRules (rules ev)
                Nothing -> Map.empty
          let finalAccum = Map.union newAccum2 newAccum1
          return finalAccum
    enumFolder _ _ accum = accum

    genFolder :: String -> SymbolInformation -> Either String (Map String [String]) -> Either String (Map String [String])
    genFolder key value@(GenerationSymbol parts) accumE = do
      accum <- accumE
      if key `Map.member` accum
        then accumE
        else addValue
      where
        addValue = do
          accum <- accumE
          gen <- buildGen parts accum
          let newAccum = Map.insert key [gen] accum
          return newAccum
    genFolder _ _ accum = accum

buildGen :: [AST] -> Map String [String] -> Either String String
buildGen asts table = foldM folder "" asts
  where
    folder accum ast@(AST ArbitraryPart val _) =
      Right $ accum ++ val
    folder accum (AST Identifier id _) = do
      let maybeVal = Map.lookup id table
      valList <- case maybeVal of
        Nothing -> Left ("Could not find identifier '" ++ id ++ "'")
        Just a -> Right a
      val <- case valList of
        [] -> Left $ "No value found for identifier: " ++ id
        [v] -> Right v
        vs -> Left $ "Identifier '" ++ id ++ "' was set by a Requires-relationship and has multiple values. You can only use single-value identifiers in generations"
      return $ accum ++ val
    folder _ ast = Left $ "Unexpected AST-Node in GenerationSymbol: " ++ show ast

evaluateRules :: [RequiresRule] -> Map String [String]
evaluateRules = foldr folder Map.empty
  where
    folder rule accum = case rule of
      RequiresValue id val -> Map.insert id [val] accum
      SetsValueArea id vals -> Map.insert id vals accum

chooseFromValueArea :: String -> SymbolInformation -> IO String
chooseFromValueArea key symbolInfo = do
  putStrLn message
  result <- getLine
  if result `elem` values
    then return result
    else chooseFromValueArea key symbolInfo
  where
    message =
      "Choose a value for '"
        ++ key
        ++ "' from values "
        ++ intercalate " , " values
    values = symbolInfoToListOfValues symbolInfo

symbolInfoToListOfValues :: SymbolInformation -> [String]
symbolInfoToListOfValues s = map enumValue $ possibleValues s

{- chooseAmount :: SymbolTable -> IO InteractionResult
chooseAmount table = do
    putStrLn message
    decision <- getLine
    case readMaybe decision of
        Just a -> generateValueTables a table
        Nothing -> chooseAmount table
    where message = "How many variants do you want to generate?"

validateAmount :: Int -> SymbolTable -> Bool
validateAmount =

generateValueTables :: Int -> SymbolTable -> IO InteractionResult
generateValueTables amount table = -}