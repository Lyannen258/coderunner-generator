module Interaction where

import SemanticAnalyzer
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (join)
import Text.Read (readMaybe)
import Data.List
import SemanticAnalyzer (RequiresRule)

data UsageDecision =
    ChooseValues |
    ChooseAmount

data InteractionResult =
    ValueResult (Map String [String]) |
    AmountResult Int

questionUser :: SymbolTable -> IO InteractionResult
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
    where message = "Do you want to choose values for the parameters yourself (self) or do you want to specify an amount of exercises (amount) ?"
          failed = do {putStr "Not a valid choice"; selfOrAmount}

chooseValues :: SymbolTable -> IO InteractionResult
chooseValues table = do
    myMap <- Map.foldrWithKey folder (return Map.empty) table
    return $ ValueResult Map.empty
    where
        folder :: String -> SymbolInformation -> IO (Map String [String]) -> IO (Map String [String])
        folder key value@(EnumerationSymbol values) accum = do
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
                    let finalAccum = Map.union newAccum1 newAccum2
                    return finalAccum



evaluateRules :: [RequiresRule] -> Map String [String]
evaluateRules = foldr folder Map.empty
    where
        folder rule accum = case rule of
            RequiresValue id val  -> Map.insert id [val] accum
            SetsValueArea id vals -> Map.insert id vals accum

chooseFromValueArea :: String -> SymbolInformation -> IO String
chooseFromValueArea key symbolInfo = do
    putStrLn message
    result <- getLine
    if result `elem` values
    then return result
    else chooseFromValueArea key symbolInfo
    where
        message = "Choose a value for '"
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
