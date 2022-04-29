{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module CoderunnerGenerator.Types.SymbolTable
  ( SymbolTable,
    empty,
    add,
    lengthSI,
    lengthSI',
    lookup,
    SymbolInformation,
    singleSymbol,
    multiSymbol,
    valuesSingle,
    Value,
    finalValue,
    incompleteValue,
  )
where

import CoderunnerGenerator.Types.AbstractSyntaxTree (ParameterValuePart)
import Data.Map (Map)
import qualified Data.Map as M
import Prelude hiding (lookup)

-- * Symbol Table Types

-- | The main symbol table type.
newtype SymbolTable = SymbolTable (Map Identifier SymbolInformation)

-- | Represents an identifier. Type synonym for string.
type Identifier = String

-- | A SymbolInformation can represent a single-parameter or a multi-parameter
data SymbolInformation
  = SingleSymbol SingleParam
  | MultiSymbol MultiParam

-- | Represents a parameter that has a single value-range. Exactly one value will be selected.
newtype SingleParam = SingleParam
  { valueRange :: ValueRange
  }

-- | Represents a parameter that has multiple value-ranges. One value-range will be selected.
-- | The values of the choosen value range can then be used with functions such as choose_at_random().
newtype MultiParam = MultiParam
  { valueRanges :: [ValueRange]
  }

-- | A list of values
newtype ValueRange = ValueRange [Value]

-- | Represents a value in a value-range of a parameter. If there is an identifier used inside of the
-- | value, the final value can only be determined after the configuration (i.e. when all parameters have
-- | have been assigned a value)
data Value
  = Final String
  | Incomplete [ParameterValuePart] -- list of text constants and used identifiers, same as in AST
  deriving (Show, Eq, Ord)

-- * Interface

-- | An empty symbol table
empty :: SymbolTable
empty = SymbolTable M.empty

-- | Add an identifier and its symbol information to the symbol table
add :: Identifier -> SymbolInformation -> SymbolTable -> Either String SymbolTable
add id si st =
  let m = unpack st
      existingSI = M.lookup id m
      newSI = case existingSI of
        Nothing -> si
        Just si2 -> mergeSymbolInfos si si2
   in Right st

-- | Construct a single symbol
singleSymbol :: [Value] -> SymbolInformation
singleSymbol = SingleSymbol . SingleParam . ValueRange

-- | Construct a multi symbol
multiSymbol :: [[Value]] -> SymbolInformation
multiSymbol vrs = MultiSymbol . MultiParam $ map ValueRange vrs

-- | Construct a final value
--
-- Right no its just an alias for the constructor.
finalValue :: String -> Value
finalValue = Final

-- | Construct a value that cannot yet be fully determined
incompleteValue :: [ParameterValuePart] -> Value
incompleteValue = Incomplete

-- | Returns the number of possible values in case of a single symbol
-- and the number of value ranges in case of a multisymbol
lengthSI :: Identifier -> SymbolTable -> Int
lengthSI id st =
  let maybeSI = lookup id st
   in maybe 0 lengthSI' maybeSI

lengthSI' :: SymbolInformation -> Int
lengthSI' (SingleSymbol si) = length (unpack si :: [Value])
lengthSI' (MultiSymbol si) = length (unpack si :: [[Value]])

valuesSingle :: SymbolInformation -> [Value]
valuesSingle (SingleSymbol ss) = unpack ss
valuesSingle (MultiSymbol ms) = []

lookup :: Identifier -> SymbolTable -> Maybe SymbolInformation
lookup id (SymbolTable m) = M.lookup id m

-- * Internal

-- | Useful if a is a newtype and therefore only contains a single value
class Unpackable a b where
  unpack :: a -> b

instance Unpackable SymbolTable (Map Identifier SymbolInformation) where
  unpack (SymbolTable m) = m

instance Unpackable ValueRange [Value] where
  unpack (ValueRange a) = a

instance Unpackable SingleParam [Value] where
  unpack (SingleParam a) = unpack a

instance Unpackable MultiParam [[Value]] where
  unpack (MultiParam a) = map unpack a

-- | Merge two symbol infos
mergeSymbolInfos :: SymbolInformation -> SymbolInformation -> SymbolInformation
mergeSymbolInfos (MultiSymbol a) (MultiSymbol b) =
  let vrsA = unpack a :: [[Value]]
      vrsB = unpack b :: [[Value]]
      f b as =
        if b `elem` as
          then as
          else as ++ [b]
   in (MultiSymbol . MultiParam . map ValueRange) $ foldr f vrsA vrsB
mergeSymbolInfos (SingleSymbol a) (SingleSymbol b) =
  let vrA = unpack a :: [Value]
      vrB = unpack b :: [Value]
      f b as = if b `elem` as then as else as ++ [b]
   in (SingleSymbol . SingleParam . ValueRange) $ foldr f vrA vrB
mergeSymbolInfos ss@(SingleSymbol a) ms@(MultiSymbol b) = mergeSymbolInfos ms ss
mergeSymbolInfos (MultiSymbol a) (SingleSymbol b) = mergeSymbolInfos (SingleSymbol b) (MultiSymbol a)