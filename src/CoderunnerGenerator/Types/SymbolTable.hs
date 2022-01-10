module CoderunnerGenerator.Types.SymbolTable where

import CoderunnerGenerator.Helper (fillToTwenty)
import CoderunnerGenerator.Types.AbstractSyntaxTree (AST)
import Data.List (intercalate, intersect, union, (\\))
import Data.Map (Map)
import qualified Data.Map as M

-- * Symbol Table

type SymbolTable = Map String SymbolInformation

showTable :: SymbolTable -> String
showTable symbolTable =
  let tableBody = intercalate "\n" (map tupleToString (M.toList symbolTable))
   in "Identifier          Type\n\n" ++ tableBody

merge :: SymbolTable -> SymbolTable -> Either String SymbolTable
merge t1 t2 =
  let keys1 = M.keys t1
      keys2 = M.keys t2
      overlappingKeys = intersect keys1 keys2
      allKeys = union keys1 keys2
      uniqueKeys = allKeys \\ overlappingKeys
      mapperOverlapping k = do
        si <- mergeSymbolInfos (t1 M.! k) (t2 M.! k)
        return (k, si)
   in do
      merged <- mapM mapperOverlapping overlappingKeys      
      let single = map (\k -> (k, M.union t1 t2 M.! k)) uniqueKeys
      let all = merged ++ single
      return $ M.fromList all

empty :: SymbolTable
empty = M.empty
  
-- * Symbol Information

data SymbolInformation
  = EnumerationSymbol Enumeration
  | GenerationSymbol Generation
  | BlueprintSymbol Blueprint
  | BlueprintUsageSymbol BlueprintUsage

instance Show SymbolInformation where
  show (EnumerationSymbol a) = "Enumeration (" ++ intercalate "," (map show a) ++ ")"
  show (GenerationSymbol a) = "Generation"
  show (BlueprintSymbol a) = "Blueprint (" ++ intercalate "," [show a] ++ ")"
  show (BlueprintUsageSymbol (BlueprintUsage b vs)) =
    "BlueprintUsage BP="
      ++ show b
      ++ ", PropertyValues: "
      ++ intercalate ", " (map (\(a, b) -> a ++ show b) (M.toList vs))

mergeSymbolInfos:: SymbolInformation -> SymbolInformation -> Either String SymbolInformation
mergeSymbolInfos (EnumerationSymbol s1) (EnumerationSymbol s2) = 
  let e = mergeEnumerations s1 s2
  in return $ EnumerationSymbol e
mergeSymbolInfos _ _ = Left "It is not possible to merge two symbol informations, that are not enumerations."

-- * Enumeration

type Enumeration = [EnumerationValue]

data EnumerationValue = EnumerationValue
  { enumValue :: String,
    rules :: [RequiresRule]
  }
  deriving (Show)

data RequiresRule
  = RequiresValue String String -- Identifier, Value
  | SetsValueArea String [String] -- Identifier, ValueArea
  deriving (Show)

mergeEnumerations :: Enumeration -> Enumeration -> Enumeration
mergeEnumerations e1 e2 =
  let mergedValues = concatMap (uncurry mergeEnumerationValues) [(a, b) | a <- e1, b <- e2]
      singleValues = filter filterPredicate (e1 ++ e2)
      filterPredicate x = enumValue x `elem` getDuplicateKeys (e1 ++ e2)
    in mergedValues ++ singleValues

getDuplicateKeys :: Enumeration -> [String]
getDuplicateKeys vs = M.keys multiOccurences
  where
    occurences = countOccurences (map enumValue vs) M.empty
    multiOccurences = M.filter (<= 1) occurences

mergeEnumerationValues :: EnumerationValue -> EnumerationValue -> [EnumerationValue]
mergeEnumerationValues v1 v2
  | enumValue v1 == enumValue v2 =
    [ EnumerationValue
        (enumValue v1)
        (rules v1 ++ rules v2)
    ]
  | otherwise = []

-- * Generation

type Generation = [AST]

-- * Blueprint

type Blueprint = [Property]

data Property
  = Property {name :: String}
  | Ellipse
  deriving (Show, Eq)

-- * Blueprint Usage

data BlueprintUsage = BlueprintUsage
  { blueprint :: String,
    propertyValues :: Map String [String]
  }

-- * Helper

tupleToString :: (String, SymbolInformation) -> String
tupleToString (a, b) = fillToTwenty a ++ show b


countOccurences :: Ord a => [a] -> M.Map a Int -> M.Map a Int -- TODO wofür braucht man die Funktion? Was macht sie genau? Als zweiter Parameter macht eig. M.Map a b mehr Sinn
countOccurences (v : vs) counted =
  if M.member v counted
    then countOccurences vs $ M.insert v ((counted M.! v) + 1) counted
    else countOccurences vs $ M.insert v 1 counted
countOccurences [] counted = counted