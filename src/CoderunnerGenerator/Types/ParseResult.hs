module CoderunnerGenerator.Types.ParseResult
  ( ParseResult,
    ParameterName,
    Value (Final, NeedsInput),
    ValuePart (StringPart, ParameterPart)
  )
where

import Data.Foldable (elem, find)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data ParseResult = ParseResult ParameterComposition -- maybe add Enumeration in future
  deriving (Show)

data ParameterComposition = ParameterComposition
  { parameters :: [Parameter],
    constraints :: [Constraint]
  }
  deriving (Show)

type ParameterName = String

data Parameter = Parameter ParameterName (Seq Value) -- id name values
  deriving (Show)

data Value = Final String | NeedsInput [ValuePart]
  deriving (Eq, Show)

data ValuePart = StringPart String | ParameterPart ParameterName
  deriving (Eq, Show)

data Constraint = Constraint (ParameterName, Int) (ParameterName, Int) -- First requires second, identified by id
  deriving (Show)

addValues :: ParseResult -> ParameterName -> [Value] -> ParseResult
addValues pr@(ParseResult pc@(ParameterComposition ps cs)) pm vs =
  ParseResult $ pc {parameters = newParameters}
  where
    currentValues :: Seq Value
    currentValues = getParameterValues pr pm

    allValues :: Seq Value
    allValues = foldr addIfNotContained currentValues vs

    addIfNotContained :: Value -> Seq Value -> Seq Value
    addIfNotContained v vs
      | v `elem` vs = vs
      | otherwise = vs Seq.|> v

    newParameters :: [Parameter]
    newParameters = (foldr d [] ps) ++ [Parameter pm allValues]

    d :: Parameter -> [Parameter] -> [Parameter]
    d p@(Parameter name _) ps
      | name == pm = ps
      | otherwise = ps ++ [p]

getParameterValues :: ParseResult -> ParameterName -> Seq Value
getParameterValues ps pm =
  case parameter of
    Just (Parameter _ vs) -> vs
    Nothing -> Seq.empty
  where
    parameter :: Maybe Parameter
    parameter = getParameter ps pm

getParameter :: ParseResult -> ParameterName -> Maybe Parameter
getParameter (ParseResult (ParameterComposition ps _)) pn =
  find f ps
  where
    f :: Parameter -> Bool
    f (Parameter name _) = name == pn

empty :: ParseResult
empty = ParseResult $ ParameterComposition [] []
