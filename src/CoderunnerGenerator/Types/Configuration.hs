module CoderunnerGenerator.Types.Configuration
  ( Configuration,
    Parameter,
    ParameterName,
    empty,
    addParameter,
    getValue,
    contains,
  )
where

import Data.Foldable (find)

newtype Configuration = Configuration {parameters :: [Parameter]}
  deriving (Show)

data Parameter = Parameter
  { name :: ParameterName,
    selectedValue :: String,
    allValues :: [String]
  }
  deriving (Show)

type ParameterName = String

empty :: Configuration
empty = Configuration []

addParameter :: String -> String -> [String] -> Configuration -> Configuration
addParameter pn sv avs c = c {parameters = parameters c ++ [Parameter pn sv avs]}

getValue :: Configuration -> ParameterName -> Maybe String
getValue c pn = do
  param <- getParameter c pn
  return $ selectedValue param

contains :: Configuration -> ParameterName -> Bool
contains c pn = case getParameter c pn of
  Nothing -> False
  Just pa -> True

getParameter :: Configuration -> ParameterName -> Maybe Parameter
getParameter c pn = find f (parameters c)
  where
    f :: Parameter -> Bool
    f p = name p == pn

--getAllValues :: Configuration -> ParameterName -> [String]
--evaluateMethod :: Configuration -> ParameterName -> String -> [String] -> [String] -- String is function name, First [String] is arguments, returned [String] is result