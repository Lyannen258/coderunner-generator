module Generator.ParameterName
  ( ParameterName,
    mkParameterName,
    unParameterName,
  )
where

newtype ParameterName = ParameterName String
  deriving (Eq, Show)

mkParameterName :: String -> ParameterName
mkParameterName = ParameterName

unParameterName :: ParameterName -> String
unParameterName (ParameterName s) = s
