module Generator.ParameterName
  ( ParameterName,
    mkParameterName,
    unParameterName,
  )
where

newtype ParameterName = ParameterName String
  deriving (Eq)

mkParameterName :: String -> ParameterName
mkParameterName = ParameterName

unParameterName :: ParameterName -> String
unParameterName (ParameterName s) = s

instance Show ParameterName where
  show pn = unParameterName pn
