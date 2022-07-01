module Generator.Configuration.Internal where

import Generator.Configuration.Type
import Generator.ParameterName
import Data.List (find, intercalate)
import Generator.Helper (maybeToEither)
import Text.Read (readMaybe)
import Lens.Micro ((^.))

getParameter :: Configuration -> ParameterName -> Maybe Parameter
getParameter c pn = find f (parameters c)
  where
    f :: Parameter -> Bool
    f (Parameter pn' _) = pn == pn'

getValueComponent :: Configuration -> ParameterName -> Maybe ValueComponent
getValueComponent c pn = valueComponent <$> getParameter c pn

getTupleX :: ParameterName -> String -> String -> Value -> Either String String
getTupleX pn arg fn v = case v of
  Regular _ -> Left . getButNotATupleErr $ pn
  Tuple ss -> do
    index <- maybeToEither (readMaybe arg) (argumentMustBeTypeErr fn "1" "int")
    if index <= length ss && not (null ss)
      then return $ ss !! index
      else Left $ tupleHasNotEnoughEntriesErr pn index (length ss)

toString :: Value -> Maybe String
toString (Regular s) = Just s
toString (Tuple _) = Nothing

getAllValues :: Configuration -> ParameterName -> Either String [Value]
getAllValues conf pn = case vc of
  Nothing -> Left $ paramNotSetErr pn
  Just (Single p) -> return $ p ^. allValues
  Just (Multi p) -> return $ p ^. selectedValueRange
  where
    vc = getValueComponent conf pn

noMatchingMethodErr :: String -> [String] -> String
noMatchingMethodErr m args =
  "There is no method with the name '" ++ m
    ++ "' and parameters "
    ++ intercalate ", " args

paramNotSetErr :: ParameterName -> String
paramNotSetErr pn =
  "Could not infer a value for parameter '"
    ++ unParameterName pn
    ++ "' because it was not declared."

argumentMustBeTypeErr :: String -> String -> String -> String
argumentMustBeTypeErr methodName argPos type' =
  "The " ++ argPos ++ ". argument for " ++ methodName ++ " must be of type " ++ type'

getButNotATupleErr :: ParameterName -> String
getButNotATupleErr pn = "Called get on parameter " ++ unParameterName pn ++ ", but it is not a tuple."

tupleHasNotEnoughEntriesErr :: ParameterName -> Int -> Int -> String
tupleHasNotEnoughEntriesErr pn requested maxVs =
  "Requested element "
    ++ show requested
    ++ " of tuple-parameter "
    ++ unParameterName pn
    ++ " but it has only "
    ++ show maxVs
    ++ " values."