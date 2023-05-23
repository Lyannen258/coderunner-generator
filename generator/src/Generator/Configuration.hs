module Generator.Configuration
  ( Configuration,
    getSingleValue,
    getMultiValue,
    -- contains,
    evaluateMethod,
  )
where

import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError (throwError))
import Data.Foldable (toList)
import Generator.Atoms (AtomicValue (value), MultiValue (value), ParameterName (name), SingleValue (value))
import Generator.Configuration.Internal
  ( getMultiParameter,
    getMultiTupleParameter,
    getSingleParameter,
    getSingleTupleParameter,
    getValueByIndexMulti,
    getValueByIndexSingle,
    noMatchingMethodErr,
  )
import Generator.Configuration.Type hiding (name)
import Generator.Helper (maybeToError)
import Text.Read (readMaybe)

getSingleValue :: Configuration -> ParameterName -> Maybe String
getSingleValue c pn = (.value) . (.value) . selectedValue <$> getSingleParameter pn c

getMultiValue :: Configuration -> ParameterName -> Maybe [String]
getMultiValue c pn =
  (map (.value))
    . toList
    . (.value)
    . selectedValue
    <$> getMultiParameter pn c

evaluateMethod :: MonadError String m => Configuration -> ParameterName -> String -> [String] -> m [String] -- String is
evaluateMethod conf pn "get" [arg] =
  maybeToError
    (evaluateGetMethodSingle conf pn arg <|> evaluateGetMethodMulti conf pn arg)
    ("Not possible to find a value in parameter " ++ name pn ++ " with index " ++ arg)
evaluateMethod _ _ fnName args = throwError $ noMatchingMethodErr fnName args

evaluateGetMethodSingle :: Configuration -> ParameterName -> String -> Maybe [String]
evaluateGetMethodSingle conf pn arg = do
  param <- getSingleTupleParameter pn conf
  index <- readMaybe arg
  v <- getValueByIndexSingle param index
  return [v.value]

evaluateGetMethodMulti :: Configuration -> ParameterName -> String -> Maybe [String]
evaluateGetMethodMulti conf pn arg = do
  param <- getMultiTupleParameter pn conf
  index <- readMaybe arg
  values <- getValueByIndexMulti param index
  return $ toList $ fmap (.value) values