module Generator.Interactive where

import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate, nub)
import Generator.App
import Generator.Configuration.Internal
import Generator.Configuration.Type
import Generator.ParameterName
import Text.Read (readMaybe)

chooseConfig ::[Configuration] -> App r u Configuration
chooseConfig cs = do
  cAsList <- foldM f cs (allParameters cs)
  return $ head cAsList
  where
    f acc pn = do
      vc <- chooseParameterValue pn (allValuesForParameter pn acc)
      return $ filterConfigs pn vc acc

allParameters :: [Configuration] -> [ParameterName]
allParameters (c : _) = map name (parameters c)
allParameters _ = []

allValuesForParameter :: ParameterName -> [Configuration] -> [ValueComponent]
allValuesForParameter pn cs = nub $ map valueComponent ps
  where
    ps = map (getParameterMustExist pn) cs

chooseParameterValue :: ParameterName -> [ValueComponent] -> App r u ValueComponent
chooseParameterValue pn vcs
  | length vcs == 1 = return $ head vcs
  | otherwise = do
      let l = zip ([1 ..] :: [Int]) vcs
      liftIO $ putStrLn $ "Choose a value for parameter " ++ show pn ++ ": "
      liftIO $ mapM_ (\(n, vc) -> putStrLn (show n ++ ")   " ++ showValueComponent vc)) l
      input <- liftIO getLine
      let maybeValue = readMaybe input >>= flip lookup l
      case maybeValue of
        Nothing -> do liftIO $ putStrLn "Invalid Input"; chooseParameterValue pn vcs
        Just a -> return a

showValueComponent :: ValueComponent -> String
showValueComponent (Single (SingleComponent v _)) = showValue v
showValueComponent (Multi (MultiComponent vs _)) = "[" ++ intercalate "," (map showValue vs) ++ "]"

showValue :: Value -> String
showValue (Regular s) = s
showValue (Tuple ss) = "(" ++ intercalate "," ss ++ ")"

filterConfigs :: ParameterName -> ValueComponent -> [Configuration] -> [Configuration]
filterConfigs pn vc = filter f
  where
    f config = valueComponent (getParameterMustExist pn config) == vc

getParameterMustExist :: ParameterName -> Configuration -> Parameter
getParameterMustExist pn c =
  case getParameter c pn of
    Nothing -> error "The parameter must exist!"
    Just a -> a
