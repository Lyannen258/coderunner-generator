{-# LANGUAGE RankNTypes #-}

module Generator.Interactive where

import Control.Monad (foldM)
import Control.Monad.IO.Class (liftIO)
import Data.Foldable
import Data.List (nub)
import Data.Maybe (mapMaybe)
import Data.Sequence (fromList)
import Generator.App
import Generator.Atoms as Atoms
import Generator.Configuration.Internal
import Generator.Configuration.Type as Config
import Text.Read (readMaybe)

chooseConfig :: [Configuration] -> App r u b Configuration
chooseConfig cs = do
  cs' <- f (singleParameters . head $ cs) getSingleParameter cs
  cs'' <- f (singleTupleParameters . head $ cs) getSingleTupleParameter cs'
  cs''' <- f (multiParameters . head $ cs) getMultiParameter cs''
  cs'''' <- f (multiTupleParameters . head $ cs) getMultiTupleParameter cs'''
  return $ head cs''''
  where
    f ps getP configs = foldM (f' getP) configs ps

    f' getP configs p
      | length configs > 1 = chooseParameterValue' (getP p.name) configs p
      | otherwise = return configs

allValuesForParameter ::
  Eq (v a) =>
  [Configuration] ->
  (Configuration -> Maybe (Parameter v a)) ->
  RangeType v a
allValuesForParameter cs f =
  RangeType
    . fromList
    . nub
    . concatMap (toList . Atoms.range . Config.range)
    . mapMaybe f
    $ cs

chooseParameterValue' ::
  (Show (v a), Eq (v a)) =>
  (Configuration -> Maybe (Parameter v a)) ->
  [Configuration] ->
  Parameter v a ->
  App r u b [Configuration]
chooseParameterValue' f configs p =
  filterConfigs
    <$> chooseParameterValue p.name validVals
    <*> pure f
    <*> pure configs
  where
    validVals = allValuesForParameter configs f

chooseParameterValue :: Show (v a) => ParameterName -> RangeType v a -> App r u b (v a)
chooseParameterValue pn r = do
  let vcs = toList . Atoms.range $ r
  let l = zip ([1 ..] :: [Int]) vcs
  liftIO $ putStrLn $ "Choose a value for parameter " ++ show pn ++ ": "
  liftIO $ mapM_ (\(n, vc) -> putStrLn (show n ++ ")   " ++ show vc)) l
  input <- liftIO getLine
  let maybeValue = readMaybe input >>= flip lookup l
  case maybeValue of
    Nothing -> do liftIO $ putStrLn "Invalid Input"; chooseParameterValue pn r
    Just a -> return a

filterConfigs ::
  Eq (v a) =>
  v a ->
  (Configuration -> Maybe (Parameter v a)) ->
  [Configuration] ->
  [Configuration]
filterConfigs val getP = filter f
  where
    f config = case getP config of
      Nothing -> False
      Just p -> val == p.selectedValue