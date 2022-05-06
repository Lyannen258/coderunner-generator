module CoderunnerGenerator.ConfigGeneration (computeConfigurations) where

import CoderunnerGenerator.Helper (singleton)
import CoderunnerGenerator.Types.App
import CoderunnerGenerator.Types.Configuration
import CoderunnerGenerator.Types.Globals (getAmount)
import CoderunnerGenerator.Types.ParseResult (Constraint, ParseResult, ValuePart)
import qualified CoderunnerGenerator.Types.ParseResult as PR
import Control.Monad (foldM)
import Control.Monad.Trans.Reader (asks)
import Data.Foldable (find, foldl')
import Data.List (delete, nub)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Debug.Pretty.Simple
import Lens.Micro.Extras (view)
import System.Random (RandomGen, getStdGen, uniformR)
import Control.Monad.Trans.Except (except, throwE)
import Control.Monad.Trans.Class (lift)

computeConfigurations :: ParseResult -> App s [Configuration]
computeConfigurations pr = do
  let all = allCombinations pr
  let withoutForbidden = removeForbidden pr (pTrace ("Length all: " ++ show (length all)) all)
  amountMaybe <- asks getAmount
  let amount = fromMaybe 0 amountMaybe
  randomNumbers <- getRandomNumbers amount (length (pTrace ("Without forbidden: " ++ show withoutForbidden) withoutForbidden))
  let configurations = map (generateConfiguration pr withoutForbidden) (pTraceShowId randomNumbers)
  sequence configurations

allCombinations :: ParseResult -> [[(ParameterName, Int)]]
allCombinations pr =
  let parameterNames :: [ParameterName]
      parameterNames = PR.getParameterNames pr

      parameterValueTuples :: [[(ParameterName, Int)]]
      parameterValueTuples = map f parameterNames
        where
          f :: ParameterName -> [(ParameterName, Int)]
          f pn = zip (repeat pn) [0 .. length (PR.getParameterValues pr pn) - 1]

      f :: [[(ParameterName, Int)]] -> [(ParameterName, Int)] -> [[(ParameterName, Int)]]
      f [] parameterValueTuple = map singleton parameterValueTuple
      f acc parameterValueTuple = concatMap f' acc
        where
          f' :: [(ParameterName, Int)] -> [[(ParameterName, Int)]]
          f' config = map (\pv -> config ++ [pv]) parameterValueTuple
   in foldl' f [] parameterValueTuples

removeForbidden :: ParseResult -> [[(ParameterName, Int)]] -> [[(ParameterName, Int)]]
removeForbidden pr = filter f
  where
    f :: [(ParameterName, Int)] -> Bool
    f config = all constraintFulfilled constraints
      where
        constraintFulfilled :: Constraint -> Bool
        constraintFulfilled c
          | PR.first c `elem` config = PR.second c `elem` config
          | otherwise = True

    constraints :: [Constraint]
    constraints = PR.getConstraints pr

getRandomNumbers :: Int -> Int -> App s [Int]
getRandomNumbers amountNumbers amountConfigs = do
  stdGen <- getStdGen
  let nDistinct = getNDistinct amountNumbers amountConfigs stdGen
  return nDistinct

getNDistinct :: RandomGen g => Int -> Int -> g -> [Int]
getNDistinct amountNumbers amountConfigs = fillToN []
  where
    fillToN acc std
      | length (nub acc) >= min amountNumbers amountConfigs = nub acc
      | otherwise =
        let (rand, newGen) = uniformR (0, amountConfigs - 1) std
         in fillToN (rand : acc) newGen

generateConfiguration :: ParseResult -> [[(ParameterName, Int)]] -> Int -> App s Configuration
generateConfiguration pr configs rand =
  let randomConfig = configs !! rand
   in buildConfiguration empty randomConfig
  where
    buildConfiguration :: Configuration -> [(ParameterName, Int)] -> App s Configuration
    buildConfiguration c [] = return c
    buildConfiguration c (x : xs)
      | contains c (fst x) = buildConfiguration c xs
      | otherwise = do
        (_, remaining, newC) <- buildOneParameter x xs c
        buildConfiguration newC remaining
      where
        buildOneParameter :: (ParameterName, Int) -> [(ParameterName, Int)] -> Configuration -> App s (String, [(ParameterName, Int)], Configuration)
        buildOneParameter x xs c = case value of
          PR.Final s -> return (s, xs, addParameter (fst x) s [] c)
          PR.NeedsInput vps -> do
            (value, newConfig, newXs) <- foldM makeFinal ("", c, xs) vps
            return (value, newXs, newConfig)
          where
            value = PR.getParameterValues pr (fst x) `Seq.index` snd x

            makeFinal :: (String, Configuration, [(ParameterName, Int)]) -> ValuePart -> App s (String, Configuration, [(ParameterName, Int)])
            makeFinal (stringBuilder, c, xs) vp = case vp of
              PR.StringPart str -> return (stringBuilder ++ str, c, xs)
              PR.ParameterPart pn ->
                let maybeValue = getValue c pn
                    maybeSearchedX = find (\x -> fst x == pn) xs
                    remainingXs = delete x xs
                 in case maybeValue of
                      Just str -> return (stringBuilder ++ str, c, xs)
                      Nothing -> case maybeSearchedX of
                        Nothing -> lift . throwE $ paramNotFoundErr pn
                        Just searchedX -> do
                          (paramValue, remainingXs', newC) <- buildOneParameter searchedX remainingXs c
                          return (stringBuilder ++ paramValue, newC, remainingXs)

paramNotFoundErr :: String -> String
paramNotFoundErr pn = "Found usage of parameter " ++ pn ++ ", but it was never defined."

{-

generateConfigs :: Int -> (SymbolTable, ConstraintGraph) -> IO [Map String [String]]
generateConfigs a (st, cg) = do
  let configs = CG.configs cg
  randomNumbers <- getRandomNumbers a (length configs)
  let valueTables = map (generateValueTable configs) randomNumbers
  return valueTables

 -}