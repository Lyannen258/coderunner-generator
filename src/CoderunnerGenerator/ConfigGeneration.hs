module CoderunnerGenerator.ConfigGeneration (computeConfigurations) where

import CoderunnerGenerator.Helper (maybeToEither, singleton)
import CoderunnerGenerator.Types.App
import CoderunnerGenerator.Types.Configuration as C
import CoderunnerGenerator.Types.Globals (getAmount)
import CoderunnerGenerator.Types.ParseResult (Constraint, ParseResult, ValuePart, isSingle)
import qualified CoderunnerGenerator.Types.ParseResult as PR
import Control.Monad (foldM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, throwE)
import Control.Monad.Trans.Reader (asks)
import Data.Either (lefts, rights)
import Data.Foldable (Foldable (toList), find, foldl')
import Data.List (nub)
import Data.Maybe (fromMaybe)
import System.Random (RandomGen, getStdGen, uniformR)

type ConfigListRaw = [[(ParameterName, Int)]]

type ConfigRaw = [(ParameterName, Int)]

computeConfigurations :: ParseResult -> App s [Configuration]
computeConfigurations pr = do
  let combs = allCombinations pr
  let withoutForbidden = removeForbidden pr combs
  amountMaybe <- asks getAmount
  let amount = fromMaybe 0 amountMaybe
  randomNumbers <- getRandomNumbers amount (length withoutForbidden)
  let configurations = map (generateConfiguration pr withoutForbidden) randomNumbers
  sequence configurations

allCombinations :: ParseResult -> ConfigListRaw
allCombinations pr =
  let parameterNames :: [ParameterName]
      parameterNames = PR.getParameterNames pr

      parameterValueTuples :: ConfigListRaw
      parameterValueTuples = map f' parameterNames
        where
          f' :: ParameterName -> ConfigRaw
          f' pn = zip (repeat pn) [0 .. PR.countValues pr pn - 1]

      f :: ConfigListRaw -> ConfigRaw -> ConfigListRaw
      f [] parameterValueTuple = map singleton parameterValueTuple
      f acc parameterValueTuple = concatMap f' acc
        where
          f' :: ConfigRaw -> ConfigListRaw
          f' config = map (\pv -> config ++ [pv]) parameterValueTuple
   in foldl' f [] parameterValueTuples

removeForbidden :: ParseResult -> ConfigListRaw -> ConfigListRaw
removeForbidden pr = filter f
  where
    f :: ConfigRaw -> Bool
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

generateConfiguration :: ParseResult -> ConfigListRaw -> Int -> App s Configuration
generateConfiguration pr configs rand = snd <$> foldM f (configRaw, empty) configRaw
  where
    configRaw = configs !! rand

    f :: (ConfigRaw, Configuration) -> (ParameterName, Int) -> App s (ConfigRaw, Configuration)
    f (cr, c) x
      | contains c (fst x) = return (cr, c)
      | otherwise = case PR.getParameter pr (fst x) of
        Nothing -> lift . except . Left $ "Parameter " ++ fst x ++ " not found."
        Just pa -> buildParameter pr cr c pa

buildParameter :: ParseResult -> ConfigRaw -> Configuration -> PR.Parameter -> App s (ConfigRaw, Configuration)
buildParameter pr cr c p@(PR.Parameter n vs)
  | PR.containsMultiParamUsage pr n =
    if checkMultiParamUsageReqs pr p
      then do
        (vs', newC) <- buildWithMultiParamUsage pr cr c p
        return (cr, C.addMultiParameter n vs' [vs'] newC)
      else (lift . throwE) incorrectUsageOfMultiParam
  | otherwise = do
    i <- lift . except $ maybeToEither (find (\(n', _) -> n == n') cr) "Something went wrong"
    (singleMultiE, newCR, newC) <- foldM (buildParameterValue pr) ([], cr, c) vs
    newC' <- addToConfiguration newC n (snd i) singleMultiE
    return (newCR, newC')

addToConfiguration :: Configuration -> PR.ParameterName -> Int -> [Either String [String]] -> App s Configuration
addToConfiguration c n i singleMultiE
  | not (null ls) && null rs && i < length ls =
    return $ C.addSingleParameter n (ls !! i) ls c
  | not (null rs) && null ls && i < length rs =
    return $ C.addMultiParameter n (rs !! i) rs c
  | otherwise = lift . throwE $ "Something went wrong"
  where
    ls = lefts singleMultiE
    rs = rights singleMultiE

checkMultiParamUsageReqs :: ParseResult -> PR.Parameter -> Bool
checkMultiParamUsageReqs pr (PR.Parameter n vs) =
  isSingle pr n && length vs == 1

buildParameterValue :: ParseResult -> ([Either String [String]], ConfigRaw, Configuration) -> PR.ParameterValue -> App s ([Either String [String]], ConfigRaw, Configuration)
buildParameterValue pr (l, cr, c) pv = case pv of
  PR.SingleValue va -> do
    (v, cr', c') <- buildValue pr ("", cr, c) va
    return (l ++ [Left v], cr', c')
  PR.MultiValue vs -> do
    (ss, newCR, newC) <- foldM f ([], cr, c) vs
    return (l ++ [Right ss], newCR, newC)
    where
      f :: ([String], ConfigRaw, Configuration) -> PR.Value -> App s ([String], ConfigRaw, Configuration)
      f (ss, cr'', c'') v = do
        (nextS, newCR, newC) <- buildValue pr ("", cr'', c'') v
        return (ss ++ [nextS], newCR, newC)

buildValue :: ParseResult -> (String, ConfigRaw, Configuration) -> PR.Value -> App s (String, ConfigRaw, Configuration)
buildValue pr (s, cr, c) v = case v of
  PR.Final str -> return (s ++ str, cr, c)
  PR.NeedsInput vps -> do
    (value, cr', c') <- foldM (makeFinal pr) ("", cr, c) vps
    return (value, cr', c')

makeFinal :: ParseResult -> (String, ConfigRaw, Configuration) -> ValuePart -> App s (String, ConfigRaw, Configuration)
makeFinal pr (stringBuilder, cr, c) vp = case vp of
  PR.StringPart str -> return (stringBuilder ++ str, cr, c)
  PR.ParameterPart pn -> do
    (s, newCR, newC) <- evaluateParameterPart pn pr cr c
    return (stringBuilder ++ s, newCR, newC)

evaluateParameterPart :: PR.ParameterName -> ParseResult -> ConfigRaw -> Configuration -> App s (String, ConfigRaw, Configuration)
evaluateParameterPart pn pr cr c =
  let maybeSingleValue = getSingleValue c pn
      maybeSearchedX = find (\x -> fst x == pn) cr
   in case maybeSingleValue of
        Just str -> return (str, cr, c)
        Nothing -> case maybeSearchedX of
          Nothing -> lift . throwE $ paramNotFoundErr pn
          Just (pn', _) -> do
            (_, newC) <- case PR.getParameter pr pn' of
              Nothing -> lift . throwE $ "Parameter " ++ pn ++ " not found"
              Just pa -> buildParameter pr cr c pa
            case getSingleValue newC pn' of
              Nothing -> lift . throwE $ "Parameter " ++ pn ++ " cannot be resolved"
              Just s -> return (s, cr, newC)

buildWithMultiParamUsage :: ParseResult -> ConfigRaw -> Configuration -> PR.Parameter -> App s ([String], Configuration)
buildWithMultiParamUsage pr cr c (PR.Parameter _ vs) = case head $ toList vs of
  PR.SingleValue (PR.NeedsInput vps) -> foldM f ([], c) vps
  _ -> lift . throwE $ "This error should not be happening"
  where
    f :: ([String], Configuration) -> ValuePart -> App s ([String], Configuration)
    f (strs, c') (PR.StringPart s) = case strs of
      [] -> return ([s], c)
      _ -> return (map (++ s) strs, c')
    f (strs, c') (PR.ParameterPart pn) =
      do
        (paramValues, newC) <- makeFinalMulti pr cr c' pn
        return ([str ++ param | str <- strs, param <- paramValues], newC)

makeFinalMulti :: ParseResult -> ConfigRaw -> Configuration -> PR.ParameterName -> App s ([String], Configuration)
makeFinalMulti pr cr c n = case PR.getParameter pr n of
  Nothing -> lift . throwE $ "Parameter " ++ n ++ " is used but not defined."
  Just p -> do
    (_, cNew) <- buildParameter pr cr c p
    case C.getSingleValue c n of
      Just s -> return ([s], cNew)
      Nothing -> case C.getMultiValue c n of
        Just ss -> return (ss, cNew)
        Nothing -> lift . throwE $ "This error should not be happening"

paramNotFoundErr :: String -> String
paramNotFoundErr pn = "Found usage of parameter " ++ pn ++ ", but it was never defined."

incorrectUsageOfMultiParam :: String
incorrectUsageOfMultiParam = "Usage of a multi parameter in the value range of another parameter that has more than one possible values or is a multi parameter itself is not allowed"