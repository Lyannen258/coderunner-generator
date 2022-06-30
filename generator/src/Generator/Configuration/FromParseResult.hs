module Generator.Configuration.FromParseResult (computeConfigurations, computeMaxAmount) where

import Control.Monad (foldM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (except, throwE)
import Control.Monad.Trans.Reader (asks)
import Data.Either (lefts, rights)
import Data.Foldable (Foldable (toList), find, foldl')
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Generator.App
import Generator.Configuration as C
import Generator.Configuration.Construction as C
import Generator.Configuration.Type
import Generator.Globals (getAmount)
import Generator.Helper (maybeToEither, printLn, singleton)
import Generator.ParameterName
import qualified Generator.ParseResult.Info as PR
import Generator.ParseResult.Type (Constraint, ParseResult, ValuePart)
import qualified Generator.ParseResult.Type as PR
import System.Random (RandomGen, getStdGen, uniformR)

type ConfigListRaw = [[(ParameterName, Int)]]

type ConfigRaw = [(ParameterName, Int)]

computeMaxAmount :: ParseResult -> App r u Int
computeMaxAmount pr = case allCombinations pr of
  [] -> return 1
  clr -> (return . length . removeForbidden pr) clr

computeConfigurations :: ParseResult -> App r u [Configuration]
computeConfigurations pr = do
  case allCombinations pr of
    [] -> do
      amount <- evaluateRequestedAmount 1
      if amount == 1
        then (liftIO . sequence) [C.empty] -- Generate single empty configuration, if no parameters specified
        else return []
    combs -> do
      let withoutForbidden = removeForbidden pr combs
      amount <- evaluateRequestedAmount (length withoutForbidden)
      randoms <- getRandomNumbers amount (length withoutForbidden)
      let configurations = map (generateConfiguration pr withoutForbidden) randoms
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
    f config = all constraintFulfilled constraintsL
      where
        constraintFulfilled :: Constraint -> Bool
        constraintFulfilled c
          | PR.first c `elem` config = PR.second c `elem` config
          | otherwise = True

    constraintsL :: [Constraint]
    constraintsL = PR.getConstraints pr

evaluateRequestedAmount :: Int -> App r u Int
evaluateRequestedAmount maxAmount = do
  amountMaybe <- asks getAmount
  let amount = fromMaybe maxAmount amountMaybe
  printAttemptedAmount amount maxAmount
  return (min amount maxAmount)

printAttemptedAmount :: Int -> Int -> App r u ()
printAttemptedAmount requestedA maxA = do
  printLn $ show requestedA ++ " variants requested"
  when (requestedA > maxA) $
    printLn $
      show requestedA
        ++ " variants not possible. Generating "
        ++ show maxA
        ++ " variants."
  when (requestedA < 0) $
    printLn $
      show requestedA
        ++ " variants not possible. Generating 0 variants."

getRandomNumbers :: Int -> Int -> App r u [Int]
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

generateConfiguration :: ParseResult -> ConfigListRaw -> Int -> App r u Configuration
generateConfiguration pr configs rand = do
  emptyC <- (lift . lift) empty
  snd <$> foldM f (configRaw, emptyC) configRaw
  where
    configRaw = configs !! rand

    f :: (ConfigRaw, Configuration) -> (ParameterName, Int) -> App r u (ConfigRaw, Configuration)
    f (cr, c) x
      | contains c (fst x) = return (cr, c)
      | otherwise = case PR.getParameter pr (fst x) of
        Nothing -> lift . except . Left $ "Parameter " ++ (unParameterName . fst $ x) ++ " not found."
        Just pa -> buildParameter pr cr c pa

buildParameter :: ParseResult -> ConfigRaw -> Configuration -> PR.Parameter -> App r u (ConfigRaw, Configuration)
buildParameter pr cr c p@(PR.Parameter n vs)
  | PR.containsMultiParamUsage pr n =
    if checkMultiParamUsageReqs pr p
      then do
        (vs', newC) <- buildWithMultiParamUsage pr cr c p
        let vs'' = vs'
        return (cr, C.addParameter n vs'' [vs''] newC)
      else (lift . throwE) incorrectUsageOfMultiParam
  | otherwise = do
    i <- lift . except $ maybeToEither (find (\(n', _) -> n == n') cr) "Something went wrong"
    (singleMultiE, newCR, newC) <- foldM (buildParameterValue pr) ([], cr, c) vs
    newC' <- addToConfiguration newC n (snd i) singleMultiE
    return (newCR, newC')

addToConfiguration :: Configuration -> ParameterName -> Int -> [Either Value [Value]] -> App r u Configuration
addToConfiguration c n i singleMultiE
  | not (null ls) && null rs && i < length ls =
    return $ C.addParameter n (ls !! i) ls c
  | not (null rs) && null ls && i < length rs =
    return $ C.addParameter n (rs !! i) rs c
  | otherwise = lift . throwE $ "Something went wrong"
  where
    ls = lefts singleMultiE
    rs = rights singleMultiE

checkMultiParamUsageReqs :: ParseResult -> PR.Parameter -> Bool
checkMultiParamUsageReqs pr (PR.Parameter n vs) =
  PR.isSingle pr n && length vs == 1

buildParameterValue :: ParseResult -> ([Either Value [Value]], ConfigRaw, Configuration) -> PR.ParameterValue -> App r u ([Either Value [Value]], ConfigRaw, Configuration)
buildParameterValue pr (l, cr, c) pv = case pv of
  PR.SingleValue va -> do
    (v, cr', c') <- buildValue pr (cr, c) va
    return (l ++ [Left v], cr', c')
  PR.MultiValue vs -> do
    (ss, newCR, newC) <- foldM f ([], cr, c) vs
    return (l ++ [Right ss], newCR, newC)
    where
      f :: ([Value], ConfigRaw, Configuration) -> PR.Value -> App r u ([Value], ConfigRaw, Configuration)
      f (ss, cr'', c'') v = do
        (nextS, newCR, newC) <- buildValue pr (cr'', c'') v
        return (ss ++ [nextS], newCR, newC)

buildValue :: ParseResult -> (ConfigRaw, Configuration) -> PR.Value -> App r u (Value, ConfigRaw, Configuration)
buildValue pr (cr, c) v = case v of
  (PR.RegularValue (PR.Final str)) -> return (Regular str, cr, c)
  (PR.RegularValue (PR.NeedsInput vps)) -> do
    (value, cr', c') <- foldM (makeFinal pr) ("", cr, c) vps
    return (Regular value, cr', c')
  (PR.TupleValue (PR.Tuple strs)) -> return (Tuple strs, cr, c)

makeFinal :: ParseResult -> (String, ConfigRaw, Configuration) -> ValuePart -> App r u (String, ConfigRaw, Configuration)
makeFinal pr (stringBuilder, cr, c) vp = case vp of
  PR.StringPart str -> return (stringBuilder ++ str, cr, c)
  PR.ParameterPart pn -> do
    (s, newCR, newC) <- evaluateParameterPart pn pr cr c
    return (stringBuilder ++ s, newCR, newC)

evaluateParameterPart :: ParameterName -> ParseResult -> ConfigRaw -> Configuration -> App r u (String, ConfigRaw, Configuration)
evaluateParameterPart pn pr cr c =
  let maybeSingleValue = getSingleValue c pn
      maybeSearchedX = find (\x -> fst x == pn) cr
   in case maybeSingleValue of
        Just str -> return (str, cr, c)
        Nothing -> case maybeSearchedX of
          Nothing -> lift . throwE $ paramNotFoundErr pn
          Just (pn', _) -> do
            (_, newC) <- case PR.getParameter pr pn' of
              Nothing -> lift . throwE $ "Parameter " ++ unParameterName pn ++ " not found"
              Just pa -> buildParameter pr cr c pa
            case getSingleValue newC pn' of
              Just s -> return (s, cr, newC)
              _ -> lift . throwE $ "Parameter " ++ unParameterName pn ++ " cannot be resolved"

buildWithMultiParamUsage :: ParseResult -> ConfigRaw -> Configuration -> PR.Parameter -> App r u ([Value], Configuration)
buildWithMultiParamUsage pr cr c (PR.Parameter _ vs) = case head $ toList vs of
  PR.SingleValue (PR.RegularValue (PR.NeedsInput vps)) -> do
    (strs, newC) <- foldM f ([], c) vps
    return (map Regular strs, newC)
  _ -> lift . throwE $ "This error should not be happening"
  where
    f :: ([String], Configuration) -> ValuePart -> App r u ([String], Configuration)
    f (vs', c') (PR.StringPart s) = case vs' of
      [] -> return ([s], c)
      _ -> return (map (++ s) vs', c')
    f (strs, c') (PR.ParameterPart pn) =
      do
        (paramValues, newC) <- makeFinalMulti pr cr c' pn
        return ([str ++ param | str <- strs, param <- paramValues], newC)

makeFinalMulti :: ParseResult -> ConfigRaw -> Configuration -> ParameterName -> App r u ([String], Configuration)
makeFinalMulti pr cr c n = case PR.getParameter pr n of
  Nothing -> lift . throwE $ paramNotFoundErr n
  Just p -> do
    (_, cNew) <- buildParameter pr cr c p
    case C.getSingleValue c n of
      Just v -> return ([v], cNew)
      Nothing -> case C.getMultiValue c n of
        Just vs -> return (foldr (:) [] vs, cNew)
        Nothing -> lift . throwE $ "This error should not be happening"

paramNotFoundErr :: ParameterName -> String
paramNotFoundErr pn = "Found usage of parameter " ++ unParameterName pn ++ ", but it was never defined."

incorrectUsageOfMultiParam :: String
incorrectUsageOfMultiParam = "Usage of a multi parameter in the value range of another parameter that has more than one possible value or is a multi parameter itself is not allowed"