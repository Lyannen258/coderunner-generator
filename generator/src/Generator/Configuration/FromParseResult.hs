{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use first" #-}

module Generator.Configuration.FromParseResult (computeConfigurations, computeMaxAmount) where

import Control.Monad.Except
import Control.Monad.Extra (ifM)
import Control.Monad.Reader
import Control.Monad.State
import Data.Foldable (Foldable (toList), foldl')
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as Seq
import Generator.App
import Generator.Atoms
import Generator.Configuration as C
import Generator.Configuration.Internal as C
import Generator.Configuration.Type
import Generator.Globals (getAmount, getInteractive)
import Generator.Helper (maybeToError, printLn, singleton)
import Generator.Interactive
import Generator.ParseResult.Info qualified as PR
import Generator.ParseResult.Type (Constraint, ParseResult)
import Generator.ParseResult.Type qualified as PR
import System.Random (RandomGen, getStdGen, uniformR)

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
      configs <- mapM (generateConfiguration pr) withoutForbidden
      ifM
        (asks getInteractive)
        (forM [1 .. amount] $ \i -> do liftIO $ putStrLn ("\nSpecify variant " ++ show i ++ ":\n"); chooseConfig configs)
        (chooseRandoms amount configs)

chooseRandoms :: Int -> [Configuration] -> App r u [Configuration]
chooseRandoms amount configs = do
  randoms <- getRandomNumbers amount (length configs)
  return $ map (configs !!) randoms

allCombinations :: ParseResult -> ConfigListRaw
allCombinations pr =
  let parameterNames :: [ParameterName]
      parameterNames = PR.getParameterNames pr

      parameterValueTuples :: ConfigListRaw
      parameterValueTuples = map f' parameterNames
        where
          f' :: ParameterName -> ConfigRaw
          f' pn = ConfigRaw $ map (pn,) [0 .. PR.countValues pr pn - 1]

      f :: ConfigListRaw -> ConfigRaw -> ConfigListRaw
      f [] parameterValueTuple = map (ConfigRaw . singleton) parameterValueTuple.config
      f acc parameterValueTuple = concatMap f' acc
        where
          f' :: ConfigRaw -> ConfigListRaw
          f' config = map (\pv -> ConfigRaw $ config.config ++ [pv]) parameterValueTuple.config
   in foldl' f [] parameterValueTuples

removeForbidden :: ParseResult -> ConfigListRaw -> ConfigListRaw
removeForbidden pr = filter f
  where
    f :: ConfigRaw -> Bool
    f config = all constraintFulfilled constraintsL
      where
        constraintFulfilled :: Constraint -> Bool
        constraintFulfilled c
          | PR.from c `elem` config.config = PR.to c `elem` config.config
          | otherwise = True

    constraintsL :: [Constraint]
    constraintsL = PR.getConstraints pr

evaluateRequestedAmount :: Int -> App r u Int
evaluateRequestedAmount maxAmount = do
  amountMaybe <- asks getAmount
  amount <-
    ifM
      (asks getInteractive)
      (return $ fromMaybe 1 amountMaybe)
      (return $ fromMaybe maxAmount amountMaybe)
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

generateConfiguration :: (MonadIO m, MonadError String m) => ParseResult -> ConfigRaw -> m Configuration
generateConfiguration = makeConfig

makeConfig :: (MonadIO m, MonadError String m) => ParseResult -> ConfigRaw -> m Configuration
makeConfig pr cr = do
  emptyC <- liftIO empty
  case execStateT (runReaderT (unConfigurationM action) (cr, pr)) emptyC of
    Left e -> throwError e
    Right cpr -> return cpr
  where
    action :: ConfigurationM ()
    action = mapM_ processParameter pr.comp.parameters

processParameter :: PR.Parameter -> ConfigurationM ()
processParameter p = case p.range of
  Single r -> processSingle p.name r
  SingleTuple r -> processSingleTuple p.name r
  Multi r -> processMulti p.name r
  MultiTuple r -> processMultiTuple p.name r

processSingle :: ParameterName -> SingleRange IncompleteAtomicValue -> ConfigurationM ()
processSingle n sr = do
  let vs = fmap (.value) sr.range
  newValues <- mapM makeFinalAtomicValue vs
  if hasMultiParamRef newValues
    then processSingleWithMultiParamRef n newValues
    else processSingleDefault n (fmap head newValues)

hasMultiParamRef :: Seq.Seq [a] -> Bool
hasMultiParamRef = any p
  where
    p [_] = False
    p _ = True

processSingleWithMultiParamRef :: ParameterName -> Seq.Seq [AtomicValue] -> ConfigurationM ()
processSingleWithMultiParamRef n values
  | length values == 1 = do
      valueRange <- maybeToError (Seq.lookup 0 values) "processSingleWithMultiParamRef: Empty value range."
      let selectedValue = Seq.fromList valueRange
      let valueRange' = Seq.singleton selectedValue
      let mvvalues = fmap MV valueRange'
      addMultiParameter $ Parameter n (MV selectedValue) (RangeType mvvalues)
  | otherwise = throwError "processSingleWithMultiParamRef: Reference of multi parameter inside of another parameter. The referencing parameter has more than one value in the value range."

processSingleDefault :: ParameterName -> Seq.Seq AtomicValue -> ConfigurationM ()
processSingleDefault n newValues = do
  let newSValues = fmap SV newValues
  index <- getSelection n
  selectedValue <- maybeToError (Seq.lookup index newValues) "processSingle: Index out of bounds"
  addSingleParameter $ Parameter n (SV selectedValue) (RangeType newSValues)

processSingleTuple :: ParameterName -> SingleTupleRange IncompleteAtomicValue -> ConfigurationM ()
processSingleTuple n str = do
  let vs = fmap (.value) str.range
  newValues <- (mapM . mapM) makeFinalAtomicValueNoMulti vs
  let newSTValues = fmap STV newValues
  index <- getSelection n
  selectedValue <- maybeToError (Seq.lookup index newValues) "processSingle: Index out of bounds"
  addSingleTupleParameter $ Parameter n (STV selectedValue) (RangeType newSTValues)

processMulti :: ParameterName -> MultiRange IncompleteAtomicValue -> ConfigurationM ()
processMulti n str = do
  let vs = fmap (.value) str.range
  newValues <- (mapM . mapM) makeFinalAtomicValueNoMulti vs
  let newMValues = fmap MV newValues
  index <- getSelection n
  selectedValue <- maybeToError (Seq.lookup index newValues) "processSingle: Index out of bounds"
  addMultiParameter $ Parameter n (MV selectedValue) (RangeType newMValues)

processMultiTuple :: ParameterName -> MultiTupleRange IncompleteAtomicValue -> ConfigurationM ()
processMultiTuple n str = do
  let vs = fmap (.value) str.range
  newValues <- (mapM . mapM . mapM) makeFinalAtomicValueNoMulti vs
  let newMValues = fmap MTV newValues
  index <- getSelection n
  selectedValue <- maybeToError (Seq.lookup index newValues) "processSingle: Index out of bounds"
  addMultiTupleParameter $ Parameter n (MTV selectedValue) (RangeType newMValues)

makeFinalAtomicValueNoMulti ::
  IncompleteAtomicValue -> ConfigurationM AtomicValue
makeFinalAtomicValueNoMulti iav = do
  x <- makeFinalAtomicValue iav
  case x of
    [single] -> return single
    _ -> throwError "makeFinalAtomicValueNoMulti: contains multi parameter reference"

makeFinalAtomicValue :: IncompleteAtomicValue -> ConfigurationM [AtomicValue]
makeFinalAtomicValue iav = do
  parts <- mapM makeFinalValuePart iav.parts
  let avs = foldl' f ([AtomicValue ""], []) parts
  return $ fst avs
  where
    f acc (Simple s) = (map (\v -> AtomicValue $ v.value ++ s) (fst acc), snd acc)
    f acc (Several n ss)
      | n `elem` snd acc && length (fst acc) == length ss -- This actually not enough! Incorrect behaviour!
        =
          (map AtomicValue $ zipWith (++) (map (.value) (fst acc)) ss, snd acc)
      | otherwise = combineValues acc ss

combineValues :: ([AtomicValue], b) -> [String] -> ([AtomicValue], b)
combineValues acc ss = ([AtomicValue $ av.value ++ s | av <- fst acc, s <- ss], snd acc)

data ValuePartProcessed
  = Simple String
  | Several ParameterName [String] -- Origin and values

makeFinalValuePart :: ValuePart -> ConfigurationM ValuePartProcessed
makeFinalValuePart (StringPart s) = return $ Simple s
makeFinalValuePart (IdUsage pn) = processIdUsage pn
makeFinalValuePart (TupleSelect pn i) = processTupleSelect pn i

processWith ::
  ParameterName ->
  (Maybe ParameterType -> ConfigurationM ValuePartProcessed -> ConfigurationM ValuePartProcessed) ->
  ConfigurationM ValuePartProcessed
processWith pn func = do
  pr <- asks snd
  pt <- parameterType pn
  func pt $
    case PR.getParameter pr pn of
      Just p -> do
        processParameter p
        func pt err
      _ -> err
  where
    err = throwError $ "processWith: Cannot resolve call to parameter " ++ pn.name

processIdUsage :: ParameterName -> ConfigurationM ValuePartProcessed
processIdUsage pn = processWith pn tryWithPT
  where
    tryWithPT :: Maybe ParameterType -> ConfigurationM ValuePartProcessed -> ConfigurationM ValuePartProcessed
    tryWithPT pt else' = case pt of
      Just SingleT -> do
        param <- getSingleParameterM pn
        return $ Simple param.selectedValue.value.value
      Just MultiT -> do
        param <- getMultiParameterM pn
        return . Several pn . toList $ fmap (.value) param.selectedValue.value
      _ -> else'

processTupleSelect :: ParameterName -> Int -> ConfigurationM ValuePartProcessed
processTupleSelect pn i = processWith pn tryWithPT
  where
    tryWithPT :: Maybe ParameterType -> ConfigurationM ValuePartProcessed -> ConfigurationM ValuePartProcessed
    tryWithPT pt else' = case pt of
      Just SingleTupleT -> do
        param <- getSingleTupleParameterM pn
        x <- liftEither $ tupleLookup param.selectedValue.value i
        return $ Simple x.value
      Just MultiTupleT -> do
        param <- getMultiTupleParameterM pn
        selectedValues <- liftEither $ mapM (`tupleLookup` i) param.selectedValue.value
        return . Several pn . toList $ fmap (.value) selectedValues
      _ -> else'