{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# LANGUAGE TupleSections #-}

{-# HLINT ignore "Use first" #-}

module Generator.Configuration.FromParseResult (computeConfigurations, computeMaxAmount) where

import Control.Monad.Extra (foldM, forM, ifM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (liftCatch)
import Data.Foldable (Foldable(toList), find, foldl')
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe (maybeToExceptT)
import Data.Sequence qualified as Seq
import Generator.App
import Generator.Atoms
import Generator.Configuration as C
import Generator.Configuration.Internal as C
import Generator.Configuration.Type
import Generator.Globals (getAmount, getInteractive)
import Generator.Interactive
import Generator.Atoms
import Generator.ParseResult.Type (Constraint, ParseResult)
import Generator.Helper (maybeToError, printLn, singleton)
import Generator.ParseResult.Info qualified as PR
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
generateConfiguration pr configRaw = makeConfig pr configRaw

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

checkMultiUsageRequirements :: ParameterName -> ConfigurationM ()
checkMultiUsageRequirements pn = do
  pr <- asks snd
  if PR.countValues pr pn == 1 then return () else throwError "It is not possible to use a multi or multi-tuple parameter inside of the definition of a parameter with a range of more than a single value"

-- buildParameter :: ParseResult -> ConfigRaw -> Configuration -> PR.Parameter -> App r u Configuration
-- buildParameter pr cr c p@(PR.Parameter n r)
--   | PR.containsMultiParamUsage pr n =
--     if checkMultiParamUsageReqs pr p
--       then do
--         (vs, newC) <- buildWithMultiParamUsage pr cr c p
--         let p' = Parameter n $ Multi $ MultiComponent vs [vs]
--         return $ C.addParameter newC p'
--       else (lift . throwE) incorrectUsageOfMultiParam
--   | otherwise = do
--     (_, i) <- lift . except $ maybeToEither (find (\(n', _) -> n == n') cr) "Something went wrong"
--     case r of
--       PR.Single (PR.SingleRange vs) -> buildSingleParameter pr cr c n i vs
--       PR.SingleTuple (PR.SingleTupleRange vs) -> buildSingleTupleParameter pr cr c n i vs
--       PR.Multi (PR.MultiRange vs) -> buildMultiParameter pr cr c n i vs
--       PR.MultiTuple (PR.MultiTupleRange vs) -> buildMultiTupleParameter pr cr c n i vs

-- buildSingleParameter :: ParseResult -> ConfigRaw -> Configuration -> ParameterName -> Int -> Seq PR.RegularValue -> App r u Configuration
-- buildSingleParameter pr cr c pn i rvs = do
--   (vs, c') <- foldM (regularFolder pr cr) ([], c) rvs
--   let p = Parameter pn $ Single $ SingleComponent (vs !! i) vs
--   return $ addParameter c' p

-- regularFolder :: ParseResult -> ConfigRaw -> ([Value], Configuration) -> PR.RegularValue -> App r u ([Value], Configuration)
-- regularFolder pr cr (vsLocal, cLocal) rv = do
--   (v, cLocal') <- buildRegularValue pr cr cLocal rv
--   return (vsLocal ++ [Regular v], cLocal')

-- buildSingleTupleParameter :: ParseResult -> ConfigRaw -> Configuration -> ParameterName -> Int -> Seq PR.TupleValue -> App r u Configuration
-- buildSingleTupleParameter pr cr c pn i tvs = do
--   (vs, c') <- foldM (tupleFolder pr cr) ([], c) tvs
--   let p = Parameter pn $ Single $ SingleComponent (vs !! i) vs
--   return $ addParameter c' p

-- tupleFolder :: ParseResult -> ConfigRaw -> ([Value], Configuration) -> PR.TupleValue -> App r u ([Value], Configuration)
-- tupleFolder pr cr (vsLocal, cLocal) (PR.Tuple rvs) = do
--   (strs, cLocal') <- buildTupleValue pr cr cLocal rvs
--   return (vsLocal ++ [Tuple strs], cLocal')

-- buildMultiParameter :: ParseResult -> ConfigRaw -> Configuration -> ParameterName -> Int -> Seq (Seq PR.RegularValue) -> App r u Configuration
-- buildMultiParameter pr cr c pn i rvss = do
--   (vss, c') <- foldM f ([], c) rvss
--   let p = Parameter pn $ Multi $ MultiComponent (vss !! i) vss
--   return $ addParameter c' p
--   where
--     f :: ([[Value]], Configuration) -> Seq PR.RegularValue -> App r u ([[Value]], Configuration)
--     f (vss, cLocal) rvs = do
--       (vs, cLocal') <- foldM (regularFolder pr cr) ([], cLocal) rvs
--       return (vss ++ [vs], cLocal')

-- buildMultiTupleParameter :: ParseResult -> ConfigRaw -> Configuration -> ParameterName -> Int -> Seq (Seq PR.TupleValue) -> App r u Configuration
-- buildMultiTupleParameter pr cr c pn i tvss = do
--   (vss, c') <- foldM f ([], c) tvss
--   let p = Parameter pn $ Multi $ MultiComponent (vss !! i) vss
--   return $ addParameter c' p
--   where
--     f :: ([[Value]], Configuration) -> Seq PR.TupleValue -> App r u ([[Value]], Configuration)
--     f (vss, cLocal) tvs = do
--       (vs, cLocal') <- foldM (tupleFolder pr cr) ([], cLocal) tvs
--       return (vss ++ [vs], cLocal')

-- buildRegularValue :: ParseResult -> ConfigRaw -> Configuration -> PR.RegularValue -> App r u (String, Configuration)
-- buildRegularValue pr cr c rv = case rv of
--   PR.Final s -> return (s, c)
--   PR.NeedsInput vps -> do
--     makeFinal pr cr c vps

-- buildTupleValue :: ParseResult -> ConfigRaw -> Configuration -> Seq PR.RegularValue -> App r u ([String], Configuration)
-- buildTupleValue pr cr c = foldM f ([], c)
--   where
--     f :: ([String], Configuration) -> PR.RegularValue -> App r u ([String], Configuration)
--     f (vsLocal, cLocal) rv = do
--       (v, cLocal') <- buildRegularValue pr cr cLocal rv
--       return (vsLocal ++ [v], cLocal')

-- makeFinal :: ParseResult -> ConfigRaw -> Configuration -> [ValuePart] -> App r u (String, Configuration)
-- makeFinal pr cr c = foldM f ("", c)
--   where
--     f :: (String, Configuration) -> ValuePart -> App r u (String, Configuration)
--     f (s, cLocal) vp = do
--       (addS, cLocal') <- makeFinalValuePart pr cr cLocal vp
--       return (s ++ addS, cLocal')

-- makeFinalValuePart :: ParseResult -> ConfigRaw -> Configuration -> ValuePart -> App r u (String, Configuration)
-- makeFinalValuePart _ _ c (PR.StringPart s) =
--   return (s, c)
-- makeFinalValuePart pr cr c (PR.ParameterPart pn) =
--   evaluateParameterPart pn pr cr c
-- makeFinalValuePart pr cr c (PR.TupleSelect pn i) =
--   evaluateTuplePart pn i pr cr c

-- evaluateParameterPart :: ParameterName -> ParseResult -> ConfigRaw -> Configuration -> App r u (String, Configuration)
-- evaluateParameterPart pn pr cr c =
--   let maybeSingleValue = getSingleValue c pn
--       maybeSearchedX = find (\x -> fst x == pn) cr
--    in case maybeSingleValue of
--         Just str -> return (str, c)
--         Nothing -> case maybeSearchedX of
--           Nothing -> lift . throwE $ paramNotFoundErr pn
--           Just (pn', _) -> do
--             newC <- case PR.getParameter pr pn' of
--               Nothing -> lift . throwE $ paramNotFoundErr pn
--               Just pa -> buildParameter pr cr c pa
--             case getSingleValue newC pn' of
--               Just s -> return (s, newC)
--               _ -> lift . throwE $ "Parameter " ++ unParameterName pn ++ " cannot be resolved"

-- evaluateTuplePart :: ParameterName -> Int -> ParseResult -> ConfigRaw -> Configuration -> App r u (String, Configuration)
-- evaluateTuplePart pn i pr cr c =
--   if contains c pn
--     then f c
--     else case find (\x -> fst x == pn) cr of
--       Nothing -> lift . throwE $ paramNotFoundErr pn
--       Just (pn', _) -> do
--         newC <- case PR.getParameter pr pn' of
--           Nothing -> lift . throwE $ paramNotFoundErr pn
--           Just pa -> buildParameter pr cr c pa
--         f newC
--   where
--     f conf' = case getTupleValueSingle conf' pn of
--       Nothing -> lift . throwE $ "Tried to call 'get' on parameter '" ++ show pn ++ "', but it is not a tuple parameter"
--       Just ss ->
--         if i < length ss
--           then return (ss !! i, conf')
--           else lift . throwE $ "Tried to get value " ++ show i ++ " of parameter '" ++ show pn ++ "', but it has only " ++ show (length ss)

-- getTupleValueSingle :: Configuration -> ParameterName -> Maybe [String]
-- getTupleValueSingle conf pn = do
--   vc <- getValueComponent conf pn
--   case vc of
--     Single (SingleComponent (Tuple ss) _) -> Just ss
--     _ -> Nothing

-- buildWithMultiParamUsage :: ParseResult -> ConfigRaw -> Configuration -> PR.Parameter -> App r u ([Value], Configuration)
-- buildWithMultiParamUsage pr cr c (PR.Parameter _ (PR.Single (PR.SingleRange vs))) = case Seq.lookup 0 vs of
--   (Just (PR.NeedsInput vps)) -> do
--     (strs, _, newC) <- foldM f ([], [], c) vps
--     return (map Regular strs, newC)
--   _ -> lift . throwE $ "This error should not be happening"
--   where
--     f :: ([String], [ParameterName {- already occured tuple parameters -}], Configuration) -> ValuePart -> App r u ([String], [ParameterName], Configuration)
--     f (strs, names, cLocal) (PR.StringPart s) = case strs of
--       [] -> return ([s], names, cLocal)
--       _ -> return (map (++ s) strs, names, cLocal)
--     f (strs, names, cLocal) (PR.ParameterPart pn) = do
--       (paramValues, cLocal') <- evaluateParameterPartMulti pr cr cLocal pn
--       return ([str ++ param | str <- strs, param <- paramValues], names, cLocal')
--     f (strs, names, cLocal) (PR.TupleSelect pn i) = do
--       (paramValues, cLocal') <- evaluateTupleMulti pr cr cLocal pn i
--       let names' = names ++ [pn]
--       if pn `elem` names
--         then return (zipWith (++) strs paramValues, names, cLocal')
--         else return ([str ++ param | str <- strs, param <- paramValues], names', cLocal')
-- buildWithMultiParamUsage _ _ _ _ = lift . throwE $ "Usage of multi parameter in another parameter, that is not a regular single parameter, is not allowed."

-- evaluateParameterPartMulti :: ParseResult -> ConfigRaw -> Configuration -> ParameterName -> App r u ([String], Configuration)
-- evaluateParameterPartMulti pr cr c n = do
--   liftCatch catchE evaluateParameterPart' catcher
--   where
--     evaluateParameterPart' :: App r u ([String], Configuration)
--     evaluateParameterPart' = do
--       (sLocal, cLocal) <- evaluateParameterPart n pr cr c
--       return ([sLocal], cLocal)

--     catcher :: String -> App r u ([String], Configuration)
--     catcher _ = case getMultiValue c n of
--       Just strs -> return (strs, c)
--       Nothing -> do
--         newC <- case PR.getParameter pr n of
--           Nothing -> lift . throwE $ paramNotFoundErr n
--           Just pa -> buildParameter pr cr c pa
--         case getMultiValue newC n of
--           Just strs -> return (strs, newC)
--           _ -> lift . throwE $ tupleInsideAnotherValue

-- evaluateTupleMulti :: ParseResult -> ConfigRaw -> Configuration -> ParameterName -> Int -> App r u ([String], Configuration)
-- evaluateTupleMulti pr cr c pn i = do
--   liftCatch catchE evaluateTuplePart' catcher
--   where
--     evaluateTuplePart' :: App r u ([String], Configuration)
--     evaluateTuplePart' = do
--       (sLocal, cLocal) <- evaluateTuplePart pn i pr cr c
--       return ([sLocal], cLocal)

--     catcher :: String -> App r u ([String], Configuration)
--     catcher _ = case getMultiValue' c pn of
--       Just vs -> do
--         case mapM valueToTupleValue vs of
--           Nothing -> lift . throwE $ show pn ++ " is not a tuple value, but 'get' was called on it."
--           Just strs -> return (strs, c)
--       Nothing -> do
--         newC <- case PR.getParameter pr pn of
--           Nothing -> lift . throwE $ paramNotFoundErr pn
--           Just pa -> buildParameter pr cr c pa
--         case getMultiValue' newC pn of
--           Just vs -> do
--             case mapM valueToTupleValue vs of
--               Nothing -> lift . throwE $ show pn ++ " is not a tuple value, but 'get' was called on it."
--               Just strs -> return (strs, c)
--           Nothing -> lift . throwE $ paramNotFoundErr pn

--     valueToTupleValue :: Value -> Maybe String
--     valueToTupleValue (Tuple strs) = if i < length strs then Just (strs !! i) else Nothing
--     valueToTupleValue _ = Nothing

-- paramNotFoundErr :: ParameterName -> String
-- paramNotFoundErr pn = "Found usage of parameter " ++ unParameterName pn ++ ", but it was never defined."

-- incorrectUsageOfMultiParam :: String
-- incorrectUsageOfMultiParam = "Usage of a multi parameter in the value range of another parameter that has more than one possible value or is a multi parameter itself is not allowed"
