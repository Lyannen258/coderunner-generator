module Generator.Configuration.FromParseResult (computeConfigurations, computeMaxAmount) where

import Control.Monad (foldM, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (catchE, except, throwE)
import Control.Monad.Trans.Reader (asks, liftCatch)
import Data.Foldable (find, foldl')
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Generator.App
import Generator.Atoms (ParameterName, ValuePart)
import Generator.Configuration as C
import Generator.Configuration.Internal as C
import Generator.Configuration.Type
import Generator.Globals (getAmount)
import Generator.Helper (maybeToEither, printLn, singleton)
import Generator.ParseResult.Info qualified as PR
import Generator.ParseResult.Type (Constraint, ParseResult)
import Generator.ParseResult.Type qualified as PR
import System.Random (RandomGen, getStdGen, uniformR)

newtype ConfigListRaw = [ConfigRaw]

newtype ConfigRaw = [(ParameterName, Int)]

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
          | PR.from c `elem` config = PR.to c `elem` config
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

computeAtomicValues :: IncompleteParseResult -> Either String CompleteParseResult
makeFinal pr = case execStateT (runReaderT (unParseResultFinal action) pr) empty of
  Left e -> Left e
  Right cpr -> Right cpr
  where
    action :: ParseResultFinal ()
    action = mapM_ processParameter pr.comp.parameters

processParameter :: Parameter IncompleteAtomicValue -> ParseResultFinal ()
processParameter p = case p.range of
  Single r -> processSingle p.name r
  SingleTuple r -> processSingleTuple p.name r
  Multi r -> processMulti p.name r
  MultiTuple r -> processMultiTuple p.name r

processSingle :: ParameterName -> SingleRange IncompleteAtomicValue -> ParseResultFinal ()
processSingle n sr = do
  let vs = fmap (.value) sr.range
  newValues <- mapM makeFinalAtomicValue vs
  let newSValues = fmap SV newValues
  addParameter $ Parameter n $ (Single . RangeType) newSValues

processSingleTuple :: ParameterName -> SingleTupleRange IncompleteAtomicValue -> ParseResultFinal ()
processSingleTuple n str = do
  let vs = fmap (.value) str.range
  newValues <- (mapM . mapM) makeFinalAtomicValue vs
  let newSTValues = fmap STV newValues
  addParameter $ Parameter n $ (SingleTuple . RangeType) newSTValues

processMulti :: ParameterName -> MultiRange IncompleteAtomicValue -> ParseResultFinal ()
processMulti n str = do
  let vs = fmap (.value) str.range
  newValues <- (mapM . mapM) makeFinalAtomicValue vs
  let newMValues = fmap MV newValues
  addParameter $ Parameter n $ (Multi . RangeType) newMValues

processMultiTuple :: ParameterName -> MultiTupleRange IncompleteAtomicValue -> ParseResultFinal ()
processMultiTuple n str = do
  let vs = fmap (.value) str.range
  newValues <- (mapM . mapM . mapM) makeFinalAtomicValue vs
  let newMValues = fmap MTV newValues
  addParameter $ Parameter n $ (MultiTuple . RangeType) newMValues

makeFinalAtomicValue :: IncompleteAtomicValue -> ParseResultFinal AtomicValue
makeFinalAtomicValue iav = mapM makeFinalValuePart iav.parts

makeFinalValuePart :: ValuePart -> ParseResultFinal String
makeFinalValuePart (StringPart s) = return s

-- makeFinalValuePart (TupleSelect n i) =
-- makeFinalValuePart (IdUsage n) =

-- generateConfiguration :: ParseResult -> ConfigListRaw -> Int -> App r u Configuration
-- generateConfiguration pr configs rand = do
--   emptyC <- (lift . lift) empty
--   foldM f emptyC configRaw
--   where
--     configRaw = configs !! rand

--     f :: Configuration -> (ParameterName, Int) -> App r u Configuration
--     f c x
--       | contains c (fst x) = return c
--       | otherwise = case PR.getParameter pr (fst x) of
--         Nothing -> lift . except . Left $ "Parameter " ++ (unParameterName . fst $ x) ++ " not found."
--         Just pa -> buildParameter pr configRaw c pa

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

-- checkMultiParamUsageReqs :: ParseResult -> PR.Parameter -> Bool
-- checkMultiParamUsageReqs pr (PR.Parameter n (PR.Single _)) =
--   PR.countValues pr n == 1
-- checkMultiParamUsageReqs _ _ = False

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
