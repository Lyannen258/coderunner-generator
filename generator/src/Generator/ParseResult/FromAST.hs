module Generator.ParseResult.FromAST where

import Control.Monad (foldM)
import Data.Foldable (Foldable (foldl'))
import qualified Data.Sequence as Seq
import Generator.ParameterName
import Generator.ParameterParser.AST as AST
import Generator.ParseResult
import Generator.ParseResult.Info (ValueType)
import Generator.ParseResult.Type as PR
import Lens.Micro ((^.))

-- | Constructs a parseResult from a ParameterAST
fromParameterAST :: AST.ParameterAST -> Either String ParseResult
fromParameterAST pAST = foldM f empty parameterStatements'
  where
    parameterStatements' :: [AST.ParameterStatement]
    parameterStatements' = pAST ^. AST.parameterStatements

    f :: ParseResult -> AST.ParameterStatement -> Either String ParseResult
    f pr (ParameterStatement _ mainPS reqPS) = case mainPS of
      AST.SingleParameterPart mainId mainPVS ->
        case reqPS of
          Nothing -> addParameterSingle pr mainPS mainId mainPVS 
          Just (AST.SingleParameterPart reqId reqPVS) -> do
            v1s <- mapM toPRValue mainPVS
            v2s <- mapM toPRValue reqPVS
            addToPR pr mainId v1s reqId v2s
          Just (AST.MultiParameterPart reqId reqPVSS) ->
            do
              v1s <- mapM toPRValue mainPVS
              v2s <- mapM (mapM toPRValue) reqPVSS
              addToPR pr mainId v1s reqId (map Seq.fromList v2s)
      AST.MultiParameterPart mainId mainPVSS ->
        case reqPS of
          Nothing -> addParameterMulti pr mainPS mainId mainPVSS 
          Just (AST.SingleParameterPart reqId reqPVS) ->
            do
              v1s <- mapM (mapM toPRValue) mainPVSS
              v2s <- mapM toPRValue reqPVS
              addToPR pr mainId (map Seq.fromList v1s) reqId v2s
          Just (AST.MultiParameterPart reqId reqPVSS) ->
            do
              v1s <- mapM (mapM toPRValue) mainPVSS
              v2s <- mapM (mapM toPRValue) reqPVSS
              addToPR pr mainId (map Seq.fromList v1s) reqId (map Seq.fromList v2s)

addParameterSingle :: ParseResult -> ParameterPart -> String -> [ParameterValue] -> Either String ParseResult
addParameterSingle pr pp i pvs
  | isTuplePart pp = do 
    vs <- mapM toPRTupleValue pvs 
    addParameter pr $ makeParam (mkParameterName i) $ makeRange vs
  | otherwise = do
    vs <- mapM toPRValue pvs 
    addParameter pr $ makeParam (mkParameterName i) $ makeRange vs

addParameterMulti :: ParseResult -> ParameterPart -> String -> [[ParameterValue]] -> Either String ParseResult
addParameterMulti pr pp i pvs
  | isTuplePart pp = do 
    vs <- mapM (mapM toPRTupleValue) pvs 
    addParameter pr $ makeParam (mkParameterName i) $ makeRange $ map Seq.fromList vs
  | otherwise = do
    vs <- mapM (mapM toPRValue) pvs 
    addParameter pr $ makeParam (mkParameterName i) $ makeRange $ map Seq.fromList vs

toPRValue :: AST.ParameterValue -> Either String RegularValue
toPRValue (AST.Regular pvps) =
  if any AST.containsOtherParameters pvps
    then Right . NeedsInput $ map toPRValuePart pvps
    else Right . Final $ foldl' (\acc (AST.Simple s) -> acc ++ s) "" pvps
toPRValue _ = Left "Cannot build a single value from a tuple value"

toPRTupleValue :: AST.ParameterValue -> Either String TupleValue
toPRTupleValue (AST.Tuple pvps)
  | any AST.containsOtherParameters (concat pvps) = Left "Using variables inside of tuples is not allowed."
  | otherwise = do
    let tvs = map Regular pvps
    rvs <- mapM toPRValue tvs
    return $ PR.Tuple $ Seq.fromList rvs
toPRTupleValue _ = Left "Cannot build a tuple value from a single value"

toPRValuePart :: AST.ParameterValuePart -> ValuePart
toPRValuePart (AST.Simple s) = StringPart s
toPRValuePart (AST.IdUsage n) = ParameterPart (mkParameterName n)
toPRValuePart (AST.TupleSelect i int) = PR.TupleSelect (mkParameterName i) int

addToPR :: (ValueType v1 r1, ValueType v2 r2) => ParseResult -> AST.Identifier -> [v1] -> AST.Identifier -> [v2] -> Either String ParseResult
addToPR pr mainId mainVs reqId reqVs =
  if length mainVs == length reqVs
    then do
      pr' <- addParameter pr $ makeParam (mkParameterName mainId) $ makeRange mainVs
      pr'' <- addParameter pr' $ makeParam (mkParameterName reqId) $ makeRange reqVs
      let pairs = zip mainVs reqVs
      foldM f pr'' pairs
    else Left $ "Value ranges of " ++ mainId ++ " and " ++ reqId ++ " do not have the same amount of values in requires constraint."
  where
    f prLocal (m, r) =
      addConstraint
        prLocal
        (mkParameterName mainId, m)
        (mkParameterName reqId, r)