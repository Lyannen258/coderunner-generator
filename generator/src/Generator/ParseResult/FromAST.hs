module Generator.ParseResult.FromAST where

import Control.Monad (foldM)
import Data.Foldable (Foldable (foldl'))
import Generator.ParameterName
import Generator.ParameterParser.AST as AST
import Generator.ParseResult
import Generator.ParseResult.Type as PR
import Lens.Micro ((^.))

-- | Constructs a parseResult from a ParameterAST
fromParameterAST :: AST.ParameterAST -> Either String ParseResult
fromParameterAST pAST = foldM f empty parameterStatements'
  where
    parameterStatements' :: [AST.ParameterStatement]
    parameterStatements' = pAST ^. AST.parameterStatements

    f :: ParseResult -> AST.ParameterStatement -> Either String ParseResult
    f pr ps =
      let psMain = ps ^. AST.main
          psReq = ps ^. AST.requires
       in case psMain of
            AST.SingleParameterPart mainId mainPVS ->
              case psReq of
                Nothing -> addParameter pr . makeParam (mkParameterName mainId) =<< mapM toPRValue mainPVS
                Just (AST.SingleParameterPart reqId reqPVS) -> do
                  v1s <- mapM toPRValue mainPVS
                  v2s <- mapM toPRValue reqPVS
                  addToPR pr mainId v1s reqId v2s
                Just (AST.MultiParameterPart reqId reqPVSS) ->
                  do
                    v1s <- mapM toPRValue mainPVS
                    v2s <- mapM (mapM toPRValue) reqPVSS
                    addToPR pr mainId v1s reqId v2s
            AST.MultiParameterPart mainId mainPVSS ->
              case psReq of
                Nothing -> addParameter pr . makeParam (mkParameterName mainId) =<< mapM (mapM toPRValue) mainPVSS
                Just (AST.SingleParameterPart reqId reqPVS) ->
                  do
                    v1s <- mapM (mapM toPRValue) mainPVSS
                    v2s <- mapM toPRValue reqPVS
                    addToPR pr mainId v1s reqId v2s
                Just (AST.MultiParameterPart reqId reqPVSS) ->
                  do
                    v1s <- mapM (mapM toPRValue) mainPVSS
                    v2s <- mapM (mapM toPRValue) reqPVSS
                    addToPR pr mainId v1s reqId v2s

toPRValue :: AST.ParameterValue -> Either String Value
toPRValue (AST.Regular pvps) = return $ RegularValue $ pvpsToPRValue pvps
toPRValue (AST.Tuple pvpss) =
  TupleValue . PR.Tuple <$> mapM pvpsToPRTupleValue pvpss

pvpsToPRValue :: [AST.ParameterValuePart] -> RegularValue
pvpsToPRValue pvps =
  if any AST.isIdUsage pvps
    then NeedsInput (map toPRValuePart pvps)
    else Final (foldl' (\acc (AST.Simple s) -> acc ++ s) "" pvps)

pvpsToPRTupleValue :: [AST.ParameterValuePart] -> Either String String
pvpsToPRTupleValue pvps
  | any AST.isIdUsage pvps = Left "Using variables inside of tuples is not allowed."
  | otherwise = return $ foldl' f "" pvps
  where
    f b (Simple s) = b ++ s
    f b _ = b

toPRValuePart :: AST.ParameterValuePart -> ValuePart
toPRValuePart (AST.Simple s) = StringPart s
toPRValuePart (AST.IdUsage n) = ParameterPart (mkParameterName n)
toPRValuePart (AST.TupleSelect i int) = PR.TupleSelect (mkParameterName i) int

addToPR :: (MakeParam v1, MakeParam v2) => ParseResult -> AST.Identifier -> [v1] -> AST.Identifier -> [v2] -> Either String ParseResult
addToPR pr mainId mainVs reqId reqVs =
  if length mainVs == length reqVs
    then do
      pr' <- addParameter pr $ makeParam (mkParameterName mainId) mainVs
      pr'' <- addParameter pr' $ makeParam (mkParameterName reqId) reqVs
      let pairs = zip mainVs reqVs
      foldM f pr'' pairs
    else Left $ "Value ranges of " ++ mainId ++ " and " ++ reqId ++ " do not have the same amount of values in requires constraint."
  where
    f prLocal (m, r) =
      addConstraint
        prLocal
        (mkParameterName mainId, makeValue m)
        (mkParameterName reqId, makeValue r)