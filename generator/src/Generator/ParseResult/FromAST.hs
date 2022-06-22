module Generator.ParseResult.FromAST where

import Generator.ParameterParser.AST as AST
import Generator.ParseResult
import Generator.ParseResult.Type
import Generator.ParameterName
import Control.Monad (foldM)
import Lens.Micro ((^.))
import Data.Foldable (Foldable(foldl'))

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
                Nothing -> addParameter pr $ makeParam (mkParameterName mainId) (map toPRValue mainPVS)
                Just (AST.SingleParameterPart reqId reqPVS) ->
                  addToPR pr mainId (map toPRValue mainPVS) reqId (map toPRValue reqPVS)
                Just (AST.MultiParameterPart reqId reqPVSS) ->
                  addToPR pr mainId (map toPRValue mainPVS) reqId (map (map toPRValue) reqPVSS)
            AST.MultiParameterPart mainId mainPVSS ->
              case psReq of
                Nothing -> addParameter pr $ makeParam (mkParameterName mainId) (map (map toPRValue) mainPVSS)
                Just (AST.SingleParameterPart reqId reqPVS) ->
                  addToPR pr mainId (map (map toPRValue) mainPVSS) reqId (map toPRValue reqPVS)
                Just (AST.MultiParameterPart reqId reqPVSS) ->
                  addToPR pr mainId (map (map toPRValue) mainPVSS) reqId (map (map toPRValue) reqPVSS)

toPRValue :: AST.ParameterValue -> Value
toPRValue (AST.ParameterValue pvps) =
  if any AST.isIdUsage pvps
    then NeedsInput (map toPRValuePart pvps)
    else Final (foldl' (\acc (AST.Simple s) -> acc ++ s) "" pvps)

toPRValuePart :: AST.ParameterValuePart -> ValuePart
toPRValuePart (AST.Simple s) = StringPart s
toPRValuePart (AST.IdUsage n) = ParameterPart (mkParameterName n)

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