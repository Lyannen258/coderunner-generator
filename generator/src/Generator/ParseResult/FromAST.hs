module Generator.ParseResult.FromAST where

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (Foldable (toList))
import Generator.Atoms
import Generator.Helper (maybeToError)
import Generator.ParameterParser.AST as AST
import Generator.ParseResult
import Generator.ParseResult.Info
import Generator.ParseResult.Type as PR
import Debug.Pretty.Simple (pTraceShowId)

fromParameterAST :: AST.ParameterAST -> Either String ParseResult
fromParameterAST pAST = case runState (runExceptT . unParseResultM $ f) empty of
  (Left s, _) -> Left s
  (Right _, pr) -> Right pr
  where
    f = mapM_ processStatement pAST.statements

processStatement :: AST.ParameterStatement -> ParseResultBuilder ()
processStatement (ParameterStatement ppMain (Just ppReq)) =
  combineParameterParts ppMain ppReq
processStatement (ParameterStatement pp Nothing) =
  addParameter $ Parameter pp.identifier pp.range

combineParameterParts :: ParameterPart -> ParameterPart -> ParseResultBuilder ()
combineParameterParts ppMain ppReq = do
  addParameter $ Parameter ppMain.identifier ppMain.range
  addParameter $ Parameter ppReq.identifier ppReq.range
  mainIndices <- indicesFromRange ppMain
  mainIndices' <- mapM (`maybeToError` "combineParameterParts: Cannot add constraint, if not all values exist") mainIndices
  reqIndices <- indicesFromRange ppReq
  reqIndices' <- mapM (`maybeToError` "combineParameterParts: Cannot add constraint, if not all values exist") reqIndices
  addConstraints
    (ppMain.identifier, mainIndices')
    (ppReq.identifier, reqIndices')
  where
    indicesFromRange :: ParameterPart -> ParseResultBuilder [Maybe Int]
    indicesFromRange pp = case pp.range of
      Single rt -> getIndices (toList rt.range) =<< getRangeSingle pp.identifier
      SingleTuple rt -> getIndices (toList rt.range) =<< getRangeSingleTuple pp.identifier
      Multi rt -> getIndices (toList rt.range) =<< getRangeMulti pp.identifier
      MultiTuple rt -> getIndices (toList rt.range) =<< getRangeMultiTuple pp.identifier

    getIndices :: Eq (v a) => [v a] -> RangeType v a -> ParseResultBuilder [Maybe Int]
    getIndices values range = return $ map (`valueIndex` range) values