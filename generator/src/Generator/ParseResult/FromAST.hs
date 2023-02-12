module Generator.ParseResult.FromAST where

import Control.Monad (foldM, liftM, mapM)
-- import Generator.ParseResult.Info (ValueType)

import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (Foldable (foldl', toList))
import Data.Sequence qualified as Seq
import Generator.Atoms
import Generator.Helper (maybeToEither, maybeToParseResult)
import Generator.ParameterParser.AST as AST
import Generator.ParseResult
import Generator.ParseResult.Info
import Generator.ParseResult.Type (ParseResultBuilder (ParseResultBuilder))
import Generator.ParseResult.Type as PR
import Lens.Micro ((^.))

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
  addParameter $ Parameter ppReq.identifier ppMain.range
  mainIndices <- indicesFromRange ppMain
  mainIndices' <- mapM (`maybeToParseResult` "combineParameterParts: Cannot add constraint, if not all values exist") mainIndices
  reqIndices <- indicesFromRange ppReq
  reqIndices' <- mapM (`maybeToParseResult` "combineParameterParts: Cannot add constraint, if not all values exist") reqIndices
  addConstraints
    (ppMain.identifier, mainIndices')
    (ppReq.identifier, reqIndices')
  where
    indicesFromRange :: ParameterPart -> ParseResultBuilder [Maybe Int]
    indicesFromRange pp = case pp.range of
      Single rt -> getIndices (toList rt.range) =<< getRangeSingle ppMain.identifier
      SingleTuple rt -> getIndices (toList rt.range) =<< getRangeSingleTuple ppMain.identifier
      Multi rt -> getIndices (toList rt.range) =<< getRangeMulti ppMain.identifier
      MultiTuple rt -> getIndices (toList rt.range) =<< getRangeMultiTuple ppMain.identifier

    getIndices :: Eq (v a) => [v a] -> RangeType v a -> ParseResultBuilder [Maybe Int]
    getIndices values range = return $ map (`valueIndex` range) values