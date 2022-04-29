module CoderunnerGenerator.SemanticAnalyzer where

import Brick (getContext)
import qualified CoderunnerGenerator.Parser as P
import CoderunnerGenerator.Types.AbstractSyntaxTree as AST
import CoderunnerGenerator.Types.ConstraintGraph (ConstraintGraph, (##>), (#>))
import qualified CoderunnerGenerator.Types.ConstraintGraph as CG
import CoderunnerGenerator.Types.SymbolTable
  ( SymbolInformation,
    SymbolTable,
  )
import qualified CoderunnerGenerator.Types.SymbolTable as ST
import Control.Monad (foldM, join)
import Control.Monad.Trans.Reader
import CoderunnerGenerator.Helper (singleton)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, maybeToList)
import Data.Set (fromList)
import Debug.Trace
import Lens.Micro (each, (^.), (^..), _2)
import Lens.Micro.Extras (view)

-- * Semantic Result

type SemanticResult = Either String (SymbolTable, ConstraintGraph)

liftSymbolTable :: SymbolTable -> SemanticResult
liftSymbolTable st = Right (st, CG.empty)

-- * Error Messages

requiresInvalidAmountOfParameters :: String
requiresInvalidAmountOfParameters = "Amount of parameter values in requires constraint should be either the same on both sides or one on the left and more than one on the right side"

-- * Semantic Analysis

semanticAnalysis :: Template -> SemanticResult
semanticAnalysis ast = do
  result <- analyzeTemplate ast
  let symbolTable = fst result
  let graph = (CG.rmNonReciprocEdges . CG.addImplicitEdges) (snd result)
  return (symbolTable, graph)

analyzeTemplate :: Template -> SemanticResult
analyzeTemplate template = do
  let statements = view parameterStatements template
  foldM analyzeStatement (ST.empty, CG.empty) statements

-- | The folding function that is used to analyze the parameter statements. The accumulator is a tuple of symbol table and constraint graph.
analyzeStatement :: (SymbolTable, ConstraintGraph) -> ParameterStatement -> SemanticResult
analyzeStatement (st, cg) ps =
  let mainPart = view main ps
      requiresPart = view requires ps
      (mainId, mainSI, mainCG) = analyzeParameterPart mainPart
   in do
        stWithMain <- ST.add mainId mainSI st
        let (stWithReq, cgWithReq) =
              case requiresPart of
                Nothing -> (return stWithMain, return mainCG)
                Just pp ->
                  let (id, si, reqCG) = analyzeParameterPart pp
                      cgWithEdges
                        | ST.lengthSI' si == ST.lengthSI' mainSI =
                          let valuesMain = ST.valuesSingle mainSI
                              valuesReq = ST.valuesSingle si
                              nodesMain = map (CG.Node mainId . singleton) valuesMain
                              nodesReq = map (CG.Node id . singleton) valuesReq
                              edges = zipWith (curry CG.edge) nodesMain nodesReq
                           in Right $ CG.merge edges
                        | ST.lengthSI' mainSI == 1 && ST.lengthSI' si > 1 =
                          let valueMain = head $ ST.valuesSingle mainSI
                              nodeMain = CG.Node mainId [valueMain]
                              valuesReq = ST.valuesSingle si
                              nodesReq = map (CG.Node id . singleton) valuesReq
                              edges = map (curry CG.edge nodeMain) nodesReq
                           in Right $ CG.merge edges
                        | otherwise = Left requiresInvalidAmountOfParameters
                   in (ST.add id si st, cgWithEdges)
        cgWithReq' <- cgWithReq
        let cgWithValues = cg #> mainCG #> cgWithReq'
        stWithReq' <- stWithReq
        return (stWithReq', cgWithValues)

-- | Analyze a parameter part in the AST
analyzeParameterPart :: ParameterPart -> (Identifier, SymbolInformation, ConstraintGraph)
analyzeParameterPart part =
  let id = view identifier part
      vs = view values part
      stValues = map toSTValue vs
      cg = CG.merge $ map (curry CG.node' id) stValues

      toSTValue :: ParameterValue -> ST.Value
      toSTValue (ParameterValue valueParts) =
        let isIncomplete = any isIdUsage valueParts
            concattedValues = foldr f "" valueParts
            f (AST.Simple s) acc = acc ++ s
            f _ acc = acc
         in if isIncomplete
              then ST.incompleteValue valueParts
              else ST.finalValue concattedValues
   in (id, ST.singleSymbol stValues, cg) -- It is possible to always use ST.singleSymbol. If needed, ST.add will convert it to a multi symbol.