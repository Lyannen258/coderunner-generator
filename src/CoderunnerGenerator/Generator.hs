module CoderunnerGenerator.Generator where

import Control.Monad (foldM)
import Data.List (intercalate)
import qualified Data.Map as M
import CoderunnerGenerator.Helper
import CoderunnerGenerator.Interaction
import CoderunnerGenerator.Parser as Parser
import CoderunnerGenerator.SemanticAnalyzer
import System.FilePath (takeBaseName, takeDirectory)
import Text.Read
import Text.XML.Light

generateOutput :: AST -> SymbolTable -> InteractionResult -> FilePath -> Either String String
generateOutput ast@(AST CoderunnerFile _ (_ : task : sol : pre : cs)) st vt filePath = do
  taskGen <- generateBody task st vt
  solGen <- generateBody sol st vt
  preGen <- generateBody pre st vt
  let nameGen = takeBaseName filePath
  -- let solTemp = createSolutionTemplate preGen "modulo"
  let ret = intercalate "\n\n" [taskGen, solGen, preGen]
  let xmlDoc =
        node (unqual "quiz") $
          add_attr
            (Attr (unqual "type") "coderunner")
            ( node
                (unqual "question")
                [
                node (unqual "name")
                  (
                    node (unqual "text") (CData CDataText nameGen Nothing)
                  ),
                  node
                    (unqual "questiontext")
                    ( Attr (unqual "format") "html",
                      node (unqual "text") (CData CDataText taskGen Nothing)
                    ),
                  node (unqual "defaultgrade") (CData CDataText "1" Nothing),
                  node (unqual "penalty") (CData CDataText "0" Nothing),
                  node (unqual "hidden") (CData CDataText "0" Nothing),
                  node (unqual "coderunnertype") (CData CDataText "cpp_function" Nothing),
                  node (unqual "prototypetype") (CData CDataText "0" Nothing),
                  node (unqual "allornothing") (CData CDataText "1" Nothing),
                  node (unqual "penaltyregime") (CData CDataText "10, 20, ..." Nothing),
                  node (unqual "precheck") (CData CDataText "0" Nothing),
                  node (unqual "hidecheck") (CData CDataText "0" Nothing),
                  node (unqual "showsource") (CData CDataText "0" Nothing),
                  node (unqual "answerboxlines") (CData CDataText "18" Nothing),
                  node (unqual "answerboxcolumns") (CData CDataText "100" Nothing),
                  node (unqual "answerpreload") (CData CDataVerbatim preGen Nothing),
              -- #TODO template for the coderunner execution  node (unqual "template") (CData CDataVerbatim preGen Nothing)
                  -- node (unqual "template") (CData CDataVerbatim solTemp Nothing),
                  node (unqual "answer") (CData CDataVerbatim solGen Nothing),
                  node (unqual "validateonsave") (CData CDataText "1" Nothing),
                  node (unqual "hoisttemplateparams") (CData CDataText "1" Nothing),
                  node (unqual "templateparamslang") (CData CDataText "twig" Nothing),
                  node (unqual "templateparamsevalpertry") (CData CDataText "0" Nothing),
                  node (unqual "templateparamsevald") (CData CDataText "{}" Nothing),
                  node (unqual "twigall") (CData CDataText "0" Nothing),
                  node (unqual "attachments") (CData CDataText "0" Nothing),
                  node (unqual "attachmentsrequired") (CData CDataText "0" Nothing),
                  node (unqual "maxfilesize") (CData CDataText "10240" Nothing),
                  node (unqual "displayfeedback") (CData CDataText "1" Nothing)
                  -- #TODO testcases for each input value.
                ]
            )
  return $ ppTopElement xmlDoc

--createSolutionTemplate :: String -> String -> String
--createSolutionTemplate haystack needle = strReplace needle "solution" haystack
-- Perhapts using Regex-> Text.Regex subRegex sieht da ganz gut aus.


generateBody :: AST -> SymbolTable -> InteractionResult -> Either String String
generateBody (AST TaskSection _ (body : cs)) st vt = generateBody body st vt
generateBody (AST SolutionSection _ (body : cs)) st vt = generateBody body st vt
generateBody (AST PreAllocationSection _ (body : cs)) st vt = generateBody body st vt
generateBody ast@(AST Body _ cs) st vt = do
  foldM folder "" cs
  where
    folder :: String -> AST -> Either String String
    folder str ast@(AST Constant v _) = do return $ str ++ v
    folder str ast@(AST ParameterUsage _ cs) = do
      let l = length cs
      val <- case l of
        0 -> Left ("Parameter Usage has no subnodes: " ++ show ast)
        1 -> analyzeId (head cs) st vt
        2 -> analyzeIdAndProp cs st vt
        x -> Left ("ParameterUsage has too many subnodes" ++ show ast)
      return $ str ++ val

-- only with single values
analyzeId :: AST -> SymbolTable -> InteractionResult -> Either String String
analyzeId ast@(AST Identifier v _) st vt = do getIrSingleValue v vt
analyzeId ast _ _ = do Left $ "Not an identifier node: " ++ show ast

analyzeIdAndProp :: [AST] -> SymbolTable -> InteractionResult -> Either String String
-- only with Blueprint
analyzeIdAndProp [AST Identifier id _, AST PropertyPart prop []] st vt = do
  symbolEntry <- maybeToEither (M.lookup id st) ("No symbol found for identifier: '" ++ id ++ "'")
  case symbolEntry of
    BlueprintUsageSymbol s m -> getPropertyValue m
    x -> Left "PropertyPart can only be used with blueprint-identifiers"
  where
    getPropertyValue m = do
      propValues <- maybeToEither (M.lookup prop m) ("Symbol '" ++ id ++ "' has no property '" ++ prop ++ "'")
      return $ head propValues

-- only with multiple values
analyzeIdAndProp [AST Identifier id _, AST PropertyPart prop [AST FunctionCallPart _ optArg]] st vt = do
  values <- getIrValues id vt
  let args =
        if null optArg
          then ""
          else Parser.value (head optArg)
  applyFunctionCall prop args values
  where
    applyFunctionCall "ALL" _ values =
      Right $ intercalate "\n" values
    applyFunctionCall "CHOOSE_AT_RANDOM" arg values =
      case readMaybe arg of
        Just i ->
          if i <= length values
            then Right $ intercalate "\n" $ take i values
            else Left "More random values requested than there are in the list"
        Nothing -> Left "CHOOSE_AT_RANDOM was called with a non-integer argument"
analyzeIdAndProp ast st vt = Left $ "Not a valid ast: " ++ show ast
