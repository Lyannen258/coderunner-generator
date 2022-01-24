module CoderunnerGenerator.Generator where

import CoderunnerGenerator.Helper
import CoderunnerGenerator.Interaction
import CoderunnerGenerator.Types.AbstractSyntaxTree
import CoderunnerGenerator.Types.SymbolTable as ST
import Control.Monad (foldM)
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Lens.Micro.Extras
import System.FilePath (takeBaseName, takeDirectory)
import Text.Read
import Text.XML.Light

generateOutput :: Template -> SymbolTable -> InteractionResult -> FilePath -> Either String String
generateOutput t st vt filePath = do
  let task = view (taskSection . body) t
  let sol = view (solutionSection . body) t
  let pre = view (preAllocationSection . body) t
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
                [ node
                    (unqual "name")
                    ( node (unqual "text") (CData CDataText nameGen Nothing)
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

generateBody :: Mixed -> SymbolTable -> InteractionResult -> Either String String
generateBody m st ir = do
  foldM folder "" m
  where
    folder :: String -> MixedPart -> Either String String
    folder str (ConstantPart c) = do return $ str ++ c
    folder str (ParameterPart pu) = do
      let maybepp = view propertyPart pu
      let id = view identifier pu
      code <-
        ( case maybepp of
            Nothing -> getIrSingleValue id ir
            Just pp -> evaluateWithPropertyPart id pp st ir
          )
      return $ str ++ code

evaluateWithPropertyPart ::
  Identifier ->
  PropertyPart ->
  SymbolTable ->
  InteractionResult ->
  Either String String
evaluateWithPropertyPart id pp st ir = do
  symbolInfo <- maybeToEither (M.lookup id st) ("No symbol found for identifier: '" ++ id ++ "'")
  case symbolInfo of
    BlueprintUsageSymbol b -> evaluateBlueprintProp id b pp
    EnumerationSymbol e -> evaluateFunctionCall id e pp
    _ -> Left "Using a parameter property or calling a function on the parameter is only possible with a blueprint usage parameter or an enumeration with multiple parameters respectively"

evaluateBlueprintProp :: Identifier -> ST.BlueprintUsage -> PropertyPart -> Either String String
evaluateBlueprintProp id bp pp =
  let prop = view property pp
      maybeFuncPart = view arguments pp
      noFuncPart = isNothing maybeFuncPart
      propValues = propertyValues bp
   in if noFuncPart
        then maybeToEither (M.lookup prop propValues) ("Symbol '" ++ id ++ "' has no property '" ++ prop ++ "'")
        else Left "It is not possible to use a function call with a blueprint usage parameter"

evaluateFunctionCall :: Identifier -> ST.Enumeration -> PropertyPart -> Either String String
evaluateFunctionCall id e pp =
  let prop = view property pp
      maybeFuncPart = view arguments pp
   in do
        funcPart <- maybeToEither maybeFuncPart ("Enumeration parameter " ++ id ++ " has no property " ++ prop ++ ". Maybe you forgot to add arguments to the function call.")
        case prop of
          "ALL" -> evaluateAllFunction e
          "CHOOSE_AT_RANDOM" -> evaluateRandomFunction id funcPart e
          _ -> Left "Not a valid function call"

evaluateAllFunction :: ST.Enumeration -> Either String String
evaluateAllFunction e =
  let valueStrings = map enumValue e
   in Right $ intercalate "\n" valueStrings

evaluateRandomFunction :: Identifier -> FunctionCallPart -> ST.Enumeration -> Either String String
evaluateRandomFunction id fcp e =
  let firstArg = head fcp
      valueStrings = map enumValue e
   in case readMaybe firstArg of
        Just i ->
          if i <= length e
            then Right $ intercalate "\n" $ take i valueStrings
            else Left "More random values requested than there are in the list"
        Nothing -> Left "CHOOSE_AT_RANDOM was called with a non-integer argument"