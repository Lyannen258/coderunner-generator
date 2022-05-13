module CPPCoderunner.Generator where

import CPPCoderunner.AbstractSyntaxTree
import CoderunnerGenerator.Helper
import CoderunnerGenerator.Types.Configuration
import Control.Monad (foldM)
import Data.List (foldl', intercalate, nub)
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.Text.Lazy (unpack)
import Debug.Pretty.Simple (pTraceShowId)
import Lens.Micro ((^.))
import Lens.Micro.Extras
import System.FilePath (takeBaseName, takeDirectory)
import Text.Pretty.Simple (pShow, pShowNoColor)
import Text.Read
import Text.XML.Light

functionCallInParameterErr :: String
functionCallInParameterErr = "Function call in a parameter is not allowed"

valueNotFoundErr :: String -> String
valueNotFoundErr p = "No value found for usage of parameter '" ++ p ++ "'"

shouldNotHappenErr :: String
shouldNotHappenErr = "This error should not happen. Please ask the software provider."

generate :: [Configuration] -> Template -> Either String [String]
generate configs tem = mapM f configs
  where
    f :: Configuration -> Either String String
    f config = generateConfiguration config tem

generateConfiguration :: Configuration -> Template -> Either String String
generateConfiguration conf tmpl = do
  taskSection <- generateTaskSection conf tmpl
  solutionSection <- generateSolutionSection conf tmpl
  preAllocationSection <- generatePreAllocationSection conf tmpl
  testSection <- generateTestSection conf tmpl
  buildOutput conf taskSection solutionSection preAllocationSection testSection

generateTaskSection :: Configuration -> Template -> Either String String
generateTaskSection conf tmpl =
  generateSection conf (tmpl ^. taskSection . body)

generateSolutionSection :: Configuration -> Template -> Either String String
generateSolutionSection conf tmpl =
  generateSection conf (tmpl ^. solutionSection . body)

generatePreAllocationSection :: Configuration -> Template -> Either String String
generatePreAllocationSection conf tmpl =
  generateSection conf (tmpl ^. preAllocationSection . body)

generateTestSection :: Configuration -> Template -> Either String [(String, String)]
generateTestSection conf tmpl = concat <$> mapM f (tmpl ^. testSection . testCases)
  where
    f :: TestCase -> Either String [(String, String)]
    f tc =
      let multiParams =
            nub $
              findMultiParams conf (tc ^. code)
                ++ findMultiParams conf (tc ^. outcome)

          getParamValueTuples :: Configuration -> String -> Either String [(String, String)]
          getParamValueTuples conf parameterName = do
            multiParamValues <- maybeToEither (getMultiValue conf parameterName) shouldNotHappenErr
            return [(parameterName, v) | v <- multiParamValues]
       in do
            paramValueTuples <- mapM (getParamValueTuples conf) multiParams
            let allCombos = combinations paramValueTuples
            code <- generateMultiSection conf allCombos (tc ^. code)
            outcome <- generateMultiSection conf allCombos (tc ^. outcome)
            return $ zip (pTraceShowId code) outcome

-- | Computes all combinations of elements of an arbitrary amount of lists
--
-- Example: @[[1, 2], [3], [4, 5]]@ -> @[[1, 3, 4], [1, 3, 5], [2, 3, 4], [2, 3, 5]]@
combinations :: [[a]] -> [[a]]
combinations = foldl' f []
  where
    f :: [[a]] -> [a] -> [[a]]
    f [] l = map singleton l
    f acc l = [a ++ [b] | a <- acc, b <- l]

findMultiParams :: Configuration -> [SectionBodyComponent] -> [String]
findMultiParams conf = foldr f []
  where
    f :: SectionBodyComponent -> [String] -> [String]
    f (OutputComponent (Parameter (ParameterUsage _ name _))) acc =
      case getMultiValue conf name of
        Just _ -> acc ++ [name]
        Nothing -> acc
    f _ acc = acc

generateMultiSection :: Configuration -> [[(String, String)]] -> [SectionBodyComponent] -> Either String [String]
generateMultiSection config combinations sbcs 
  | (not . null) combinations = mapM f combinations
  | otherwise = singleton <$> f []
  where
    f :: [(String, String)] -> Either String String
    f combination = foldM (buildSBC combination) "" sbcs

    buildSBC :: [(String, String)] -> String -> SectionBodyComponent -> Either String String
    buildSBC comb acc (TextComponent s) = return (acc ++ s)
    buildSBC comb acc (OutputComponent (TextConstant s)) = return (acc ++ s)
    buildSBC comb acc (OutputComponent (Parameter (ParameterUsage _ id (Just cp)))) =
      do
        values <- evaluateMethod config id (cp ^. identifier) (cp ^. arguments)
        return $ acc ++ intercalate "\n" values
    buildSBC comb acc (OutputComponent (Parameter (ParameterUsage _ id Nothing))) =
      do
        value <- case getSingleValue config id of
          Just s -> return s
          Nothing -> case lookup id comb of
            Just ss -> return ss
            Nothing -> Left $ valueNotFoundErr id
        return $ acc ++ value

generateSection :: Configuration -> [SectionBodyComponent] -> Either String String
generateSection conf = foldM f ""
  where
    f :: String -> SectionBodyComponent -> Either String String
    f acc (TextComponent s) = return (acc ++ s)
    f acc (OutputComponent (TextConstant s)) = return (acc ++ s)
    f acc (OutputComponent (Parameter (ParameterUsage _ id (Just cp)))) =
      do
        values <- evaluateMethod conf id (cp ^. identifier) (cp ^. arguments)
        return $ acc ++ intercalate "\n" values
    f acc (OutputComponent (Parameter (ParameterUsage _ id Nothing))) =
      do
        value <- case getSingleValue conf id of
          Just s -> return s
          Nothing -> case getMultiValue conf id of
            Just ss -> return $ intercalate "\n" ss
            Nothing -> Left $ valueNotFoundErr id
        return $ acc ++ value

buildOutput :: Configuration -> String -> String -> String -> [(String, String)] -> Either String String
buildOutput conf task solution preAllocation tests =
  let xmlDoc :: Element
      xmlDoc =
        node (unqual "quiz") $
          add_attr
            (Attr (unqual "type") "coderunner")
            ( node
                (unqual "question")
                [ node
                    (unqual "name")
                    ( node (unqual "text") (CData CDataText "" Nothing) -- TODO Set name once it is available
                    ),
                  node
                    (unqual "questiontext")
                    ( Attr (unqual "format") "html",
                      node (unqual "text") (CData CDataText task Nothing)
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
                  node (unqual "answerpreload") (CData CDataVerbatim preAllocation Nothing),
                  moodleTemplate solution,
                  node (unqual "validateonsave") (CData CDataText "1" Nothing),
                  node (unqual "hoisttemplateparams") (CData CDataText "1" Nothing),
                  node (unqual "templateparamslang") (CData CDataText "twig" Nothing),
                  node (unqual "templateparamsevalpertry") (CData CDataText "0" Nothing),
                  node (unqual "templateparamsevald") (CData CDataText "{}" Nothing),
                  node (unqual "twigall") (CData CDataText "0" Nothing),
                  node (unqual "attachments") (CData CDataText "0" Nothing),
                  node (unqual "attachmentsrequired") (CData CDataText "0" Nothing),
                  node (unqual "maxfilesize") (CData CDataText "10240" Nothing),
                  node (unqual "displayfeedback") (CData CDataText "1" Nothing),
                  node (unqual "testcases") (testNodes tests)
                ]
            )
   in return $ ppTopElement xmlDoc

testNodes :: [(String, String)] -> [Element]
testNodes = map f
  where
    f :: (String, String) -> Element
    f (code, outcome) =
      node
        (unqual "testcase")
        [ node
            (unqual "testcode")
            (node (unqual "text") (CData CDataVerbatim code Nothing)),
          node
            (unqual "expected")
            (node (unqual "text") (CData CDataVerbatim outcome Nothing))
        ]

moodleTemplate :: String -> Element
moodleTemplate solution =
  node (unqual "template") (CData CDataVerbatim t Nothing)
  where
    t =
      unlines
        [ "#include <iostream>",
          "#include <fstream>",
          "#include <string>",
          "#include <cmath>",
          "#include <vector>",
          "#include <algorithm>",
          "",
          "using namespace std;",
          "#define SEPARATOR \"#<ab@17943918#@>#\"",
          "",
          "{{ STUDENT_ANSWER }}",
          ""
        ]
        ++ solution
        ++ unlines
          [ "",
            "int main() {",
            "{% for TEST in TESTCASES %}",
            "   {",
            "    {{ TEST.extra }};",
            "    {{ TEST.testcode }};",
            "   }",
            "    {% if not loop.last %}cout << SEPARATOR << endl;{% endif %}",
            "{% endfor %}",
            "    return 0;",
            "}"
          ]

--createSolutionTemplate :: String -> String -> String
--createSolutionTemplate haystack needle = strReplace needle "solution" haystack
-- Perhapts using Regex-> Text.Regex subRegex sieht da ganz gut aus.

{-

generateBody :: Mixed -> SymbolTable -> ValueTable -> Either String String
generateBody m st vt = do
  foldM folder "" m
  where
    folder :: String -> MixedPart -> Either String String
    folder str (ConstantPart c) = do return $ str ++ c
    folder str (ParameterPart pu) = do
      let maybepp = view propertyPart pu
      let id = view identifier pu
      code <-
        ( case maybepp of
            Nothing -> getSingleValue id vt
            Just pp -> evaluateWithPropertyPart id pp st vt
          )
      return $ str ++ code

evaluateWithPropertyPart ::
  Identifier ->
  PropertyPart ->
  SymbolTable ->
  ValueTable ->
  Either String String
evaluateWithPropertyPart id pp st vt = do
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
        Nothing -> Left "CHOOSE_AT_RANDOM was called with a non-integer argument" -}