module Generator.Moodle.CPPTracing.Generator (generate) where

import Generator.Moodle.CPPTracing.AbstractSyntaxTree
import Generator.Helper
import Generator.Configuration 
import Control.Monad (foldM)
import Data.List (foldl', intercalate, nub)
import Lens.Micro ((^.))
import Text.XML.Light
import Generator.Atoms (ParameterName (..))

valueNotFoundErr :: ParameterName -> String
valueNotFoundErr p = "No value found for usage of parameter '" ++ name p ++ "'"

shouldNotHappenErr :: String
shouldNotHappenErr = "This error should not happen. Please ask the software provider."

generate :: [Configuration] -> Template -> Either String String
generate configs tem = do
  elements <- mapM f configs
  let doc = node (unqual "quiz") elements
  return $ ppTopElement doc
  where
    f :: Configuration -> Either String Element
    f config = generateConfiguration config tem

generateConfiguration :: Configuration -> Template -> Either String Element
generateConfiguration conf tmpl = do
  ts <- generateTaskSection conf tmpl
  ss <- generateSolutionSection conf tmpl
  pas <- generatePreAllocationSection conf tmpl
  testS <- generateTestSection conf tmpl
  buildOutput conf ts ss pas testS tmpl

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

          getParamValueTuples :: ParameterName -> Either String [(ParameterName, String)]
          getParamValueTuples parameterName = do
            multiParamValues <- maybeToError (getMultiValue conf parameterName) shouldNotHappenErr
            return [(parameterName, v) | v <- multiParamValues]
       in do
            paramValueTuples <- mapM getParamValueTuples multiParams
            let allCombos = combinations paramValueTuples
            c <- generateMultiSection conf allCombos (tc ^. code)
            o <- generateMultiSection conf allCombos (tc ^. outcome)
            return $ zip c o

-- | Computes all combinations of elements of an arbitrary amount of lists
--
-- Example: @[[1, 2], [3], [4, 5]]@ -> @[[1, 3, 4], [1, 3, 5], [2, 3, 4], [2, 3, 5]]@
combinations :: [[a]] -> [[a]]
combinations = foldl' f []
  where
    f :: [[a]] -> [a] -> [[a]]
    f [] l = map singleton l
    f acc l = [a ++ [b] | a <- acc, b <- l]

findMultiParams :: Configuration -> [SectionBodyComponent] -> [ParameterName]
findMultiParams conf = foldr f []
  where
    f :: SectionBodyComponent -> [ParameterName] -> [ParameterName]
    f (OutputComponent (Parameter (ParameterUsage _ n _))) acc =
      case getMultiValue conf n of
        Just _ -> acc ++ [n]
        Nothing -> acc
    f _ acc = acc

generateMultiSection :: Configuration -> [[(ParameterName, String)]] -> [SectionBodyComponent] -> Either String [String]
generateMultiSection config combs sbcs
  | (not . null) combs = mapM f combs
  | otherwise = singleton <$> f []
  where
    f :: [(ParameterName, String)] -> Either String String
    f comb = foldM (buildSBC comb) "" sbcs

    buildSBC :: [(ParameterName, String)] -> String -> SectionBodyComponent -> Either String String
    buildSBC _ acc (TextComponent s) = return (acc ++ s)
    buildSBC _ acc (OutputComponent (TextConstant s)) = return (acc ++ s)
    buildSBC _ acc (OutputComponent (Parameter (ParameterUsage _ name (Just cp)))) =
      do
        vs <- evaluateMethod config name (cp ^. identifier) (cp ^. arguments)
        return $ acc ++ intercalate "\n" vs
    buildSBC comb acc (OutputComponent (Parameter (ParameterUsage _ name Nothing))) =
      do
        value <- case getSingleValue config name of
          Just s -> return s
          Nothing -> case lookup name comb of
            Just ss -> return ss
            Nothing -> Left $ valueNotFoundErr name
        return $ acc ++ value

generateSection :: Configuration -> [SectionBodyComponent] -> Either String String
generateSection conf = foldM f ""
  where
    f :: String -> SectionBodyComponent -> Either String String
    f acc (TextComponent s) = return (acc ++ s)
    f acc (OutputComponent (TextConstant s)) = return (acc ++ s)
    f acc (OutputComponent (Parameter (ParameterUsage _ name (Just cp)))) =
      do
        vs <- evaluateMethod conf name (cp ^. identifier) (cp ^. arguments)
        return $ acc ++ intercalate "\n" vs
    f acc (OutputComponent (Parameter (ParameterUsage _ name Nothing))) =
      do
        value <- case getSingleValue conf name of
          Just s -> return s
          Nothing -> case getMultiValue conf name of
            Just ss -> return $ intercalate "\n" ss
            Nothing -> Left $ valueNotFoundErr name
        return $ acc ++ value

buildOutput :: Configuration -> String -> String -> String -> [(String, String)] -> Template -> Either String Element
buildOutput _ task solution preAllocation tests t =
  return $
    add_attr
      (Attr (unqual "type") "coderunner")
      ( node
          (unqual "question")
          [ node
              (unqual "name")
              ( node (unqual "text") (CData CDataText (t ^. nameSection . body) Nothing)
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

testNodes :: [(String, String)] -> [Element]
testNodes = map f
  where
    f :: (String, String) -> Element
    f (c, o) =
      node
        (unqual "testcase")
        [ node
            (unqual "testcode")
            (node (unqual "text") (CData CDataVerbatim c Nothing)),
          node
            (unqual "expected")
            (node (unqual "text") (CData CDataVerbatim o Nothing))
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
          "",
          "namespace Solution {"
        ]
        ++ solution
        ++ unlines
          [ "}",
            "",
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