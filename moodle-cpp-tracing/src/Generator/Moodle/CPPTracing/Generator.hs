module Generator.Moodle.CPPTracing.Generator where

import Control.Monad (foldM, join)
import Control.Monad.Except (MonadError, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString qualified as BS (readFile)
import Data.ByteString.Base64 (encodeBase64)
import Data.List (foldl', intercalate, nub)
import Data.Text (Text, pack, unpack)
import GHC.IO.Exception
import Generator.Atoms (ParameterName (..))
import Generator.Configuration
import Generator.Helper
import Generator.Moodle.CPPTracing.AbstractSyntaxTree
import Lens.Micro ((^.))
import System.Process
import Text.XML.Light

valueNotFoundErr :: ParameterName -> String
valueNotFoundErr p = "No value found for usage of parameter '" ++ name p ++ "'"

shouldNotHappenErr :: String
shouldNotHappenErr = "This error should not happen. Please ask the software provider."

generate :: [Configuration] -> Template -> IO (Either String String)
generate configs tem = do
  elements <- mapM f configs
  case sequence elements of
    Right es ->
      let doc = node (unqual "quiz") es
       in return . return $ ppTopElement doc
    Left err -> return $ Left err
  where
    f :: Configuration -> IO (Either String Element)
    f config = generateConfiguration config tem

generateConfiguration :: Configuration -> Template -> IO (Either String Element)
generateConfiguration conf tmpl =
  case sections of
    Right (cs, fs) -> buildOutput conf cs fs tmpl
    Left str -> return $ Left str
  where
    sections = do
      cs <- generateCodeSection conf tmpl
      fs <- generateFeedbackSection conf tmpl
      return (cs, fs)

generateCodeSection :: Configuration -> Template -> Either String String
generateCodeSection conf tmpl =
  generateSection conf (tmpl.codeSection.body)

codeImageBase64 :: String -> IO Text
codeImageBase64 code = do
  liftIO $ writeFile "/tmp/moodle-cpp-tracing-code.cpp" code
  liftIO $ callCommand "silicon --no-window-controls --no-round-corner --background \"#ffffff\" --theme \"1337\" --pad-horiz 0 --pad-vert 0 --output /tmp/img.png /tmp/moodle-cpp-tracing-code.cpp"
  x <- liftIO $ BS.readFile "/tmp/img.png"
  return $ encodeBase64 x

generateFeedbackSection :: Configuration -> Template -> Either String String
generateFeedbackSection conf tmpl =
  generateSection conf (tmpl.feedbackSection.body)

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
    f (OutputComponent (Parameter (ParameterUsage n _))) acc =
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
    buildSBC _ acc (OutputComponent (Parameter (ParameterUsage name (Just cp)))) =
      do
        vs <- evaluateMethod config name (cp.identifier) (cp.arguments)
        return $ acc ++ intercalate "\n" vs
    buildSBC comb acc (OutputComponent (Parameter (ParameterUsage name Nothing))) =
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
    f acc (OutputComponent (Parameter (ParameterUsage name (Just cp)))) =
      do
        vs <- evaluateMethod conf name (cp.identifier) (cp.arguments)
        return $ acc ++ intercalate "\n" vs
    f acc (OutputComponent (Parameter (ParameterUsage name Nothing))) =
      do
        value <- case getSingleValue conf name of
          Just s -> return s
          Nothing -> case getMultiValue conf name of
            Just ss -> return $ intercalate "\n" ss
            Nothing -> Left $ valueNotFoundErr name
        return $ acc ++ value

buildOutput ::
  Configuration ->
  String ->
  String ->
  Template ->
  IO (Either String Element)
buildOutput _ code feedback t = do
  questionText <- buildText code t.traceType
  sol <- buildSolution code t.traceType
  case sol of
    Left err -> return $ Left err
    Right sol ->
      return . Right $
        add_attr
          (Attr (unqual "type") (determineQuestionType t.traceType))
          ( node
              (unqual "question")
              ( [ node
                    (unqual "name")
                    ( node (unqual "text") (CData CDataText (t.titleSection.body) Nothing)
                    ),
                  node
                    (unqual "questiontext")
                    ( Attr (unqual "format") "html",
                      node (unqual "text") (CData CDataVerbatim questionText Nothing)
                    ),
                  node (unqual "generalfeedback") (Attr (unqual "format") "html", node (unqual "text") (CData CDataVerbatim feedback Nothing)),
                  node (unqual "defaultgrade") (CData CDataText "1" Nothing),
                  node (unqual "penalty") (CData CDataText "0" Nothing),
                  node (unqual "hidden") (CData CDataText "0" Nothing),
                  node (unqual "idnumber") (CData CDataText "" Nothing),
                  node (unqual "usecase") (CData CDataText "0" Nothing),
                  node
                    (unqual "answer")
                    ( [ Attr (unqual "fraction") "100",
                        Attr (unqual "format") "moodle_auto_format"
                      ],
                      [ node (unqual "text") (CData CDataText sol Nothing),
                        node (unqual "feedback") (Attr (unqual "format") "html", node (unqual "text") (CData CDataText "" Nothing))
                      ]
                    )
                ]
                  ++ case t.traceType of
                    Output -> []
                    Compile ->
                      [ node
                          (unqual "answer")
                          ( [ Attr (unqual "fraction") "0",
                              Attr (unqual "format") "moodle_auto_format"
                            ],
                            [ node (unqual "text") (CData CDataText (stringNot sol) Nothing),
                              node (unqual "feedback") (Attr (unqual "format") "html", node (unqual "text") (CData CDataText "" Nothing))
                            ]
                          )
                      ]
              )
          )

buildText :: String -> TraceType -> IO String
buildText code tt = do
  b64 <- codeImageBase64 code
  return $
    "<p>"
      ++ case tt of
        Output -> "What is the output of the following program?"
        Compile -> "Does the following program compile?"
      ++ "</p><img style=\"max-width:700px;\" src=\"data:image/png;base64,"
      ++ unpack b64
      ++ "\">"

buildSolution :: String -> TraceType -> IO (Either String String)
buildSolution code tt = do
  writeFile filePathCpp code
  (exit, stdout, stdin) <- readProcessWithExitCode "g++" ["--std=c++20", "-o", filePathEx, filePathCpp] ""
  case (exit, tt) of
    (ExitSuccess, Compile) -> return $ Right "true"
    (ExitFailure _, Compile) -> return $ Right "false"
    (ExitSuccess, Output) -> runProgram
    (ExitFailure _, Output) -> return $ Left "The tasks tracing type is set to 'output', but the program does not compile"
  where
    runProgram = do
      (exit, stdout, stdin) <- readProcessWithExitCode filePathEx [] ""
      case exit of
        ExitSuccess -> return $ Right stdout
        ExitFailure _ -> return $ Left "The tasks tracing type is set to 'output', but the program exits with failure"
    filePathCpp = "/tmp/cpp-tracing-program.cpp"
    filePathEx = "/tmp/cpp-tracing-program"

determineQuestionType :: TraceType -> String
determineQuestionType Compile = "truefalse"
determineQuestionType Output = "shortanswer"

stringNot :: String -> String
stringNot "true" = "false"
stringNot "false" = "true"