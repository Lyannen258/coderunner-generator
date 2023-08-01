module Generator.Moodle.CPPTracing.Generator where

import Codec.Picture
import Control.Concurrent.Async
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
import HTMLEntities.Text as Encoder
import Lens.Micro ((^.))
import System.Directory
import System.Process
import System.Random
import Text.XML.Light
import System.Posix.User

getTmpPath :: IO String
getTmpPath = do
  i <- fmap toInteger getRealUserID
  e <- doesDirectoryExist $ tmpfspath i
  if e
    then do
      return $ tmpfspath i ++ "/moodle-cpp-tracing"
    else do 
      return "/tmp/moodle-cpp-tracing"
  where
    tmpfspath i = "/run/user/" ++ show i

valueNotFoundErr :: ParameterName -> String
valueNotFoundErr p = "No value found for usage of parameter '" ++ name p ++ "'"

shouldNotHappenErr :: String
shouldNotHappenErr = "This error should not happen. Please ask the software provider."

generate :: [Configuration] -> Template -> IO (Either String String)
generate configs tem = do
  tempDirectoryHandling
  elements <- mapConcurrently f configs
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

tempDirectoryHandling :: IO ()
tempDirectoryHandling = do
  tmpPath <- getTmpPath
  createDirectoryIfMissing True tmpPath
  files <- listDirectory tmpPath
  mapM_ (\f -> removeFile $ tmpPath ++ "/" ++ f) files

generateCodeSection :: Configuration -> Template -> Either String String
generateCodeSection conf tmpl =
  generateSection conf (tmpl.codeSection.body)

codeImageBase64 :: String -> IO (Text, Int)
codeImageBase64 code = do
  tmpPath <- getTmpPath
  r <- randomIO :: IO Integer
  liftIO $ writeFile (filePathCode tmpPath r) code
  liftIO $ callCommand $ "silicon --no-window-controls --no-round-corner --background \"#ffffff\" --theme \"1337\" --pad-horiz 0 --pad-vert 0 --output " ++ filePathImg tmpPath r ++ " " ++ filePathCode tmpPath r
  eitherImg <- readImage (filePathImg tmpPath r)
  width <- case eitherImg of
    Right img -> return $ dynamicMap imageWidth img
    Left str -> ioError . userError $ "The call to silicon did not produce a valid png image."
  x <- liftIO $ BS.readFile $ filePathImg tmpPath r
  return (encodeBase64 x, width)
  where
    filePathCode path rand = path ++ "/moodle-cpp-tracing-code" ++ show rand ++ ".cpp"
    filePathImg path rand = path ++ "/img" ++ show rand ++ ".png"

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

enhanceWithCode :: String -> String -> String
enhanceWithCode feedback code =
  feedback
    ++ "<br><br><p>Code for copying:</p><code><pre>"
    ++ (unpack . Encoder.text $ pack code)
    ++ "</pre></code>"

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
                  node (unqual "generalfeedback") (Attr (unqual "format") "html", node (unqual "text") (CData CDataVerbatim (enhanceWithCode feedback code) Nothing)),
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
  (b64, width) <- codeImageBase64 code
  return $
    "<p>"
      ++ case tt of
        Output -> "What is the output of the following program?"
        Compile -> "Does the following program compile?"
      ++ "</p><img style=\"max-width:"
      ++ show (fromIntegral width * 0.65)
      ++ "px;\" src=\"data:image/png;base64,"
      ++ unpack b64
      ++ "\">"

buildSolution :: String -> TraceType -> IO (Either String String)
buildSolution code tt = do
  tmpPath <- getTmpPath
  r <- randomIO :: IO Integer
  writeFile (filePathCpp tmpPath r) code
  (exit, stdout, stderr) <- readProcessWithExitCode "g++" ["--std=c++20", "-o", filePathEx tmpPath r, filePathCpp tmpPath r] ""
  case (exit, tt) of
    (ExitSuccess, Compile) -> return $ Right "true"
    (ExitFailure _, Compile) -> return $ Right "false"
    (ExitSuccess, Output) -> runProgram tmpPath r
    (ExitFailure _, Output) -> return $ compileErr code stderr
  where
    runProgram path r = do
      (exit, stdout, stderr) <- readProcessWithExitCode (filePathEx path r) [] ""
      case exit of
        ExitSuccess -> return $ Right stdout
        ExitFailure _ -> return $ runtimeErr code stderr 
    filePathCpp :: String -> Integer -> String
    filePathCpp path rand = filePathEx path rand ++ ".cpp"
    filePathEx :: String -> Integer -> String
    filePathEx path rand = path ++ "/cpp-tracing-program" ++ show rand

compileErr :: String -> String -> Either String b
compileErr code compError =
  Left $
    "The tasks tracing type is set to 'output', but the program does not compile"
      ++ "\nCode:\n"
      ++ code
      ++ "\n\nCompiler Output:\n"
      ++ compError

runtimeErr :: String -> String -> Either String b
runtimeErr code err = 
    Left $
      "The tasks tracing type is set to 'output', but the program exits with failure"
        ++ "\nCode:\n"
        ++ code
        ++ "\n\nError Output:\n"
        ++ err

determineQuestionType :: TraceType -> String
determineQuestionType Compile = "truefalse"
determineQuestionType Output = "shortanswer"

stringNot :: String -> String
stringNot "true" = "false"
stringNot "false" = "true"