-- | Compile the pipeline examples of the AIHC Manual.
--
-- Each example is one Haskell module. It is compiled in memory against the
-- aihc-prim modules that the GRIN golden tests use, and the System FC, GRIN
-- and Lir text of the module is written for the manual to include. The
-- manual build runs this command, so the pages always show what the
-- compiler of the same commit produces.
--
-- The Markdown puts each program in a fence with the language @aihc-fc@,
-- @aihc-grin@ or @aihc-lir@. The manual highlights these fences with the
-- TextMate grammars in @editors/grammars@.
module Aihc.Dev.PipelineExamples
  ( PipelineExamplesOptions (..),
    ExampleOutput (..),
    compileExample,
    runPipelineExamples,
  )
where

import Aihc.Cli.Backend (lowerTargetFor, renderLirModule)
import Aihc.Fc qualified as Fc
import Aihc.Grin qualified as Grin
import Aihc.Lir.Lower qualified as Lower
import Aihc.Native (NativeTarget (LinuxAmd64), renderNativeTarget)
import Control.Monad (forM, unless, when)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import GrinGolden (buildFcPrograms)
import Prettyprinter (defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory, removePathForcibly)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)

data PipelineExamplesOptions = PipelineExamplesOptions
  { -- | One directory for each example. The directory names give the order.
    pipelineExamplesInput :: !FilePath,
    -- | Replaced with the source and the intermediate programs of each
    -- example, and with the Markdown that the manual includes.
    pipelineExamplesOutput :: !FilePath
  }

-- | The intermediate programs of one example module.
data ExampleOutput = ExampleOutput
  { exampleCore :: !Text,
    exampleGrin :: !Text,
    exampleLir :: !Text
  }

-- | The target of the Lir text. A fixed target makes the manual the same on
-- each host that builds it.
exampleTarget :: NativeTarget
exampleTarget = LinuxAmd64

-- | The source file of an example.
exampleSourceName :: FilePath
exampleSourceName = "Example.hs"

-- | The prose above the tabs of an example.
exampleDescriptionName :: FilePath
exampleDescriptionName = "description.md"

-- | Compile one module the way @aihc install@ compiles it at the default
-- optimization level, which runs no System FC pass.
compileExample :: Text -> Either String ExampleOutput
compileExample source = do
  programs <- buildFcPrograms [] [source]
  program <- case programs of
    [single] -> Right single
    _ -> Left ("expected one System FC program, got " <> show (length programs))
  grin <- either (Left . ("GRIN generation failed: " <>)) Right (Grin.lowerProgram program)
  let grinErrors = Grin.lintProgram grin
  unless (null grinErrors) (Left ("GRIN lint failed: " <> show grinErrors))
  cps <- either (Left . ("CPS-GRIN generation failed: " <>) . show) Right (Grin.toCpsGrin grin)
  let gc = Grin.lowerGc cps
      gcErrors = Grin.lintGcProgram gc
  unless (null gcErrors) (Left ("GC-GRIN lint failed: " <> show gcErrors))
  lir <- either (Left . ("Lir generation failed: " <>) . show) Right (Lower.lowerModule (lowerTargetFor exampleTarget) Lower.defaultModuleSettings gc)
  pure
    ExampleOutput
      { exampleCore = Fc.renderProgram program,
        exampleGrin = renderStrict (layoutPretty defaultLayoutOptions (Grin.prettyProgram grin)),
        exampleLir = renderLirModule lir
      }

runPipelineExamples :: PipelineExamplesOptions -> IO ()
runPipelineExamples options = do
  let input = pipelineExamplesInput options
      output = pipelineExamplesOutput options
  names <- sort <$> (listDirectory input >>= filterDirectories input)
  when (null names) (failWith ("no examples in " <> input))
  removePathForcibly output
  createDirectoryIfMissing True output
  sections <- forM names $ \name -> do
    let directory = input </> name
    source <- readRequired (directory </> exampleSourceName)
    description <- readRequired (directory </> exampleDescriptionName)
    hPutStrLn stderr ("Compiling pipeline example " <> name)
    compiled <- either (failWith . ((name <> ": ") <>)) pure (compileExample source)
    let files =
          [ (exampleSourceName, source),
            ("core", exampleCore compiled),
            ("grin", exampleGrin compiled),
            ("lir", exampleLir compiled)
          ]
    createDirectoryIfMissing True (output </> name)
    mapM_ (\(file, text) -> TIO.writeFile (output </> name </> file) (withFinalNewline text)) files
    either (failWith . ((name <> ": ") <>)) pure (exampleSection description compiled source)
  TIO.writeFile (output </> "examples.md") (T.intercalate "\n" sections)
  TIO.writeFile (output </> "settings.md") settingsText
  hPutStrLn stderr ("Wrote " <> show (length names) <> " pipeline examples to " <> output)
  where
    filterDirectories input = fmap concat . mapM (\name -> do isDirectory <- doesDirectoryExist (input </> name); pure [name | isDirectory])
    readRequired path = do
      exists <- doesFileExist path
      unless exists (failWith ("missing " <> path))
      TIO.readFile path

-- | The sentence that each page gives about how the programs were made.
settingsText :: Text
settingsText =
  "The compiler made these programs at the default optimization level. The Lir is for the target `"
    <> T.pack (renderNativeTarget exampleTarget)
    <> "`.\n"

-- | The description of an example, then one tab for each language. Each
-- example is its own tab set, so a tab choice changes only that example.
exampleSection :: Text -> ExampleOutput -> Text -> Either String Text
exampleSection description compiled source = do
  tabs <-
    sequence
      [ tab "Haskell" "haskell" source,
        tab "System FC" "aihc-fc" (exampleCore compiled),
        tab "GRIN" "aihc-grin" (exampleGrin compiled),
        tab "Lir" "aihc-lir" (exampleLir compiled)
      ]
  pure (T.stripEnd description <> "\n\n" <> T.concat tabs)
  where
    tab title language text = do
      -- A fence line in the program would end its code block too early.
      when (any isFence (T.lines text)) (Left (T.unpack title <> " has a Markdown fence line"))
      pure $
        "=== \""
          <> title
          <> "\"\n\n    ```"
          <> language
          <> "\n"
          <> T.unlines (map indent (T.lines (T.stripEnd text)))
          <> "    ```\n\n"
    indent line
      | T.null line = line
      | otherwise = "    " <> line
    isFence line = any (`T.isPrefixOf` T.stripStart line) ["```", "~~~"]

withFinalNewline :: Text -> Text
withFinalNewline text
  | "\n" `T.isSuffixOf` text = text
  | otherwise = text <> "\n"

failWith :: String -> IO a
failWith message = do
  hPutStrLn stderr ("error: " <> message)
  exitFailure
