{-# LANGUAGE OverloadedStrings #-}

module Aihc.Dev.Golden.Update
  ( Options (..),
    optionsParser,
    run,
    replaceYamlValueAt,
  )
where

import Aihc.Cpp (Result (..))
import Aihc.Dev.Parser.Run qualified as ParserRun
import Aihc.Fc (DesugarResult (..), FcProgram (..), desugarModuleWithBindings, evalProgramBinding, renderProgram, renderRawValue)
import Aihc.Fmt (defaultFormatOptions, formatErrorMessage, formatExtensions, formatText)
import Aihc.Parser
  ( ParseResult (..),
    ParserConfig (..),
    defaultConfig,
    formatParseErrors,
    parseExpr,
    parseModule,
    parsePattern,
  )
import Aihc.Parser.Shorthand (Shorthand (..))
import Aihc.Parser.Syntax
  ( Decl (..),
    Expr,
    Extension,
    ExtensionSetting,
    ImportDecl (..),
    LanguageEdition (Haskell2010Edition),
    Match (..),
    MatchHeadForm (..),
    Module (..),
    NameType (..),
    Pragma (..),
    Rhs (..),
    ValueDecl (..),
    editionFromExtensionSettings,
    effectiveExtensions,
    mkUnqualifiedName,
    parseExtensionName,
    parseExtensionSettingName,
  )
import Aihc.Parser.Token (LexToken (..), LexTokenKind (..), lexModuleTokensWithExtensions, lexTokensWithExtensions)
import Aihc.Resolve (ResolveResult (..), resolve, resolveWithDeps)
import Aihc.Tc (TcBindingResult, TcModuleResult (..), tcmBindings, typecheck, typecheckModulesWithEnv)
import Control.Exception (IOException, bracket, catch)
import Control.Monad (foldM, forM, forM_, unless)
import CppSupport (cppEnabledInSource, preprocessForParserWithoutIncludes)
import Data.Aeson (Value (..), toJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (parseEither, withArray)
import Data.Bifunctor (first)
import Data.ByteString qualified as BS
import Data.Char (isSpace, toLower)
import Data.List (dropWhileEnd, sort, sortOn)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Scientific (toBoundedInteger)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import Data.Yaml qualified as Y
import GhcOracle (oracleModuleAstFingerprint)
import LexerGolden qualified as LG
import Options.Applicative hiding (value)
import Options.Applicative qualified as OA
import ParserErrorGolden qualified as PEG
import ParserGolden qualified as PG
import ResolverGolden qualified as RG
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable, getTemporaryDirectory, listDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (joinPath, makeRelative, normalise, takeDirectory, takeExtension, (</>))
import System.IO (Handle, hClose, openTempFile)
import System.Process (readProcessWithExitCode)
import TcAnnotatedRender (renderAnnotatedTcResults)

data Options = Options
  { optRoot :: !FilePath,
    optDryRun :: !Bool,
    optExternalFormatters :: !Bool
  }
  deriving (Eq, Show)

data Summary = Summary
  { summaryScanned :: !Int,
    summaryUpdated :: !Int,
    summarySkipped :: ![(FilePath, String)]
  }
  deriving (Eq, Show)

emptySummary :: Summary
emptySummary = Summary 0 0 []

data FixtureUpdate
  = FixtureUnchanged
  | FixtureUpdated Value
  | FixtureSkipped String

optionsParser :: Parser Options
optionsParser =
  Options
    <$> strOption
      ( long "root"
          <> metavar "DIR"
          <> OA.value "."
          <> showDefault
          <> help "Repository root containing components/, tooling/, and bin/"
      )
    <*> switch
      ( long "dry-run"
          <> help "Report fixtures that would change without writing files"
      )
    <*> switch
      ( long "external-formatters"
          <> help "Also refresh formatter goldens for installed external tools"
      )

run :: Options -> IO ()
run opts = do
  summaries <-
    sequence
      [ updateParserGoldens opts,
        updateLexerGoldens opts,
        updateParserErrorGoldens opts,
        updateResolverGoldens opts,
        updateTcAnnotatedGoldens opts,
        updateFcGoldens opts,
        updateFcEvalGoldens opts,
        updateFormatterGoldens opts,
        updateParserCliGoldens opts
      ]
  let total = foldl mergeSummary emptySummary summaries
  putStrLn $
    "golden fixtures: scanned "
      <> show (summaryScanned total)
      <> ", updated "
      <> show (summaryUpdated total)
      <> skippedSuffix total
  unless (null (summarySkipped total)) $
    forM_ (summarySkipped total) $ \(path, reason) ->
      putStrLn ("skipped " <> path <> ": " <> reason)
  where
    skippedSuffix summary =
      case summarySkipped summary of
        [] -> ""
        skipped -> ", skipped " <> show (length skipped)

mergeSummary :: Summary -> Summary -> Summary
mergeSummary left right =
  Summary
    { summaryScanned = summaryScanned left + summaryScanned right,
      summaryUpdated = summaryUpdated left + summaryUpdated right,
      summarySkipped = summarySkipped left <> summarySkipped right
    }

updateParserGoldens :: Options -> IO Summary
updateParserGoldens opts =
  updateYamlTree opts (optRoot opts </> "components/aihc-parser/test/Test/Fixtures/golden") $ \path value -> do
    let rel = makeRelative (optRoot opts </> "components/aihc-parser/test/Test/Fixtures/golden") path
    case parserKindFromPath rel of
      Nothing -> pure (FixtureSkipped "not an expr/module/pattern parser golden")
      Just kind ->
        case PG.parseParserCaseText kind rel (decodeFixtureValue value) of
          Left err -> pure (FixtureSkipped err)
          Right meta ->
            case parserActualAst meta of
              Left err -> pure (skipForStatus (textField "status" value) err)
              Right actual
                | trim actual == trim (PG.caseAst meta) -> pure FixtureUnchanged
                | not (shouldUpdateOutput value) -> pure FixtureUnchanged
                | otherwise -> pure (setValueAt ["ast"] (String (T.pack actual)) value)

parserKindFromPath :: FilePath -> Maybe PG.CaseKind
parserKindFromPath path =
  case takeWhile (/= '/') path of
    "expr" -> Just PG.CaseExpr
    "import" -> Just PG.CaseModule
    "module" -> Just PG.CaseModule
    "pattern" -> Just PG.CasePattern
    "pragma" -> Just PG.CaseModule
    _ -> Nothing

parserActualAst :: PG.ParserCase -> Either String String
parserActualAst meta =
  case PG.caseKind meta of
    PG.CaseExpr ->
      case parseExpr (parserConfig (PG.casePath meta) (PG.caseExtensions meta)) (PG.caseInput meta) of
        ParseOk ast -> Right (show (shorthand ast))
        ParseErr err -> Left (formatParseErrors (PG.casePath meta) (Just (PG.caseInput meta)) err)
    PG.CaseModule ->
      let (errs, ast) = parseModule (parserConfig (PG.casePath meta) (PG.caseExtensions meta)) (PG.caseInput meta)
       in if null errs
            then Right (show (shorthand ast))
            else Left (formatParseErrors (PG.casePath meta) (Just (PG.caseInput meta)) errs)
    PG.CasePattern ->
      case parsePattern (parserConfig (PG.casePath meta) (PG.caseExtensions meta)) (PG.caseInput meta) of
        ParseOk ast -> Right (show (shorthand ast))
        ParseErr err -> Left (formatParseErrors (PG.casePath meta) (Just (PG.caseInput meta)) err)

parserConfig :: FilePath -> [ExtensionSetting] -> ParserConfig
parserConfig sourceName exts =
  let edition = fromMaybe Haskell2010Edition (editionFromExtensionSettings exts)
   in defaultConfig
        { parserSourceName = sourceName,
          parserExtensions = effectiveExtensions edition exts
        }

updateLexerGoldens :: Options -> IO Summary
updateLexerGoldens opts =
  updateYamlTree opts (optRoot opts </> "components/aihc-parser/test/Test/Fixtures/lexer") $ \path value -> do
    let root = optRoot opts </> "components/aihc-parser/test/Test/Fixtures/lexer"
        rel = makeRelative root path
    case LG.parseLexerCaseText rel (decodeFixtureValue value) of
      Left err -> pure (FixtureSkipped err)
      Right meta ->
        let actual = actualLexerTokens meta
            expected = map normalizeTokenKind (LG.caseTokens meta)
         in if actual == expected || not (shouldUpdateOutput value)
              then pure FixtureUnchanged
              else pure (setValueAt ["tokens"] (toJSON (map renderTokenKind actual)) value)

actualLexerTokens :: LG.LexerCase -> [LexTokenKind]
actualLexerTokens meta =
  let tokens =
        if LG.caseIsModule meta
          then lexModuleTokensWithExtensions (LG.caseExtensions meta) (LG.caseInput meta)
          else lexTokensWithExtensions (LG.caseExtensions meta) (LG.caseInput meta)
   in map (normalizeTokenKind . lexTokenKind) tokens

normalizeTokenKind :: LexTokenKind -> LexTokenKind
normalizeTokenKind (TkPragma pragma) = TkPragma (pragma {pragmaRawText = ""})
normalizeTokenKind tok = tok

renderTokenKind :: LexTokenKind -> Text
renderTokenKind (TkFloat value floatType) =
  "TkFloat " <> T.pack (renderRational value) <> " " <> T.pack (show floatType)
renderTokenKind tok = T.pack (show tok)

renderRational :: Rational -> String
renderRational value =
  let sign = if value < 0 then "-" else ""
      n = abs (numerator value)
      d = denominator value
   in case decimalScale d of
        Nothing -> show value
        Just scale ->
          let scaled = n * (10 ^ scale) `div` d
              raw = show scaled
              padded = replicate (scale + 1 - length raw) '0' <> raw
              splitAt' = length padded - scale
              (whole, frac0) = splitAt splitAt' padded
              frac = dropWhileEnd (== '0') frac0
           in sign <> if null frac then whole else whole <> "." <> frac

decimalScale :: Integer -> Maybe Int
decimalScale = go 0
  where
    go scale 1 = Just scale
    go scale n
      | even n = go (scale + 1) (n `div` 2)
      | n `mod` 5 == 0 = go (scale + 1) (n `div` 5)
      | otherwise = Nothing

updateParserErrorGoldens :: Options -> IO Summary
updateParserErrorGoldens opts =
  updateYamlTree opts (optRoot opts </> "components/aihc-parser/test/Test/Fixtures/error-messages") $ \path value -> do
    let root = optRoot opts </> "components/aihc-parser/test/Test/Fixtures/error-messages"
        rel = makeRelative root path
    case PEG.parseErrorMessageCaseText rel (decodeFixtureValue value) of
      Left err -> pure (FixtureSkipped err)
      Right meta ->
        case parserErrorActual meta of
          Left err -> pure (FixtureSkipped err)
          Right (actualGhc, actualAihc) ->
            pure $
              applyFieldUpdates
                [ (["ghc"], String actualGhc, actualGhc /= PEG.caseExpectedGhc meta),
                  (["aihc"], String actualAihc, actualAihc /= PEG.caseExpectedAihc meta)
                ]
                value

parserErrorActual :: PEG.ErrorMessageCase -> Either String (Text, Text)
parserErrorActual meta = do
  ghcText <-
    case oracleModuleAstFingerprint "test.hs" Haskell2010Edition [] (PEG.caseSource meta) of
      Right {} -> Left "GHC accepted the source"
      Left err -> Right (normalizeErrorText err)
  aihcText <- renderAihcErrorMessage (PEG.caseSource meta)
  Right (ghcText, aihcText)

renderAihcErrorMessage :: Text -> Either String Text
renderAihcErrorMessage source =
  let preprocessed =
        if cppEnabledInSource source
          then resultOutput (preprocessForParserWithoutIncludes "test.hs" [] source)
          else source
      (errs, _) = parseModule defaultConfig {parserSourceName = "test.hs"} preprocessed
   in case errs of
        [] -> Left "AIHC accepted the source"
        _ -> Right (normalizeErrorText (T.pack (formatParseErrors "test.hs" (Just preprocessed) errs)))

normalizeErrorText :: Text -> Text
normalizeErrorText =
  T.intercalate "\n" . map T.stripEnd . T.lines . T.dropWhileEnd (`elem` ['\n', '\r']) . T.replace "\r\n" "\n"

updateResolverGoldens :: Options -> IO Summary
updateResolverGoldens opts =
  updateYamlTree opts (optRoot opts </> "components/aihc-resolve/test/Test/Fixtures/golden") $ \_ value ->
    if not (shouldUpdateOutput value)
      then pure FixtureUnchanged
      else case resolverActual value of
        Left err -> pure (FixtureSkipped err)
        Right actualAnnotated -> do
          let annotated = parseTextArrayField "annotated" value
          pure $
            applyFieldUpdates
              [(["annotated"], toJSON (map T.pack actualAnnotated), either (const True) ((/= map trim actualAnnotated) . map (trim . T.unpack)) annotated)]
              value

resolverActual :: Value -> Either String [String]
resolverActual value = do
  exts <- parseExtensions value
  modules <- parseTextArrayField "modules" value
  parsed <- traverse (parseModuleText exts) modules
  let result = resolve parsed
  Right (RG.renderAnnotatedResolveResult modules result)

updateTcAnnotatedGoldens :: Options -> IO Summary
updateTcAnnotatedGoldens opts =
  updateYamlTree opts (optRoot opts </> "components/aihc-tc/test/Test/Fixtures/annotated") $ \_ value ->
    if not (shouldUpdateOutput value)
      then pure FixtureUnchanged
      else case tcAnnotatedActual value of
        Left err -> pure (skipForStatus (textField "status" value) err)
        Right actual -> do
          let annotated = parseTextArrayField "annotated" value
          pure $
            if either (const True) ((/= map trim actual) . map (trim . T.unpack)) annotated
              then setValueAt ["annotated"] (toJSON (map T.pack actual)) value
              else FixtureUnchanged

tcAnnotatedActual :: Value -> Either String [String]
tcAnnotatedActual value = do
  exts <- parseExtensions value
  modules <- parseTextArrayField "modules" value
  parsed <- traverse (parseModuleText exts) modules
  case resolve parsed of
    ResolveResult {resolvedModules, resolveErrors = []} ->
      let results = typecheck resolvedModules
       in if all tcmSuccess results
            then Right (map trim (renderAnnotatedTcResults modules results))
            else Left (unlines [show d | r <- results, d <- tcmDiagnostics r])
    ResolveResult {resolveErrors} -> Left ("resolve error: " <> show resolveErrors)

updateFcGoldens :: Options -> IO Summary
updateFcGoldens opts =
  updateYamlTree opts (optRoot opts </> "components/aihc-fc/test/Test/Fixtures/golden") $ \_ value ->
    if not (shouldUpdateOutput value)
      then pure FixtureUnchanged
      else case fcActual value of
        Left err -> pure (skipForStatus (textField "status" value) err)
        Right actual -> updateExpectedText value actual

fcActual :: Value -> Either String String
fcActual value = do
  exts <- parseExtensions value
  modules <- parseTextArrayField "modules" value
  parsed <- traverse (parseModuleText exts) modules
  case resolve parsed of
    ResolveResult {resolvedModules, resolveErrors = []} ->
      let tcResults = typecheck resolvedModules
       in if all tcmSuccess tcResults
            then
              let allBindings = moduleGroupBindings tcResults
                  dsResults = zipWith (desugarModuleWithBindings allBindings) tcResults resolvedModules
               in if all dsSuccess dsResults
                    then Right (trim (unlines (map (renderProgram . dsProgram) dsResults)))
                    else Left (unlines [err | r <- dsResults, err <- dsErrors r])
            else Left ("typecheck error: " <> renderTcErrors tcResults)
    ResolveResult {resolveErrors} -> Left ("resolve error: " <> show resolveErrors)

updateFcEvalGoldens :: Options -> IO Summary
updateFcEvalGoldens opts =
  updateYamlTree opts (optRoot opts </> "components/aihc-fc/test/Test/Fixtures/eval") $ \_ value ->
    if not (shouldUpdateOutput value)
      then pure FixtureUnchanged
      else do
        actualResult <- fcEvalActual opts value
        case actualResult of
          Left err -> pure (skipForStatus (textField "status" value) err)
          Right actual -> do
            let expected = T.unpack <$> textField "output" value
            pure $
              if either (const True) ((/= trim actual) . trim) expected
                then setValueAt ["output"] (String (T.pack actual)) value
                else FixtureUnchanged

fcEvalActual :: Options -> Value -> IO (Either String String)
fcEvalActual opts value =
  case parseEvalInput value of
    Left err -> pure (Left err)
    Right (dependencies, evalModules) -> do
      dependencyModules <- loadFcEvalDependencyModules opts dependencies evalModules
      pure $ do
        deps <- dependencyModules
        case resolveWithDeps mempty (deps <> evalModules) of
          ResolveResult {resolvedModules, resolveErrors = []} ->
            let tcResults = typecheckModulesWithEnv [] resolvedModules
             in if all tcmSuccess tcResults
                  then
                    let allBindings = moduleGroupBindings tcResults
                        dsResults = zipWith (desugarModuleWithBindings allBindings) tcResults resolvedModules
                     in if all dsSuccess dsResults
                          then first (("eval error: " <>) . show) (T.unpack <$> (evalProgramBinding evalBindingName (concatPrograms (map dsProgram dsResults)) >>= renderRawValue))
                          else Left ("desugar error: " <> unlines (concatMap dsErrors dsResults))
                  else Left ("typecheck error: " <> renderTcErrors tcResults)
          ResolveResult {resolveErrors} -> Left ("resolve error: " <> show resolveErrors)

parseEvalInput :: Value -> Either String ([Text], [Module])
parseEvalInput value = do
  exts <- parseExtensions value
  dependencies <- optionalTextArrayField "dependencies" value
  moduleTexts <- parseTextArrayField "modules" value
  exprText <- textField "expression" value
  parsedModules <- traverse (parseModuleText exts) moduleTexts
  expr <- parseExprText exts exprText
  pure (dependencies, combineEvalModules parsedModules expr)

evalBindingName :: Text
evalBindingName = "__aihc_eval__"

combineEvalModules :: [Module] -> Expr -> [Module]
combineEvalModules modules expr =
  case modules of
    [] -> [emptyEvalModule expr]
    _ ->
      let depModules = init modules
          evalModule = last modules
       in depModules <> [evalModule {moduleDecls = moduleDecls evalModule <> [evalDecl expr]}]

loadFcEvalDependencyModules :: Options -> [Text] -> [Module] -> IO (Either String [Module])
loadFcEvalDependencyModules _ [] _ =
  pure (Right [])
loadFcEvalDependencyModules opts dependencies evalModules = do
  roots <- traverse (resolveFcEvalDependencyRoot opts) dependencies
  case sequence roots of
    Left errMsg -> pure (Left errMsg)
    Right packageRoots ->
      loadTransitiveFcEvalModules packageRoots (initialFcEvalDependencyModules evalModules)

resolveFcEvalDependencyRoot :: Options -> Text -> IO (Either String (Text, FilePath))
resolveFcEvalDependencyRoot opts dependency =
  case dependency of
    "aihc-base" -> pure (Right (dependency, optRoot opts </> "core-libs" </> "aihc-base"))
    _ -> pure (Left ("unknown eval fixture dependency: " <> T.unpack dependency))

initialFcEvalDependencyModules :: [Module] -> Set.Set Text
initialFcEvalDependencyModules modules =
  Set.insert "Prelude" (importedFcEvalModuleNames modules)

importedFcEvalModuleNames :: [Module] -> Set.Set Text
importedFcEvalModuleNames modules =
  Set.fromList [importDeclModule importDecl | modu <- modules, importDecl <- moduleImports modu]

loadTransitiveFcEvalModules :: [(Text, FilePath)] -> Set.Set Text -> IO (Either String [Module])
loadTransitiveFcEvalModules packageRoots initialModules =
  go Set.empty [] (Set.toAscList initialModules)
  where
    go _ loaded [] =
      pure (Right (reverse loaded))
    go seen loaded (moduleName : pending)
      | moduleName `Set.member` seen =
          go seen loaded pending
      | otherwise = do
          maybePath <- findFcEvalModulePath packageRoots moduleName
          case maybePath of
            Nothing -> do
              let dependencyNames = T.intercalate ", " (map fst packageRoots)
              pure (Left ("dependency module " <> T.unpack moduleName <> " not found in dependencies: " <> T.unpack dependencyNames))
            Just path -> do
              source <- TIO.readFile path
              case parseModuleText [] source of
                Left errMsg -> pure (Left ("dependency module " <> T.unpack moduleName <> " parse error: " <> errMsg))
                Right modu -> do
                  let seen' = Set.insert moduleName seen
                      newImports = Set.toAscList (importedFcEvalModuleNames [modu] `Set.difference` seen')
                  go seen' (modu : loaded) (pending <> newImports)

findFcEvalModulePath :: [(Text, FilePath)] -> Text -> IO (Maybe FilePath)
findFcEvalModulePath [] _ = pure Nothing
findFcEvalModulePath ((_dependency, root) : rest) moduleName = do
  let path = root </> "src" </> fcEvalModuleNamePath moduleName
  exists <- doesFileExist path
  if exists
    then pure (Just path)
    else findFcEvalModulePath rest moduleName

fcEvalModuleNamePath :: Text -> FilePath
fcEvalModuleNamePath moduleName =
  joinPath (map T.unpack (T.splitOn "." moduleName)) <> ".hs"

emptyEvalModule :: Expr -> Module
emptyEvalModule expr =
  Module
    { moduleAnns = [],
      moduleHead = Nothing,
      moduleLanguagePragmas = [],
      moduleImports = [],
      moduleDecls = [evalDecl expr]
    }

evalDecl :: Expr -> Decl
evalDecl expr =
  DeclValue $
    FunctionBind
      (mkUnqualifiedName NameVarId evalBindingName)
      [ Match
          { matchAnns = [],
            matchHeadForm = MatchHeadPrefix,
            matchPats = [],
            matchRhs = UnguardedRhs [] expr Nothing
          }
      ]

concatPrograms :: [FcProgram] -> FcProgram
concatPrograms programs =
  FcProgram (concatMap fcTopBinds programs)

renderTcErrors :: [TcModuleResult] -> String
renderTcErrors results =
  let rendered = unlines [show diagnostic | result <- results, diagnostic <- tcmDiagnostics result]
   in if null (trim rendered)
        then "type checker failed without diagnostics"
        else rendered

moduleGroupBindings :: [TcModuleResult] -> [TcBindingResult]
moduleGroupBindings =
  concatMap tcmBindings

updateFormatterGoldens :: Options -> IO Summary
updateFormatterGoldens opts =
  updateYamlTree opts (optRoot opts </> "bin/aihc-fmt/test/Test/Fixtures/fmt") $ \_ value -> do
    result <- formatterActuals opts value
    case result of
      Left err -> pure (FixtureSkipped err)
      Right actuals -> do
        let updates =
              [ (["formatters", name, "output"], String output, formatterOutputDiffers name output value && formatterShouldUpdate name value)
              | (name, output) <- actuals
              ]
        pure (applyFieldUpdates updates value)

formatterActuals :: Options -> Value -> IO (Either String [(Text, Text)])
formatterActuals opts value = do
  case (,,) <$> parseExtensionSettings value <*> textField "input" value <*> (objectKeys =<< objectField "formatters" value) of
    Left err -> pure (Left err)
    Right (exts, input, formatterNames) -> do
      let wanted name = name == "aihc-fmt" || optExternalFormatters opts
      results <-
        traverse
          ( \name ->
              if wanted name
                then formatterActual name exts input
                else pure (Right Nothing)
          )
          formatterNames
      pure (catMaybesEither <$> sequence results)

formatterActual :: Text -> [ExtensionSetting] -> Text -> IO (Either String (Maybe (Text, Text)))
formatterActual "aihc-fmt" exts input =
  case formatText defaultFormatOptions {formatExtensions = exts} "<fixture>" input of
    Left err -> pure (Left (T.unpack (formatErrorMessage err)))
    Right output -> pure (Right (Just ("aihc-fmt", output)))
formatterActual name _ input =
  Right <$> runExternalFormatter name input

updateParserCliGoldens :: Options -> IO Summary
updateParserCliGoldens opts =
  updateYamlTree opts (optRoot opts </> "tooling/aihc-dev/test/Test/Fixtures/cli") $ \_ value ->
    if not (shouldUpdateOutput value)
      then pure FixtureUnchanged
      else case cliActual value of
        Left err -> pure (FixtureSkipped err)
        Right (actualOutput, actualExit) ->
          pure $
            applyFieldUpdates
              [ (["output"], String actualOutput, either (const True) ((/= actualOutput) . T.stripEnd) (textField "output" value)),
                (["exit_code"], toJSON actualExit, either (const True) (/= actualExit) (intField "exit_code" 0 value))
              ]
              value

cliActual :: Value -> Either String (Text, Int)
cliActual value = do
  args <- map T.unpack <$> optionalTextArrayField "args" value
  input <- textField "input" value
  includes <- parseIncludes value
  let result = ParserRun.runCLI includes args input
      output = T.stripEnd (ParserRun.cliStdout result <> ParserRun.cliStderr result)
  Right (output, exitCodeToInt (ParserRun.cliExitCode result))

parseIncludes :: Value -> Either String (Map FilePath Text)
parseIncludes value =
  case optionalObjectField "includes" value of
    Left err -> Left err
    Right Nothing -> Right M.empty
    Right (Just obj) ->
      traverseTextObject obj
        >>= Right . M.fromList . map (\(path, source) -> (normalise (T.unpack path), source))

exitCodeToInt :: ExitCode -> Int
exitCodeToInt ExitSuccess = 0
exitCodeToInt (ExitFailure n) = n

-- Shared fixture traversal

updateYamlTree :: Options -> FilePath -> (FilePath -> Value -> IO FixtureUpdate) -> IO Summary
updateYamlTree opts root updater = do
  paths <- listYamlFiles root
  foldM step emptySummary paths
  where
    step summary path = do
      valueResult <- readYamlValue path
      result <-
        case valueResult of
          Left err -> pure (FixtureSkipped err)
          Right value -> updater path value
      case result of
        FixtureUnchanged -> pure summary {summaryScanned = summaryScanned summary + 1}
        FixtureSkipped reason ->
          pure
            summary
              { summaryScanned = summaryScanned summary + 1,
                summarySkipped = summarySkipped summary <> [(path, reason)]
              }
        FixtureUpdated value -> do
          if optDryRun opts
            then putStrLn ("would update " <> path)
            else writeYamlValue path value
          pure
            summary
              { summaryScanned = summaryScanned summary + 1,
                summaryUpdated = summaryUpdated summary + 1
              }

listYamlFiles :: FilePath -> IO [FilePath]
listYamlFiles root = do
  exists <- doesDirectoryExist root
  if not exists
    then pure []
    else do
      entries <- sort <$> listDirectory root
      fmap concat $
        forM entries $ \entry -> do
          let path = root </> entry
          isDir <- doesDirectoryExist path
          if isDir
            then listYamlFiles path
            else pure [path | yamlExtension path]

yamlExtension :: FilePath -> Bool
yamlExtension path =
  case map toLower (takeExtension path) of
    ".yaml" -> True
    ".yml" -> True
    _ -> False

readYamlValue :: FilePath -> IO (Either String Value)
readYamlValue path = do
  bytes <- BS.readFile path
  pure $
    first Y.prettyPrintParseException $
      Y.decodeEither' bytes

writeYamlValue :: FilePath -> Value -> IO ()
writeYamlValue path value = do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (Y.encode value)

decodeFixtureValue :: Value -> Text
decodeFixtureValue =
  TE.decodeUtf8Lenient . Y.encode

-- YAML editing

replaceYamlValueAt :: [Text] -> Value -> Value -> Either String Value
replaceYamlValueAt [] replacement _ = Right replacement
replaceYamlValueAt (key : rest) replacement (Object obj) = do
  current <-
    maybe (Left ("missing key: " <> T.unpack key)) Right $
      KeyMap.lookup (Key.fromText key) obj
  updated <- replaceYamlValueAt rest replacement current
  Right (Object (KeyMap.insert (Key.fromText key) updated obj))
replaceYamlValueAt (key : _) _ _ =
  Left ("cannot descend into non-object at key: " <> T.unpack key)

setValueAt :: [Text] -> Value -> Value -> FixtureUpdate
setValueAt path replacement value =
  case replaceYamlValueAt path replacement value of
    Left err -> FixtureSkipped err
    Right updated
      | updated == value -> FixtureUnchanged
      | otherwise -> FixtureUpdated updated

applyFieldUpdates :: [([Text], Value, Bool)] -> Value -> FixtureUpdate
applyFieldUpdates updates value =
  let wanted = [(path, replacement) | (path, replacement, changed) <- updates, changed]
   in case foldM (\v (path, replacement) -> replaceYamlValueAt path replacement v) value wanted of
        Left err -> FixtureSkipped err
        Right updated
          | updated == value -> FixtureUnchanged
          | otherwise -> FixtureUpdated updated

updateExpectedText :: Value -> String -> IO FixtureUpdate
updateExpectedText value actual = do
  let expected = parseExpectedField "expected" value
  pure $
    if either (const True) ((/= trim actual) . trim . T.unpack) expected
      then setValueAt ["expected"] (String (T.pack actual)) value
      else FixtureUnchanged

-- YAML parsing helpers

objectField :: Text -> Value -> Either String (KeyMap.KeyMap Value)
objectField field (Object obj) =
  case KeyMap.lookup (Key.fromText field) obj of
    Just (Object nested) -> Right nested
    Just _ -> Left (T.unpack field <> " must be an object")
    Nothing -> Left ("missing " <> T.unpack field)
objectField field _ = Left (T.unpack field <> " must be read from an object")

optionalObjectField :: Text -> Value -> Either String (Maybe (KeyMap.KeyMap Value))
optionalObjectField field (Object obj) =
  case KeyMap.lookup (Key.fromText field) obj of
    Nothing -> Right Nothing
    Just (Object nested) -> Right (Just nested)
    Just _ -> Left (T.unpack field <> " must be an object")
optionalObjectField field _ = Left (T.unpack field <> " must be read from an object")

textField :: Text -> Value -> Either String Text
textField field (Object obj) =
  case KeyMap.lookup (Key.fromText field) obj of
    Just (String txt) -> Right txt
    Just _ -> Left (T.unpack field <> " must be a string")
    Nothing -> Left ("missing " <> T.unpack field)
textField field _ = Left (T.unpack field <> " must be read from an object")

statusText :: Value -> Text
statusText value =
  either (const "") (T.toLower . T.strip) (textField "status" value)

shouldUpdateOutput :: Value -> Bool
shouldUpdateOutput value =
  statusText value `elem` ["pass", "xpass"]

intField :: Text -> Int -> Value -> Either String Int
intField field defaultValue (Object obj) =
  case KeyMap.lookup (Key.fromText field) obj of
    Nothing -> Right defaultValue
    Just (Number n) ->
      maybe (Left (T.unpack field <> " is not a bounded integer")) Right (toBoundedInteger n)
    Just _ -> Left (T.unpack field <> " must be a number")
intField field _ _ = Left (T.unpack field <> " must be read from an object")

parseTextArrayField :: Text -> Value -> Either String [Text]
parseTextArrayField field (Object obj) =
  case KeyMap.lookup (Key.fromText field) obj of
    Just value -> parseTextArray field value
    Nothing -> Left ("missing " <> T.unpack field)
parseTextArrayField field _ = Left (T.unpack field <> " must be read from an object")

optionalTextArrayField :: Text -> Value -> Either String [Text]
optionalTextArrayField field (Object obj) =
  case KeyMap.lookup (Key.fromText field) obj of
    Nothing -> Right []
    Just value -> parseTextArray field value
optionalTextArrayField field _ = Left (T.unpack field <> " must be read from an object")

parseTextArray :: Text -> Value -> Either String [Text]
parseTextArray field =
  parseEither $
    withArray (T.unpack field) $ \arr ->
      traverse parseEntry (foldr (:) [] arr)
  where
    parseEntry (String txt) = pure txt
    parseEntry _ = fail (T.unpack field <> " entries must be strings")

parseExpectedField :: Text -> Value -> Either String Text
parseExpectedField field (Object obj) =
  case KeyMap.lookup (Key.fromText field) obj of
    Just value -> parseExpectedValue value
    Nothing -> Right ""
parseExpectedField field _ = Left (T.unpack field <> " must be read from an object")

parseExpectedValue :: Value -> Either String Text
parseExpectedValue (String txt) = Right txt
parseExpectedValue (Array arr) =
  T.intercalate "\n" <$> traverse parseLine (foldr (:) [] arr)
  where
    parseLine (String txt) = Right txt
    parseLine _ = Left "expected array entries must be strings"
parseExpectedValue (Object obj) =
  T.intercalate "\n" <$> traverse parseModuleExpected (sortOn (Key.toText . fst) (KeyMap.toList obj))
  where
    parseModuleExpected (moduleName, value) = do
      lines' <- parseModuleLines value
      Right (Key.toText moduleName <> ":\n" <> T.intercalate "\n" (map ("  " <>) lines'))
    parseModuleLines (String txt) = Right [txt]
    parseModuleLines (Array arr) = traverse parseLine (foldr (:) [] arr)
    parseModuleLines _ = Left "expected object values must be strings or arrays"
    parseLine (String txt) = Right txt
    parseLine _ = Left "expected array entries must be strings"
parseExpectedValue _ = Left "expected must be a string, array, or object"

parseExtensions :: Value -> Either String [Extension]
parseExtensions value = do
  names <- optionalTextArrayField "extensions" value
  forM names $ \name ->
    maybe (Left ("unknown extension: " <> T.unpack name)) Right (parseExtensionName name)

parseExtensionSettings :: Value -> Either String [ExtensionSetting]
parseExtensionSettings value = do
  names <- optionalTextArrayField "extensions" value
  forM names $ \name ->
    maybe (Left ("unknown extension setting: " <> T.unpack name)) Right (parseExtensionSettingName name)

objectKeys :: KeyMap.KeyMap Value -> Either String [Text]
objectKeys = Right . map Key.toText . KeyMap.keys

traverseTextObject :: KeyMap.KeyMap Value -> Either String [(Text, Text)]
traverseTextObject obj =
  forM (KeyMap.toList obj) $ \(key, value) ->
    case value of
      String txt -> Right (Key.toText key, txt)
      _ -> Left ("object value for " <> T.unpack (Key.toText key) <> " must be a string")

formatterOutputDiffers :: Text -> Text -> Value -> Bool
formatterOutputDiffers formatterName actual (Object obj) =
  case KeyMap.lookup "formatters" obj of
    Just (Object formatters) ->
      case KeyMap.lookup (Key.fromText formatterName) formatters of
        Just (Object formatter) ->
          case KeyMap.lookup "output" formatter of
            Just (String expected) -> expected /= actual
            _ -> True
        _ -> True
    _ -> True
formatterOutputDiffers _ _ _ = True

formatterShouldUpdate :: Text -> Value -> Bool
formatterShouldUpdate formatterName (Object obj) =
  case KeyMap.lookup "formatters" obj of
    Just (Object formatters) ->
      case KeyMap.lookup (Key.fromText formatterName) formatters of
        Just formatter -> shouldUpdateOutput formatter
        _ -> False
    _ -> False
formatterShouldUpdate _ _ = False

parseModuleText :: [Extension] -> Text -> Either String Module
parseModuleText exts input =
  let sourceName = T.unpack (T.takeWhile (/= '\n') input)
      (errs, ast) =
        parseModule
          defaultConfig
            { parserSourceName = sourceName,
              parserExtensions = exts
            }
          input
   in if null errs
        then Right ast
        else Left (formatParseErrors sourceName (Just input) errs)

parseExprText :: [Extension] -> Text -> Either String Expr
parseExprText exts input =
  case parseExpr defaultConfig {parserSourceName = "<expression>", parserExtensions = exts} input of
    ParseOk expr -> Right expr
    ParseErr err -> Left (formatParseErrors "<expression>" (Just input) err)

skipForStatus :: Either String Text -> String -> FixtureUpdate
skipForStatus status err =
  case T.toLower . T.strip <$> status of
    Right "pass" -> FixtureSkipped err
    _ -> FixtureUnchanged

catMaybesEither :: [Maybe a] -> [a]
catMaybesEither = catMaybes

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

-- External formatter support is intentionally isolated because missing tools
-- should never make the all-golden updater fail. The public flag opts into it.
runExternalFormatter :: Text -> Text -> IO (Maybe (Text, Text))
runExternalFormatter name input = do
  mExe <- findExecutable (T.unpack name)
  case mExe of
    Nothing -> pure Nothing
    Just exe ->
      bracket
        acquireTempFile
        releaseTempFile
        ( \(tmpPath, handle) -> do
            TIO.hPutStr handle input
            hClose handle
            (exitCode, stdout, _stderr) <- readProcessWithExitCode exe [tmpPath] ""
            formattedFile <- TIO.readFile tmpPath
            let actual =
                  if null stdout
                    then formattedFile
                    else T.pack stdout
            pure $
              case exitCode of
                ExitSuccess -> Just (name, actual)
                ExitFailure _ -> Nothing
        )

acquireTempFile :: IO (FilePath, Handle)
acquireTempFile = do
  tmpDir <- getTemporaryDirectory
  openTempFile tmpDir "aihc-golden-format.hs"

releaseTempFile :: (FilePath, Handle) -> IO ()
releaseTempFile (tmpPath, handle) = do
  hClose handle `catch` ignoreIOException
  removeFile tmpPath `catch` ignoreIOException

ignoreIOException :: IOException -> IO ()
ignoreIOException _ = pure ()
