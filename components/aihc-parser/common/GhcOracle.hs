{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GhcOracle
  ( oracleModuleAstFingerprint,
    oracleModuleAstFingerprintNoCPP,
    toGhcExtension,
    fromGhcExtension,
  )
where

import Aihc.Cpp (resultOutput)
import Aihc.Parser.Lex qualified as Lex
import Aihc.Parser.Syntax qualified as Syntax
import Control.Exception (bracket, catch, displayException, evaluate)
import CppSupport (preprocessForParserWithoutIncludes)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Data.Bag qualified as Bag
import GHC.Data.EnumSet qualified as EnumSet
import GHC.Data.FastString (mkFastString)
import GHC.Data.StringBuffer (stringToStringBuffer)
import GHC.Hs (GhcPs, HsModule)
import GHC.LanguageExtensions.Type qualified as GHC
import GHC.Parser (parseModule)
import GHC.Parser.Lexer
  ( PState,
    ParseResult (..),
    Token (..),
    getPsErrorMessages,
    getPsMessages,
    initParserState,
    lexer,
    mkParserOpts,
    unP,
  )
import GHC.Types.Error
  ( NoDiagnosticOpts (NoDiagnosticOpts),
    diagnosticCode,
    diagnosticReason,
    errMsgDiagnostic,
    errMsgSpan,
    errorsFound,
    getMessages,
  )
import GHC.Types.SourceError (SourceError)
import GHC.Types.SrcLoc
  ( Located,
    mkRealSrcLoc,
    unLoc,
  )
import GHC.Utils.Error
  ( emptyDiagOpts,
    getCaretDiagnostic,
    mkMCDiagnostic,
    pprMessages,
  )
import GHC.Utils.Outputable (ppr, showSDocUnsafe)
import System.Directory (removeFile)
import System.FilePath (takeFileName)
import System.IO (hClose, hPutStr, openTempFile)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (foldl')

-- | Compute an AST fingerprint using extension names and a language edition,
-- reading in-file pragmas to determine the full effective extension set.
oracleModuleAstFingerprint :: String -> Syntax.LanguageEdition -> [Syntax.ExtensionSetting] -> Text -> Either Text Text
oracleModuleAstFingerprint sourceTag edition extNames input =
  let initialPragmas = Lex.readModuleHeaderPragmas input
      initialExts = computeEffectiveExtensions edition extNames initialPragmas
      preprocessedInput =
        if Syntax.CPP `elem` initialExts
          then resultOutput (preprocessForParserWithoutIncludes sourceTag [] input)
          else input
   in oracleModuleAstFingerprintNoCPP sourceTag edition extNames preprocessedInput

-- | Compute an AST fingerprint using extension names and a language edition,
-- reading in-file pragmas to determine the full effective extension set.
-- This version does not preprocess the input for CPP.
oracleModuleAstFingerprintNoCPP :: String -> Syntax.LanguageEdition -> [Syntax.ExtensionSetting] -> Text -> Either Text Text
oracleModuleAstFingerprintNoCPP sourceTag edition extNames input =
  let pragmas = Lex.readModuleHeaderPragmas input
      exts = computeEffectiveExtensions edition extNames pragmas
      ghcExts = mapMaybe toGhcExtension exts
   in do
        parsed <- parseWithGhcWithExtensions sourceTag ghcExts input
        pure (T.pack (showSDocUnsafe (ppr parsed)))

-- | Parse a module with GHC using the given extensions.
-- The extensions should be the complete set - no base extensions are added.
-- Extension handling (language editions, pragmas, implied extensions) should be done
-- by the caller using aihc-parser infrastructure.
parseWithGhcWithExtensions :: String -> [GHC.Extension] -> Text -> Either Text (HsModule GhcPs)
parseWithGhcWithExtensions sourceTag exts input =
  let opts = mkParserOpts (EnumSet.fromList exts) emptyDiagOpts False False False True
      buffer = stringToStringBuffer (T.unpack input)
      start = mkRealSrcLoc (mkFastString sourceTag) 1 1
   in case catchPureExceptionText $ case unP parseModule (initParserState opts buffer start) of
        POk st modu ->
          if not (parserStateHasErrors st)
            then case firstSignificantTokenAfterModule input st of
              Right tok ->
                case unLoc tok of
                  ITeof -> Right (unLoc modu)
                  _ ->
                    Left
                      ( "GHC parser accepted module prefix but left trailing token: "
                          <> T.pack (show tok)
                      )
              Left lexErr ->
                Left
                  ( "GHC lexer failed while checking for trailing tokens: "
                      <> lexErr
                  )
            else Left (renderParserErrors input st)
        PFailed st ->
          Left (renderParserErrors input st) of
        Left err -> Left ("GHC parser exception: " <> err)
        Right result -> result

firstSignificantTokenAfterModule :: Text -> PState -> Either Text (Located Token)
firstSignificantTokenAfterModule input st =
  case unP (lexer False pure) st of
    POk st' tok
      | isIgnorableToken (unLoc tok) -> firstSignificantTokenAfterModule input st'
      | otherwise -> Right tok
    PFailed st' ->
      Left (renderParserErrors input st')

renderParserErrors :: Text -> PState -> Text
renderParserErrors source st =
  unsafePerformIO $ do
    -- Write source to a temporary file so getCaretDiagnostic can access it
    bracket
      (openTempFile "." "ghc-error.hs")
      (\(path, handle) -> hClose handle >> removeFile path)
      ( \(path, handle) -> do
          hPutStr handle (T.unpack source)
          hClose handle
          -- Re-parse from the temp file to get SrcSpans pointing to it
          let opts = mkParserOpts (EnumSet.fromList []) emptyDiagOpts False False False True
              buffer = stringToStringBuffer (T.unpack source)
              start = mkRealSrcLoc (mkFastString path) 1 1
          case unP parseModule (initParserState opts buffer start) of
            POk tempSt _ ->
              if parserStateHasErrors tempSt
                then do
                  result <- renderWithCarets tempSt
                  -- Replace temp file name with "test.hs" for consistent output
                  let fileName = takeFileName path
                  pure (T.replace (T.pack fileName) "test.hs" result)
                else pure (T.pack (showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages st))))
            PFailed tempSt -> do
              result <- renderWithCarets tempSt
              let fileName = takeFileName path
              pure (T.replace (T.pack fileName) "test.hs" result)
      )
  where
    renderWithCarets :: PState -> IO Text
    renderWithCarets pState = do
      let (_, errorMsgs) = getPsMessages pState
          envelopeList = Bag.bagToList (getMessages errorMsgs)
          rawError = T.pack (showSDocUnsafe (pprMessages NoDiagnosticOpts (getPsErrorMessages pState)))
      -- Get the caret visualizations for each error
      carets <- mapM renderEnvelope envelopeList
      -- Parse the raw error to extract individual error blocks and combine with carets
      pure (combineErrorsWithCarets rawError carets)

    combineErrorsWithCarets :: Text -> [Text] -> Text
    combineErrorsWithCarets rawError carets =
      -- Split rawError by error location patterns and interleave with carets
      let errorBlocks = splitErrorBlocks rawError
          -- Strip trailing whitespace from each block and combine with caret
          combined = zipWith (\block caret -> T.stripEnd block <> "\n" <> caret) errorBlocks carets
       in T.intercalate "\n" combined
      where
        splitErrorBlocks :: Text -> [Text]
        splitErrorBlocks text =
          let textLines = T.lines text
              -- Extract filename prefix from first line (e.g., "test.hs:" or "ghc-error123-0.hs:")
              fileNamePrefix =
                case textLines of
                  [] -> ""
                  (first : _) ->
                    case T.takeWhile (/= ':') first of
                      "" -> ""
                      fileName -> fileName <> ":"
              (blocks, currentBlock, _) = foldl' (accumulate fileNamePrefix) ([], [], 0) textLines
              finalBlock = reverse (dropWhile T.null (reverse currentBlock))
           in case finalBlock of
                [] -> blocks
                _ -> blocks <> [T.unlines finalBlock]

        accumulate :: Text -> ([Text], [Text], Int) -> Text -> ([Text], [Text], Int)
        accumulate fileNamePrefix (blocks, current, idx) line =
          case T.stripPrefix fileNamePrefix line of
            Just _ ->
              -- New error location, finalize current block (strip trailing blank lines)
              let stripped = reverse (dropWhile T.null (reverse current))
                  newBlocks = if null stripped then blocks else blocks <> [T.unlines stripped]
               in (newBlocks, [line], idx + 1)
            Nothing ->
              -- Continuation of current error
              (blocks, current <> [line], idx)

    renderEnvelope env = do
      let srcSpan = errMsgSpan env
          reason = diagnosticReason (errMsgDiagnostic env)
          code = diagnosticCode (errMsgDiagnostic env)
          messageClass = mkMCDiagnostic emptyDiagOpts reason code
      -- Get the caret visualization (includes | separator, source line, and caret line)
      caretDoc <- getCaretDiagnostic messageClass srcSpan
      pure (T.pack (showSDocUnsafe caretDoc))

parserStateHasErrors :: PState -> Bool
parserStateHasErrors st = errorsFound (getPsErrorMessages st)

isIgnorableToken :: Token -> Bool
isIgnorableToken tok =
  case tok of
    ITdocComment {} -> True
    ITdocOptions {} -> True
    ITlineComment {} -> True
    ITblockComment {} -> True
    _ -> False

catchPureExceptionText :: a -> Either Text a
catchPureExceptionText value =
  unsafePerformIO $ do
    (Right <$> evaluate value)
      `catch` \(err :: SourceError) ->
        pure (Left (T.pack (displayException err)))
{-# NOINLINE catchPureExceptionText #-}

toGhcExtension :: Syntax.Extension -> Maybe GHC.Extension
toGhcExtension ext =
  case ext of
    Syntax.NondecreasingIndentation ->
      lookupAny ["NondecreasingIndentation", "AlternativeLayoutRule", "AlternativeLayoutRuleTransitional", "RelaxedLayout"]
    _ ->
      lookupAny [toGhcExtensionName ext]
  where
    ghcExtensions = [(show ghcExt, ghcExt) | ghcExt <- [minBound .. maxBound]]
    lookupAny [] = Nothing
    lookupAny (name : names) =
      case lookup name ghcExtensions of
        Just ghcExt -> Just ghcExt
        Nothing -> lookupAny names

    toGhcExtensionName Syntax.CPP = "Cpp"
    toGhcExtensionName Syntax.GeneralizedNewtypeDeriving = "GeneralisedNewtypeDeriving"
    toGhcExtensionName Syntax.SafeHaskell = "Safe"
    toGhcExtensionName Syntax.Trustworthy = "Trustworthy"
    toGhcExtensionName Syntax.UnsafeHaskell = "Unsafe"
    toGhcExtensionName Syntax.Rank2Types = "RankNTypes"
    toGhcExtensionName Syntax.PolymorphicComponents = "RankNTypes"
    toGhcExtensionName other = T.unpack (Syntax.extensionName other)

fromGhcExtension :: GHC.Extension -> Maybe Syntax.Extension
fromGhcExtension ghcExt = Syntax.parseExtensionName (T.pack (show ghcExt))

-- | Compute the effective set of parser extensions from:
-- 1. A default language edition (from cabal file)
-- 2. Extension names from cabal file
-- 3. Module header pragmas (which may override the language edition)
--
-- This is the unified extension computation that should be used by all parsers.
computeEffectiveExtensions ::
  -- | Default language edition (from cabal file)
  Syntax.LanguageEdition ->
  -- | Extension names from cabal file (e.g., ["UnicodeSyntax", "NoFieldSelectors"])
  [Syntax.ExtensionSetting] ->
  -- | Module header pragmas (from readModuleHeaderPragmas)
  Syntax.ModuleHeaderPragmas ->
  -- | Effective set of extensions
  [Syntax.Extension]
computeEffectiveExtensions edition extensionSettings headerPragmas =
  Syntax.effectiveExtensions edition (extensionSettings ++ Syntax.headerExtensionSettings headerPragmas)
