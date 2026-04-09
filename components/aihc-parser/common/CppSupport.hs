{-# LANGUAGE OverloadedStrings #-}

module CppSupport
  ( preprocessForParser,
    preprocessForParserIfEnabled,
    preprocessForParserWithoutIncludes,
    preprocessForParserWithoutIncludesIfEnabled,
    moduleHeaderExtensionSettings,
    moduleHeaderPragmas,
    cppEnabledInSource,
  )
where

import Aihc.Cpp
  ( Config (..),
    IncludeRequest (..),
    Result (..),
    Step (..),
    defaultConfig,
    preprocess,
  )
import Aihc.Parser.Lex (readModuleHeaderExtensions, readModuleHeaderPragmas)
import Aihc.Parser.Syntax (Extension (CPP), ExtensionSetting (..), ModuleHeaderPragmas (..))
import Data.ByteString (ByteString)
import Data.Char (toLower)
import Data.Functor.Identity (Identity (..), runIdentity)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath (takeExtension)

preprocessForParser :: (Monad m) => FilePath -> [Text] -> (IncludeRequest -> m (Maybe ByteString)) -> Text -> m Result
preprocessForParser inputFile deps resolveInclude source =
  preprocessForParserWithCppOptions [] inputFile deps resolveInclude (normalizeSourceForParser inputFile source)

preprocessForParserWithCppOptions :: (Monad m) => [String] -> FilePath -> [Text] -> (IncludeRequest -> m (Maybe ByteString)) -> Text -> m Result
preprocessForParserWithCppOptions cppOptions inputFile deps resolveInclude source = do
  let minVersionMacros = minVersionMacroNamesFromDeps deps
  let injected = injectSyntheticCppMacros cppOptions minVersionMacros source
  let cfg =
        defaultConfig
          { configInputFile = inputFile,
            configMacros = cppMacrosFromOptions cppOptions
          }
  drive (preprocess cfg (TE.encodeUtf8 injected))
  where
    drive (Done result) = pure result
    drive (NeedInclude req k) = resolveInclude req >>= drive . k

preprocessForParserWithoutIncludes :: FilePath -> [Text] -> Text -> Result
preprocessForParserWithoutIncludes inputFile deps source =
  runIdentity (preprocessForParser inputFile deps (\_ -> Identity Nothing) source)

preprocessForParserIfEnabled :: (Monad m) => [ExtensionSetting] -> [String] -> FilePath -> [Text] -> (IncludeRequest -> m (Maybe ByteString)) -> Text -> m Result
preprocessForParserIfEnabled globalExtensionNames cppOptions inputFile deps resolveInclude source =
  let normalizedSource = normalizeSourceForParser inputFile source
      shouldPreprocess = cppEnabledInSourceWithGlobals globalExtensionNames normalizedSource
   in if shouldPreprocess
        then preprocessForParserWithCppOptions cppOptions inputFile deps resolveInclude normalizedSource
        else pure Result {resultOutput = normalizedSource, resultDiagnostics = []}

preprocessForParserWithoutIncludesIfEnabled :: [ExtensionSetting] -> [String] -> FilePath -> [Text] -> Text -> Result
preprocessForParserWithoutIncludesIfEnabled globalExtensionNames cppOptions inputFile deps source =
  runIdentity (preprocessForParserIfEnabled globalExtensionNames cppOptions inputFile deps (\_ -> Identity Nothing) source)

moduleHeaderExtensionSettings :: Text -> [ExtensionSetting]
moduleHeaderExtensionSettings = readModuleHeaderExtensions

moduleHeaderPragmas :: Text -> ModuleHeaderPragmas
moduleHeaderPragmas = readModuleHeaderPragmas

cppEnabledInSource :: Text -> Bool
cppEnabledInSource = cppEnabledInSettings . moduleHeaderExtensionSettings

cppEnabledInSourceWithGlobals :: [ExtensionSetting] -> Text -> Bool
cppEnabledInSourceWithGlobals globalExtensionNames source =
  cppEnabledInSettings globalExtensionNames
    || cppEnabledInSettings (moduleHeaderExtensionSettings source)

cppEnabledInSettings :: [ExtensionSetting] -> Bool
cppEnabledInSettings = foldl apply False
  where
    apply enabled setting =
      case setting of
        EnableExtension CPP -> True
        DisableExtension CPP -> False
        _ -> enabled

cppMacrosFromOptions :: [String] -> M.Map Text Text
cppMacrosFromOptions cppOptions =
  foldl apply builtinCppMacros (mapMaybe parseCppMacroOption cppOptions)
  where
    apply macros option =
      case option of
        CppDefine name value -> M.insert name value macros
        CppUndef name -> M.delete name macros

builtinCppMacros :: M.Map Text Text
builtinCppMacros =
  M.fromList
    [ ("__GLASGOW_HASKELL__", "906"),
      ("__GLASGOW_HASKELL_FULL_VERSION__", "\"9.6.7\""),
      ("__GLASGOW_HASKELL_PATCHLEVEL1__", "7"),
      ("__GLASGOW_HASKELL_PATCHLEVEL2__", "0")
    ]

data CppMacroOption
  = CppDefine Text Text
  | CppUndef Text

parseCppMacroOption :: String -> Maybe CppMacroOption
parseCppMacroOption raw =
  let opt = T.strip (stripWrappingQuotes (T.pack raw))
   in case T.stripPrefix "-D" opt of
        Just rest ->
          case T.breakOn "=" rest of
            (name, "") | validMacroName name -> Just (CppDefine name "1")
            (name, value) | validMacroName name -> Just (CppDefine name (T.drop 1 value))
            _ -> Nothing
        Nothing ->
          case T.stripPrefix "-U" opt of
            Just name | validMacroName name -> Just (CppUndef name)
            _ -> Nothing

validMacroName :: Text -> Bool
validMacroName = not . T.null . T.strip

stripWrappingQuotes :: Text -> Text
stripWrappingQuotes txt =
  if T.length txt >= 2 && T.head txt == '"' && T.last txt == '"'
    then T.dropEnd 1 (T.drop 1 txt)
    else txt

normalizeSourceForParser :: FilePath -> Text -> Text
normalizeSourceForParser inputFile =
  unliterateIfNeeded inputFile . stripLeadingBom

stripLeadingBom :: Text -> Text
stripLeadingBom txt =
  fromMaybe txt (T.stripPrefix "\xfeff" txt)

unliterateIfNeeded :: FilePath -> Text -> Text
unliterateIfNeeded inputFile source
  | map toLower (takeExtension inputFile) /= ".lhs" = source
  | otherwise =
      let ls = T.lines source
       in if any (\line -> T.strip line == "\\begin{code}") ls
            then T.unlines (unlitLatex False ls)
            else T.unlines (map unlitBirdLine ls)
  where
    unlitBirdLine line =
      case T.stripPrefix ">" line of
        Just rest -> fromMaybe rest (T.stripPrefix " " rest)
        Nothing -> ""

    unlitLatex _ [] = []
    unlitLatex inCode (line : rest)
      | T.strip line == "\\begin{code}" = "" : unlitLatex True rest
      | T.strip line == "\\end{code}" = "" : unlitLatex False rest
      | inCode = line : unlitLatex inCode rest
      | otherwise = "" : unlitLatex inCode rest

minVersionMacroNamesFromDeps :: [Text] -> S.Set Text
minVersionMacroNamesFromDeps = S.fromList . map toMinVersionMacroName
  where
    toMinVersionMacroName pkg = "MIN_VERSION_" <> T.map sanitizePkgChar pkg
    sanitizePkgChar '-' = '_'
    sanitizePkgChar c = c

injectSyntheticCppMacros :: [String] -> S.Set Text -> Text -> Text
injectSyntheticCppMacros cppOptions minVersionMacroNames source =
  let existingFromOptions = cppDefinedOrUndefinedFromOptions cppOptions
      reservedCompilerMinVersionNames = S.fromList ["MIN_VERSION_ghc", "MIN_VERSION_GLASGOW_HASKELL"]
      shouldDefine name = not (name `S.member` existingFromOptions)
      compilerMacroLines =
        [ "#define MIN_VERSION_ghc(major1,major2,minor) 1"
        | shouldDefine "MIN_VERSION_ghc"
        ]
          ++ [ "#define MIN_VERSION_GLASGOW_HASKELL(ma,mi,pl1,pl2) 1"
             | shouldDefine "MIN_VERSION_GLASGOW_HASKELL"
             ]
      dynamicLines =
        [ "#define " <> name <> "(major1,major2,minor) 1"
        | name <- S.toAscList minVersionMacroNames,
          not (name `S.member` reservedCompilerMinVersionNames),
          shouldDefine name
        ]
      header =
        if null (compilerMacroLines ++ dynamicLines)
          then ""
          else T.unlines (compilerMacroLines ++ dynamicLines)
   in if T.null header then source else header <> source

cppDefinedOrUndefinedFromOptions :: [String] -> S.Set Text
cppDefinedOrUndefinedFromOptions =
  foldl addName S.empty . mapMaybe parseCppMacroOption
  where
    addName acc option =
      case option of
        CppDefine name _ -> S.insert name acc
        CppUndef name -> S.insert name acc
