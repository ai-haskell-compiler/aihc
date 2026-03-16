{-# LANGUAGE OverloadedStrings #-}

module CppSupport
  ( preprocessForParser,
    preprocessForParserIfEnabled,
    preprocessForParserWithoutIncludes,
    preprocessForParserWithoutIncludesIfEnabled,
    moduleHeaderExtensionSettings,
    cppEnabledInSource,
  )
where

import Cpp
  ( Config (..),
    IncludeRequest,
    Result (..),
    Step (..),
    defaultConfig,
    preprocess,
  )
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Parser as P
import Parser.Ast (Extension (CPP), ExtensionSetting (..), parseExtensionSettingName)
import System.FilePath (takeExtension)

preprocessForParser :: (Monad m) => FilePath -> (IncludeRequest -> m (Maybe Text)) -> Text -> m Result
preprocessForParser inputFile resolveInclude source =
  preprocessForParserWithCppOptions [] inputFile resolveInclude (normalizeSourceForParser inputFile source)

preprocessForParserWithCppOptions :: (Monad m) => [String] -> FilePath -> (IncludeRequest -> m (Maybe Text)) -> Text -> m Result
preprocessForParserWithCppOptions cppOptions inputFile resolveInclude source = do
  let cfg =
        defaultConfig
          { configInputFile = inputFile,
            configMacros = cppMacrosFromOptions cppOptions
          }
  result <- drive (preprocess cfg source)
  pure result {resultOutput = stripLinePragmas (resultOutput result)}
  where
    drive (Done result) = pure result
    drive (NeedInclude req k) = resolveInclude req >>= drive . k

preprocessForParserWithoutIncludes :: FilePath -> Text -> Result
preprocessForParserWithoutIncludes inputFile source =
  runIdentity (preprocessForParser inputFile (\_ -> Identity Nothing) source)

preprocessForParserIfEnabled :: (Monad m) => [String] -> [String] -> FilePath -> (IncludeRequest -> m (Maybe Text)) -> Text -> m Result
preprocessForParserIfEnabled globalExtensionNames cppOptions inputFile resolveInclude source =
  let normalizedSource = normalizeSourceForParser inputFile source
   in if cppEnabledInSourceWithGlobals globalExtensionNames normalizedSource || hasCppOptionMacros cppOptions
        then preprocessForParserWithCppOptions cppOptions inputFile resolveInclude normalizedSource
        else pure Result {resultOutput = normalizedSource, resultDiagnostics = []}

preprocessForParserWithoutIncludesIfEnabled :: [String] -> [String] -> FilePath -> Text -> Result
preprocessForParserWithoutIncludesIfEnabled globalExtensionNames cppOptions inputFile source =
  runIdentity (preprocessForParserIfEnabled globalExtensionNames cppOptions inputFile (\_ -> Identity Nothing) source)

moduleHeaderExtensionSettings :: Text -> [ExtensionSetting]
moduleHeaderExtensionSettings = P.readModuleHeaderExtensions

cppEnabledInSource :: Text -> Bool
cppEnabledInSource = cppEnabledInSettings . moduleHeaderExtensionSettings

cppEnabledInSourceWithGlobals :: [String] -> Text -> Bool
cppEnabledInSourceWithGlobals globalExtensionNames source =
  cppEnabledInSettings (settingsFromExtensionNames globalExtensionNames)
    || cppEnabledInSettings (moduleHeaderExtensionSettings source)

settingsFromExtensionNames :: [String] -> [ExtensionSetting]
settingsFromExtensionNames = mapMaybe (parseExtensionSettingName . T.pack)

hasCppOptionMacros :: [String] -> Bool
hasCppOptionMacros = any isCppMacroOption
  where
    isCppMacroOption opt =
      case T.strip (stripWrappingQuotes (T.pack opt)) of
        x -> "-D" `T.isPrefixOf` x || "-U" `T.isPrefixOf` x

cppEnabledInSettings :: [ExtensionSetting] -> Bool
cppEnabledInSettings = foldl apply False
  where
    apply enabled setting =
      case setting of
        EnableExtension CPP -> True
        DisableExtension CPP -> False
        _ -> enabled

stripLinePragmas :: Text -> Text
stripLinePragmas =
  T.unlines
    . filter (not . isLinePragma)
    . T.lines
  where
    isLinePragma line =
      let stripped = T.stripStart line
       in "#line " `T.isPrefixOf` stripped
            || "{-# LINE " `T.isPrefixOf` stripped
            || "{-# COLUMN " `T.isPrefixOf` stripped

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
    [ ("__GLASGOW_HASKELL__", "910")
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
  case T.stripPrefix "\xfeff" txt of
    Just rest -> rest
    Nothing -> txt

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
        Just rest -> T.stripPrefix " " rest `orElse` rest
        Nothing -> ""

    unlitLatex _ [] = []
    unlitLatex inCode (line : rest)
      | T.strip line == "\\begin{code}" = "" : unlitLatex True rest
      | T.strip line == "\\end{code}" = "" : unlitLatex False rest
      | inCode = line : unlitLatex inCode rest
      | otherwise = "" : unlitLatex inCode rest

orElse :: Maybe a -> a -> a
orElse maybeValue fallback =
  case maybeValue of
    Just value -> value
    Nothing -> fallback
