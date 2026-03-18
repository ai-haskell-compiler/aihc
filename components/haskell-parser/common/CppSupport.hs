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
    IncludeKind (..),
    IncludeRequest (..),
    Result (..),
    Step (..),
    defaultConfig,
    includeFrom,
    includeKind,
    includePath,
    preprocess,
  )
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.Functor.Identity (Identity (..), runIdentity)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Parser as P
import Parser.Ast (Extension (CPP), ExtensionSetting (..), parseExtensionSettingName)
import System.FilePath (takeDirectory, takeExtension, (</>))

preprocessForParser :: (Monad m) => FilePath -> (IncludeRequest -> m (Maybe Text)) -> Text -> m Result
preprocessForParser inputFile resolveInclude source =
  preprocessForParserWithCppOptions [] inputFile resolveInclude (normalizeSourceForParser inputFile source)

preprocessForParserWithCppOptions :: (Monad m) => [String] -> FilePath -> (IncludeRequest -> m (Maybe Text)) -> Text -> m Result
preprocessForParserWithCppOptions cppOptions inputFile resolveInclude source = do
  minVersionMacros <- discoverMinVersionMacros inputFile resolveInclude source
  let injected = injectSyntheticCppMacros cppOptions minVersionMacros source
  let cfg =
        defaultConfig
          { configInputFile = inputFile,
            configMacros = cppMacrosFromOptions cppOptions
          }
  result <- drive (preprocess cfg injected)
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

discoverMinVersionMacros :: (Monad m) => FilePath -> (IncludeRequest -> m (Maybe Text)) -> Text -> m (S.Set Text)
discoverMinVersionMacros inputFile resolveInclude =
  go S.empty S.empty inputFile
  where
    go seenFiles seenMacros filePath txt
      | filePath `S.member` seenFiles = pure seenMacros
      | otherwise = do
          let seenFiles' = S.insert filePath seenFiles
              found = extractMinVersionMacroNames txt
              includeReqs = extractIncludeRequests filePath txt
              seenMacros' = S.union seenMacros found
          foldl
            (\acc req -> acc >>= \curr -> collectInclude seenFiles' curr req)
            (pure seenMacros')
            includeReqs

    collectInclude seenFiles seenMacros req = do
      content <- resolveInclude req
      case content of
        Nothing -> pure seenMacros
        Just includeText ->
          let includeFilePath =
                case includeKind req of
                  IncludeLocal -> takeDirectory (includeFrom req) </> includePath req
                  IncludeSystem -> includePath req
           in go seenFiles seenMacros includeFilePath includeText

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

extractMinVersionMacroNames :: Text -> S.Set Text
extractMinVersionMacroNames = go S.empty
  where
    prefix = "MIN_VERSION_"
    go acc txt =
      case T.breakOn prefix txt of
        (_, "") -> acc
        (_, rest) ->
          let candidateWithPrefix = T.takeWhile isMinVersionIdentChar rest
              suffix = T.drop (T.length candidateWithPrefix) rest
              acc' =
                if T.length candidateWithPrefix > T.length prefix
                  && case T.uncons suffix of
                    Just ('(', _) -> True
                    _ -> False
                  then S.insert candidateWithPrefix acc
                  else acc
              nextTxt =
                if T.null rest
                  then ""
                  else T.drop 1 rest
           in go acc' nextTxt

    isMinVersionIdentChar c = c == '_' || isAsciiAlphaNum c
    isAsciiAlphaNum c = isAsciiLower c || isAsciiUpper c || isDigit c

extractIncludeRequests :: FilePath -> Text -> [IncludeRequest]
extractIncludeRequests filePath =
  mapMaybe parseIncludeLine . T.lines
  where
    parseIncludeLine raw =
      let line = T.stripStart raw
       in case T.stripPrefix "#include" line of
            Nothing -> Nothing
            Just rest ->
              let stripped = T.stripStart rest
               in case T.uncons stripped of
                    Just ('"', t1) ->
                      let (path, t2) = T.breakOn "\"" t1
                       in if T.null path || T.null t2
                            then Nothing
                            else
                              Just
                                IncludeRequest
                                  { includePath = T.unpack path,
                                    includeKind = IncludeLocal,
                                    includeFrom = filePath,
                                    includeLine = 1
                                  }
                    Just ('<', t1) ->
                      let (path, t2) = T.breakOn ">" t1
                       in if T.null path || T.null t2
                            then Nothing
                            else
                              Just
                                IncludeRequest
                                  { includePath = T.unpack path,
                                    includeKind = IncludeSystem,
                                    includeFrom = filePath,
                                    includeLine = 1
                                  }
                    _ -> Nothing
