{-# LANGUAGE OverloadedStrings #-}

module Aihc.Dev.Snippet.CppSupport
  ( preprocessForParserWithoutIncludes,
    preprocessForParserWithoutIncludesIfEnabled,
  )
where

import Aihc.Cpp
  ( Config (..),
    Result (..),
    Step (..),
    defaultConfig,
    preprocess,
  )
import Aihc.Hackage.Cpp qualified as HackageCpp
import Aihc.Parser.Lex (readModuleHeaderExtensions)
import Aihc.Parser.Syntax (Extension (CPP), ExtensionSetting (..))
import Data.Char (toLower)
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import System.FilePath (takeExtension)

preprocessForParserWithoutIncludes :: FilePath -> [Text] -> Text -> Result
preprocessForParserWithoutIncludes inputFile deps source =
  runIdentity (preprocessForParser inputFile deps (\_ -> Identity Nothing) source)

preprocessForParserWithoutIncludesIfEnabled :: [ExtensionSetting] -> [String] -> FilePath -> [Text] -> Text -> Result
preprocessForParserWithoutIncludesIfEnabled globalExtensionNames cppOptions inputFile deps source =
  runIdentity (preprocessForParserIfEnabled globalExtensionNames cppOptions inputFile deps (\_ -> Identity Nothing) source)

preprocessForParser :: (Monad m) => FilePath -> [Text] -> (a -> m (Maybe b)) -> Text -> m Result
preprocessForParser inputFile deps resolveInclude source =
  preprocessForParserWithCppOptions [] inputFile deps resolveInclude (normalizeSourceForParser inputFile source)

preprocessForParserIfEnabled :: (Monad m) => [ExtensionSetting] -> [String] -> FilePath -> [Text] -> (a -> m (Maybe b)) -> Text -> m Result
preprocessForParserIfEnabled globalExtensionNames cppOptions inputFile deps resolveInclude source =
  let normalizedSource = normalizeSourceForParser inputFile source
      shouldPreprocess = cppEnabledInSourceWithGlobals globalExtensionNames normalizedSource
   in if shouldPreprocess
        then preprocessForParserWithCppOptions cppOptions inputFile deps resolveInclude normalizedSource
        else pure Result {resultOutput = normalizedSource, resultDiagnostics = []}

preprocessForParserWithCppOptions :: (Monad m) => [String] -> FilePath -> [Text] -> (a -> m (Maybe b)) -> Text -> m Result
preprocessForParserWithCppOptions cppOptions inputFile deps _resolveInclude source = do
  let minVersionMacros = HackageCpp.minVersionMacroNamesFromDeps deps
  let injected = HackageCpp.injectSyntheticCppMacros cppOptions minVersionMacros source
  let cfg =
        defaultConfig
          { configInputFile = inputFile,
            configMacros = HackageCpp.cppMacrosFromOptions cppOptions
          }
  pure (drive (preprocess cfg (TE.encodeUtf8 injected)))
  where
    drive (Done result) = result
    drive (NeedInclude _ k) = drive (k Nothing)

cppEnabledInSourceWithGlobals :: [ExtensionSetting] -> Text -> Bool
cppEnabledInSourceWithGlobals globalExtensionNames source =
  cppEnabledInSettings globalExtensionNames
    || cppEnabledInSettings (readModuleHeaderExtensions source)

cppEnabledInSettings :: [ExtensionSetting] -> Bool
cppEnabledInSettings = foldl apply False
  where
    apply enabled setting =
      case setting of
        EnableExtension CPP -> True
        DisableExtension CPP -> False
        _ -> enabled

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
        Just rest -> " " <> rest
        Nothing -> ""

    unlitLatex _ [] = []
    unlitLatex inCode (line : rest)
      | T.strip line == "\\begin{code}" = "" : unlitLatex True rest
      | T.strip line == "\\end{code}" = "" : unlitLatex False rest
      | inCode = line : unlitLatex inCode rest
      | otherwise = "" : unlitLatex inCode rest
