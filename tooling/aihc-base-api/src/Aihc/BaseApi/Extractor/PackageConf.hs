{-# LANGUAGE OverloadedStrings #-}

module Aihc.BaseApi.Extractor.PackageConf
  ( BasePackageInfo (..),
    parseBasePackageConf,
    splitExposedModules,
    resolvePkgroot,
  )
where

import Data.Char (isSpace)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import System.FilePath ((</>))

data BasePackageInfo = BasePackageInfo
  { basePackageId :: !Text,
    basePackageVersion :: !Text,
    baseExposedModules :: ![Text],
    baseImportDirs :: ![FilePath]
  }
  deriving (Eq, Show)

parseBasePackageConf :: FilePath -> Text -> Either String BasePackageInfo
parseBasePackageConf pkgroot raw = do
  let fields = parseFields raw
      requiredField key =
        maybe
          (Left ("missing field: " <> T.unpack key))
          Right
          (Map.lookup key fields)
  name <- requiredField "name"
  if name /= "base"
    then Left ("expected package name 'base', got " <> T.unpack name)
    else do
      packageId <- requiredField "id"
      version <- requiredField "version"
      exposedModules <- splitExposedModules <$> requiredField "exposed-modules"
      importDirsField <- requiredField "import-dirs"
      let importDirs =
            [ resolvePkgroot pkgroot (T.unpack (T.strip dir))
            | dir <- T.splitOn "," importDirsField,
              not (T.null (T.strip dir))
            ]
      pure
        BasePackageInfo
          { basePackageId = packageId,
            basePackageVersion = version,
            baseExposedModules = exposedModules,
            baseImportDirs = importDirs
          }

parseFields :: Text -> Map Text Text
parseFields source =
  finalize (foldl' step (Nothing, mempty) (T.lines source))
  where
    step (Nothing, acc) line
      | T.null (T.strip line) = (Nothing, acc)
      | isIndented line = (Nothing, acc)
      | otherwise =
          case T.breakOn ":" line of
            (key, rest)
              | T.null rest -> (Nothing, acc)
              | otherwise -> (Just (T.strip key), Map.insert (T.strip key) (T.strip (T.drop 1 rest)) acc)
    step (Just current, acc) line
      | T.null line = (Just current, acc)
      | isIndented line =
          let appended = Map.findWithDefault "" current acc <> "\n" <> T.strip line
           in (Just current, Map.insert current appended acc)
      | otherwise =
          case T.breakOn ":" line of
            (key, rest)
              | T.null rest -> (Nothing, acc)
              | otherwise -> (Just (T.strip key), Map.insert (T.strip key) (T.strip (T.drop 1 rest)) acc)

    finalize (_, acc) = acc

    isIndented line =
      case T.uncons line of
        Just (c, _) -> isSpace c
        Nothing -> False

splitExposedModules :: Text -> [Text]
splitExposedModules txt =
  [ stripModuleQualification (T.strip modu)
  | modu <- T.splitOn "," (T.replace "\n" " " txt),
    not (T.null (T.strip modu))
  ]

stripModuleQualification :: Text -> Text
stripModuleQualification modu =
  case T.breakOn " from " modu of
    (name, _) -> name

resolvePkgroot :: FilePath -> FilePath -> FilePath
resolvePkgroot pkgroot rawPath =
  case stripPrefix "${pkgroot}" rawPath of
    Just suffix -> pkgroot </> dropWhile (== '/') suffix
    Nothing -> rawPath
  where
    stripPrefix prefix path =
      if prefix == take (length prefix) path
        then Just (drop (length prefix) path)
        else Nothing
