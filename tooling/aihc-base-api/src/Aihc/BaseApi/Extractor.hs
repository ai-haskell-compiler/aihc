{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Aihc.BaseApi.Extractor
  ( ExtractOptions (..),
    BaseApiSnapshot (..),
    BaseApiModule (..),
    BaseApiExport (..),
    ExportKind (..),
    extractBaseApi,
    extractBaseApiFromPaths,
  )
where

import Aihc.BaseApi.Extractor.Bindist
import Aihc.BaseApi.Extractor.PackageConf
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON)
import Data.List (isPrefixOf, sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import GHC
import GHC.Core.Class (className)
import GHC.Core.ConLike (ConLike (..))
import GHC.Core.DataCon (dataConDisplayType, dataConOrigTyCon)
import GHC.Core.PatSyn (patSynSig)
import GHC.Core.TyCon (isAlgTyCon, isDataFamilyTyCon)
import GHC.Data.FastString (fsLit)
import GHC.Types.Name (nameOccName)
import GHC.Types.Name.Occurrence (occNameString)
import GHC.Utils.Outputable (Outputable, showSDocUnsafe, ppr)
import System.Directory (listDirectory)
import System.FilePath ((</>))
import GHC.Generics (Generic)

data ExtractOptions = ExtractOptions
  { eoGhcVersion :: !Text,
    eoTarget :: !Text,
    eoCacheDir :: !FilePath,
    eoArchiveUrl :: !(Maybe Text)
  }
  deriving (Eq, Show)

data BaseApiSnapshot = BaseApiSnapshot
  { snapshotSchemaVersion :: !Int,
    snapshotGhcVersion :: !Text,
    snapshotBaseVersion :: !Text,
    snapshotPackageId :: !Text,
    snapshotTarget :: !Text,
    snapshotModules :: ![BaseApiModule]
  }
  deriving (Eq, Show, Generic)

instance ToJSON BaseApiSnapshot

data BaseApiModule = BaseApiModule
  { apiModuleName :: !Text,
    apiModuleExports :: ![BaseApiExport]
  }
  deriving (Eq, Show, Generic)

instance ToJSON BaseApiModule

data ExportKind
  = ExportValue
  | ExportConstructor
  | ExportPattern
  | ExportClass
  | ExportType
  | ExportNewtype
  | ExportTypeSynonym
  | ExportTypeFamily
  deriving (Eq, Show, Generic)

instance ToJSON ExportKind

data BaseApiExport = BaseApiExport
  { exportName :: !Text,
    exportModule :: !Text,
    exportKind :: !ExportKind,
    exportSignature :: !(Maybe Text),
    exportParent :: !(Maybe Text)
  }
  deriving (Eq, Show, Generic)

instance ToJSON BaseApiExport

extractBaseApi :: ExtractOptions -> IO BaseApiSnapshot
extractBaseApi options = do
  validateLocalCompilerVersion (eoGhcVersion options)
  prepared <- downloadAndPrepareBindist (eoCacheDir options) (eoGhcVersion options) (eoTarget options) (eoArchiveUrl options)
  extractBaseApiFromPaths (eoGhcVersion options) (eoTarget options) (preparedLibdir prepared) (preparedPackageDb prepared)

extractBaseApiFromPaths :: Text -> Text -> FilePath -> FilePath -> IO BaseApiSnapshot
extractBaseApiFromPaths ghcVersion target libdir packageDb = do
  baseConf <- readBasePackageInfo packageDb
  modules <- extractModules libdir baseConf
  pure
    BaseApiSnapshot
      { snapshotSchemaVersion = 1,
        snapshotGhcVersion = ghcVersion,
        snapshotBaseVersion = basePackageVersion baseConf,
        snapshotPackageId = basePackageId baseConf,
        snapshotTarget = target,
        snapshotModules = sortOn apiModuleName modules
      }

readBasePackageInfo :: FilePath -> IO BasePackageInfo
readBasePackageInfo packageDb = do
  entries <- listDirectory packageDb
  case sortOn id [packageDb </> entry | entry <- entries, "base-" `isPrefixOf` entry, ".conf" `T.isSuffixOf` T.pack entry] of
    [] -> fail ("could not find base package conf under " <> packageDb)
    path : _ -> do
      source <- T.pack <$> readFile path
      either fail pure (parseBasePackageConf packageDb source)

extractModules :: FilePath -> BasePackageInfo -> IO [BaseApiModule]
extractModules libdir baseInfo =
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    _ <- setSessionDynFlags dflags
    forM (baseExposedModules baseInfo) $ \modu -> do
      moduleRef <- findModule (mkModuleName (T.unpack modu)) (Just (fsLit "base"))
      maybeInfo <- getModuleInfo moduleRef
      case maybeInfo of
        Nothing -> liftIO (fail ("failed to load module info for " <> T.unpack modu))
        Just info -> do
          exports <- catMaybes <$> mapM lookupExport (modInfoExports info)
          pure
            BaseApiModule
              { apiModuleName = modu,
                apiModuleExports = sortOn exportName exports
              }

lookupExport :: Name -> Ghc (Maybe BaseApiExport)
lookupExport name = do
  lookupName name >>= \case
    Nothing -> pure Nothing
    Just thing -> pure (Just (toExport thing))

toExport :: TyThing -> BaseApiExport
toExport thing =
  case thing of
    AnId ident ->
      BaseApiExport
        { exportName = T.pack (getOccString ident),
          exportModule = nameModuleText ident,
          exportKind = ExportValue,
          exportSignature = Just (renderType (idType ident)),
          exportParent = T.pack . getOccString . className <$> isClassOpId_maybe ident
        }
    AConLike conLike ->
      case conLike of
        RealDataCon dataCon ->
          BaseApiExport
            { exportName = T.pack (getOccString dataCon),
              exportModule = nameModuleText dataCon,
              exportKind = ExportConstructor,
              exportSignature = Just (renderType (dataConDisplayType True dataCon)),
              exportParent = Just (T.pack (getOccString (dataConOrigTyCon dataCon)))
            }
        PatSynCon patSyn ->
          let (_, reqTheta, _, provTheta, argTys, resultTy) = patSynSig patSyn
              rendered =
                T.intercalate
                  " -> "
                  (map renderType argTys <> [renderContext reqTheta provTheta resultTy])
           in BaseApiExport
                { exportName = T.pack (getOccString patSyn),
                  exportModule = nameModuleText patSyn,
                  exportKind = ExportPattern,
                  exportSignature = Just rendered,
                  exportParent = Nothing
                }
    ATyCon tyCon ->
      BaseApiExport
        { exportName = T.pack (getOccString tyCon),
          exportModule = nameModuleText tyCon,
          exportKind = tyConExportKind tyCon,
          exportSignature = Just (renderType (tyConKind tyCon)),
          exportParent = Nothing
        }
    ACoAxiom axiom ->
      BaseApiExport
        { exportName = T.pack (getOccString axiom),
          exportModule = nameModuleText axiom,
          exportKind = ExportTypeFamily,
          exportSignature = Nothing,
          exportParent = Nothing
        }

tyConExportKind :: TyCon -> ExportKind
tyConExportKind tyCon
  | isClassTyCon tyCon = ExportClass
  | isTypeSynonymTyCon tyCon = ExportTypeSynonym
  | isDataFamilyTyCon tyCon || isOpenFamilyTyCon tyCon || isFamilyTyCon tyCon = ExportTypeFamily
  | isNewTyCon tyCon = ExportNewtype
  | isAlgTyCon tyCon = ExportType
  | otherwise = ExportType

nameModuleText :: NamedThing a => a -> Text
nameModuleText thing =
  T.pack (moduleNameString (moduleName (nameModule (getName thing))))

renderType :: Outputable a => a -> Text
renderType = T.pack . showSDocUnsafe . ppr

getOccString :: NamedThing a => a -> String
getOccString = occNameString . nameOccName . getName

renderContext :: [PredType] -> [PredType] -> Type -> Text
renderContext reqTheta provTheta resultTy =
  let constraints = reqTheta <> provTheta
      body = renderType resultTy
   in if null constraints
        then body
        else T.intercalate ", " (map renderType constraints) <> " => " <> body
