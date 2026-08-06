{-# LANGUAGE OverloadedStrings #-}

-- | System-FC-owned semantic identities.
--
-- Desugaring translates resolver and type-checker identities into these
-- values. System FC transformations therefore depend only on FC concepts.
module Aihc.Fc.Name
  ( PackageId (..),
    ModuleId (..),
    Namespace (..),
    OccName (..),
    GlobalName (..),
    LocalName (..),
    WiredInName (..),
    ResolvedId (..),
    moduleId,
    globalName,
    fromResolverModuleId,
    fromResolverGlobalName,
    fromResolverLocalName,
    fromResolverWiredInName,
    fromTcResolvedId,
    renderPackageId,
    renderModuleId,
    renderGlobalName,
    renderResolvedId,
  )
where

import Aihc.Resolve.Name qualified as Resolve
import Aihc.Tc.Name qualified as Tc
import Data.Text (Text)
import Data.Text qualified as T

data PackageId = PackageId
  { packageName :: !Text,
    packageVersion :: !Text,
    packageDependencyHash :: !Text
  }
  deriving (Eq, Ord, Show, Read)

data ModuleId = ModuleId
  { modulePackage :: !PackageId,
    moduleName :: !Text
  }
  deriving (Eq, Ord, Show, Read)

data Namespace
  = TermNamespace
  | TypeNamespace
  | FieldNamespace
  | ModuleNamespace
  | AxiomNamespace
  deriving (Eq, Ord, Show, Read)

newtype OccName = OccName {unOccName :: Text}
  deriving (Eq, Ord, Show, Read)

data GlobalName = GlobalName
  { globalModule :: !ModuleId,
    globalNamespace :: !Namespace,
    globalOccName :: !OccName
  }
  deriving (Eq, Ord, Show, Read)

data LocalName = LocalName
  { localModule :: !ModuleId,
    localUnique :: !Int,
    localNamespace :: !Namespace,
    localOccName :: !OccName
  }
  deriving (Eq, Ord, Show, Read)

data WiredInName = WiredInName
  { wiredInNamespace :: !Namespace,
    wiredInOccName :: !OccName
  }
  deriving (Eq, Ord, Show, Read)

data ResolvedId
  = ResolvedGlobal !GlobalName
  | ResolvedLocal !LocalName
  | ResolvedWiredIn !WiredInName
  deriving (Eq, Ord, Show, Read)

globalName :: ModuleId -> Namespace -> Text -> GlobalName
globalName owner namespace name = GlobalName owner namespace (OccName name)

moduleId :: PackageId -> Text -> ModuleId
moduleId = ModuleId

fromResolverPackageId :: Resolve.PackageId -> PackageId
fromResolverPackageId packageId =
  PackageId
    { packageName = Resolve.unPackageName (Resolve.packageName packageId),
      packageVersion = Resolve.unPackageVersion (Resolve.packageVersion packageId),
      packageDependencyHash = Resolve.unDependencyHash (Resolve.packageDependencyHash packageId)
    }

fromResolverModuleId :: Resolve.ModuleId -> ModuleId
fromResolverModuleId owner = ModuleId (fromResolverPackageId (Resolve.modulePackage owner)) (Resolve.unModuleName (Resolve.moduleName owner))

fromResolverNamespace :: Resolve.Namespace -> Namespace
fromResolverNamespace namespace =
  case namespace of
    Resolve.TermNamespace -> TermNamespace
    Resolve.TypeNamespace -> TypeNamespace
    Resolve.FieldNamespace -> FieldNamespace
    Resolve.ModuleNamespace -> ModuleNamespace
    Resolve.AxiomNamespace -> AxiomNamespace

fromResolverGlobalName :: Resolve.GlobalName -> GlobalName
fromResolverGlobalName name =
  GlobalName
    (fromResolverModuleId (Resolve.globalModule name))
    (fromResolverNamespace (Resolve.globalNamespace name))
    (OccName (Resolve.unOccName (Resolve.globalOccName name)))

fromResolverLocalName :: Resolve.LocalName -> LocalName
fromResolverLocalName name =
  LocalName
    (fromResolverModuleId (Resolve.localModule name))
    (Resolve.localUnique name)
    (fromResolverNamespace (Resolve.localNamespace name))
    (OccName (Resolve.unOccName (Resolve.localOccName name)))

fromResolverWiredInName :: Resolve.WiredInName -> WiredInName
fromResolverWiredInName name =
  WiredInName
    (fromResolverNamespace (Resolve.wiredInNamespace name))
    (OccName (Resolve.unOccName (Resolve.wiredInOccName name)))

fromTcPackageId :: Tc.PackageId -> PackageId
fromTcPackageId packageId = PackageId (Tc.packageName packageId) (Tc.packageVersion packageId) (Tc.packageDependencyHash packageId)

fromTcModuleId :: Tc.ModuleId -> ModuleId
fromTcModuleId owner = ModuleId (fromTcPackageId (Tc.modulePackage owner)) (Tc.moduleName owner)

fromTcNamespace :: Tc.Namespace -> Namespace
fromTcNamespace namespace =
  case namespace of
    Tc.TermNamespace -> TermNamespace
    Tc.TypeNamespace -> TypeNamespace
    Tc.FieldNamespace -> FieldNamespace
    Tc.ModuleNamespace -> ModuleNamespace
    Tc.AxiomNamespace -> AxiomNamespace

fromTcGlobalName :: Tc.GlobalName -> GlobalName
fromTcGlobalName name = GlobalName (fromTcModuleId (Tc.globalModule name)) (fromTcNamespace (Tc.globalNamespace name)) (OccName (Tc.unOccName (Tc.globalOccName name)))

fromTcLocalName :: Tc.LocalName -> LocalName
fromTcLocalName name = LocalName (fromTcModuleId (Tc.localModule name)) (Tc.localUnique name) (fromTcNamespace (Tc.localNamespace name)) (OccName (Tc.unOccName (Tc.localOccName name)))

fromTcWiredInName :: Tc.WiredInName -> WiredInName
fromTcWiredInName name = WiredInName (fromTcNamespace (Tc.wiredInNamespace name)) (OccName (Tc.unOccName (Tc.wiredInOccName name)))

fromTcResolvedId :: Tc.ResolvedId -> ResolvedId
fromTcResolvedId identity =
  case identity of
    Tc.ResolvedGlobal name -> ResolvedGlobal (fromTcGlobalName name)
    Tc.ResolvedLocal name -> ResolvedLocal (fromTcLocalName name)
    Tc.ResolvedWiredIn name -> ResolvedWiredIn (fromTcWiredInName name)

renderPackageId :: PackageId -> Text
renderPackageId packageId = packageName packageId <> "-" <> packageVersion packageId <> "[" <> packageDependencyHash packageId <> "]"

renderModuleId :: ModuleId -> Text
renderModuleId owner = renderPackageId (modulePackage owner) <> ":" <> moduleName owner

renderGlobalName :: GlobalName -> Text
renderGlobalName name = renderModuleId (globalModule name) <> "." <> unOccName (globalOccName name)

renderResolvedId :: ResolvedId -> Text
renderResolvedId identity =
  case identity of
    ResolvedGlobal name -> renderGlobalName name
    ResolvedLocal name -> renderModuleId (localModule name) <> "." <> unOccName (localOccName name) <> "#" <> T.pack (show (localUnique name))
    ResolvedWiredIn name -> "<wired-in>:" <> unOccName (wiredInOccName name)
