-- | Type-checker-owned semantic identities.
--
-- Conversion from resolver identities happens at the TC boundary. The type
-- checker never stores resolver-owned names in its environments or output.
module Aihc.Tc.Name
  ( PackageId (..),
    ModuleId (..),
    Namespace (..),
    OccName (..),
    GlobalName (..),
    LocalName (..),
    WiredInName (..),
    ResolvedId (..),
    globalName,
    fromResolverModuleId,
    fromResolverGlobalName,
    fromResolverLocalName,
    fromResolverWiredInName,
  )
where

import Aihc.Resolve.Name qualified as Resolve
import Data.Text (Text)

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

fromResolverPackageId :: Resolve.PackageId -> PackageId
fromResolverPackageId packageId =
  PackageId
    { packageName = Resolve.unPackageName (Resolve.packageName packageId),
      packageVersion = Resolve.unPackageVersion (Resolve.packageVersion packageId),
      packageDependencyHash = Resolve.unDependencyHash (Resolve.packageDependencyHash packageId)
    }

fromResolverModuleId :: Resolve.ModuleId -> ModuleId
fromResolverModuleId owner =
  ModuleId
    { modulePackage = fromResolverPackageId (Resolve.modulePackage owner),
      moduleName = Resolve.unModuleName (Resolve.moduleName owner)
    }

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
    { globalModule = fromResolverModuleId (Resolve.globalModule name),
      globalNamespace = fromResolverNamespace (Resolve.globalNamespace name),
      globalOccName = OccName (Resolve.unOccName (Resolve.globalOccName name))
    }

fromResolverLocalName :: Resolve.LocalName -> LocalName
fromResolverLocalName name =
  LocalName
    { localModule = fromResolverModuleId (Resolve.localModule name),
      localUnique = Resolve.localUnique name,
      localNamespace = fromResolverNamespace (Resolve.localNamespace name),
      localOccName = OccName (Resolve.unOccName (Resolve.localOccName name))
    }

fromResolverWiredInName :: Resolve.WiredInName -> WiredInName
fromResolverWiredInName name =
  WiredInName
    { wiredInNamespace = fromResolverNamespace (Resolve.wiredInNamespace name),
      wiredInOccName = OccName (Resolve.unOccName (Resolve.wiredInOccName name))
    }
