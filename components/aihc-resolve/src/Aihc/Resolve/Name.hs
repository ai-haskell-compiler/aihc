{-# LANGUAGE OverloadedStrings #-}

-- | Names and package identities used by name resolution.
--
-- These types are deliberately owned by @aihc-resolve@. Downstream compiler
-- stages must translate them into their own IR-specific identity types.
module Aihc.Resolve.Name
  ( PackageName (..),
    PackageVersion (..),
    DependencyHash (..),
    PackageId (..),
    ModuleName (..),
    ModuleId (..),
    Namespace (..),
    OccName (..),
    GlobalName (..),
    LocalName (..),
    WiredInName (..),
    defaultPackageId,
    moduleId,
    globalName,
    renderPackageId,
    renderModuleId,
    renderGlobalName,
    packageIdComponents,
  )
where

import Data.Text (Text)

newtype PackageName = PackageName {unPackageName :: Text}
  deriving (Eq, Ord, Show, Read)

newtype PackageVersion = PackageVersion {unPackageVersion :: Text}
  deriving (Eq, Ord, Show, Read)

newtype DependencyHash = DependencyHash {unDependencyHash :: Text}
  deriving (Eq, Ord, Show, Read)

data PackageId = PackageId
  { packageName :: !PackageName,
    packageVersion :: !PackageVersion,
    packageDependencyHash :: !DependencyHash
  }
  deriving (Eq, Ord, Show, Read)

newtype ModuleName = ModuleName {unModuleName :: Text}
  deriving (Eq, Ord, Show, Read)

data ModuleId = ModuleId
  { modulePackage :: !PackageId,
    moduleName :: !ModuleName
  }
  deriving (Eq, Ord, Show, Read)

-- | Haskell lookup namespaces. Symbol roles such as function, constructor, or
-- class are metadata; they must not create extra namespaces in which invalid
-- duplicate declarations could coexist.
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

defaultPackageId :: PackageId
defaultPackageId =
  PackageId
    { packageName = PackageName "main",
      packageVersion = PackageVersion "0",
      packageDependencyHash = DependencyHash "local"
    }

moduleId :: PackageId -> Text -> ModuleId
moduleId packageId name = ModuleId packageId (ModuleName name)

globalName :: ModuleId -> Namespace -> Text -> GlobalName
globalName owner namespace name = GlobalName owner namespace (OccName name)

renderPackageId :: PackageId -> Text
renderPackageId packageId' =
  unPackageName (packageName packageId')
    <> "-"
    <> unPackageVersion (packageVersion packageId')
    <> "["
    <> unDependencyHash (packageDependencyHash packageId')
    <> "]"

renderModuleId :: ModuleId -> Text
renderModuleId moduleId' = renderPackageId (modulePackage moduleId') <> ":" <> unModuleName (moduleName moduleId')

renderGlobalName :: GlobalName -> Text
renderGlobalName name = renderModuleId (globalModule name) <> "." <> unOccName (globalOccName name)

packageIdComponents :: PackageId -> [Text]
packageIdComponents packageId' =
  [ unPackageName (packageName packageId'),
    unPackageVersion (packageVersion packageId'),
    unDependencyHash (packageDependencyHash packageId')
  ]
