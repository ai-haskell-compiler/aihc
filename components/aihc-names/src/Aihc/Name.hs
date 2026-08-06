{-# LANGUAGE OverloadedStrings #-}

-- | Stable, structured identities shared by every compiler phase.
--
-- Rendering is deliberately separate from equality. A short rendering may be
-- ambiguous; the identity never is.
module Aihc.Name
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
    ResolvedId (..),
    defaultPackageId,
    moduleId,
    globalName,
    resolvedOccName,
    resolvedModuleId,
    renderPackageId,
    renderModuleId,
    renderGlobalName,
    renderResolvedId,
    renderLinkName,
    packageIdComponents,
    moduleNameComponents,
  )
where

import Data.Text (Text)
import Data.Text qualified as T

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

data ResolvedId
  = ResolvedGlobal !GlobalName
  | ResolvedLocal !LocalName
  | ResolvedWiredIn !WiredInName
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

resolvedOccName :: ResolvedId -> OccName
resolvedOccName resolved =
  case resolved of
    ResolvedGlobal name -> globalOccName name
    ResolvedLocal name -> localOccName name
    ResolvedWiredIn name -> wiredInOccName name

resolvedModuleId :: ResolvedId -> Maybe ModuleId
resolvedModuleId resolved =
  case resolved of
    ResolvedGlobal name -> Just (globalModule name)
    ResolvedLocal name -> Just (localModule name)
    ResolvedWiredIn _ -> Nothing

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

renderResolvedId :: ResolvedId -> Text
renderResolvedId resolved =
  case resolved of
    ResolvedGlobal name -> renderGlobalName name
    ResolvedLocal name ->
      renderModuleId (localModule name)
        <> "."
        <> unOccName (localOccName name)
        <> "#"
        <> T.pack (show (localUnique name))
    ResolvedWiredIn name -> "<wired-in>:" <> unOccName (wiredInOccName name)

-- | Structured logical linker name. NUL is an encoding delimiter only; it is
-- never used as the semantic identity.
renderLinkName :: GlobalName -> Text
renderLinkName name =
  T.intercalate
    "\0"
    [ unPackageName (packageName packageId'),
      unPackageVersion (packageVersion packageId'),
      unDependencyHash (packageDependencyHash packageId'),
      unModuleName (moduleName owner),
      namespaceTag (globalNamespace name),
      unOccName (globalOccName name)
    ]
  where
    owner = globalModule name
    packageId' = modulePackage owner
    namespaceTag namespace =
      case namespace of
        TermNamespace -> "term"
        TypeNamespace -> "type"
        FieldNamespace -> "field"
        ModuleNamespace -> "module"
        AxiomNamespace -> "axiom"

packageIdComponents :: PackageId -> [Text]
packageIdComponents packageId' =
  [ unPackageName (packageName packageId'),
    unPackageVersion (packageVersion packageId'),
    unDependencyHash (packageDependencyHash packageId')
  ]

moduleNameComponents :: ModuleName -> [Text]
moduleNameComponents = T.splitOn "." . unModuleName
