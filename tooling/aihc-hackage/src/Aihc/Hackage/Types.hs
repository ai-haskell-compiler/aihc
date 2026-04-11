-- | Core types for Hackage package identification.
module Aihc.Hackage.Types
  ( PackageSpec (..),
    formatPackage,
  )
where

-- | A package specification: name and version.
data PackageSpec = PackageSpec
  { pkgName :: !String,
    pkgVersion :: !String
  }
  deriving (Eq, Show)

-- | Format a package spec as @name-version@.
formatPackage :: PackageSpec -> String
formatPackage spec = pkgName spec ++ "-" ++ pkgVersion spec
