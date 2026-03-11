
-- | A data type representing the Bower.json package description file, together
-- with a parser and related functions.
--
-- This code is based on the specification at
-- <https://github.com/bower/bower.json-spec>.

module Web.Bower.PackageMeta
  (
  -- * Data types
  PackageMeta(..)
  , PackageName
  , runPackageName
  , mkPackageName
  , Author(..)
  , ModuleType(..)
  , moduleTypes
  , Repository(..)
  , Version(..)
  , VersionRange(..)
  , BowerError(..)
  , showBowerError
  , PackageNameError(..)
  , showPackageNameError
  -- * Parsing
  , decodeFile
  , displayError
  , asPackageMeta
  , parseModuleType
  , parsePackageName
  , asAuthor
  , asRepository
  ) where

import Web.Bower.PackageMeta.Internal
