{-# LANGUAGE ScopedTypeVariables #-}
-- |Functions for dealing with source and binary packages in an abstract-way
module Debian.Apt.Package where

-- Standard GHC Modules

import qualified Data.Map as Map

-- Local Modules

import Debian.Version
import Debian.Relation

type PackageNameMap a = Map.Map BinPkgName [a]

-- |'packageNameMap' creates a map from a package name to all the versions of that package
-- NOTE: Provides are not included in the map
-- NOTE: the sort order is random -- this is perhaps a bug
-- see also: 'addProvides'
packageNameMap :: (a -> BinPkgName) -> [a] -> PackageNameMap a
packageNameMap getName packages = foldl (\m p -> Map.insertWith (++) (getName p) [p] m) Map.empty packages

-- |'addProvides' finds packages that Provide other packages and adds
-- them to the PackageNameMap. They will be adde to the end of the
-- list, so that real packages have 'higher priority' than virtual
-- packages.
-- NOTE: Does not check for duplication or multiple use
addProvides :: (p -> [BinPkgName]) -> [p] -> PackageNameMap p -> PackageNameMap p
addProvides providesf ps pnm =
    let provides = findProvides providesf ps in
    foldl (\m (packageName, package) -> Map.insertWith (flip (++)) packageName [package] m) pnm provides

-- |'findProvides'
findProvides :: forall p. (p -> [BinPkgName]) -> [p] -> [(BinPkgName, p)]
findProvides providesf packages = foldl addProvides' [] packages
    where addProvides' :: [(BinPkgName, p)] -> p -> [(BinPkgName, p)]
          addProvides' providesList package =
              foldl (\pl pkgName -> (pkgName, package): pl) providesList (providesf package)

-- |'lookupPackageByRel' returns all the packages that satisfy the specified relation
-- TODO: Add architecture check
lookupPackageByRel :: PackageNameMap a -> (a -> (BinPkgName, DebianVersion)) -> Relation -> [a]
lookupPackageByRel pm packageVersionF (Rel pkgName mVerReq _mArch) =
    case Map.lookup pkgName pm of
      Nothing -> []
      Just packages -> filter filterVer packages
    where filterVer p =
              case mVerReq of
                Nothing -> True
                Just _verReq ->
                    let (pName, pVersion) = packageVersionF p
                    in if pName /= pkgName
                       then False -- package is a virtual package, hence we can not do a version req
                       else checkVersionReq mVerReq (Just pVersion)
