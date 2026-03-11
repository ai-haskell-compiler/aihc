# Changelog

`simple-cabal` uses [PVP Versioning](https://pvp.haskell.org)

## 0.1.3 (2020-10-04)
- add allLibraries (from Cabal>= 2.0) and Library
- deprecate allBuildInfo

## 0.1.2 (2020-05-23)
- readFinalPackageDescription deprecates finalPackageDescription
- add makeFinalPackageDescription
- add parseFinalPackageDescription for Cabal-2.2+
- export simpleParse
- export hasExes and hasLibs

## 0.1.1 (2019-10-05)
- fix tryFindPackageDesc on Cabal 3

## 0.1.0 (2019-09-30)
- add buildDependencies, setupDependencies, testsuiteDependencies
  (from cabal-rpm), and tryFindPackageDesc
- more deps compatibility: showVersion, depPkgName, exeDepName, pkgcfgDepName
- export PackageName, mkPackageName, unPackageName,
  allBuildInfo, BuildInfo, FlagName & mkFlagName, licenseFiles
- no longer export: normal
- support Cabal-3 (ghc-8.8.1)

## 0.0.0.1 (2019-07-09)
- allow building with Cabal 1!

## 0.0.0 (2019-06-08)
- Initially version, mostly taken from cabal-rpm PackageUtils.hs
