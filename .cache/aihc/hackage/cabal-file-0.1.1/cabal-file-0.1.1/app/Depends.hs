{-# LANGUAGE CPP #-}

module Depends (
  Deps(..),
  Details(..),
  dependsCmd)
where

import Control.Monad (unless, when)
import Data.List (nub, (\\))
import Data.Maybe
import System.Directory
import System.FilePath

import Distribution.PackageDescription (Library(..), exeName, setupDepends)
import Distribution.Pretty
import Distribution.Simple.BuildToolDepends (getAllToolDependencies)
import Distribution.Text (simpleParse)
import Distribution.Types.ExeDependency

import SimpleCabal (allBuildInfo, buildDepends, buildDependencies, buildTools,
                    exeDepName, extraLibs, findCabalFile,
                    finalPackageDescription,
                    mkPackageName, package, PackageDescription(..), PackageName,
                    pkgcfgDepName, pkgconfigDepends, pkgName,
                    setupBuildInfo, setupDependencies)

import Hackage.Index (packageIdOrLatest, withCabalFile)

data Deps = Build | Setup | Tool | Legacy | CLib | PkgConfig | NotBuild
  deriving (Eq, Show)

data Details = Normal | Unique | Detail
  deriving Eq


dependsCmd :: Bool -> Details -> Maybe Deps -> Maybe FilePath -> IO ()
dependsCmd quiet details opt mfile =
  case mfile of
    Nothing -> findCabalFile >>= displayDepends
    Just path -> do
      dir <- doesDirectoryExist path
      if dir
        then setCurrentDirectory path >> findCabalFile >>= displayDepends
        else
        if "cabal" `isExtensionOf` path
        then do
          isfile <- doesFileExist path
          if isfile then displayDepends path
            else error $ path ++ " does not exist"
        else
          case simpleParse path of
            Nothing -> error $ path ++ " not a package identifier"
            Just pkgid -> do
              pkgver <- packageIdOrLatest pkgid
              withCabalFile pkgver displayDepends
  where
    displayDepends :: FilePath -> IO ()
    displayDepends cabal = do
      pkgdesc <- finalPackageDescription [] cabal
      work (details == Detail) (details == Unique) (pkgName (package pkgdesc)) pkgdesc
      where
        work :: Bool -> Bool -> PackageName -> PackageDescription -> IO ()
        work detail unique self pkgdesc = do
          printDeps Build
          printDeps Setup
          printDeps Tool
          printDeps Legacy
          printDeps CLib
          printDeps PkgConfig
          printLib
          printExe
            where
              printDeps :: Deps -> IO ()
              printDeps kind =
                unless (opt == Just NotBuild && kind == Build) $ do
                  let deps = nub $ lens kind
                  unless (null deps) $
                    when (isNothing opt || opt `elem` [Just kind, Just NotBuild]) $ do
                      when (not quiet && (opt /= Just kind || opt == Just NotBuild)) $
                        title $ show kind ++ " depends"
                      mapM_ putStrLn deps

              lens :: Deps -> [String]
              lens Build =
                if detail
                then map prettyShow $ buildDepends pkgdesc
                else map prettyShow $ buildDependencies pkgdesc \\ [self | unique]
              lens Setup =
                if detail
                then map prettyShow $ maybe [] setupDepends (setupBuildInfo pkgdesc)
                else map prettyShow $ setupDependencies pkgdesc \\ (mkPackageName "Cabal" : if unique then buildDependencies pkgdesc else [])
              lens Tool =
                let bts = concatMap (getAllToolDependencies pkgdesc) $
                          allBuildInfo pkgdesc
                in if detail then map prettyShow bts
                   else map (prettyShow . exeDepPkgName) bts \\ ([prettyShow self | unique] ++ ["hsc2hs" | unique])
              lens Legacy =
                let legacy = concatMap buildTools $ allBuildInfo pkgdesc
                in (if detail then map prettyShow legacy
                    else map exeDepName legacy) \\ if unique then "hsc2hs" : prettyShow self : lens Tool else []
              lens CLib = (concatMap extraLibs .  allBuildInfo) pkgdesc
              lens PkgConfig =
                let ds = (concatMap pkgconfigDepends . allBuildInfo) pkgdesc
                in (if detail then map prettyShow
                    else map pkgcfgDepName) ds
              lens NotBuild = error "No lens for NotBuild!"

              printLib :: IO ()
              printLib =
                case library pkgdesc of
                  Nothing -> return ()
                  Just lib -> do
                    title "Library"
                    when detail $ do
                      subtitle "Exposed Modules"
                      mapM_ prettyPut $ exposedModules lib
                      let export = reexportedModules lib
                      unless (null export) $ do
                        subtitle "Re-exported Modules"
                        mapM_ prettyPut export

              printExe :: IO ()
              printExe = do
                let exes = executables pkgdesc
                unless (null exes) $ do
                  title $ "Executable" ++ ['s' | length exes > 1]
                  when detail $
                    mapM_ (prettyPut . exeName) exes

title :: String -> IO ()
title ts =
  putStrLn $ "# " ++ ts

subtitle :: String -> IO ()
subtitle ts =
  putStrLn $ "- " ++ ts

prettyPut :: Pretty a => a -> IO ()
prettyPut = putStrLn . prettyShow

exeDepPkgName :: ExeDependency -> PackageName
exeDepPkgName (ExeDependency n _ _) = n

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif
