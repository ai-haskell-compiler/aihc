-- | The @Paths_@ module that Cabal generates for a package.
--
-- A package that lists @Paths_<name>@ in its modules gets a module with its
-- version and its installation directories. The directories are the ones
-- that Cabal gives to a global installation under @/usr/local@ for the
-- emulated GHC release. An environment variable such as @<name>_datadir@
-- replaces each directory at run time, as in Cabal.
module Aihc.Hackage.PathsModule
  ( pathsModuleName,
    generatePathsModule,
  )
where

import Aihc.Hackage.Package (Arch, OS, Version, archName, osName, showVersion, versionToList)
import Data.List (intercalate)

-- | The name of the @Paths_@ module of a package.
pathsModuleName :: String -> String
pathsModuleName packageName = "Paths_" <> map underscore packageName

underscore :: Char -> Char
underscore character = if character == '-' then '_' else character

-- | The source of the @Paths_@ module of one component. The unit name
-- identifies the component in the library directory, as Cabal does.
generatePathsModule ::
  -- | The platform, which appears in the directory names.
  (OS, Arch) ->
  -- | The major and minor version of the emulated compiler.
  (Int, Int) ->
  -- | The package name.
  String ->
  Version ->
  -- | The unit name of the component.
  String ->
  String
generatePathsModule (os, arch) (compilerMajor, compilerMinor) packageName version unitName =
  unlines $
    [ "{-# LANGUAGE NoRebindableSyntax #-}",
      "{-# OPTIONS_GHC -w #-}",
      "module " <> pathsModuleName packageName,
      "  ( version,",
      "    getBinDir,",
      "    getLibDir,",
      "    getDynLibDir,",
      "    getLibexecDir,",
      "    getDataFileName,",
      "    getDataDir,",
      "    getSysconfDir",
      "  ) where",
      "",
      "import qualified Control.Exception as Exception",
      "import qualified Data.List as List",
      "import Data.Version (Version(..))",
      "import System.Environment (getEnv)",
      "import Prelude",
      "",
      "catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a",
      "catchIO = Exception.catch",
      "",
      "version :: Version",
      "version = Version [" <> intercalate "," (map show (versionToList version)) <> "] []",
      "",
      "getDataFileName :: FilePath -> IO FilePath",
      "getDataFileName name = do",
      "  dir <- getDataDir",
      "  return (dir `joinFileName` name)"
    ]
      <> concatMap directory directories
      <> [ "",
           "joinFileName :: String -> String -> FilePath",
           "joinFileName \"\"  fname = fname",
           "joinFileName \".\" fname = fname",
           "joinFileName dir \"\"    = dir",
           "joinFileName dir fname",
           "  | isPathSeparator (List.last dir) = dir ++ fname",
           "  | otherwise                       = dir ++ pathSeparator : fname",
           "",
           "pathSeparator :: Char",
           "pathSeparator = '/'",
           "",
           "isPathSeparator :: Char -> Bool",
           "isPathSeparator c = c == '/'"
         ]
  where
    prefix = "/usr/local"
    platform = archName arch <> "-" <> osName os <> "-ghc-" <> show compilerMajor <> "." <> show compilerMinor
    packageId = packageName <> "-" <> showVersion version
    directories =
      [ ("bindir", "getBinDir", prefix <> "/bin"),
        ("libdir", "getLibDir", prefix <> "/lib/" <> platform <> "/" <> unitName),
        ("dynlibdir", "getDynLibDir", prefix <> "/lib/" <> platform),
        ("datadir", "getDataDir", prefix <> "/share/" <> platform <> "/" <> packageId),
        ("libexecdir", "getLibexecDir", prefix <> "/libexec/" <> platform <> "/" <> packageId),
        ("sysconfdir", "getSysconfDir", prefix <> "/etc")
      ]
    directory (variable, getter, path) =
      [ "",
        variable <> " :: FilePath",
        variable <> " = " <> show path,
        getter <> " :: IO FilePath",
        getter <> " = catchIO (getEnv " <> show (map underscore packageName <> "_" <> variable) <> ") (\\_ -> return " <> variable <> ")"
      ]
