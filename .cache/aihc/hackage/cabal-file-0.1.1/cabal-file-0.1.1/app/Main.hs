{-# LANGUAGE CPP #-}

import Data.Version.Extra (readVersion)
import Distribution.Package
import Distribution.Text (simpleParse)
import Distribution.Version
#if MIN_VERSION_simple_cmd_args(0,1,7)
#else
import Options.Applicative
#endif
import SimpleCmdArgs

import Cmds
import Depends

main :: IO ()
main =
  simpleCmdArgs Nothing "Cabal file tool"
    "cabal-file can access and compare .cabal files from the cache" $
    subcommands
    [ Subcommand "diff" "Diff .cabal files of package versions" $
      diffCmd <$> switchWith 'W' "whitespace" "Show whitespace changes" <*> strArg "PKG" <*> versionArg <*> versionArg
    , Subcommand "list" "List package versions" $
      listPkg <$> optional pkgArg
    , Subcommand "latest" "Latest package version" $
      latestPkg <$> pkgArg
    , Subcommand "files" "List all index files" $
      pure listFiles
    , Subcommand "get" "Get .cabal file for package version" $
      saveCabal <$> pkgIdArg
    , Subcommand "date" "Timestamp for package version (revision)" $
      dateCabal <$> pkgIdArg
    , Subcommand "show" "Show .cabal file for package version" $
      showCabal <$> pkgIdArg
    , Subcommand "metadata" "Show metadata for package version" $
      showMetadata <$> pkgIdArg
    , Subcommand "preferred" "Show preferred versions for package" $
      preferCmd <$> pkgArg
    , Subcommand "depends" "Print package dependencies" $
      dependsCmd <$> switchWith 'q' "quiet" "No depends section headers"
      <*> detailOpt
      <*> optional depsOpt
      <*> optional (strArg "DIRorCABAL")
    ]
  where
    versionArg :: Parser Version
    versionArg = argumentWith versionM "VERSION"

    versionM = maybeReader (Just . mkVersion' . readVersion)

    pkgIdArg :: Parser PackageIdentifier
    pkgIdArg = argumentWith (maybeReader simpleParse) "PKG[VER]"

    pkgArg :: Parser PackageName
    pkgArg = argumentWith (maybeReader simpleParse) "PKG"

    detailOpt :: Parser Details
    detailOpt =
      flagWith' Detail 'd' "detail" "Show version ranges" <|>
      flagWith Normal Unique 'u' "unique" "Remove self and build-depends from other depends"

    depsOpt :: Parser Deps
    depsOpt =
      flagWith' Build 'b' "build" "Only build-depends" <|>
      flagWith' Setup 's' "setup" "Only setup-depends" <|>
      flagWith' Tool 't' "tool" "Only tool-build-depends" <|>
      flagWith' Legacy 'l' "legacy" "Only build-tools" <|>
      flagWith' CLib 'c' "clibs" "Only extra-libraries" <|>
      flagWith' PkgConfig 'p' "pkgconfig" "Only extra-libraries" <|>
      flagWith' NotBuild 'B' "not-build" "Hide build-depends"
