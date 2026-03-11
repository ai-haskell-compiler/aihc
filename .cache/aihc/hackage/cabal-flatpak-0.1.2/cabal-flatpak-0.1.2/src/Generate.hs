module Generate where

import qualified Package
import qualified Flatpak
import Retrieve (formatVersion, archGHC, ghcDirUrl, ghcArchive)
import Flatpak (ModuleItem(ModuleEmbed))

import qualified Cabal.Plan as Plan
import qualified Shell.Utility.Quote as Quote

import qualified Data.Map as Map
import qualified Data.Text as Text
import Data.Foldable (foldMap)
import Data.Map (Map)
import Data.Bool.HT (if')

import Text.Printf (printf)


tinfoPath :: Flatpak.Arch -> String
tinfoPath arch =
   case arch of
      Flatpak.I386 -> "/usr/lib/i386-linux-gnu/libtinfo.so"
      Flatpak.X86_64 -> "/usr/lib/x86_64-linux-gnu/libtinfo.so"
      Flatpak.ARM -> "/lib/arm-linux-gnueabihf/libtinfo.so"
      -- FixMe: What is the correct path?
      Flatpak.AArch64 -> "/lib/arm???-linux-gnueabihf/libtinfo.so"


projectMain :: Flatpak.Cabal -> (String, [Flatpak.Source])
projectMain project =
   (Flatpak.mainPackage project, Flatpak.mainSources project)

matchName :: String -> Plan.Unit -> Bool
matchName expected unit =
   case Plan.uPId unit of
      Plan.PkgId (Plan.PkgName name) _version -> expected == Text.unpack name


type Options = ([String], [String])

projectOptions :: Flatpak.Cabal -> Options
projectOptions project =
   (Flatpak.cabalConfigureOptions project, Flatpak.ghcOptions project)


ghcPkgCleanup :: Plan.PlanJson -> Flatpak.Arch -> [String]
ghcPkgCleanup plan arch =
   let archLinux =
         archGHC arch ++ "-linux-" ++
         Text.unpack (Plan.dispPkgId (Plan.pjCompilerId plan))
   in map (++ archLinux) $
         "/lib/" :
         "/share/doc/" :
         []

ghcCleanup :: Plan.PlanJson -> [String]
ghcCleanup plan =
   let (Plan.PkgId _ ghcVer) = Plan.pjCompilerId plan
       withVersion prg = prg ++ "-" ++ formatVersion ghcVer
   in map ("/" ++) $
         concat [
            map ("bin/" ++) $
               "ghc" : withVersion "ghc" :
               "ghc-pkg" : withVersion "ghc-pkg" :
               "ghci" : withVersion "ghci" :
               "haddock" : withVersion "haddock-ghc" :
               "runghc" : withVersion "runghc" :
               "runhaskell" :
               "hp2ps" :
               "hpc" :
               "hsc2hs" :
               [],
            map ("lib/" ++) $
               "libtinfo.so.5" :
               "debug" :
               withVersion "ghc" :
               [],
            map ("share/" ++) $
               "man" :
               ("doc/" ++ withVersion "ghc") :
               []
            ]

-- | Generate the module to download, compile and install GHC.
ghcModule :: Plan.PlanJson -> Flatpak.Arch -> Plan.Sha256 -> Flatpak.Module
ghcModule plan arch hash =
   let compiler = Plan.pjCompilerId plan
   in Flatpak.Module {
         Flatpak.name =
            Text.unpack (Plan.dispPkgId compiler) ++
               "-" ++ Flatpak.archString arch,
         Flatpak.onlyArches = [arch],
         Flatpak.buildsystem = "simple",
         Flatpak.builddir = False,
         Flatpak.configOpts = [],
         Flatpak.buildCommands =
            "mkdir -p /app/lib" :
            printf "ln -s %s /app/lib/libtinfo.so.5" (tinfoPath arch) :
            "./configure --prefix=/app" :
            "make install" :
            [],
         Flatpak.postInstall = [],
         Flatpak.cleanupModule = ghcCleanup plan,
         Flatpak.sources =
            [Flatpak.Source {
               Flatpak.typ = Flatpak.archive,
               Flatpak.url =
                  Just $
                  ghcDirUrl compiler ++ ghcArchive compiler arch,
               Flatpak.destFilename = Nothing,
               Flatpak.sha256 = Just hash
             }]
      }

type RevisedUnit = (Plan.Unit, Package.Revision)

packageURLs :: RevisedUnit -> (String, String, String)
packageURLs (pkg, revision) =
   let pkgId@(Plan.PkgId (Plan.PkgName name) _version) = Plan.uPId pkg
       pkgName = Text.unpack name
       pkgNameVer = Text.unpack $ Plan.dispPkgId pkgId
   in (printf "https://hackage.haskell.org/package/%s/%s.tar.gz"
         pkgNameVer pkgNameVer,
       printf "https://hackage.haskell.org/package/%s/revision/%d.cabal"
         pkgNameVer revision,
       printf "%s.cabal" pkgName)

packageSources ::
   (String, [Flatpak.Source]) -> Flatpak.SourceType ->
   RevisedUnit -> [Flatpak.Source]
packageSources (mainPkg, mainSrcs) typ pkgRev@(pkg,_) =
   let (pkgUrl, cabalUrl, cabalPath) = packageURLs pkgRev
   in if' (matchName mainPkg pkg && not (null mainSrcs)) mainSrcs $
      Flatpak.Source {
         Flatpak.typ = typ,
         Flatpak.url = Just pkgUrl,
         Flatpak.destFilename = Nothing,
         Flatpak.sha256 = Plan.uSha256 pkg
      } :
      Flatpak.Source {
         Flatpak.typ = Flatpak.File,
         Flatpak.url = Just cabalUrl,
         Flatpak.destFilename = Just cabalPath,
         Flatpak.sha256 = Plan.uCabalSha256 pkg
      } :
      []

ghcOption :: String -> String
ghcOption opt = "--ghc-option=" ++ Quote.minimal opt

{- |
Generate a module to download, build and install a package from Hackage.
Due to the Cabal package "revision" not being part of a package,
we have to download the package,
extract and then overwrite the .cabal file with the latest revision.
-}
modul ::
   (String, [Flatpak.Source]) -> Options -> Map String Flatpak.PackageHook ->
   RevisedUnit -> Flatpak.Module
modul main (cabalCfgOptions, ghcOptions) hookMap pkgRev@(pkg,_) =
   let (Plan.PkgId (Plan.PkgName name) _version) = Plan.uPId pkg
       nameStr = Text.unpack name
   in Flatpak.Module {
         Flatpak.name = nameStr,
         Flatpak.onlyArches = [],
         Flatpak.buildsystem = "simple",
         Flatpak.builddir = False,
         Flatpak.configOpts = [],
         Flatpak.buildCommands =
            "echo '#! /usr/bin/env runhaskell' >Setup.txt" :
            "echo '> import Distribution.Simple' >>Setup.txt" :
            "echo '> main = defaultMain' >>Setup.txt" :
            "ln -s Setup.txt Setup.lhs || true" :
            unwords ("runhaskell Setup configure --prefix=/app" :
                     cabalCfgOptions) :
            unwords ("runhaskell Setup build" : map ghcOption ghcOptions) :
            "runhaskell Setup install" :
            [],
         Flatpak.postInstall =
            foldMap Flatpak.postInstallHook $ Map.lookup nameStr hookMap,
         Flatpak.cleanupModule = [],
         Flatpak.sources = packageSources main Flatpak.archive pkgRev
      }

manifest ::
   Plan.PlanJson ->
   [(Flatpak.Arch, Plan.Sha256)] -> [RevisedUnit] -> Flatpak.Cabal -> Flatpak.T
manifest plan archs units project =
   let base = Flatpak.base project
       hookMap =
         Map.fromList $ map (\hook -> (Flatpak.nameHook hook, hook)) $
         Flatpak.packageHooks project
   in
   base {
      Flatpak.cleanup =
         concatMap (ghcPkgCleanup plan . fst) archs ++ Flatpak.cleanup base,
      Flatpak.modules =
         map (ModuleEmbed . uncurry (ghcModule plan)) archs ++
         Flatpak.modules base ++
         map
            (ModuleEmbed .
             modul (projectMain project) (projectOptions project) hookMap)
            units
   }



cabalHash :: Flatpak.Arch -> Maybe String
cabalHash arch =
   case arch of
      Flatpak.I386 -> Just $
         "b2da736cc27609442b10f77fc1a687aba603a7a33045b722dbf1a0066fade198"
      Flatpak.X86_64 -> Just $
         "6136c189ffccaa39916f9cb5788f757166444a2d0c473b987856a79ecbf0c714"
      _ -> Nothing

cabalInstallModule :: Flatpak.Arch -> Flatpak.Module
cabalInstallModule arch =
   let cabalVerName = "cabal-install-2.4.1.0"
       url =
         printf
            "https://downloads.haskell.org/cabal/%s/%s-%s-unknown-linux.tar.xz"
               cabalVerName cabalVerName (archGHC arch)
   in Flatpak.Module {
         Flatpak.name = cabalVerName ++ "-" ++ Flatpak.archString arch,
         Flatpak.onlyArches = [arch],
         Flatpak.buildsystem = "simple",
         Flatpak.builddir = False,
         Flatpak.configOpts = [],
         Flatpak.buildCommands = ["install cabal /app/bin"],
         Flatpak.postInstall = [],
         Flatpak.cleanupModule = ["/bin/cabal"],
         Flatpak.sources =
            [Flatpak.Source {
               Flatpak.typ =
                  Flatpak.Archive {Flatpak.stripComponents = 0},
               Flatpak.url = Just url,
               Flatpak.destFilename = Nothing,
               Flatpak.sha256 = Plan.parseSha256 . Text.pack =<< cabalHash arch
             }]
      }

reviseCabalScript :: [String]
reviseCabalScript =
   "tarball=\"$1\"" :
   "pkgid=\"$(basename $tarball .tar.gz)\"" :
   "name=\"$(echo $pkgid | sed -r 's:^(.*)-(.*)$:\\1:')\"" :
   "ver=\"$(echo $pkgid | sed -r 's:^(.*)-(.*)$:\\2:')\"" :
   "echo Revising .cabal file for \"$pkgid\"" :
   "gunzip \"$tarball\"" :
   "mkdir \"$pkgid\"" :
   "mv \"$name.cabal\" \"$pkgid\"" :
   "tar rf \"$pkgid.tar\" \"$pkgid/$name.cabal\"" :
   "gzip \"$pkgid.tar\"" :
   []

allInOneModule ::
   (String, [Flatpak.Source]) -> Options -> [String] ->
   [RevisedUnit] -> Flatpak.Module
allInOneModule main (cabalCfgOptions, ghcOptions) postInstall pkgs =
      Flatpak.Module {
         Flatpak.name = "haskell-parts",
         Flatpak.onlyArches = [],
         Flatpak.buildsystem = "simple",
         Flatpak.builddir = False,
         Flatpak.configOpts = [],
         Flatpak.buildCommands =
            "find . -name '*.tar.gz' | while read pkg; do bash revise.sh \"$pkg\"; done" :
            "mkdir .cabal" :
            "touch .cabal/config" :
            unwords
               ("cabal --config-file=.cabal/config install" :
                  "-j$FLATPAK_BUILDER_N_JOBS" :
                  "--offline --prefix=/app" :
                  cabalCfgOptions ++ map ghcOption ghcOptions ++
                  ["*.tar.gz"]) :
            [],
         Flatpak.postInstall = postInstall,
         Flatpak.cleanupModule = [],
         Flatpak.sources =
            Flatpak.Source {
               Flatpak.typ = Flatpak.Script reviseCabalScript,
               Flatpak.url = Nothing,
               Flatpak.destFilename = Just "revise.sh",
               Flatpak.sha256 = Nothing
            } :
            concatMap (packageSources main Flatpak.File) pkgs
      }

manifestCabalInstall ::
   Plan.PlanJson ->
   [(Flatpak.Arch, Plan.Sha256)] -> [RevisedUnit] -> Flatpak.Cabal -> Flatpak.T
manifestCabalInstall plan archs pkgs project =
   let base = Flatpak.base project in
   base {
      Flatpak.cleanup =
         concatMap (ghcPkgCleanup plan . fst) archs ++ Flatpak.cleanup base,
      Flatpak.modules =
         (map ModuleEmbed $
          concatMap
            (\(arch,ghcHash) ->
               [ghcModule plan arch ghcHash, cabalInstallModule arch])
            archs) ++
         Flatpak.modules base ++
         (ModuleEmbed $
          allInOneModule (projectMain project) (projectOptions project)
            (concatMap Flatpak.postInstallHook $ Flatpak.packageHooks project)
            pkgs) :
         []
   }
