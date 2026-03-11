module Main where

import qualified Distribution.PackageDescription as P
import qualified Distribution.Verbosity as Verbosity
import qualified Distribution.ReadE as ReadE
import Distribution.Simple.PackageDescription (readGenericPackageDescription)
import Distribution.PackageDescription
         (GenericPackageDescription, package, packageDescription)
import Distribution.Types.Dependency (Dependency(Dependency))
import Distribution.Types.PackageName (PackageName, unPackageName)
import Distribution.Types.PackageId (pkgName)

import qualified Options.Applicative as OP
import Shell.Utility.Exit (exitFailureMsg)

import qualified System.FilePath as FilePath
import System.FilePath ((</>))

import qualified Control.Monad.Exception.Synchronous as Exc
import qualified Control.Monad.Trans.Class as Trans
import Control.Arrow ((***))
import Control.Monad (guard, when)
import Control.Applicative (pure, (<*>), (<|>))

import qualified Data.Graph.Comfort as Graph
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Graph.Comfort (Graph)
import Data.Map (Map)
import Data.Maybe (fromMaybe)


main :: IO ()
main = do
   (flags, cabalPaths) <- OP.execParser $ info options
   Exc.resolveT (\e -> exitFailureMsg $ "Aborted: " ++ e) $
      sortCabalFiles flags cabalPaths


data Flags =
   Flags {
      optVerbosity :: Verbosity.Verbosity,
      optInfo :: SourcePackage -> String,
      optOutputFormat :: Format,
      optBuilddir :: FilePath,
      optInstall  :: String
   }

info :: OP.Parser a -> OP.ParserInfo (a, [String])
info p =
   OP.info
      (OP.helper <*>
         OP.liftA2 (,) p (OP.many (OP.strArgument (OP.metavar "CABAL-FILE"))))
      (OP.fullDesc <>
       OP.progDesc
         "Topological sort of Cabal packages according to dependencies.")

infoMap :: Map String (SourcePackage -> String)
infoMap =
   Map.fromList $
      ("name", unPackageName . pkgNameFromDescription . description) :
      ("path", location) :
      ("dir", FilePath.takeDirectory . location) :
      []

data Format = Serial | Parallel | Makefile
   deriving (Eq, Ord, Show, Enum)

options :: OP.Parser Flags
options =
   pure Flags
   <*>
      OP.option
         (OP.eitherReader $ ReadE.runReadE Verbosity.flagToVerbosity)
         (OP.short 'v' <>
          OP.long "verbose" <>
          OP.metavar "N" <>
          OP.value Verbosity.silent <>
          OP.help "verbosity level: 0..3")
   <*>
      OP.option
         (OP.eitherReader $ \str ->
            maybe (Left $ "unknown info type " ++ str) Right $
            Map.lookup str infoMap)
         (OP.long "info" <>
          OP.metavar "KIND" <>
          OP.value location <>
          OP.help ("kind of output: " ++
                     List.intercalate ", " (Map.keys infoMap)))
   <*>
      (OP.flag' Makefile
         (OP.short 'm' <>
          OP.long "makefile" <>
          OP.help "Generate a makefile of package dependencies")
       <|>
       OP.flag Serial Parallel
         (OP.short 'p' <>
          OP.long "parallel" <>
          OP.help "Display independently buildable groups of packages"))
   <*>
      OP.strOption
         (OP.long "builddir" <>
          OP.metavar "PATH" <>
          OP.value "." <>
          OP.help "Specify the build dir to use for generated makefile")
   <*>
      OP.strOption
         (OP.long "install-cmd" <>
          OP.metavar "CMD" <>
          OP.value "cabal v1-install" <>
          OP.help "Specify the install command to use in generated makefile")


data SourcePackage =
   SourcePackage {
      location :: FilePath,
      description :: GenericPackageDescription
   }
   deriving (Show, Eq)

type DependencyGraph = Graph Graph.DirEdge PackageName () SourcePackage

sortCabalFiles :: Flags -> [FilePath] -> Exc.ExceptionalT String IO ()
sortCabalFiles flags cabalPaths =
   do pkgDescs <-
         Trans.lift $
         mapM (readGenericPackageDescription (optVerbosity flags)) cabalPaths
      when (optVerbosity flags >= Verbosity.verbose) $
         Trans.lift $
         Fold.for_ pkgDescs $ \pkgDesc -> do
            putStrLn $ unPackageName (pkgNameFromDescription pkgDesc) ++ ":"
            let deps =
                   Set.toAscList $ Set.fromList $
                   map (unPackageName . depName) $
                   allDependencies pkgDesc
            Fold.for_ deps $ \dep -> putStrLn $ "  " ++ dep
      let graph = dependencyGraph $ zipWith SourcePackage cabalPaths pkgDescs
      let lookupSrcPkg =
            fromMaybe (error "package not found anymore") .
            flip Graph.lookupNode graph
      let (sorted, cyclicPart) = Graph.topologicalSort graph
      when (not $ Graph.isEmpty cyclicPart) $
         Exc.throwT $ unlines $
            "Cycles in dependencies:" : showCycles cyclicPart
      Trans.lift $
         case optOutputFormat flags of
            Makefile -> printMakefile flags $ getDeps graph
            Serial -> mapM_ (putStrLn . optInfo flags . lookupSrcPkg) sorted
            Parallel ->
               mapM_ (putStrLn . unwords . map (optInfo flags)) $
               map (map lookupSrcPkg . fst . Graph.topologicalSort) $
               Graph.components graph


printMakefile :: Flags -> [(SourcePackage, [SourcePackage])] -> IO ()
printMakefile flags deps = do
    let printDep (l, ls) = putStrLn (l ++ ": " ++ unwords ls)
        stamp =
           (optBuilddir flags </>) .
           flip FilePath.replaceExtension "cstamp" . location
        allDeps = unwords (map (stamp . fst) deps)
    putStrLn (optBuilddir flags </> "%.cstamp:")
    putStrLn ("\t" ++ optInstall flags ++ " `dirname $*`")
    putStrLn "\tmkdir -p `dirname $@`"
    putStrLn "\ttouch $@"
    putStrLn ""
    putStrLn ("all: " ++ allDeps)
    putStrLn ""
    putStrLn "clean:"
    putStrLn ("\t$(RM) " ++ allDeps)
    putStrLn ""
    mapM_ (printDep . (stamp *** map stamp)) deps

getDeps :: DependencyGraph -> [(SourcePackage, [SourcePackage])]
getDeps gr =
   let nodes = Graph.nodeLabels gr in
   Map.elems .
   Map.mapWithKey
      (\pName srcPkg ->
         (srcPkg,
            Map.elems $ Map.restrictKeys nodes $
            Set.fromList $ Graph.predecessors gr pName))
      $ nodes

dependencyGraph :: [SourcePackage] -> DependencyGraph
dependencyGraph srcPkgs =
   let nodes =
          map (\pkg -> (pkgNameFromDescription (description pkg), pkg)) srcPkgs
       nodeMap = Map.fromList nodes
       edges = do
          (srcNode,desc) <- nodes
          dstNode <- map depName $ allDependencies $ description desc
          guard (Map.member dstNode nodeMap)
          guard (dstNode /= srcNode)
          return (Graph.DirEdge dstNode srcNode, ())
   in  Graph.fromMap nodeMap $ Map.fromList edges


showCycles :: DependencyGraph -> [String]
showCycles graph =
   map (unwords . Map.elems . Map.map location .
        Map.restrictKeys (Graph.nodeLabels graph)) .
   filter ((>=2) . Set.size) . Graph.stronglyConnectedComponents $ graph


allDependencies :: GenericPackageDescription -> [Dependency]
allDependencies pkg =
   P.allBuildDepends (packageDescription pkg) ++
   maybe [] (concatMap snd . flattenCondTree) (P.condLibrary pkg) ++
   concatMap (concatMap snd . flattenCondTree . snd) (P.condExecutables pkg)

flattenCondTree :: P.CondTree v c a -> [(a,c)]
flattenCondTree tree =
   (P.condTreeData tree, P.condTreeConstraints tree) :
   concatMap
      (\(P.CondBranch _ thenBranch elseBranch) ->
         flattenCondTree thenBranch ++
         maybe [] flattenCondTree elseBranch)
      (P.condTreeComponents tree)

depName :: Dependency -> PackageName
depName (Dependency name _ _) = name

pkgNameFromDescription :: GenericPackageDescription -> PackageName
pkgNameFromDescription = pkgName . package . packageDescription
