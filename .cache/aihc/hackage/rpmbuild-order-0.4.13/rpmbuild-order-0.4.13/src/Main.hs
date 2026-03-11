{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Applicative (
#if !MIN_VERSION_simple_cmd_args(0,1,3)
                            (<|>),
#endif
#if !MIN_VERSION_simple_cmd_args(0,1,4)
                            many, some,
#endif
#if !MIN_VERSION_base(4,8,0)
                            (<$>), (<*>)
#endif
                           )
import Data.Graph.Inductive.Query.DFS (components)
import Data.List (intercalate)
import Data.List.Extra (dropSuffix)

import SimpleCmdArgs

import Distribution.RPM.Build.Graph
import Distribution.RPM.Build.Order
import Paths_rpmbuild_order (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Order packages by build dependencies"
  "Sort package sources (spec files) in build dependency order" $
  subcommands
  [ Subcommand "sort" "sort packages in build order" $
    sortPackages <$> rpmOpts <*> verboseOpt <*> lenientOpt <*> componentsOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "deps" "sort dependencies in neighbouring package dirs" $
    depsPackages False <$> rpmOpts <*> verboseOpt <*> excludeOpts <*> ignoredBRopts <*> lenientOpt <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "rdeps" "sort dependents in neighbouring package dirs" $
    depsPackages True <$> rpmOpts <*> verboseOpt <*> excludeOpts <*> ignoredBRopts <*> lenientOpt <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "layers" "ordered output of dependency layers" $
    layerPackages <$> rpmOpts <*> verboseOpt <*> lenientOpt <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "chain" "ordered output suitable for a chain-build" $
    chainPackages <$> rpmOpts <*> verboseOpt <*> lenientOpt <*> combineOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "leaves" "List of the top leaves of package graph" $
    leavesPackages <$> rpmOpts <*> verboseOpt <*> lenientOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "roots" "List lowest root packages" $
    rootPackages <$> rpmOpts <*> verboseOpt <*> lenientOpt <*> subdirOpt <*> pkgArgs
  , Subcommand "render" "Show graph with graphviz" $
    renderPkgGraph <$> switchWith 'g' "gv-output" "Output graph in gv/dot format" <*> rpmOpts <*> verboseOpt <*> lenientOpt <*> subdirOpt <*> pkgArgs
  ]
  where
    verboseOpt = switchWith 'v' "verbose" "Verbose output for debugging"
    lenientOpt = switchWith 'l' "lenient" "Ignore rpmspec errors"
    combineOpt = switchWith 'c' "combine" "Combine independent packages"
    subdirOpt = optional (strOptionWith 'd' "dir" "SUBDIR" "Branch directory")
    pkgArgs = some (strArg "PKG...")
    componentsOpt =
      flagWith' Connected 'C' "connected" "Only include connected packages" <|>
      flagWith' Separate 'i' "independent" "Only list independent packages" <|>
      flagWith Parallel Combine 'c' "combine" "Combine connected and independent packages"
    rpmOpts = many (strOptionWith 'r' "rpmopt" "RPMOPT" "Option for rpmspec")
    ignoredBRopts = many (strOptionWith 'I' "ignore-BR" "PKG" "BuildRequires to exclude from graph")
    excludeOpts = many (dropSuffix "/" <$> strOptionWith 'x' "exclude" "PKG" "Package dirs to exclude from graph")

sortPackages :: [String] -> Bool -> Bool -> Components -> Maybe FilePath
             -> [FilePath] -> IO ()
sortPackages rpmopts verbose lenient opts mdir pkgs = do
  createGraph2 rpmopts verbose lenient True mdir pkgs >>= sortGraph opts

layerPackages :: [String] -> Bool -> Bool -> Bool -> Maybe FilePath
              -> [FilePath] -> IO ()
layerPackages rpmopts verbose lenient combine mdir pkgs = do
  graph <- createGraph2 rpmopts verbose lenient True mdir pkgs
  if combine
    then printLayers graph
    else mapM_ (printLayers . subgraph' graph) (components graph)
  where
    printLayers =  putStrLn . unlines . map unwords . packageLayers

chainPackages :: [String] -> Bool -> Bool -> Bool -> Maybe FilePath
              -> [FilePath] -> IO ()
chainPackages rpmopts verbose lenient combine mdir pkgs = do
  graph <- createGraph2 rpmopts verbose lenient True mdir pkgs
  if combine then doChain graph
    else mapM_ (doChain . subgraph' graph) (components graph)
  where
    doChain graph =
      let chain = intercalate [":"] $ packageLayers graph
      in putStrLn $ unwords chain

leavesPackages :: [String] -> Bool -> Bool -> Maybe FilePath -> [FilePath]
               -> IO ()
leavesPackages rpmopts verbose lenient mdir pkgs = do
  graph <- createGraph2 rpmopts verbose lenient True mdir pkgs
  mapM_ putStrLn $ packageLeaves graph

rootPackages :: [String] -> Bool -> Bool -> Maybe FilePath -> [FilePath]
             -> IO ()
rootPackages rpmopts verbose lenient mdir pkgs = do
  graph <- createGraph2 rpmopts verbose lenient True mdir pkgs
  mapM_ putStrLn $ lowestLayer graph

renderPkgGraph :: Bool -> [String] -> Bool -> Bool -> Maybe FilePath
               -> [FilePath] -> IO ()
renderPkgGraph dot rpmopts verbose lenient mdir pkgs =
  createGraph4 False [] rpmopts verbose lenient False mdir pkgs >>=
  if dot then printGraph else renderGraph
