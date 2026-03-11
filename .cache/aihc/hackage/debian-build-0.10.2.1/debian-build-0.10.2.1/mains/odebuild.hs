import Control.Monad (void, when, (>=>))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Maybe (listToMaybe)
import Data.List (intercalate)
import System.Environment (getProgName, getArgs)
import System.Console.GetOpt
  (OptDescr (Option), ArgDescr (ReqArg, NoArg), ArgOrder (RequireOrder),
   usageInfo, getOpt)

import Debian.Package.Data (Source, Hackage, isBinaryPackage)
import qualified Debian.Package.Build.Command as Command
import Debian.Package.Build
  (BuildMode (Dep, Indep, Src), debi', pwd,
   defaultConfig, Build, runBuild, liftTrace,
   removeBuildDir, findDebianChanges, genSources, findGeneratedSource, removeGhcLibrary)


remove' :: Hackage -> Build ()
remove' hkg = liftTrace $ sequence_ [removeGhcLibrary m hkg | m <- [Dep, Indep, Src]]

install' :: Build ()
install' =  do
  ps <- findDebianChanges
  let cs = [c | (c, t) <- ps, isBinaryPackage t ]
  liftTrace $ do
    when (null cs) $ fail "No .changes files found!"
    mapM_ (\c -> debi' [c]) cs


data ODebuildOptions =
  ODebuildOptions
  { revision    :: Maybe String
  , installDeps :: Bool
  , buildModes  :: [BuildMode] -> [BuildMode]
  , reuseSource :: Bool
  }

defaultOptions :: ODebuildOptions
defaultOptions =
  ODebuildOptions
  { revision     =  Nothing
  , installDeps  =  False
  , buildModes   =  id
  , reuseSource  =  False
  }

descs :: [OptDescr (ODebuildOptions -> Either String ODebuildOptions)]
descs =
  [ Option [] ["revision"]
    (ReqArg (\s opts -> return $ opts { revision = Just s }) "DEBIAN_REVISION") $
    unlines
    ["debian package revision to pass to cabal-debian",
     "revision string may use on auto-generating debian directory"]
  , Option [] ["install-deps"]
    (NoArg $ \opts   -> return $ opts { installDeps = True })
    "install build depends before running build"
  , Option [] ["mode"]
    (ReqArg (\s opts -> maybe (Left $ "Unknown build mode: " ++ s) Right $ do
                m <- listToMaybe [m | (m, "") <- reads s]
                return $ opts { buildModes = buildModes opts . (m : ) } )
     modesEx)
    "add build-mode to build-mode list to specify"
  , Option ['R'] ["reuse-source"]
    (NoArg $ \opts   -> return $ opts { reuseSource = True })
    "not clean generated source directory, and build operation will reuse it"
  ]

modesEx :: String
modesEx =
  ("{" ++ ) . (++ "}") . intercalate "|" $
  map show ([minBound .. maxBound] :: [BuildMode])

parseOption :: [String]
            -> (ODebuildOptions -> Either String ODebuildOptions, ([String], [String]))
parseOption args = (foldr (>=>) return ufs, (as, es))  where
  (ufs, as, es) = getOpt RequireOrder descs args

help :: IO ()
help =  do
  prog <- getProgName
  let opts  = "[options]"
      debOpts = "[-- [cabal-debian-options [-- [debuild-options]]]"
      msg = unlines $ map unwords
            [ ["Usage:", prog, "clean"]
            , ["      ", prog, "prepare", opts, "[-- [cabal-debian-options]]"]
            , ["      ", prog, "source", opts, debOpts]
            , ["      ", prog, "build", opts, debOpts]
            , ["      ", prog, "install", opts, debOpts]
            , ["      ", prog, "reinstall", opts, debOpts]
            , ["      ", "  reinstall (Remove and install) support only for Haskell library packages"]
            ]
  putStr $ usageInfo msg descs

clean :: Build ()
clean =  removeBuildDir

prepare :: ODebuildOptions -> [String] -> Build (FilePath, Source, Maybe Hackage)
prepare opts cdArgs
  | reuseSource opts = do
      (dir, src, hkg) <- maybe (fail "generated source not found") return
                         =<< runMaybeT findGeneratedSource
      return (dir, src, Just hkg)
  | otherwise        = do
      clean
      ((_, dir), src, mayH) <- maybe (fail "Illegal state: genSources") return
                               =<< genSources (revision opts) cdArgs
      return (dir, src, mayH)

build' :: [BuildMode] -> ODebuildOptions -> [String] -> [String] -> Build (Source, Maybe Hackage)
build' modes opts cdArgs debArgs = do
  (dir, src, mayH) <- prepare opts cdArgs
  liftTrace $ Command.build dir modes (installDeps opts) debArgs
  return (src, mayH)

source :: ODebuildOptions -> [String] -> [String] -> Build (Source, Maybe Hackage)
source = build' [Src]

build :: ODebuildOptions -> [String] -> [String] -> Build (Source, Maybe Hackage)
build opts = build' (buildModes opts []) opts

install :: ODebuildOptions -> [String] -> [String] -> Build ()
install opts cdArgs debArgs = do
  void $ build opts cdArgs debArgs
  install'

reinstall :: ODebuildOptions -> [String] -> [String] -> Build ()
reinstall opts cdArgs debArgs = do
  (_src, mayH) <- build opts cdArgs debArgs
  maybe (return ()) remove' mayH
  install'

run :: Build a -> IO a
run b = do
  cur <- pwd
  uncurry (runBuild b cur) defaultConfig

parseArgs :: [String] -> IO (ODebuildOptions, ([String], [String]))
parseArgs args0
  | not $ null errs  = fail $ '\n' : concat [ "  " ++ e | e <- errs ]
  | not $ null args1 = fail $ "Unknown arguments: " ++ unwords args1
  | otherwise        =
      either (fail . ("Option parse error: " ++)) return $ do
        opts <- f defaultOptions
        return (opts, (cdArgs, drop 1 rest1))
      where
        (opt, rest0) = break (== "--") args0
        (cdArgs, rest1) = break (== "--") $ drop 1 rest0
        (f, (args1, errs)) = parseOption opt

runArgs :: (ODebuildOptions -> [String] -> [String] -> Build a) -> [String] -> IO a
runArgs act as = do
  (opts, (cdArgs, debArgs)) <- parseArgs as
  run $ act opts cdArgs debArgs

main :: IO ()
main =  do
  as0 <- getArgs
  case as0 of
    "-h"     : _          ->  help
    "--help" : _          ->  help
    "help"   : _          ->  help
    as2@(c : as1)  ->  do
      case c of
        "clean"         ->    run clean
        "prepare"       ->    void $ runArgs (const . prepare) as1
        "source"        ->    void $ runArgs source as1
        "build"         ->    void $ runArgs build as1
        "install"       ->    runArgs install as1
        "reinstall"     ->    runArgs reinstall as1
        _               ->    void $ runArgs build as2

    []                  ->    run . void $ build defaultOptions [] []
