------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : bm: open bookmarks and queries from the command line
-- Copyright   : Copyright (c) 2021-2023 Travis Cardwell
-- License     : MIT
--
-- See the README for details.
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- https://hackage.haskell.org/package/base
import Control.Applicative (optional, some)
import Control.Monad (forM, unless, when)
import Data.List (find, isInfixOf, isPrefixOf, sort, uncons)
import Data.Maybe (catMaybes)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)
import Text.Read (readMaybe)

-- https://hackage.haskell.org/package/directory
import qualified System.Directory as Dir

-- https://hackage.haskell.org/package/filepath
import qualified System.FilePath as FP
import System.FilePath ((</>))

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA
import qualified Options.Applicative.Types as OAT

-- https://hackage.haskell.org/package/typed-process
import qualified System.Process.Typed as TP

-- https://hackage.haskell.org/package/yaml
import qualified Data.Yaml as Yaml

-- (bm)
import qualified BM

-- (bm:executable)
import qualified LibOA

------------------------------------------------------------------------------
-- $Options

data Options
  = Options
    { configOpt :: !(Maybe FilePath)
    , traceOpt  :: !Bool
    , args      :: ![String]
    }
  deriving Show

configOption :: OA.Parser FilePath
configOption = OA.strOption $ mconcat
    [ OA.long "config"
    , OA.short 'c'
    , OA.metavar "CONFIG"
    , OA.help "config file (default: $XDG_CONFIG_HOME/bm.yaml)"
    ]

traceOption :: OA.Parser Bool
traceOption = OA.switch $ mconcat
    [ OA.long "trace"
    , OA.short 't'
    , OA.help "show trace output for debugging"
    ]

arguments :: OA.Parser [String]
arguments = some . OA.strArgument $ mconcat
    [ OA.metavar "ARG [ARG ...]"
    , OA.help "arguments"
    ]

options :: OA.Parser Options
options = Options
    <$> optional configOption
    <*> traceOption
    <*> arguments

------------------------------------------------------------------------------
-- $Completion

data CompletionOption
  = CompleteDefault
  | CompleteNoSpace

instance Show CompletionOption where
  show = \case
    CompleteDefault -> "DEFAULT"
    CompleteNoSpace -> "NOSPACE"

handleCompletion :: [String] -> IO (Either OA.ParseError [String])
handleCompletion args = case find isOAOption optArgs of
    Just arg ->
      return . Left $ OA.UnexpectedError arg (OAT.SomeParser options)
    Nothing
      | "--complete"      `elem` optArgs -> handleComplete args
      | "--complete-bash" `elem` optArgs -> handleCompleteBash args
      | otherwise                        -> return $ Right args
  where
    optArgs :: [String]
    optArgs = takeWhile (/= "--") args

    isOAOption :: String -> Bool
    isOAOption =
      (&&) <$> ("--" `isPrefixOf`) <*> ("completion" `isInfixOf`)

handleComplete :: [String] -> IO a
handleComplete = \case
    "--complete" : idxArg : args -> do
      idx <- case readMaybe idxArg of
        Just n | n > 0 -> return n
        _invalidIdx    -> errorExit $ "invalid index: " ++ idxArg
      goOpts Nothing [] (idx - 1) (drop 1 args)
    _invalidArgs -> do
      mapM_ (hPutStrLn stderr)
        [ "Usage: bm --complete INDEX [ARG ...]"
        , "  get completion options"
        , ""
        , "Use one of the following commands to enable bm completion:"
        , "  * bm --complete-bash"
        ]
      exitWith $ ExitFailure 2
  where
    longOpts, shortOpts :: [String]
    longOpts = ["--help", "--version", "--config", "--trace"]
    shortOpts = ["-h", "-c", "-t"]

    goOpts :: Maybe FilePath -> [String] -> Int -> [String] -> IO a
    goOpts mConfigPath bmArgsAcc 0 (arg:_args) = do
      when (arg `elem` ["-", "--"]) $ reply longOpts
      when (arg `elem` shortOpts) $ reply [arg]
      case find (isPrefixOf arg) longOpts of
        Just longOpt -> reply [longOpt]
        Nothing -> return ()
      when ("-" `isPrefixOf` arg) $ reply []
      goBM mConfigPath $ reverse (arg : bmArgsAcc)
    goOpts mConfigPath bmArgsAcc 0 [] =
      goBM mConfigPath $ reverse ("" : bmArgsAcc)
    goOpts mConfigPath bmArgsAcc idx (arg:args)
      | arg == "--" = goArgs mConfigPath bmArgsAcc (idx - 1) args
      | arg `elem` ["-c", "--config"] = case uncons args of
          Just (configPath, args')
            | idx == 1  -> goConfig =<< expandHomeDirectory configPath
            | otherwise -> goOpts (Just configPath) bmArgsAcc (idx -2) args'
          Nothing -> goConfig "./"
      | "-" `isPrefixOf` arg = goOpts mConfigPath bmArgsAcc (idx -1) args
      | otherwise = goOpts mConfigPath (arg : bmArgsAcc) (idx - 1) args
    goOpts _mConfigPath _bmArgsAcc _idx [] = reply []

    goArgs :: Maybe FilePath -> [String] -> Int -> [String] -> IO a
    goArgs mConfigPath bmArgsAcc 0 (arg:_args) =
      goBM mConfigPath $ reverse (arg : bmArgsAcc)
    goArgs mConfigPath bmArgsAcc 0 [] =
      goBM mConfigPath $ reverse ("" : bmArgsAcc)
    goArgs mConfigPath bmArgsAcc idx (arg:args) =
      goArgs mConfigPath (arg : bmArgsAcc) (idx - 1) args
    goArgs _mConfigPath _bmArgsAcc _idx [] = reply []

    expandHomeDirectory :: FilePath -> IO FilePath
    expandHomeDirectory arg = case uncons (FP.splitPath arg) of
      Just (part, parts)
        | part `elem` ["~", "~/"] -> do
            homeDir <- Dir.getHomeDirectory
            return . FP.joinPath $ homeDir : parts
      _otherwise -> return arg

    goConfig :: FilePath -> IO a
    goConfig arg = do
      isDir <- Dir.doesDirectoryExist arg
      when isDir $
        goConfigDir . map (arg </>) . sort =<< Dir.listDirectory arg
      when (FP.hasTrailingPathSeparator arg) $ reply []
      isFile <- Dir.doesFileExist arg
      when isFile $ reply [arg | ".yaml" `FP.isExtensionOf` arg]
      let (dir, partial) = FP.splitFileName arg
      isDirDir <- Dir.doesDirectoryExist dir
      unless isDirDir $ reply []
      goConfigDir . map (dir </>) . sort . filter (isPrefixOf partial)
        =<< Dir.listDirectory dir

    goConfigDir :: [FilePath] -> IO a
    goConfigDir paths = do
      dirOrFiles <- fmap catMaybes . forM paths $ \path -> do
          isDir <- Dir.doesDirectoryExist path
          return $ if isDir
            then Just . Left $ FP.addTrailingPathSeparator path
            else if ".yaml" `FP.isExtensionOf` path
              then Just $ Right path
              else Nothing
      case dirOrFiles of
        [Right path] -> reply [path]
        _otherwise -> do
          print CompleteNoSpace
          mapM_ (putStrLn . either id id) dirOrFiles
          exitSuccess

    goBM :: Maybe FilePath -> [String] -> IO a
    goBM mConfigPath args =  do
      print CompleteDefault
      configPath <- case mConfigPath of
        Just path -> pure path
        Nothing -> Dir.getXdgDirectory Dir.XdgConfig "bm.yaml"
      exists <- Dir.doesFileExist configPath
      when exists $ do
        eec <- Yaml.decodeFileEither configPath
        case eec of
          Right config -> mapM_ putStrLn $ BM.getCompletion config args
          Left{} -> return ()
      exitSuccess

    reply :: [String] -> IO a
    reply choices = do
      print CompleteDefault
      mapM_ putStrLn choices
      exitSuccess

handleCompleteBash :: [String] -> IO a
handleCompleteBash = \case
    ["--complete-bash", path] | not ("-" `isPrefixOf` path) -> do
      mapM_ putStrLn
        [ "_bm() {"
        , "mapfile -t COMPREPLY < <(\"" ++ path ++
          "\" --complete ${COMP_CWORD} ${COMP_WORDS[@]})"
        , "test \"${COMPREPLY[0]}\" != \"" ++ show CompleteNoSpace ++
          "\" || compopt -o nospace"
        , "unset \"COMPREPLY[0]\""
        , "}"
        , ""
        , "complete -F _bm bm"
        ]
      exitSuccess
    _invalidArgs -> do
      mapM_ (hPutStrLn stderr)
        [ "Usage: bm --complete-bash PATH"
        , "  print a Bash script to enable bm completion"
        , ""
        , "Example usage:"
        , ""
        , "  $ source <(bm --complete-bash `which bm`)"
        ]
      exitWith $ ExitFailure 2

parseArgs :: IO Options
parseArgs = do
    eeas <- handleCompletion =<< getArgs
    OA.handleParseResult $ case eeas of
      Right args -> OA.execParserPure OA.defaultPrefs pinfo args
      Left parseError ->
        OA.Failure $ OA.parserFailure OA.defaultPrefs pinfo parseError mempty
  where
    pinfo :: OA.ParserInfo Options
    pinfo
      = OA.info (LibOA.helper <*> LibOA.versioner BM.version <*> options)
      $ mconcat
          [ OA.fullDesc
          , OA.progDesc "open bookmarks and queries from the command line"
          , OA.failureCode 2
          ]

------------------------------------------------------------------------------
-- $Main

errorExit :: String -> IO a
errorExit message = do
    hPutStrLn stderr $ "error: " ++ message
    exitWith $ ExitFailure 1

runProc :: BM.Proc -> IO ()
runProc proc
    = TP.runProcess_
    . TP.setStdin TP.nullStream
    . TP.setStdout TP.nullStream
    . TP.setStderr TP.nullStream
    $ TP.proc (BM.command proc) (BM.arguments proc)

main :: IO ()
main = do
    Options{..} <- parseArgs
    configPath <- case configOpt of
      Just path -> pure path
      Nothing -> Dir.getXdgDirectory Dir.XdgConfig "bm.yaml"
    exists <- Dir.doesFileExist configPath
    unless exists . errorExit $ "configuration file not found: " ++ configPath
    eec <- Yaml.decodeFileEither configPath
    case eec of
      Right config -> do
        let (eep, traceLines) = BM.run config args
        when traceOpt $ mapM_ (hPutStrLn stderr) traceLines
        either errorExit runProc eep
      Left parseException -> do
        hPutStrLn stderr $ "error parsing config file: " ++ configPath
        hPutStrLn stderr $ Yaml.prettyPrintParseException parseException
        exitWith $ ExitFailure 1
