{-# LANGUAGE CPP #-}

#if !MIN_VERSION_simple_cmd_args(0,1,4)
import Control.Applicative (
#if !MIN_VERSION_simple_cmd_args(0,1,3)
    (<|>),
#endif
    many)
#endif
import Control.Monad.Extra
import qualified Data.List.Extra as L
import Data.Version (showVersion)
import Language.Haskell.Interpreter
import SimpleCmdArgs
import System.Directory
import System.FilePath

import Common
import Hwk.IO
import Hwk.Types
import Paths_hwk (getDataDir, version)

data HwkMode = DefaultMode | LineMode | WordsMode | WholeMode | TypeMode | EvalMode | RunMode | ShellMode
  deriving Eq

main :: IO ()
main = do
  userdir <- getXdgDirectory XdgConfig "hwk"
  simpleCmdArgs (Just version) "A Haskell awk/sed like tool"
    "Simple shell text processing with Haskell" $
    runExpr userdir <$> modeOpt <*> cfgdirOpt userdir <*> strArg "FUNCTION" <*> many (strArg "FILE...")
  where
    modeOpt :: Parser HwkMode
    modeOpt =
      flagWith' LineMode 'l' "line" "Apply function to each line" <|>
      flagWith' WordsMode 'w' "words" "Apply function to list of words per line" <|>
      flagWith' WholeMode 'a' "all" "Apply function once to the whole input" <|>
      flagWith' TypeMode 't' "typecheck" "Print out the type of the given function" <|>
      flagWith' EvalMode 'e' "eval" "Evaluate a Haskell expression" <|>
      flagWith' RunMode 'r' "run" "Run Haskell IO" <|>
      flagWith DefaultMode ShellMode 's' "shell" "Haskell Shell"

    cfgdirOpt :: String -> Parser (Maybe FilePath)
    cfgdirOpt dir =
      optional (normalise <$> strOptionWith 'c' "config-dir" "DIR" ("Override the config dir [default:" ++ dir ++ "]"))

runExpr :: FilePath -> HwkMode -> Maybe FilePath -> String -> [FilePath] -> IO ()
runExpr userdir mode mcfgdir stmt files = do
  mapM_ checkFileExists files
  cfgdir <-
    case mcfgdir of
      Just dir -> do
        unlessM (doesDirectoryExist dir) $
          error' $ dir ++ ": directory not found"
        return dir
      Nothing -> do
        let usercfg = userdir </> "Hwk.hs"
        datadir <- getDataDir
        let datafile = datadir </> "Hwk.hs"
        unlessM (doesFileExist datafile) $
          error' $ "default config file does not exist: " ++ datafile
        createDirectoryIfMissing True userdir
        let versionCfg = usercfg ++ "-" ++ showVersion version
        unlessM (doesFileExist versionCfg) $
          copyFile datafile versionCfg
        unlessM (doesFileExist usercfg) $ do
          copyFile datafile usercfg
          warn $ usercfg ++ " created"
        return userdir
  runInterpreter (runHint cfgdir) >>=
    either (putStrLn . errorString) return
  where
    runHint :: FilePath -> Interpreter ()
    runHint cfgdir = do
      -- could ignore this for eval or typecheck
      set [searchPath := [cfgdir], languageExtensions := [TypeApplications]]
      loadModules ["Hwk"]
      let setHwkImports ms = setImports (L.nub (ms ++ ["Hwk"]))
      setHwkImports ["Prelude"]
      imports <- do
        haveModules <- typeChecks "userModules"
        if haveModules
          then interpret "userModules" infer
          else return ["Prelude", "Data.List"]
      setHwkImports imports
      let inputs = if null files then ["-"] else files
      case mode of
        DefaultMode ->
          withInputFiles inputs (mapInputList stmt . lines)
        LineMode ->
          withInputFiles inputs (mapEachLine stmt . lines)
        WordsMode ->
          withInputFiles inputs (mapWordsLine stmt . map words . lines)
        WholeMode -> do
          withInputFiles inputs (applyToInput stmt . removeTrailingNewline)
        -- FIXME take or warn about args
        TypeMode -> typeOfExpr stmt
        EvalMode -> evalExpr stmt
        RunMode -> execExpr stmt
        ShellMode -> liftIO setNoBuffering >> shellSession
      where
        withInputFiles :: [FilePath] -> (String -> Interpreter ())
                       -> Interpreter ()
        withInputFiles inputs interp = do
          forM_ inputs $ \ file ->
            liftIO (if file == "-" then getContents else readFile file) >>=
            interp

        removeTrailingNewline :: String -> String
        removeTrailingNewline "" = ""
        removeTrailingNewline s =
          if last s == '\n'
          then init s
          else s

    checkFileExists :: FilePath -> IO ()
    checkFileExists "-" = return ()
    checkFileExists file = do
      unlessM (doesFileExist file) $
        error' $ "file not found: " ++ file

    errorString :: InterpreterError -> String
    errorString (WontCompile es) =
      unlines $ "ERROR: Won't compile:" : map errMsg es
    errorString e = show e

-- fn $ lines input
mapInputList :: String -> [String] -> Interpreter ()
mapInputList stmt inputs = do
  typ <- resultTypeOfApplied stmt "[String]"
  case cleanupType typ of
    "String" -> do
      fn <- interpret stmt (as :: [String] -> String)
      liftIO $ putStrLn (fn inputs)
    "[String]" -> do
      fn <- interpret stmt (as :: [String] -> [String])
      liftIO $ mapM_ putStrLn (fn inputs)
    "[[String]]" -> do
      fn <- interpret stmt (as :: [String] -> [[String]])
      liftIO $ mapM_ (putStrLn . unwords) (fn inputs)
    "Int" -> do
      fn <- interpret stmt (as :: [String] -> Int)
      liftIO $ print (fn inputs)
    "[Int]" -> do
      fn <- interpret stmt (as :: [String] -> [Int])
      liftIO $ mapM_ print (fn inputs)
    "[[Int]]" -> do
      fn <- interpret stmt (as :: [String] -> [[Int]])
      liftIO $ mapM_ (putStrLn . unwords . map show) (fn inputs)
    _ -> do
      liftIO $ warn typ
      fn <- interpret stmt (as :: [String] -> [String])
      liftIO $ mapM_ putStrLn (fn inputs)

-- map fn $ lines input
mapEachLine :: String -> [String] -> Interpreter ()
mapEachLine stmt inputs = do
  typ <- resultTypeOfApplied stmt "String"
  case cleanupType typ of
    "String" -> do
      fn <- interpret stmt (as :: String -> String)
      liftIO $ mapM_ (putStrLn . fn) inputs
    "[String]" -> do
      fn <- interpret stmt (as :: String -> [String])
      liftIO $ mapM_ (putStrLn . unwords . fn) inputs
    "[[String]]" -> do
      fn <- interpret stmt (as :: String -> [[String]])
      liftIO $ mapM_ (putStrLn . L.intercalate "\t" . map unwords . fn) inputs
    "Int" -> do
      fn <- interpret stmt (as :: String -> Int)
      liftIO $ mapM_ (print . fn) inputs
    "[Int]" -> do
      fn <- interpret stmt (as :: String -> [Int])
      liftIO $ mapM_ (putStrLn . unwords . map show . fn) inputs
    "[[Int]]" -> do
      fn <- interpret stmt (as :: String -> [[Int]])
      liftIO $ mapM_ (putStrLn . L.intercalate "\t" . map (unwords . map show) .fn) inputs
    _ -> do
      liftIO $ warn typ
      fn <- interpret stmt (as :: String -> String)
      liftIO $ mapM_ (putStrLn . fn) inputs

-- map fn $ lines input
mapWordsLine :: String -> [[String]] -> Interpreter ()
mapWordsLine stmt inputs = do
  typ <- resultTypeOfApplied stmt "[String]"
  case cleanupType typ of
    "String" -> do
      fn <- interpret stmt (as :: [String] -> String)
      liftIO $ mapM_ (putStrLn . fn) inputs
    "[String]" -> do
      fn <- interpret stmt (as :: [String] -> [String])
      liftIO $ mapM_ (putStrLn . unwords . fn) inputs
    "[[String]]" -> do
      fn <- interpret stmt (as :: [String] -> [[String]])
      liftIO $ mapM_ (putStrLn . L.intercalate "\t" . map unwords . fn) inputs
    "Int" -> do
      fn <- interpret stmt (as :: [String] -> Int)
      liftIO $ mapM_ (print . fn) inputs
    "[Int]" -> do
      fn <- interpret stmt (as :: [String] -> [Int])
      liftIO $ mapM_ (putStrLn . unwords . map show . fn) inputs
    "[[Int]]" -> do
      fn <- interpret stmt (as :: [String] -> [[Int]])
      liftIO $ mapM_ (putStrLn . L.intercalate "\t" . map (unwords . map show) .fn) inputs
    _ -> do
      liftIO $ warn typ
      fn <- interpret stmt (as :: [String] -> [String])
      liftIO $ mapM_ (putStrLn . unwords . fn) inputs

-- fn input
applyToInput :: String -> String -> Interpreter ()
applyToInput stmt input = do
  typ <- resultTypeOfApplied stmt "String"
  case cleanupType typ of
    "String" -> do
      fn <- interpret stmt (as :: String -> String)
      liftIO $ putStrLn (fn input)
    "[String]" -> do
      fn <- interpret stmt (as :: String -> [String])
      liftIO $ mapM_ putStrLn (fn input)
    "[[String]]" -> do
      fn <- interpret stmt (as :: String -> [[String]])
      liftIO $ mapM_ (putStrLn . unwords) (fn input)
    "Int" -> do
      fn <- interpret stmt (as :: String -> Int)
      liftIO $ print (fn input)
    "[Int]" -> do
      fn <- interpret stmt (as :: String -> [Int])
      liftIO $ mapM_ print (fn input)
    "[[Int]]" -> do
      fn <- interpret stmt (as :: String -> [[Int]])
      liftIO $ mapM_ (putStrLn . unwords . map show) (fn input)
    _ -> do
      liftIO $ warn typ
      fn <- interpret stmt (as :: String -> [String])
      liftIO $ mapM_ putStrLn (fn input)

resultTypeOfApplied :: MonadInterpreter m => String -> String -> m String
resultTypeOfApplied expr typ =
  L.dropPrefix (typ ++ " -> ") <$> typeOf (expr ++ " . (id  @" ++ typ ++ ")")

typeOfExpr :: String -> Interpreter ()
typeOfExpr stmt = do
  etypchk <- typeChecksWithDetails stmt
  case etypchk of
    Left err -> liftIO $ mapM_ (putStrLn . errMsg) err
    Right typ -> liftIO $ putStrLn (cleanupType typ)

evalExpr :: String -> Interpreter ()
evalExpr stmt = do
  typ <- typeOf stmt
  case cleanupType typ of
    "String" -> do
      interpret stmt "a String" >>= liftIO . putStrLn
    "[String]" -> do
      interpret stmt ["a String"] >>= liftIO . mapM_ putStrLn
    "[[String]]" -> do
      interpret stmt [["a String"]] >>= liftIO . mapM_ (putStrLn . unwords)
    "Int" -> do
      interpret stmt (1 :: Int) >>= liftIO . print
    "[Int]" -> do
      interpret stmt [1 :: Int] >>= liftIO . mapM_ print
    "[[Int]]" -> do
      interpret stmt [[1 :: Int]] >>= liftIO . mapM_ (putStrLn . unwords . map show)
    _ ->
      if " -> " `L.isInfixOf` typ
        then liftIO $ putStrLn typ
        else
        -- FIXME option to display type
        eval stmt >>= liftIO . putStrLn

-- from simple-cmd
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' = errorWithoutStackTrace
#else
error' = error
#endif
