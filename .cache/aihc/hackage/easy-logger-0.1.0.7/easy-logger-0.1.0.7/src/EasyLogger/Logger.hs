{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module EasyLogger.Logger
    ( LogDestination (..)
    , LogLevel (..)
    , initLogger
    , initLoggerAllPackages
    , setLoggingDestination
    , setMinLogLevel
    , setPrintLocationToConsole
    , logAll
    , logPrintAll
    , logDebug
    , logPrintDebug
    , logInfo
    , logPrintInfo
    , logWarning
    , logPrintWarning
    , logError
    , logPrintError
    , pureLogAll
    , pureLogPrintAll
    , pureLogDebug
    , pureLogPrintDebug
    , pureLogInfo
    , pureLogPrintInfo
    , pureLogWarning
    , pureLogPrintWarning
    , pureLogError
    , pureLogPrintError
    , logAllText
    , logPrintAllText
    , logDebugText
    , logPrintDebugText
    , logInfoText
    , logPrintInfoText
    , logWarningText
    , logPrintWarningText
    , logErrorText
    , logPrintErrorText
    , pureLogAllText
    , pureLogPrintAllText
    , pureLogDebugText
    , pureLogPrintDebugText
    , pureLogInfoText
    , pureLogPrintInfoText
    , pureLogWarningText
    , pureLogPrintWarningText
    , pureLogErrorText
    , pureLogPrintErrorText
    , finalizeAllLoggers
    , finalizeLogger
    , flushLoggers
    ) where

import           Control.Applicative        ((<|>))
import           Control.Monad              (join, when)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.ByteString.Char8      as S8
import           Data.IORef
import           Data.List                  (find)
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as T
import           Language.Haskell.TH.Syntax as TH
import           System.IO
import           System.IO.Unsafe           (unsafePerformIO)

import           EasyLogger.Date
import           EasyLogger.LogStr
import           EasyLogger.LoggerSet
import           EasyLogger.Push
import           EasyLogger.Util            (liftLoc)


-- | Add a @LoggerSet@ to the known loggers.
setLoggerSet :: String -> LoggerSet -> IO ()
setLoggerSet pkgName set = modifyIORef' loggerSets (M.insert pkgName set)


-- | Set of loggers. We have one @LoggerSet@ for each package.
loggerSets :: IORef (M.Map String LoggerSet)
loggerSets = unsafePerformIO $ newIORef mempty
{-# NOINLINE loggerSets  #-}


-- | Should be used to ensure all logs are completely written before the program exists. Cleans all the file descriptors. You (and also no other library) MUST NOT log after this command as all loggers
-- are deinitalized. However, you might initialize the loggers again and before restarting to log.
finalizeAllLoggers :: IO ()
finalizeAllLoggers = do
  pkgs <- map fst . M.toList <$> readIORef loggerSets
  mapM_ closeLoggerPkg pkgs


-- | Can be used to destroy your own logger (from your package) only. You MUST NOT log after this command.
finalizeLogger :: Q Exp
finalizeLogger = [| closeLogger $(qLocation >>= liftLoc)|]


-- | Flush all loggers of all packages.
flushLoggers :: IO ()
flushLoggers = readIORef loggerSets >>= mapM_ flushLoggerSet


-- | Close logger of calling package.
closeLogger :: Loc -> IO ()
closeLogger (Loc _ pkgName _ _ _) = closeLoggerPkg pkgName

-- | Close logger of package with provided package name.
closeLoggerPkg :: String -> IO ()
closeLoggerPkg pkgName = do
  refs <- readIORef loggerSets
  case M.lookup pkgName refs of
    Nothing -> return ()
    Just set@(LoggerSet Nothing _ _ _) -> deletePackage pkgName >> rmLoggerSet set
    Just set@(LoggerSet justFp _ _ _) -> do
      deletePackage pkgName
      let nrFD = length $ filter (\(LoggerSet mFp _ _ _) -> mFp == justFp) (M.elems refs)
      if nrFD <= 1
        then rmLoggerSet set
        else flushLoggerSet set

-- | Delete a package from the logger sets and with this disable all logging. Ensure the LoggerSet is deleted in case this is the last FD before calling this function!
deletePackage :: String -> IO ()
deletePackage pkg = modifyIORef' loggerSets (M.delete pkg)


-- | Logging destination. See also `setLoggingDestination`.
data LogDestination
  = LogStdErr
  | LogStdOut
  | LogFile FilePath

-- | Log messages from other packages that use this library too, even if they did not call @initLogger@?
type LogFromAllPackages = Bool

-- | Initialise the logger. MUST only be called in the executable code (not the exposed library code)! Takes a `Bool` that decides wether to log messages from other packages that use the same library
-- and did not initalize the Logger (which should be the case for all of them!).
initLoggerAllPackages :: Q Exp
initLoggerAllPackages = [| \dest logAllPkgs -> setLoggingDestination (loc_package $(qLocation >>= liftLoc)) dest logAllPkgs |]

-- | Initialise the logger. MUST only be called in the executable code (not the exposed library code)! Ignores the other packages logs, if the same packages is used for logging.
initLogger :: Q Exp
initLogger = [| \dest -> setLoggingDestination (loc_package $(qLocation >>= liftLoc)) dest False |]

-- | Set the destination for all consequitive for logging. You should only set this once, at the beginning of the program! The default is `LogStdOut`.
setLoggingDestination :: String -> LogDestination -> LogFromAllPackages -> IO ()
setLoggingDestination pkgName LogStdErr logAllPkgs    = newStderrLoggerSet defaultBufSize  >>= \ls -> setLoggerSet pkgName ls >> when logAllPkgs (setLoggingDestinationAllPkgs ls defaultLogPkgName LogStdErr)
setLoggingDestination pkgName LogStdOut logAllPkgs    = newStdoutLoggerSet defaultBufSize  >>= \ls -> setLoggerSet pkgName ls >> when logAllPkgs (setLoggingDestinationAllPkgs ls defaultLogPkgName LogStdOut)
setLoggingDestination pkgName (LogFile fp) logAllPkgs = do
  allLs <- M.elems <$> readIORef loggerSets
  ls <-
    case find (\(LoggerSet mFp _ _ _) -> mFp == Just fp) allLs of
      Nothing     -> newFileLoggerSet defaultBufSize fp
      Just lsFile -> newFileLoggerSetSameFile defaultBufSize lsFile
  setLoggerSet pkgName ls >> when logAllPkgs (setLoggingDestinationAllPkgs ls defaultLogPkgName (LogFile fp))


setLoggingDestinationAllPkgs :: LoggerSet -> String -> LogDestination -> IO ()
setLoggingDestinationAllPkgs _ pkgName LogStdErr  = newStderrLoggerSet defaultBufSize          >>= setLoggerSet pkgName
setLoggingDestinationAllPkgs _ pkgName LogStdOut  = newStdoutLoggerSet defaultBufSize          >>= setLoggerSet pkgName
setLoggingDestinationAllPkgs ls pkgName LogFile{} = newFileLoggerSetSameFile defaultBufSize ls >>= setLoggerSet pkgName


defaultLogPkgName :: String
defaultLogPkgName = "__default__"

mainLogPkgName :: String
mainLogPkgName = "main"


-- | The default buffer size (4,096 bytes).
defaultBufSize :: BufSize
defaultBufSize = 4096


-- | Log Level. Levels are sorted. `All` < `Debug` < `Info` < `Warning` < `Error`. None disables all logging. Default: All
data LogLevel
  = LogNone
  | LogAll
  | LogDebug
  | LogInfo
  | LogWarning
  | LogError
  deriving (Show, Read, Bounded, Enum, Eq, Ord)

-- | Log level text. Make sure you call @initLogger@, or logging will be disabled.
logLevelText :: LogLevel -> T.Text
logLevelText LogNone    = mempty
logLevelText LogAll     = "ALL"
logLevelText LogDebug   = "DEBUG"
logLevelText LogInfo    = "INFO "
logLevelText LogWarning = "WARN "
logLevelText LogError   = "ERROR"

-- | Generic log function. Use TH version, e.g. `logDebug`.
logFun :: (ToLogStr msg) => Bool -> Loc -> LogLevel -> msg -> IO ()
logFun _ _ LogNone _ = return ()
logFun printMsg loc@(Loc _ pkg _ _ _) level msg = do
  (minLevel, printLoc) <- readIORef minLogLevel
  when (level >= minLevel) $ do
    now <- join (readIORef cachedTime)
    readIORef loggerSets >>= \sets ->
      case getLogger sets of
        Nothing -- Check the package name of the caller, as otherwise any library logging would halt the process.
          | M.null sets && pkg == mainLogPkgName -> error "You must call `initLogger` at the start of your application! See the documentation of `EasyLogger.Logger`."
        Nothing -> return ()
        Just set -> do
          let logStr = defaultLogStr True loc now level (toLogStr msg)
              logStrPrint | printLoc = logStr
                          | otherwise = defaultLogStr False loc now level (toLogStr msg)
          when printMsg $ S8.hPutStr handle (fromLogStr logStrPrint) >> hFlush handle
          pushLogStr set logStr
  where
    getLogger sets = M.lookup pkg sets <|> M.lookup defaultLogPkgName sets
    handle = case level of
      LogError -> stderr
      _        -> stdout


mkTxt :: T.Text -> T.Text
mkTxt = id


cachedTime :: IORef (IO FormattedTime)
cachedTime = unsafePerformIO $ do
  cache <- newTimeCache simpleTimeFormat'
  newIORef cache

-- | Min Log Level and whether to print location to console.
minLogLevel :: IORef (LogLevel, Bool)
minLogLevel = unsafePerformIO $ newIORef (LogAll, False)
{-# NOINLINE minLogLevel  #-}

-- | Set the least logging level. Levels lower will not be logged. Log Level Order: `Debug` < `Info` < `Warning` < `Error`. `None` disables all logging. Note that the output to stderr using e.g. `logPrintError` will not
-- be affected!
setMinLogLevel :: LogLevel -> IO ()
setMinLogLevel x = modifyIORef minLogLevel (\(_, b) -> (x, b))

-- | Set the least logging level. Levels lower will not be logged. Log Level Order: `Debug` < `Info` < `Warning` < `Error`. `None` disables all logging. Note that the output to stderr using e.g. `logPrintError` will not
-- be affected!
setPrintLocationToConsole :: Bool -> IO ()
setPrintLocationToConsole x = modifyIORef minLogLevel (\(l, _) -> (l, x))


------------------------------ All ------------------------------

-- | Generates a function that takes a 'Text' and logs a 'LevelAll' message. Usage:
--
-- > $(logAll) ("This is a debug log message" :: T.Text)
logAll :: Q Exp
logAll = [| liftIO . logFun False $(qLocation >>= liftLoc) LogAll |]

-- | Same as logAll, but with concrete type `Text` as message.
--
-- > $(logAll) "This is a debug log message"
--
logAllText :: Q Exp
logAllText = [| liftIO . logFun False $(qLocation >>= liftLoc) LogAll . mkTxt |]


-- | Same as `logAll`, but for pure code. Uses @unsafePerformIO@.
--
-- > $(pureLogAll) "This is a debug log message" (3 * 3)
pureLogAll :: Q Exp
pureLogAll = [| \txt a -> unsafePerformIO (logFun False $(qLocation >>= liftLoc) LogAll txt >> return a) |]

-- | Same as `pureLogAll`, but with concrete type `Text` as message.
pureLogAllText :: Q Exp
pureLogAllText = [| \txt a -> unsafePerformIO (logFun False $(qLocation >>= liftLoc) LogAll (mkTxt txt) >> return a) |]


-- | Same as `logAll`, but also prints the message on `stdout`.
logPrintAll :: Q Exp
logPrintAll = [| liftIO . logFun True $(qLocation >>= liftLoc) LogAll |]

-- | Same as `logAll`, but also prints the message on `stdout`. Only for `Text`.
logPrintAllText :: Q Exp
logPrintAllText = [| liftIO . logFun True $(qLocation >>= liftLoc) LogAll . mkTxt |]


-- | Same as `pureLogAll`, but also prints the message on `stdout`.
--
-- > $(pureLogPrintAll) "This is a debug log message" (3 * 3)
pureLogPrintAll :: Q Exp
pureLogPrintAll = [| \txt a -> unsafePerformIO (logFun True $(qLocation >>= liftLoc) LogAll txt >> return a) |]


-- | Same as `pureLogPrintAll`, but with concrete type `Text` as log message.
pureLogPrintAllText :: Q Exp
pureLogPrintAllText = [| \txt a -> unsafePerformIO (logFun True $(qLocation >>= liftLoc) LogAll (mkTxt txt) >> return a) |]


------------------------------ Debug ------------------------------

-- | Generates a function that takes a 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $(logDebug) "This is a debug log message"
logDebug :: Q Exp
logDebug = [| liftIO . logFun False $(qLocation >>= liftLoc) LogDebug |]

-- | Same as `logDebug` but with `Text` as fixed message type.
logDebugText :: Q Exp
logDebugText = [| liftIO . logFun False $(qLocation >>= liftLoc) LogDebug . mkTxt |]


-- | Same as `logDebug`, but for pure code. Uses @unsafePerformIO@
--
-- > $(pureLogDebug) "This is a debug log message" defaultValue
pureLogDebug :: Q Exp
pureLogDebug = [| \txt a -> unsafePerformIO (logFun False $(qLocation >>= liftLoc) LogDebug txt >> return a) |]


-- | Same as `pureLogDebug`, but with concrete type `Text` as message.
pureLogDebugText :: Q Exp
pureLogDebugText = [| \txt a -> unsafePerformIO (logFun False $(qLocation >>= liftLoc) LogDebug (mkTxt txt) >> return a) |]


-- | Same as `logDebug`, but also prints the message on `stdout`.
logPrintDebug :: Q Exp
logPrintDebug = [| liftIO . logFun True $(qLocation >>= liftLoc) LogDebug |]

-- | Same as `logDebug`, but also prints the message on `stdout`. Only for Text.
logPrintDebugText :: Q Exp
logPrintDebugText = [| liftIO . logFun True $(qLocation >>= liftLoc) LogDebug . mkTxt |]


-- | Same as `pureLogDebug`, but also prints the message on `stdout`.
--
-- > $(purePrintLogDebug) "This is a debug log message" defaultValue
pureLogPrintDebug :: Q Exp
pureLogPrintDebug = [| \txt a -> unsafePerformIO (logFun True $(qLocation >>= liftLoc) LogDebug txt >> return a) |]


-- | Same as `pureLogPrintDebug`, but with concrete type `Text` as log message.
pureLogPrintDebugText :: Q Exp
pureLogPrintDebugText = [| \txt a -> unsafePerformIO (logFun True $(qLocation >>= liftLoc) LogDebug (mkTxt txt) >> return a) |]


------------------------------ Info ------------------------------

-- | Generates a function that takes a 'Text' and logs a 'LevelInfo' message. Usage:
--
-- > $(logInfo) "This is a info log message"
logInfo :: Q Exp
logInfo = [| liftIO . logFun False $(qLocation >>= liftLoc) LogInfo |]


-- | Same as `logInfo` but with `Text` as fixed message type.
logInfoText :: Q Exp
logInfoText = [| liftIO . logFun False $(qLocation >>= liftLoc) LogInfo . mkTxt |]


-- | Same as `logInfo`, but for pure code. Uses @unsafePerformIO@.
--
-- > $(pureLogInfo) "This is a warning log message" (funcX 10)
pureLogInfo :: Q Exp
pureLogInfo = [| \txt a -> unsafePerformIO (logFun False $(qLocation >>= liftLoc) LogInfo txt >> return a) |]


-- | Same as `pureLogInfo`, but with concrete type `Text` as message.
pureLogInfoText :: Q Exp
pureLogInfoText = [| \txt a -> unsafePerformIO (logFun False $(qLocation >>= liftLoc) LogInfo (mkTxt txt) >> return a) |]


-- | Same as `logInfo`, but also prints the message on `stdout`.
logPrintInfo :: Q Exp
logPrintInfo = [| liftIO . logFun True $(qLocation >>= liftLoc) LogInfo |]

-- | Same as `logInfo`, but also prints the message on `stdout`.
logPrintInfoText :: Q Exp
logPrintInfoText = [| liftIO . logFun True $(qLocation >>= liftLoc) LogInfo . mkTxt |]


-- | Same as `pureLogInfo`, but also prints the message on `stdout`.
--
-- > $(pureLogPrintInfo) "This is a warning log message" (funcX 10)
pureLogPrintInfo :: Q Exp
pureLogPrintInfo = [| \txt a -> unsafePerformIO (logFun True $(qLocation >>= liftLoc) LogInfo txt >> return a) |]


-- | Same as `pureLogPrintInfo`, but with concrete type `Text` as log message.
pureLogPrintInfoText :: Q Exp
pureLogPrintInfoText = [| \txt a -> unsafePerformIO (logFun True $(qLocation >>= liftLoc) LogInfo (mkTxt txt) >> return a) |]


------------------------------ Warning ------------------------------

-- | Generates a function that takes a 'Text' and logs a 'LevelWarning' message. Usage:
--
-- > $(logWarning) "This is a warning log message"
logWarning :: Q Exp
logWarning = [| liftIO . logFun False $(qLocation >>= liftLoc) LogWarning |]

-- | Same as `logWarning` but with `Text` as fixed message type.
logWarningText :: Q Exp
logWarningText = [| liftIO . logFun False $(qLocation >>= liftLoc) LogWarning . mkTxt |]


-- | Same as `logWarning`, but for pure code. Uses @unsafePerformIO@.
--
-- > $(pureLogWarning) "This is a warning log message" "myresult"
pureLogWarning :: Q Exp
pureLogWarning = [| \txt a -> unsafePerformIO (logFun False $(qLocation >>= liftLoc) LogWarning txt >> return a) |]

-- | Same as `pureLogWarning`, but with concrete type `Text` as message.
pureLogWarningText :: Q Exp
pureLogWarningText = [| \txt a -> unsafePerformIO (logFun False $(qLocation >>= liftLoc) LogWarning (mkTxt txt) >> return a) |]


-- | Same as `logWarning`, but also prints the message on `stdout`.
logPrintWarning :: Q Exp
logPrintWarning = [| liftIO . logFun True $(qLocation >>= liftLoc) LogWarning |]


-- | Same as `logWarning`, but also prints the message on `stdout`.
logPrintWarningText :: Q Exp
logPrintWarningText = [| liftIO . logFun True $(qLocation >>= liftLoc) LogWarning . mkTxt |]


-- | Same as `pureLogWarning`, but also prints the warning.
--
-- > $(pureLogPrintWarning) "This is a error log message" (4 + 4)
pureLogPrintWarning :: Q Exp
pureLogPrintWarning = [| \txt a -> unsafePerformIO (logFun True $(qLocation >>= liftLoc) LogWarning txt >> return a)  |]


-- | Same as `pureLogPrintWarning`, but with concrete type `Text` as log message.
pureLogPrintWarningText :: Q Exp
pureLogPrintWarningText = [| \txt a -> unsafePerformIO (logFun True $(qLocation >>= liftLoc) LogWarning (mkTxt txt) >> return a) |]


------------------------------ Error ------------------------------

-- | Generates a function that takes a 'Text' and logs a 'LevelError' message. Usage:
--
-- > $(logError) "This is a error log message"
logError :: Q Exp
logError = [| liftIO . logFun False $(qLocation >>= liftLoc) LogError |]

-- | Same as `logError` but with `Text` as fixed message type.
logErrorText :: Q Exp
logErrorText = [| liftIO . logFun False $(qLocation >>= liftLoc) LogError . mkTxt |]

-- | Same as `logError`, but for pure code. Uses @unsafePerformIO@.
--
-- > $(pureLogError) "This is a error log message" (4 + 4)
pureLogError :: Q Exp
pureLogError = [| \txt a -> unsafePerformIO (logFun False $(qLocation >>= liftLoc) LogError txt >> return a) |]


-- | Same as `pureLogError`, but with concrete type `Text` as message.
pureLogErrorText :: Q Exp
pureLogErrorText = [| \txt a -> unsafePerformIO (logFun False $(qLocation >>= liftLoc) LogError (mkTxt txt) >> return a) |]


-- | Same as `logError`, but also prints the message on `stderr`.
logPrintError :: Q Exp
logPrintError = [| liftIO . logFun True $(qLocation >>= liftLoc) LogError |]


-- | Same as `logError`, but also prints the message on `stderr`.
logPrintErrorText :: Q Exp
logPrintErrorText = [| liftIO . logFun True $(qLocation >>= liftLoc) LogError . mkTxt |]


-- | Same as `pureLogError`, but also prints the message on `stderr`.
--
-- > $(pureLogPrintError) "This is a error log message" (4 + 4)
pureLogPrintError :: Q Exp
pureLogPrintError = [| \txt a -> unsafePerformIO (logFun True $(qLocation >>= liftLoc) LogError txt >> return a) |]

-- | Same as `pureLogPrintError`, but with concrete type `Text` as log message.
pureLogPrintErrorText :: Q Exp
pureLogPrintErrorText = [| \txt a -> unsafePerformIO (logFun True $(qLocation >>= liftLoc) LogError (mkTxt txt) >> return a) |]


---- Helpers:

defaultLogStr :: Bool
              -> Loc
              -> FormattedTime
              -> LogLevel
              -> LogStr
              -> LogStr
defaultLogStr prLoc loc time level msg =
  "[" <> toLogStr (logLevelText level) <> ("#" <> toLogStr time) <> "] " <> mkTrailWs msg <> (if prLoc then " @(" <> toLogStr (S8.pack fileLocStr) <> ")\n" else "\n")
  where
    mkTrailWs = mkMinLogStrLen defaultMinLogMsgLen
    fileLocStr = loc_package loc ++ ':' : loc_module loc ++ ' ' : loc_filename loc ++ ':' : line loc ++ ':' : char loc ++ '-' : lineEnd loc ++ ':' : charEnd loc
    line = show . fst . loc_start
    char = show . snd . loc_start
    lineEnd = show . fst . loc_end
    charEnd = show . snd . loc_end


defaultMinLogMsgLen :: Int
defaultMinLogMsgLen = 60


