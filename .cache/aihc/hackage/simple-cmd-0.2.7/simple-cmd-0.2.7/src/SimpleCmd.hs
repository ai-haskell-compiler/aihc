{-# LANGUAGE CPP #-}

{-|
Some simple String wrappers of `readProcess`, `readProcessWithExitCode`,
`rawSystem` from the Haskell <https://hackage.haskell.org/package/process process> library.

Simplest is

@cmd_ :: String -> [String] -> IO ()@

which outputs to stdout. For example:

@cmd_ "git" ["clone", url]@

Then

@cmd :: String -> [String] -> IO String@

returns stdout as a @String@.

There are also @cmdBool@, @cmdMaybe@, @cmdLines@, @shell@, and others.

Other examples:

@grep_ pat file :: IO Bool@

@sudo c args :: IO ()@

-}

module SimpleCmd (
  cmd, cmd_,
  cmdBool,
  cmdIgnoreErr, {- badly named -}
  cmdLines,
  cmdMaybe,
  cmdFull,
  cmdLog_, cmdLog, cmdlog {-TODO: remove for 0.3 -},
  cmdN,
  cmdQuiet,
  cmdSilent,
  cmdStdIn,
  cmdStdErr,
  cmdTry_,
  cmdStderrToStdout,
  cmdStderrToStdoutIn,
  needProgram,
  error',
  warning,
  newline,
  logMsg,
  (+-+),
  removePrefix, removeStrictPrefix, removeSuffix,
  egrep_, grep, grep_,
  shell, shell_,
  shellBool,
#ifndef mingw32_HOST_OS
  sudo, sudo_, sudoLog, sudoInternal,
#endif
  PipeCommand,
  pipe, pipe_, pipeBool,
  pipe3, pipe3_, pipeFile_,
  ifM,
  whenM,
  filesWithExtension,
  fileWithExtension,
  timeIO
  ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception
import Control.Monad.Extra

import Data.List (
#if !MIN_VERSION_filepath(1,4,2)
  isSuffixOf,
#endif
  stripPrefix)
import Data.Maybe (isJust, isNothing, fromMaybe)
import Data.Time.Clock
#if MIN_VERSION_time(1,9,0)
import Data.Time.Format (formatTime, defaultTimeLocale)
#endif
import System.Directory (findExecutable, listDirectory)
import System.Exit (ExitCode (..))
import System.FilePath
import System.IO (hGetContents, hPutStr, hPutStrLn, IOMode(ReadMode),
                  stderr, stdout, withFile, Handle)
#ifndef mingw32_HOST_OS
import System.Posix.User (getEffectiveUserID)
#endif
import System.Process (createProcess, CreateProcess (cmdspec), proc,
                       ProcessHandle,
                       rawSystem, readProcess,
                       readProcessWithExitCode, runProcess, showCommandForUser,
                       std_err, std_in, std_out,
                       StdStream(CreatePipe, UseHandle),
                       waitForProcess, withCreateProcess)

removeTrailingNewline :: String -> String
removeTrailingNewline "" = ""
removeTrailingNewline str =
  if last str == '\n'
  then init str
  else str

quoteCmd :: String -> [String] -> String
quoteCmd = showCommandForUser

-- | Alias for errorWithoutStackTrace (for base >= 4.9)
--
-- @since 0.1.4
error' :: String -> a
#if (defined(MIN_VERSION_base) && MIN_VERSION_base(4,9,0))
error' s = errorWithoutStackTrace $! s
#else
error' s = error $! s
#endif

-- | @cmd c args@ runs a command in a process and returns stdout
cmd :: String -- ^ command to run
    -> [String] -- ^ list of arguments
    -> IO String -- ^ stdout
cmd c args = cmdStdIn c args ""

-- | @cmd_ c args@ runs command in a process, output goes to stdout and stderr
cmd_ :: String -> [String] -> IO ()
cmd_ c args = do
  ret <- rawSystem c args
  case ret of
    ExitSuccess -> return ()
    ExitFailure n -> error' $ quoteCmd c args +-+ "failed with exit code" +-+ show n

boolWrapper :: IO ExitCode -> IO Bool
boolWrapper pr = do
  ret <- pr
  case ret of
    ExitSuccess -> return True
    ExitFailure _ -> return False

-- | @cmdBool c args@ runs a command, and return Boolean status
cmdBool :: String -> [String] -> IO Bool
cmdBool c args =
  boolWrapper (rawSystem c args)

-- | @cmdMaybe c args@ runs a command, maybe returning output if it succeeds
cmdMaybe :: String -> [String] -> IO (Maybe String)
cmdMaybe c args = do
  (ok, out, _err) <- cmdFull c args ""
  return $ if ok then Just out else Nothing

-- | @cmdLines c args@ runs a command, and returns list of stdout lines
--
-- @since 0.1.1
cmdLines :: String -> [String] -> IO [String]
cmdLines c args = lines <$> cmd c args

-- | @cmdStdIn c args inp@ runs a command, passing input string as stdin, and returns stdout
cmdStdIn :: String -> [String] -> String -> IO String
cmdStdIn c args inp = removeTrailingNewline <$> readProcess c args inp

-- | @shell cs@ runs a command string in a shell, and returns stdout
shell :: String -> IO String
shell cs = cmd "sh" ["-c", cs]

-- | @shell_ cs@ runs a command string in a shell, output goes to stdout
shell_ :: String -> IO ()
shell_ cs = cmd_ "sh" ["-c", cs]

-- | @shellBool cs@ runs a command string in a shell, output goes to stdout
--
-- @since 0.2.0
shellBool :: String -> IO Bool
shellBool cs =
  boolWrapper (rawSystem "sh" ["-c", cs])

-- | @cmdLog_ c args@ logs a command with a datestamp
--
-- @since 0.2.7
cmdLog_ :: String -> [String] -> IO ()
cmdLog_ c args = do
  logMsg $ unwords $ c:args
  cmd_ c args

-- FIXME change in 0.3
-- | deprecated, use @cmdLog_@ instead (will change type in 0.3)
--
-- @since 0.1.4
cmdLog :: String -> [String] -> IO ()
cmdLog = cmdLog_

-- | @cmdlog@ deprecated alias for 'cmdLog_' (will be removed in 0.3)
cmdlog :: String -> [String] -> IO ()
cmdlog = cmdLog_

-- | @logMsg msg@ outputs message with a timestamp
logMsg :: String -> IO ()
logMsg msg = do
  date <- cmd "date" ["+%T"]
  putStrLn $ date +-+ msg

-- | @cmdN c args@ dry-runs a command: prints command to stdout - more used for debugging
cmdN :: String -> [String] -> IO ()
cmdN c args = putStrLn $ unwords $ c : map show args

-- | @cmdStdErr c args@ runs command in a process, returning stdout and stderr
cmdStdErr :: String -> [String] -> IO (String, String)
cmdStdErr c args = do
  (_ok, out, err) <- cmdFull c args ""
  return (out, err)

-- -- | @cmdAssert msg c args@ runs command, if it fails output msg as error'.
-- cmdAssert :: String -> String -> [String] -> IO ()
-- cmdAssert msg c args = do
--   ret <- rawSystem c args
--   case ret of
--     ExitSuccess -> return ()
--     ExitFailure _ -> error' msg

-- | @cmdQuiet c args@ runs a command hiding stderr, if it succeeds returns stdout
cmdQuiet :: String -> [String] -> IO String
cmdQuiet c args = do
  (ok, out, err) <- cmdFull c args ""
  return $ if ok
    then out
    else error' $ quoteCmd c args +-+ "failed with\n" ++ err

-- | @cmdSilent c args@ runs a command hiding stdout: stderr is only output if it fails.
cmdSilent :: String -> [String] -> IO ()
cmdSilent c args = do
  (ret, _, err) <- cmdFull c args ""
  unless ret $
    error' $ quoteCmd c args +-+ "failed with\n" ++ err

-- -- | @cmdSilentIn c args inp@ is like @cmdSilent@ but additionally takes some stdin
-- cmdSilentIn :: String -> [String] -> String -> IO ()
-- cmdSilentIn c args inp = do
--   (ret, _, err) <- cmdFull c args inp
--   unless ret $
--     error' $ quoteCmd c args +-+ "failed with:\n" ++ err

-- | @cmdIgnoreErr c args inp@ runs a command with input, drops stderr, and return stdout
cmdIgnoreErr :: String -> [String] -> String -> IO String
cmdIgnoreErr c args input = do
  (_ret, out, _err) <- cmdFull c args input
  return out

-- | @cmdFull c args inp@ runs readProcessWithExitCode and converts the ExitCode to Bool
-- Removes the last newline from stdout and stderr (like the other functions)
cmdFull :: String -> [String] -> String -> IO (Bool, String, String)
cmdFull c args input = do
  (ret, out, err) <- readProcessWithExitCode c args input
  return (ret == ExitSuccess, removeTrailingNewline out, removeTrailingNewline err)

-- | @cmdTry_ c args@ runs the command if available
--
-- @since 0.2.1
cmdTry_ :: String -> [String] -> IO ()
cmdTry_ c args = do
  have <- findExecutable c
  when (isJust have) $
    cmd_ c args

-- | Redirect stderr to stdout, ie with interleaved output
--
-- @since 0.2.2
cmdStderrToStdout :: String -> [String] -> IO (ExitCode, String)
cmdStderrToStdout c args = do
  (_ , Just hout, _, p) <- createProcess ((proc c args)
                                          {std_out = CreatePipe,
                                           std_err = UseHandle stdout})
  ret <- waitForProcess p
  out <- hGetContents hout
  return (ret, removeTrailingNewline out)

-- | Redirect stderr to stdout, ie with interleaved output
--
-- @since 0.2.3
cmdStderrToStdoutIn :: String -> [String] -> String -> IO (Bool, String)
cmdStderrToStdoutIn c args inp = do
  (Just hin, Just hout, _, p) <- createProcess ((proc c args)
                                          {std_in  = CreatePipe,
                                           std_out = CreatePipe,
                                           std_err = UseHandle stdout})
  hPutStr hin inp
  ret <- waitForProcess p
  out <- hGetContents hout
  return (ret == ExitSuccess, removeTrailingNewline out)

-- | @grep pat file@ greps pattern in file, and returns list of matches
--
-- @since 0.1.2 (fixed not to error in 0.2.2)
grep :: String -> FilePath -> IO [String]
grep pat file = do
  mres <- cmdMaybe "grep" [pat, file]
  return $ maybe [] lines mres

-- | @grep_ pat file@ greps pattern in file and returns Boolean status
grep_ :: String -- ^ pattern
      -> FilePath -- ^ file
      -> IO Bool -- ^ result
grep_ pat file =
  cmdBool "grep" ["-q", pat, file]

-- | @egrep_ pat file@ greps extended regexp in file, and returns Boolean status
egrep_ :: String -> FilePath -> IO Bool
egrep_ pat file =
  cmdBool "grep" ["-q", "-e", pat, file]

#ifndef mingw32_HOST_OS
-- | @sudo c args@ runs a command as sudo returning stdout
--
-- Result type changed from IO () to IO String in 0.2.0
sudo :: String -- ^ command
     -> [String] -- ^ arguments
     -> IO String
sudo = sudoInternal cmd

-- | @sudo_ c args@ runs a command as sudo
--
-- @since 0.2.0
-- @since 0.2.7 no longer logs (use sudoLog)
sudo_ :: String -- ^ command
     -> [String] -- ^ arguments
     -> IO ()
sudo_ = sudoInternal cmd_

-- | @sudo_ c args@ runs a command as sudo
--
-- @since 0.2.7
sudoLog :: String -- ^ command
     -> [String] -- ^ arguments
     -> IO ()
sudoLog = sudoInternal cmdLog_

-- | @sudoInternal@ converts a command runner to sudo
--
-- Uses sudo unless the effective uid is 0, otherwise errors if sudo not found.
--
-- @since 0.2.7
sudoInternal :: (String -> [String] -> IO a) -> String -> [String] -> IO a
sudoInternal exc c args = do
  uid <- getEffectiveUserID
  sd <- if uid == 0
    then return Nothing
    else findExecutable "sudo"
  let noSudo = isNothing sd
  when (uid /= 0 && noSudo) $
    warning "'sudo' not found"
  exc (fromMaybe c sd) (if noSudo then args else c:args)
#endif

-- | Combine two strings with a single space
infixr 4 +-+
(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t | last s == ' ' = s ++ t
        | head t == ' ' = s ++ t
s +-+ t = s ++ " " ++ t

-- singleLine :: String -> String
-- singleLine "" = ""
-- singleLine s = (head . lines) s

-- | @removePrefix prefix original@ removes prefix from string if present
removePrefix :: String -> String-> String
removePrefix prefix orig =
  fromMaybe orig $ stripPrefix prefix orig

-- | @removeStrictPrefix prefix original@ removes prefix, or fails with error'
removeStrictPrefix :: String -> String -> String
removeStrictPrefix prefix orig =
  fromMaybe (error' prefix +-+ "is not prefix of" +-+ orig) $ stripPrefix prefix orig

-- | @removeSuffix suffix original@ removes suffix from string if present
removeSuffix :: String -> String -> String
removeSuffix suffix orig =
  fromMaybe orig $ stripSuffix suffix orig
  where
    stripSuffix sf str = reverse <$> stripPrefix (reverse sf) (reverse str)

-- | @warning@ outputs to stderr
--
-- @since 0.2.0
warning :: String -> IO ()
warning s = hPutStrLn stderr $! s

-- | output a newline
--
-- @since 0.2.7
newline ::IO ()
newline = putStrLn ""

-- | Type alias for a command in a pipe
--
-- @since 0.2.0
type PipeCommand = (String,[String])

withCreateProcessOutput :: CreateProcess -> (Handle  -> ProcessHandle -> IO a) -> IO a
withCreateProcessOutput p act =
  withCreateProcess p $
    \ _si mso _se p' ->
      case mso of
        Nothing -> error $ "no stdout handle for: " ++ show (cmdspec p)
        Just so -> act so p'

-- | Return stdout from piping the output of one process to another
--
-- @since 0.2.0
pipe :: PipeCommand -> PipeCommand -> IO String
pipe (c1,args1) (c2,args2) =
  withCreateProcessOutput ((proc c1 args1) { std_out = CreatePipe }) $
    \ ho1 p1 -> do
      (_, mho2, _, p2) <- createProcess ((proc c2 args2) {std_in = UseHandle ho1, std_out = CreatePipe})
      case mho2 of
        Nothing -> error $ "no stdout handle for: " ++ c2
        Just ho2 -> do
          out <- hGetContents ho2
          void $ waitForProcess p1
          void $ waitForProcess p2
          return $ removeTrailingNewline out

-- | Pipe two commands without returning anything
--
-- @since 0.2.0
pipe_ :: PipeCommand -> PipeCommand -> IO ()
pipe_ (c1,args1) (c2,args2) =
  void $ pipeBool (c1,args1) (c2,args2)

-- | Bool result of piping of commands
-- @since 0.2.0
-- Returns False if either command fails (since 0.2.4).
pipeBool :: PipeCommand -> PipeCommand -> IO Bool
pipeBool (c1,args1) (c2,args2) =
  -- nicer with process-typed:
  -- withProcess_ (setStdout createPipe proc1) $ \ p -> runProcess (setStdin (useHandleClose (getStdout p)) proc2)
  withCreateProcess ((proc c1 args1) { std_out = CreatePipe }) $
    \ _si so _se p1 -> do
      p2 <- runProcess c2 args2 Nothing Nothing so Nothing Nothing
      ok1 <- boolWrapper $ waitForProcess p1
      ok2 <- boolWrapper $ waitForProcess p2
      return $ ok1 && ok2

-- | Pipe 3 commands, returning stdout
--
-- @since 0.2.3
pipe3 :: PipeCommand -> PipeCommand -> PipeCommand -> IO String
pipe3 (c1,a1) (c2,a2) (c3,a3) =
  withCreateProcessOutput ((proc c1 a1) { std_out = CreatePipe }) $
  \ ho1 p1 ->
    withCreateProcessOutput ((proc c2 a2) {std_in = UseHandle ho1, std_out = CreatePipe}) $
    \ ho2 p2 -> do
      (_, Just ho3, _, p3) <- createProcess ((proc c3 a3) {std_in = UseHandle ho2, std_out = CreatePipe})
      out <- hGetContents ho3
      forM_ [p1,p2,p3] waitForProcess
      return $ removeTrailingNewline out

-- | Pipe 3 commands, not returning anything
--
-- @since 0.2.0
pipe3_ :: PipeCommand -> PipeCommand -> PipeCommand -> IO ()
pipe3_ (c1,a1) (c2,a2) (c3,a3) =
  withCreateProcessOutput ((proc c1 a1) { std_out = CreatePipe }) $
  \ ho1 p1 ->
    withCreateProcess ((proc c2 a2) {std_in = UseHandle ho1, std_out = CreatePipe}) $
    \ _hi2 mho2 _he2 p2 -> do
      p3 <- runProcess c3 a3 Nothing Nothing mho2 Nothing Nothing
      forM_ [p1,p2,p3] waitForProcess

-- | Pipe a file to the first of a pipe of commands
--
-- @since 0.2.0
pipeFile_ :: FilePath -> PipeCommand -> PipeCommand -> IO ()
pipeFile_ infile (c1,a1) (c2,a2) =
  withFile infile ReadMode $
  \ hin ->
    withCreateProcess ((proc c1 a1) { std_in = UseHandle hin, std_out = CreatePipe }) $
    \ _si so _se p1 -> do
      p2 <- runProcess c2 a2 Nothing Nothing so Nothing Nothing
      void $ waitForProcess p1
      void $ waitForProcess p2

-- | Assert program in PATH
--
-- @needProgram progname@
--
-- @since 0.2.1
needProgram :: String -> IO ()
needProgram prog = do
  mx <- findExecutable prog
  unless (isJust mx) $ error' $ "missing program: " ++ prog

-- FIXME handle empty extension?
-- | returns the files with the give extension
filesWithExtension :: FilePath -- directory
                   -> String   -- file extension
                   -> IO [FilePath]
filesWithExtension dir ext =
  filter (ext `isExtensionOf`) <$> listDirectory dir

-- looks in dir for a unique file with given extension
fileWithExtension :: FilePath -- directory
                  -> String   -- file extension
                  -> IO (Maybe FilePath)
fileWithExtension dir ext = do
  files <- filesWithExtension dir ext
  case files of
       [file] -> return $ Just $ dir </> file
       [] -> return Nothing
       _ -> putStrLn ("More than one " ++ ext ++ " file found!") >> return Nothing

#if !MIN_VERSION_filepath(1,4,2)
isExtensionOf :: String -> FilePath -> Bool
isExtensionOf ext@('.':_) = isSuffixOf ext . takeExtensions
isExtensionOf ext         = isSuffixOf ('.':ext) . takeExtensions
#endif

-- | Run an IO action and then print how long it took
timeIO :: IO a -> IO a
timeIO action = do
  bracket
    getCurrentTime
    (\start -> do
        end <- getCurrentTime
        let duration = diffUTCTime end start
        putStrLn $ "took " ++ renderDuration duration)
    (const action)
  where
#if MIN_VERSION_time(1,9,0)
    renderDuration dur =
      let fmtstr
            | dur < 60 = "%s sec"
            | dur < 3600 = "%m min %S sec"
            | otherwise = "%h hours %M min"
      in formatTime defaultTimeLocale fmtstr dur
#else
    renderDuration = show
#endif
