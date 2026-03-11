{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-|
Module      : Language.Alloy.Internal.Call
Copyright   : (c) Marcellus Siegburg, 2019 - 2025
License     : MIT

This module provides the basic internal functionality to retrieve the raw
results from calling Alloy.
It provides data types and functions to interact with Alloy.
-}
module Language.Alloy.Internal.Call (
  CallAlloyConfig (..),
  SatSolver (..),
  defaultCallAlloyConfig,
  getRawInstances,
  getRawInstancesWith,
  ) where

import qualified Data.ByteString                  as BS (
  intercalate,
  stripPrefix,
  )
import qualified Data.ByteString.Char8            as BS (
  hGetLine,
  putStrLn,
  unlines,
  )

import Control.Concurrent (
  threadDelay,
  )
import Control.Concurrent.Async (
  concurrently,
  mapConcurrently_,
  wait,
  withAsync
  )
import Control.Concurrent.Extra         (Lock, newLock, withLock)
import Control.Exception                (IOException, bracket, catch)
import Control.Monad                    (unless, when)
import Control.Monad.Extra              (whenJust)
import Data.ByteString                  (ByteString)
import Data.ByteString.Char8            (unpack)
import Data.IORef (
  IORef,
  atomicWriteIORef,
#ifdef mingw32_HOST_OS
  newIORef,
#endif
  readIORef,
  )
import Data.List                        (intercalate)
import Data.List.Split                  (splitOn)
import Data.Maybe                       (fromMaybe)
import System.Exit                      (ExitCode (..))
import System.FilePath
  (searchPathSeparator)
import System.IO (
#ifndef mingw32_HOST_OS
  BufferMode (..),
  hSetBuffering,
#endif
  Handle,
  hClose,
  hFlush,
  hIsEOF,
  hPutStr,
  hPutStrLn,
  stderr,
  )
import System.IO.Unsafe                 (unsafePerformIO)
import System.Process (
  CreateProcess (..), StdStream (..), ProcessHandle,
  cleanupProcess,
  createProcess, proc, terminateProcess, waitForProcess,
  )

import Language.Alloy.ResourceNames (
  className,
  classPackage,
  )
import Language.Alloy.Resources (
  alloyJar,
  commonsCliJar,
  slf4jJar,
  )
import Paths_call_alloy                 (getDataDir)

{-|
Available SAT solvers.

Note: solvers marked as not supported by default were supported
in earlier versions of Alloy, but got removed.
There might be a way of integrating those manually, but this has net been tried.
-}
data SatSolver
  = BerkMin
  -- ^ BerkMin
  --
  -- * not supported by default
  | Glucose
  -- ^ Glucose
  --
  -- * incremental
  | Glucose41
  -- ^ Glucose41
  --
  -- * not supported by default
  | Lingeling
  -- ^ Lingeling
  --
  -- * not incremental
  | MiniSat
  -- ^ MiniSat
  --
  -- * incremental
  | MiniSatProver
  -- ^ MiniSatProver
  --
  -- * incremental
  | PLingeling
  -- ^ PLingeling
  --
  -- * not supported by default
  | SAT4J
  -- ^ SAT4J
  --
  -- * incremental
  | SAT4JLight
  -- ^ SAT4J Light
  --
  -- * incremental
  | SAT4JPMax
  -- ^ SAT4J with Partial Maximum Satisfiability
  --
  -- * incremental
  | Spear
  -- ^ Spear
  --
  -- * not supported by default
  deriving (Bounded, Enum, Eq, Read, Show)

toParameter :: SatSolver -> String
toParameter = \case
  BerkMin -> "berkmin"
  Glucose -> "glucose"
  Glucose41 -> "glucose41"
  Lingeling -> "lingeling.parallel"
  MiniSat -> "minisat"
  MiniSatProver -> "minisat.prover"
  PLingeling -> "plingeling"
  SAT4J -> "sat4j"
  SAT4JLight -> "sat4j.light"
  SAT4JPMax -> "sat4j.pmax"
  Spear -> "spear"

{-|
Configuration for calling alloy.
-}
data CallAlloyConfig =
  -- | you should use 'defaultCallAlloyConfig' instead of this constructor
  -- as additional configuration parameters might be introduced in later
  -- releases.
  CallAlloyConfig {
  -- | maximal number of instances to retrieve ('Nothing' for all)
  maxInstances :: !(Maybe Integer),
  -- | whether to not overflow when calculating numbers within Alloy
  noOverflow   :: !Bool,
  -- | the 'SatSolver' to choose. Note that some are not incremental,
  --   i.e. will return only one solution, even if 'maxInstances' is set higher.
  satSolver    :: !SatSolver,
  -- | the time in microseconds after which to forcibly kill Alloy
  --   ('Nothing' for never)
  timeout      :: !(Maybe Int)
  }

{-|
Default configuration for calling Alloy. Defaults to:

 * retrieve all instances
 * do not overflow
 * 'SAT4J'
-}
defaultCallAlloyConfig :: CallAlloyConfig
defaultCallAlloyConfig = CallAlloyConfig {
  maxInstances = Nothing,
  noOverflow   = True,
  satSolver    = SAT4J,
  timeout      = Nothing
  }

{-# NOINLINE outLock #-}
outLock :: Lock
outLock = unsafePerformIO newLock

putOutLn :: String -> IO ()
putOutLn = withLock outLock . putStrLn

putErrLn :: String -> IO ()
putErrLn = withLock outLock . hPutStrLn stderr

{-|
This function may be used to get all raw model instances for a given Alloy
specification. It calls Alloy via a Java interface and splits the raw instance
answers before returning the resulting list of raw instances.
-}
getRawInstances
  :: Maybe Integer
  -- ^ How many instances to return; 'Nothing' for all.
  -> String
  -- ^ The Alloy specification which should be loaded.
  -> IO [ByteString]
getRawInstances maxIs = getRawInstancesWith defaultCallAlloyConfig {
  maxInstances = maxIs
  }

{-|
Creates an Alloy process using the given config.
-}
callAlloyWith
  :: CallAlloyConfig
  -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
callAlloyWith config = do
  classPath <- getClassPath
  let callAlloy = proc "java"
        $ [
          "--enable-native-access=ALL-UNNAMED",
          "-cp", classPath, classPackage ++ '.' : className,
          "-i", show $ fromMaybe (-1) $ maxInstances config
          ]
        ++ ["-o" | not $ noOverflow config]
        ++ ["-s", toParameter (satSolver config)]
  createProcess callAlloy {
    std_out = CreatePipe,
    std_in  = CreatePipe,
    std_err = CreatePipe
  }

{-|
This function may be used to get all raw model instances for a given Alloy
specification. It calls Alloy via a Java interface and splits the raw instance
answers before returning the resulting list of raw instances.
Parameters are set using a t'CallAlloyConfig'.
-}
getRawInstancesWith
  :: CallAlloyConfig
  -- ^ The configuration to be used.
  -> String
  -- ^ The Alloy specification which should be loaded.
  -> IO [ByteString]
getRawInstancesWith config content
  = bracket (callAlloyWith config) cleanupProcess $ \p -> do
  (Just hin, Just hout, Just herr, ph) <- return p
#ifndef mingw32_HOST_OS
  hSetBuffering hin NoBuffering
  let abort = Nothing
#else
  abort <- Just <$> newIORef False
#endif
  let evaluateAlloy' = do
        hPutStr hin content
        hFlush hin
        hClose hin
      evaluateAlloy = catch evaluateAlloy' $ \e -> do
        let err = show (e :: IOException)
            warn = "Maybe not complete instance was sent to Alloy "
            explain = "(Are timeouts set? Make sure they are not too small!): "
        putErrLn ("Warning: " ++ warn ++ explain ++ err)
  withTimeout hin hout herr ph abort (timeout config) $ do
    (out, err) <- fst <$> concurrently
      (concurrently (getOutput hout) (getOutput herr))
      evaluateAlloy
    printContentOnError out abort ph
    let err' = removeInfoLines err
    unless (null err') $ fail $ unpack $ BS.unlines err'
    return $ fmap (BS.intercalate "\n")
      $ filterLast ((/= partialInstance) . last)
      $ drop 1 $ splitOn [begin] out
  where
    begin :: ByteString
    begin = "---Trace---"
    filterLast _ []     = []
    filterLast p x@[_]  = filter p x
    filterLast p (x:xs) = x:filterLast p xs
    getOutput' h = do
      eof <- hIsEOF h
      if eof
        then return []
        else (:) <$> BS.hGetLine h <*> getOutput h
    getOutput h = catch
      (getOutput' h)
      (\(_ :: IOException) -> return [partialInstance])
    printContentOnError out abort ph = do
      code <- waitForProcess ph
      aborted <- maybe (return False) readIORef abort
      when (code == ExitFailure 1 && not aborted)
        $ putOutLn $ "Failed parsing the Alloy code?:\n" <> content
      when (code == ExitFailure 2 && not aborted)
        $ withLock outLock $ BS.putStrLn $ BS.unlines out

partialInstance :: ByteString
partialInstance = "---PARTIAL_INSTANCE---"

{-|
Removes lines starting with any of

@
[main] INFO
[main] WARN
[Finalizer] WARN
@

and partial instances

@
---PARTIAL_INSTANCE---
@

which seem to be appearing since Alloy-6.0.0
-}
removeInfoLines :: [ByteString] -> [ByteString]
removeInfoLines (x:xs)
  | Just _ <- BS.stripPrefix "[main] INFO" x
  = removeInfoLines xs
  | Just _ <- BS.stripPrefix "[main] WARN" x
  = removeInfoLines xs
  | Just _ <- BS.stripPrefix "[Finalizer] WARN" x
  = removeInfoLines xs
  | x == partialInstance
  = removeInfoLines xs
removeInfoLines xs = xs

{-|
Start a new sub process that communicates with the worker process
if a timeout is provided.
Execution is aborted by closing all handles and
killing the underlying worker processes after the given amount of time
(if it has not finished by then).
The process will wait for the sub process to make the result available.

If the provided timeout is 'Nothing', evaluation happens without
scheduled interruption in the main thread.
-}
withTimeout
  :: Handle
  -- ^ the input handle (of the worker) to close
  -> Handle
  -- ^ the output handle (of the worker) to close
  -> Handle
  -- ^ the error handle (of the worker) to close
  -> ProcessHandle
  -- ^ the worker process handle
  -> Maybe (IORef Bool)
  -- ^ the IORef to communicate process abortion on Windows
  -> Maybe Int
  -- ^ the timeout (Nothing if no timeout)
  -> IO a
  -- ^ some action interacting with the worker and its handles
  -> IO a
withTimeout _ _ _ _  _     Nothing  p = p
withTimeout i o e ph abort (Just t) p = withAsync p $ \a -> do
  threadDelay t
  whenJust abort (`atomicWriteIORef` True)
  mapConcurrently_ id [
    hClose e,
    hClose o,
    terminateProcess ph
    ]
  hClose i
  wait a

{-|
Get the class path of all files in the data directory.

Returns the class path.
-}
getClassPath :: IO FilePath
getClassPath =
  concatPaths <$> getDataDir <*> alloyJar <*> commonsCliJar <*> slf4jJar
  where
    concatPaths w x y z = intercalate
      [searchPathSeparator]
      [w, x, y, z]
