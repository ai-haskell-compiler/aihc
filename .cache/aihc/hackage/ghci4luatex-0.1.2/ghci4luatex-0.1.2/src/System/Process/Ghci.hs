{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}


--------------------------------------------------------------------------------
-- |
--
-- Module      :  System.Process.Ghci
-- Description :  GHCi as a process
-- Copyright   :  (c) Alice Rixte 2024
-- License     :  BSD 3
-- Maintainer  :  alice.rixte@u-bordeaux.fr
-- Stability   :  unstable
-- Portability :  non-portable (GHC extensions)
--
-- Run GHCi as a process, and execute commands.
--
-- = Usage
--
-- >>> ghci <- startGhci Quiet "ghci" []
-- >>> execGhciCmd ghci Quiet "1+2"
-- GhciResult {ghciErr = "", ghciOut = "3"}
--
--------------------------------------------------------------------------------

module System.Process.Ghci
  ( Ghci (..)
  , startGhci
  , GhciResult (..)
  , execGhciCmd
  )
where

import Control.Exception
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import GHC.Generics
import System.IO
import System.IO.Error
import System.Process
import Data.String
import Data.Text

import System.Console.CmdArgs.Verbosity



import Data.Aeson

-- | A GHCi process
--
data Ghci = Ghci
  { ghciIn :: Handle -- ^ Standard input
  , ghciErrVar :: TMVar String -- ^ Current line on stderr
  , ghciOutVar :: TMVar String -- ^ Current line on stdout
  , ghciProcess :: ProcessHandle -- ^ Process handle
  , ghciErrId :: ThreadId -- ^ Stderr listener thread
  , ghciOutId :: ThreadId -- ^ Stdout listener thread
  }


-- | Start a GHCi process.
--
-- >>> ghci = startGhci Normal "ghci" []
--
-- >>> ghci = startGhci Loud "stack" ["ghci"]
--
startGhci ::
      Verbosity -- ^  When @verbosity >= 'Normal'@, the entire output
                --    of GHCi is printed
  -> String     -- ^ The command to run (e.g. "ghci", "cabal" or "stack")
  -> [String]   -- ^ The list of the command's arguments
                -- (e.g. "repl" for "cabal" or "ghci" for "stack")
  -> IO Ghci
startGhci v cmd args = do
  chans <- createProcess (proc cmd args) { std_in = CreatePipe
                                  , std_err = CreatePipe
                                  , std_out = CreatePipe
                                  }
  case chans of
    (Just hin, Just hout , Just herr , hp) -> do
      verr <- newEmptyTMVarIO
      vout <- newEmptyTMVarIO

      errId <- forkIO $ listenHandle verr herr
      outId <- forkIO $ listenHandle vout hout

      let g = Ghci hin verr vout hp errId outId

      -- the following is very hacky : Sometimes, "stack ghci" may ask some file
      -- to the user. For this reason we send ghci a confirmation for whatever
      -- default values it proposes, then wait for 200 ms to make sur ghci did
      -- receive that answer.
      flushGhciCmd (ghciIn g) "\n"
      threadDelay 2000000 -- wait 200 ms to make sure GHCi is ready to listen

      _ <- waitGhciResult g v

      return g
    _ -> error "Error : Ghci command failed."

-- | Listen to a handle by putting lines into a mutable variable.
--
listenHandle :: TMVar String -> Handle -> IO ()
listenHandle v h =
  catch (forever $ do
    s <- hGetLine h
    atomically $ putTMVar v s) handler
  where
    handler :: IOError -> IO ()
    handler err =
      unless (isEOFError err) $ throw err

-- | Merge stderr and stdout streams.
--
mergeErrOut :: TMVar String -> TMVar String -> IO (Either String String)
mergeErrOut verr vout=
  atomically $ (Left <$> takeTMVar verr) `orElse`
      ( Right . cleanResultString <$> takeTMVar vout)



-- | The result printed by GHCi.
--
data GhciResult = GhciResult
  { ghciErr :: Text -- Errors and warnings of GHCi
  , ghciOut :: Text -- Output of GHCi
  }
  deriving (Show, Generic)
  deriving Semigroup via Generically GhciResult
  deriving Monoid via Generically GhciResult

instance ToJSON GhciResult where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON GhciResult

-- | Sends a command to ghci and wait for its result.
--
-- >>> execGhciCmd ghci Normal "1+2"
-- GhciResult {ghciErr = "", ghciOut = "3"}
--
execGhciCmd
  :: Ghci       -- ^ A GHCi process
  -> Verbosity  -- ^ When @verbosity >= Normal@,  the entire output
                --    of GHCi is printed
  -> String     -- ^ The command to execute
  -> IO GhciResult -- ^ The result of the execution
execGhciCmd g v cmd = do
  flushGhciCmd (ghciIn g) cmd
  waitGhciResult g v



-- | Wait for GHCi to complete its computation
--
waitGhciResult :: Ghci -> Verbosity ->  IO GhciResult
waitGhciResult g v  = do

  -- this is a hack : we send a "putStrLn" command to ghci in order to be able
  -- to wait for the result of the command.
  flushGhciCmd (ghciIn g) $ "putStrLn\"" ++ readyString ++ "\"\n"
  loop mempty

  where
    loop acc = do
      s <- mergeErrOut (ghciErrVar g) (ghciOutVar g)
      if s == Right readyString then
        return acc
      else do
        when (v >= Normal) $
          case s of
            Left s' -> do
              hPutStr stderr s'
              hPutChar stderr '\n'
              hFlush stderr
            Right s' -> do
              putStrLn s'
              hFlush stdout
        loop (appendGhciResult acc s)

-- | A very unlikely string that we make ghci print in order to know when ghci
-- is finished.
--
-- This is a hack but it works well. The same hack is used by lhs2tex.
--
readyString :: String
readyString = "`}$/*^`a`('))}{h}"

-- | Flush a string to the standard input of of ghci.
--
-- Multiple line strings are
-- accepted and will be surrounded by ":{" and ":}"
--
flushGhciCmd :: Handle -> String -> IO ()
flushGhciCmd hin cmd = do
  hPutStr hin (":{\n" ++ cmd ++ "\n:}\n") -- TODO optimization when no newline ?
  hFlush hin

-- | Remove the @"ghci> "@ and @"ghci| "@ prefixes from the output stream of
-- ghci.
--
cleanResultString :: String -> String
cleanResultString ('g' : 'h' : 'c' : 'i' : '>': ' ' : s) =
  cleanResultString s
cleanResultString ('g' : 'h' : 'c' : 'i' : '|': ' ' : s) =
  cleanResultString s
cleanResultString s = s


-- | Utility function that merges the stderr and the stdout streams of
-- ghci.
--
appendGhciResult :: GhciResult -> Either String String -> GhciResult
appendGhciResult acc (Left s) =
  acc { ghciErr = ghciErr acc <> fromString s }
appendGhciResult acc (Right s) =
  acc { ghciOut = ghciOut acc <> fromString s }


