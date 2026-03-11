{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module      :  ELynx.Tools.ELynx
-- Description :  The ELynx transformer
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Sep  2 18:55:11 2021.
module ELynx.Tools.ELynx
  ( ELynx,
    eLynxWrapper,
    out,
    outHandle,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader hiding (local)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import ELynx.Tools.Environment
import ELynx.Tools.InputOutput
import ELynx.Tools.Logger
import ELynx.Tools.Options
import ELynx.Tools.Reproduction
import System.IO
import System.Random.Stateful

-- | ELynx transformer to be used with all executables.
type ELynx a = ReaderT (Environment a) IO

fixSeed :: (Reproducible a) => a -> IO a
fixSeed x = case getSeed x of
  (Just RandomUnset) -> do
    s <- uniformM globalStdGen :: IO Int
    return $ setSeed x (RandomSet s)
  _ -> return x

eLynxRun ::
  forall a b.
  (Eq a, Reproducible a, Reproducible b, Show a, ToJSON a) =>
  (b -> a) ->
  ELynx b () ->
  ELynx b ()
eLynxRun f worker = do
  -- Header.
  logInfoHeader (cmdName @b) (cmdDsc @b)
  mso <- reader (getSeed . localArguments)
  case mso of
    Nothing -> return ()
    Just (RandomSet s) -> logInfoS $ "Seed: random; set to " <> show s <> "."
    Just (Fixed s) -> logInfoS $ "Seed: fixed to " <> show s <> "."
    Just RandomUnset -> error "eLynxRun: Seed unset."
  -- Worker.
  worker
  -- Footer.
  e <- ask
  let g = globalArguments e
      l = localArguments e
  case (writeElynxFile g, outFileBaseName g) of
    (False, _) ->
      logInfoS "No elynx file option --- skip writing ELynx file for reproducible runs."
    (True, Nothing) ->
      logInfoS "No output file given --- skip writing ELynx file for reproducible runs."
    (True, Just bn) -> do
      logInfoS "Write ELynx reproduction file."
      liftIO $ writeReproduction bn (Arguments g (f l))
  -- Footer.
  logInfoFooter

-- | The 'ReaderT' wrapper for ELynx. Prints a header and a footer, logs to
-- 'stdout' and possibly a log file, if provided. Initializes the seed if none
-- is provided.
eLynxWrapper ::
  (Eq a, Show a, Reproducible a, Reproducible b, ToJSON a) =>
  GlobalArguments ->
  -- Local arguments.
  b ->
  -- Local arguments across all commands.
  (b -> a) ->
  ELynx b () ->
  IO ()
eLynxWrapper gArgs lArgs f worker = do
  -- 1. Fix seed.
  lArgs' <- fixSeed lArgs

  -- 2. Initialize environment.
  e <- initializeEnvironment gArgs lArgs'

  -- 3. Run.
  runReaderT (eLynxRun f worker) e

  -- 4. Close environment.
  closeEnvironment e

-- Get out file path with extension.
getOutFilePath ::
  forall a. (Reproducible a) => String -> ELynx a (Maybe FilePath)
getOutFilePath ext = do
  a <- ask
  let bn = outFileBaseName . globalArguments $ a
      sfxs = outSuffixes . localArguments $ a
  if ext `elem` sfxs
    then return $ (++ ext) <$> bn
    else
      error
        "getOutFilePath: out file suffix not registered; please contact maintainer."

-- | Write a result with a given name to file with given extension or standard
-- output. Supports compression.
out :: (Reproducible a) => String -> BL.ByteString -> String -> ELynx a ()
out name res ext = do
  mfp <- getOutFilePath ext
  case mfp of
    Nothing -> do
      logInfoS $ "Write " <> name <> " to standard output."
      liftIO $ BL.putStr res
    Just fp -> do
      logInfoS $ "Write " <> name <> " to file '" <> fp <> "'."
      em <- executionMode . globalArguments <$> ask
      liftIO $ writeGZFile em fp res

-- BUG: 'outHandle' is flawed. If '-o BASENAME' is not provided, the output
-- handle is stdout, but then, when closing the handle, stdout will be closed!
-- Big Bug.

-- | Get an output handle, does not support compression. The handle has to be
-- closed after use!
outHandle :: (Reproducible a) => String -> String -> ELynx a Handle
outHandle name ext = do
  mfp <- getOutFilePath ext
  case mfp of
    Nothing -> do
      logInfoS $ "Write " <> name <> " to standard output."
      return stdout
    Just fp -> do
      logInfoS $ "Write " <> name <> " to file '" <> fp <> "'."
      em <- executionMode . globalArguments <$> ask
      liftIO $ openFileWithExecutionMode em fp
