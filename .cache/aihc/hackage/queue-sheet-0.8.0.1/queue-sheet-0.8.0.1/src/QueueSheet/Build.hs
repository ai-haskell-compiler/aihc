------------------------------------------------------------------------------
-- |
-- Module      : QueueSheet.Build
-- Description : queue sheet build functions
-- Copyright   : Copyright (c) 2020-2025 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

module QueueSheet.Build
  ( -- * API
    buildPdf
  ) where

-- https://hackage.haskell.org/package/base
import Control.Exception (displayException)
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)
import System.IO.Error (tryIOError)

-- https://hackage.haskell.org/package/directory
import System.Directory
  ( createDirectory, doesPathExist, removeDirectoryRecursive, renameFile
  , withCurrentDirectory
  )

-- https://hackage.haskell.org/package/filepath
import System.FilePath ((</>), replaceExtension, takeFileName)

-- https://hackage.haskell.org/package/process
import qualified System.Process as Proc

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Class (MonadTrans(lift))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)

-- (queue-sheet)
import QueueSheet.File (loadYaml)
import QueueSheet.Template (loadTemplate, renderTemplate)

------------------------------------------------------------------------------
-- $Constants

-- | Build directory name
--
-- @since 0.3.0.0
buildDir :: FilePath
buildDir = "queue-sheet-build"

------------------------------------------------------------------------------
-- $API

-- | Build a PDF
--
-- @since 0.3.0.0
buildPdf
  :: FilePath        -- ^ queues path
  -> FilePath        -- ^ template path
  -> Maybe FilePath  -- ^ output path (default: queues path w/ .pdf extension)
  -> IO (Either String ())
buildPdf queuesPath templatePath mOutputPath = runExceptT $ do
    queueSheet <- ExceptT $ loadYaml queuesPath
    template <- ExceptT $ loadTemplate templatePath
    exists <- lift $ doesPathExist buildDir
    when exists . throwE $ "directory already exists: " ++ buildDir
    let outputPath = fromMaybe (replaceExtension queuesPath "pdf") mOutputPath
        sourcePath = takeFileName $ replaceExtension outputPath "tex"
    ExceptT . fmap (first displayException) . tryIOError $ do
      createDirectory buildDir
      withCurrentDirectory buildDir $ do
        renderTemplate sourcePath template queueSheet
        Proc.callProcess "xelatex" ["-halt-on-error", sourcePath]
      renameFile (buildDir </> takeFileName outputPath) outputPath
      removeDirectoryRecursive buildDir
