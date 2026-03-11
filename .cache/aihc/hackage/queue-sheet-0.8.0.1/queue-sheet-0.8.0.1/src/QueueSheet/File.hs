------------------------------------------------------------------------------
-- |
-- Module      : QueueSheet.File
-- Description : queue sheet file loading
-- Copyright   : Copyright (c) 2020-2025 Travis Cardwell
-- License     : MIT
------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module QueueSheet.File
  ( -- * YAML
    loadYaml
  , loadYaml'
  ) where

-- https://hackage.haskell.org/package/base
import Control.Exception (displayException)
import Control.Monad (forM, forM_, unless, when)
import Data.Bifunctor (first)
import System.IO.Error (tryIOError)

-- https://hackage.haskell.org/package/bytestring
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)

-- https://hackage.haskell.org/package/directory
import System.Directory (makeAbsolute)

-- https://hackage.haskell.org/package/filepath
import System.FilePath ((</>), isAbsolute, normalise, takeDirectory)

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT, throwE)

-- https://hackage.haskell.org/package/ttc
import qualified Data.TTC as TTC

-- https://hackage.haskell.org/package/yaml
import qualified Data.Yaml as Yaml

-- (queue-sheet)
import QueueSheet.Types
  ( Import(Import, importPath, importSection)
  , ImportOrQueue(IQImport, IQQueue)
  , Queue(Queue, queueName, queueSection)
  , QueuesFile(QueuesFile, qfSections, qfImportOrQueues)
  , QueueSheet(QueueSheet, qsSections, qsQueues)
  )

------------------------------------------------------------------------------
-- $Yaml

-- | Load a queues YAML file, resolving imports
--
-- @since 0.3.0.0
loadYaml
  :: FilePath
  -> IO (Either String QueueSheet)
loadYaml path = runExceptT $ do
    let tryIOError' = fmap (first displayException) . tryIOError
    absPath <- ExceptT . tryIOError' $ makeAbsolute path
    ExceptT $ loadYaml' (tryIOError' . BS.readFile) absPath

-- | Load a queues YAML file using the given file loader, resolving imports
--
-- This function defines the logic for 'loadYaml' using an arbitrary monad.
-- It is exposed for testing purposes.
--
-- @since 0.3.0.0
loadYaml'
  :: forall m. Monad m
  => (FilePath -> m (Either String ByteString))  -- ^ file loader
  -> FilePath                                    -- ^ absolute path
  -> m (Either String QueueSheet)
loadYaml' loadFile = runExceptT . go []
  where
    go :: [FilePath] -> FilePath -> ExceptT String m QueueSheet
    go seenPaths path = do
      let error' = throwE . (("error loading " ++ path ++ ": ") ++)
          yamlError = error' . Yaml.prettyPrintParseException
      content <- ExceptT $ loadFile path
      QueuesFile{..} <- either yamlError pure $ Yaml.decodeEither' content
      queues <- fmap concat . forM qfImportOrQueues $ \case
        IQImport Import{..} -> do
          let seenPaths' = path : seenPaths
              importAbsPath
                | isAbsolute importPath = importPath
                | otherwise = normalise $ takeDirectory path </> importPath
          when (importAbsPath `elem` seenPaths') . error' $
            "cyclic import: " ++ importAbsPath
          queueSheet <- go seenPaths' importAbsPath
          let queues' = qsQueues queueSheet
          case importSection of
            Just section -> do
              unless (section `elem` qfSections) . error' . unwords $
                [ "import", importPath
                , "has unknown section", TTC.render section
                ]
              return [queue{ queueSection = section } | queue <- queues']
            Nothing -> do
              forM_ queues' $ \Queue{..} ->
                unless (queueSection `elem` qfSections) . error' . unwords $
                  [ "queue", TTC.render queueName
                  , "imported from", importPath
                  , "has unknown section", TTC.render queueSection
                  ]
              return queues'
        IQQueue queue@Queue{..} -> do
          unless (queueSection `elem` qfSections) . error' . unwords $
            [ "queue", TTC.render queueName
            , "has unknown section", TTC.render queueSection
            ]
          return [queue]
      return $ QueueSheet
        { qsSections = qfSections
        , qsQueues   = queues
        }
