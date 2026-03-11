{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{- |
Module      :  Neovim.BuildTool
Description :  Utilities and types to manage build tool dependent things
Copyright   :  (c) Sebastian Witte
License     :  Apache-2.0

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental
Portability :  GHC

-}
module Neovim.BuildTool
    where

import Neovim
import Data.List (isSuffixOf)
import Control.Monad.IO.Class
import GHC.Generics
import Data.Yaml
import System.Directory
import System.FilePath (takeDirectory, (</>))

data BuildTool
    = Stack
    | Cabal CabalType
    | Shake
    | Make
    | Cmake
    | Ninja
    | Scons
    | Custom
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON BuildTool
instance FromJSON BuildTool

data CabalType
    = Plain
    | Sandbox
    | NewBuild
    deriving (Show, Read, Eq, Ord, Enum, Generic)

instance ToJSON CabalType
instance FromJSON CabalType

newtype Directory = Directory { getDirectory :: FilePath }
    deriving (Show, Eq, Ord)


-- | If the monadic boolean predicate returns true, wrap the given object in a
-- 'Just' constructor, otherwise return 'Nothing'.
partialM :: Monad m => (a -> m Bool) -> a -> m (Maybe a)
partialM fp a = fp a >>= \case
    True -> return (Just a)
    False -> return Nothing

-- | Create 'Just' a 'Directory' value if the given filepath exists and
-- otherwise return 'Nothing'. This method does not create an actual directory
-- on your file system.
mkDirectory :: MonadIO io => FilePath -> io (Maybe Directory)
mkDirectory mdir =
    fmap Directory <$> partialM (liftIO . doesDirectoryExist) mdir

newtype File = File { getFile :: FilePath }
    deriving (Show, Eq, Ord)

-- | Create 'Just' a 'File' value if the given file exists and is not a
-- directory. Otherwise return 'Nothing'. This function does not alter your
-- filesystem.
mkFile :: MonadIO io => Maybe Directory -> FilePath -> io (Maybe File)
mkFile mdir mfile =
    let f = maybe mfile (\d -> getDirectory d </> mfile) mdir
    in fmap File <$> partialM (liftIO . doesFileExist) f

-- | Calculate the list of all parent directories for the given directory. This
-- function also returns the initially specified directory.
thisAndParentDirectories :: Directory -> [Directory]
thisAndParentDirectories dir
    | parentDir == dir = [dir]
    | otherwise = dir : thisAndParentDirectories parentDir
  where
    parentDir = Directory .  takeDirectory $ getDirectory dir


-- | Given a list of build tool identifier functions, apply these to all the
-- given directories and return the value of the first function that returns a
-- 'BuildTool' value or 'Nothing' if no function ever returns a 'BuildTool'.
-- The identifier functions and directories are tried in the order as supplied
-- to this function.
determineProjectSettings
    :: MonadIO io
    => [Directory -> io (Maybe BuildTool)]
    -> [Directory]
    -> io (Maybe (BuildTool, Directory))
determineProjectSettings identifiers = go identifiers
  where
    go _ []             = return Nothing
    go [] (_:ps)        = go identifiers ps
    go (i:is) pps@(p:_) = i p >>= \case
                        Just buildTool -> return (Just (buildTool, p))
                        Nothing -> go is pps


-- | This list contains some build tool identifier functions for usual setups.
defaultProjectIdentifiers :: MonadIO io => [Directory -> io (Maybe BuildTool)]
defaultProjectIdentifiers =
    [ maybeCabalSandbox, maybeStack, maybeCabal ]

-- | Same as 'determineProjectSettings' 'defaultProjetIdentifiers'.
guessProjectSettings :: MonadIO io
                     => [Directory]
                     -> io (Maybe (BuildTool, Directory))
guessProjectSettings = determineProjectSettings defaultProjectIdentifiers


-- | Check if directory contains a @stack.yaml@ file and return 'Just' 'Stack'
-- in this case.
maybeStack :: MonadIO io => Directory -> io (Maybe BuildTool)
maybeStack d = fmap (const Stack) <$> mkFile (Just d) "stack.yaml"


-- | Check if the directory contains a @cabal.sandbox.config@ file and return
-- 'Just' ('Cabal' 'Sandbox') in that case.
maybeCabalSandbox :: MonadIO io => Directory -> io (Maybe BuildTool)
maybeCabalSandbox d = fmap (const (Cabal Sandbox))
    <$> mkFile (Just d) "cabal.sandbox.config"


-- | Check if the directory contains a cabal file and return 'Just' ('Cabal'
-- 'Plain') if present.
maybeCabal :: MonadIO io => Directory -> io (Maybe BuildTool)
maybeCabal d = do
    ls <-  liftIO . getDirectoryContents $ getDirectory d
    go $ filter (".cabal" `isSuffixOf`)ls
  where
      go [] = return Nothing
      go (f:fs) = mkFile (Just d) f >>= \case
                    Nothing -> go fs
                    Just _ ->
                        -- TODO if cabal version >= 1.24(?), use NewBuild here?
                        return $ Just (Cabal Plain)



