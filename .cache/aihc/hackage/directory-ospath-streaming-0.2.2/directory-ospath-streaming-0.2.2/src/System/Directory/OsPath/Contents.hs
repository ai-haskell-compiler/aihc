-- |
-- Module:     System.Directory.OsPath.Contents
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Directory.OsPath.Contents
  ( getDirectoryContentsRecursive
  , getDirectoryContentsWithFilterRecursive

  , listContentsRecFold
  ) where

import Control.Exception (onException)
import Data.Coerce (coerce, Coercible)
import System.IO.Unsafe (unsafeInterleaveIO)
import System.OsPath

import System.Directory.OsPath.Streaming.Internal (DirStream)
import qualified System.Directory.OsPath.Streaming.Internal as Streaming
import qualified System.Directory.OsPath.Streaming.Internal.Raw as Raw
import System.Directory.OsPath.Types

-- | Recursively list all the files and directories in a directory and all subdirectories.
--
-- The directory structure is traversed depth-first.
--
-- The result is generated lazily so is not well defined if the source
-- directory structure changes before the list is fully consumed.
--
-- Symlinks within directory structure may cause result to be infinitely long.
getDirectoryContentsRecursive
  :: OsPath
  -> IO [(OsPath, FileType)]
getDirectoryContentsRecursive root =
  listContentsRecFold'
    Nothing
    (\_ _ (Relative path) _ ft _ cons prependSubdir rest -> cons (path, ft) $ prependSubdir rest)
    (\_ _ (Relative path) _ ft -> pure (Just (path, ft)))
    (Just root)

-- | Recursively list all the files and directories that satisfy given
-- predicate in a directory and all subdirectories. Descending into
-- some subdirectories may be avoided by filtering them out with a
-- visiting predicate.
--
-- Not visited directory entry may still be reported depending on the
-- collection predicate.
--
-- The directory structure is traversed depth-first.
--
-- The result is generated lazily so is not well defined if the source
-- directory structure changes before the list is fully consumed.
--
-- Symlinks within directory structure may cause result to be infinitely long, but
-- they can be filtered out with a suitable directory visiting predicate.
getDirectoryContentsWithFilterRecursive
  :: (Basename OsPath -> SymlinkType -> Bool) -- ^ Whether to visit a directory
  -> (Basename OsPath ->                Bool) -- ^ Whether to collect given directory element, either file or directory.
  -> OsPath
  -> IO [(OsPath, FileType)]
getDirectoryContentsWithFilterRecursive visitPred collectPred root =
  listContentsRecFold'
    Nothing
    (\_ _ (Relative path) basename ft symlink cons prependSubdir rest ->
       (if collectPred basename then cons (path, ft) else id) $
         if visitPred basename symlink
         then prependSubdir rest
         else rest)
    (\_ _ (Relative path) basename ft ->
      pure $
        if collectPred basename
        then Just (path, ft)
        else Nothing)
    (Just root)

{-# INLINE listContentsRecFold #-}
-- | The most general form of gathering directory contents.
--
-- Treats symlinks the same as regular files and directories. Folding functions can
-- decide how to handle symlinks.
--
-- Both directory and file actions can throw exceptions and this function
-- will try to close finished directory streams promptly (they’ll be closed
-- by GC in the worst case).
listContentsRecFold
  :: forall f a b. (Foldable f, Coercible b OsPath)
  => Maybe Int
  -- ^ Depth limit if specified, negative values treated the same as positive ones.
  -> (forall c. OsPath -> b -> Relative OsPath -> Basename OsPath -> SymlinkType -> (a -> IO c -> IO c) -> (IO c -> IO c) -> IO c -> IO c)
  -- ^ Decide how to fold directory and its children given its path.
  --
  -- Can do IO actions to plan what to do and typically should derive its
  -- result from last @IO c@ argument.
  --
  -- Returns @IO c@ where @c@ is hidden from the user so the only way
  -- to make it is to construct from the passed @IO c@ action.
  --
  -- Arguments:
  --
  -- * @OsPath@              - absolute path to the visited directory
  -- * @b@                   - root of the visited directory as passed originally in @f b@ to the bigger fold function
  -- * @Relative OsPath@     - path to the visited directory relative to the previous @b@ argument
  -- * @Basename OsPath@     - name of the visited directory without slashes
  -- * @SymlinkType@         - symlink status of the visited directory
  -- * @(a -> IO c -> IO c)@ - can be used to record some output (@a@) about the directory itself
  -- * @(IO c -> IO c)@      - traverse inside this directory, can be ignored to skip its children
  -- * @IO c@                - continue scanning not yet visited parts, must be used to construct return value (otherwise it won’t typecheck!)
  --
  -- The passed @(IO c -> IO c)@ argument function should (but is not required to)
  -- be applied in the returned function and it will prepend results for subdirectories
  -- of the directory being analyzed. If not applied these subdirectories will be skipped,
  -- this way ignoring particular directory and all its children can be achieved.
  -> (OsPath -> b -> Relative OsPath -> Basename OsPath -> FileType -> IO (Maybe a))
  -- ^ What to do with file
  -> f b
  -- ^ Roots to search in, either absolute or relative
  -> IO [a]
listContentsRecFold = \depthLimit foldDir filePred input ->
  listContentsRecFold' depthLimit (\a b c d _f g h i j -> foldDir a b c d g h i j) filePred input

{-# INLINE listContentsRecFold' #-}
-- Actual worker with slightly worse type signature that we don’t want to expose to the users.
-- But it’s better candidate for implementing getDirectoryContentsRecursive here than
-- listContentsRecFold.
listContentsRecFold'
  :: forall f a b. (Foldable f, Coercible b OsPath)
  => Maybe Int
  -> (forall c. OsPath -> b -> Relative OsPath -> Basename OsPath -> FileType -> SymlinkType -> (a -> IO c -> IO c) -> (IO c -> IO c) -> IO c -> IO c)
  -> (OsPath -> b -> Relative OsPath -> Basename OsPath -> FileType -> IO (Maybe a))
  -> f b
  -> IO [a]
listContentsRecFold' depthLimit foldDir filePred input =
  goCache =<< Raw.allocateDirReadCache
  where
    goCache cache =
      foldr (goNewDir initLimit) (Raw.releaseDirReadCache cache *> pure []) input
      where
        !initLimit = case depthLimit of
          Nothing -> -1 -- Loop until overflow, basically infinitely
          Just x  -> abs x

        goNewDir :: Int -> b -> IO [a] -> IO [a]
        goNewDir !d root rest = do
          stream <- Streaming.openDirStream $ coerce root
          goDirStream root d (Streaming.closeDirStream stream *> rest) stream

        goDirStream :: b -> Int -> IO [a] -> DirStream -> IO [a]
        goDirStream _    0     rest _      = rest
        goDirStream root depth rest stream = go
          where
            go :: IO [a]
            go = (`onException` Streaming.closeDirStream stream) $ do
              x <- Streaming.readDirStreamWithCache cache stream
              case x of
                Nothing                -> rest
                Just (yAbs, yBase, ft) -> do
                  let yRel :: Relative OsPath
                      yRel = coerce yBase
                  case ft of
                    Other _       -> addLazy (filePred yAbs root yRel yBase ft) go
                    File _        -> addLazy (filePred yAbs root yRel yBase ft) go
                    Directory ft' -> foldDir yAbs root yRel yBase ft ft' cons (goNewDirAcc yRel (depth - 1) yAbs) go

            goNewDirAcc :: Relative OsPath -> Int -> OsPath -> IO [a] -> IO [a]
            goNewDirAcc rootAcc !d dir rest1 = do
              stream1 <- Streaming.openDirStream dir
              goDirStreamAcc rootAcc d (Streaming.closeDirStream stream1 *> rest1) stream1

            goDirStreamAcc :: Relative OsPath -> Int -> IO [a] -> DirStream -> IO [a]
            goDirStreamAcc _       0      rest1 _       = rest1
            goDirStreamAcc rootAcc depth1 rest1 stream1 = go1
              where
                go1 :: IO [a]
                go1 = (`onException` Streaming.closeDirStream stream1) $ do
                  x <- Streaming.readDirStreamWithCache cache stream1
                  case x of
                    Nothing                -> rest1
                    Just (yAbs, yBase, ft) -> do
                      let yRel :: Relative OsPath
                          yRel = coerce (</>) rootAcc yBase
                      case ft of
                        Other _       -> addLazy (filePred yAbs root yRel yBase ft) go1
                        File _        -> addLazy (filePred yAbs root yRel yBase ft) go1
                        Directory ft' -> foldDir yAbs root yRel yBase ft ft' cons (goNewDirAcc yRel (depth1 - 1) yAbs) go1

        addLazy :: IO (Maybe a) -> IO [a] -> IO [a]
        addLazy x y = do
          x' <- x
          case x' of
            Nothing  -> y
            Just x'' -> cons x'' y

        cons :: a -> IO [a] -> IO [a]
        cons x y =
          (x :) <$> unsafeInterleaveIO y
