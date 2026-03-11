{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.FileSystem.Fake
  ( FileSystem
  , FileSystemT (..)
  , FileSystemM
  , MonadFileSystem
  , runFileSystemT
  , evalFileSystemT
  , execFileSystemT
  , runFileSystemM
  , evalFileSystemM
  , execFileSystemM
  , readPath
  , writePath
  , modifyPath
  ) where

import           Control.Applicative        (Alternative)
import           Control.Monad              (MonadPlus)
import           Control.Monad.Catch        (MonadCatch, MonadMask, MonadThrow,
                                             throwM)
import           Control.Monad.Cont.Class   (MonadCont)
import           Control.Monad.Except       (MonadError)
import           Control.Monad.Fix          (MonadFix)
import           Control.Monad.IO.Class     (MonadIO)
import           Control.Monad.RWS.Class    (MonadRWS, MonadReader, MonadState,
                                             MonadWriter)
import           Control.Monad.State.Strict (StateT, gets, modify', runStateT)
import           Control.Monad.Trans        (MonadTrans)
import           Data.Functor.Identity      (Identity, runIdentity)
import qualified Data.Map.Strict            as M
import           System.IO.Error            (doesNotExistErrorType, mkIOError)

#if __GLASGOW_HASKELL__ < 808
import           Control.Monad.Fail         (MonadFail)
#endif


type FileSystem contents = M.Map String contents


newtype FileSystemT contents m a =
  FileSystemT { unFileSystemT :: StateT (FileSystem contents) m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , Alternative
    , MonadFix
    , MonadFail
    , MonadPlus
    , MonadState (FileSystem contents)
    , MonadReader r
    , MonadWriter w
    , MonadRWS r w (FileSystem contents)
    , MonadCont
    , MonadError e
    , MonadCatch
    , MonadMask
    , MonadThrow
    )

type FileSystemM contents = FileSystemT contents Identity


type MonadFileSystem contents = MonadState (FileSystem contents)


runFileSystemT :: FileSystemT contents m a -> FileSystem contents -> m (a, FileSystem contents)
runFileSystemT (FileSystemT act) = runStateT act


evalFileSystemT :: Functor f => FileSystemT contents f a -> FileSystem contents -> f a
evalFileSystemT act = fmap fst . runFileSystemT act


execFileSystemT :: Functor f => FileSystemT contents f a -> FileSystem contents -> f (FileSystem contents)
execFileSystemT act = fmap snd . runFileSystemT act


runFileSystemM :: FileSystemM contents a -> FileSystem contents -> (a, FileSystem contents)
runFileSystemM act = runIdentity . runFileSystemT act


evalFileSystemM :: FileSystemM contents a -> FileSystem contents -> a
evalFileSystemM act = fst . runFileSystemM act


execFileSystemM :: FileSystemM contents a -> FileSystem contents -> FileSystem contents
execFileSystemM act = snd . runFileSystemM act


readPath :: (MonadThrow m, MonadFileSystem contents m) => FilePath -> m contents
readPath path = do
  mContent <- gets $ M.lookup path
  case mContent of
      Just content -> return content
      Nothing ->
        throwM
          . mkIOError doesNotExistErrorType "openFile" Nothing
          $ Just path


writePath :: MonadFileSystem contents m => FilePath -> contents -> m ()
writePath path = modify' . M.insert path


modifyPath :: MonadFileSystem contents m => FilePath -> (Maybe contents -> Maybe contents) -> m ()
modifyPath path f = modify' $ M.alter f path
