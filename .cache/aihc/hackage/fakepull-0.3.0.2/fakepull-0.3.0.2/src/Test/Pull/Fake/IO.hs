{-# LANGUAGE LambdaCase #-}

module Test.Pull.Fake.IO
  ( FakeStream
  , newFakeStream
  , getFakeStreamContents
  , pull
  ) where


import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Data.IORef             (IORef, atomicModifyIORef', newIORef,
                                         readIORef)


type FakeStream payload = IORef [payload]


newFakeStream :: MonadIO m => [payload] -> m (FakeStream payload)
newFakeStream = liftIO . newIORef


getFakeStreamContents :: MonadIO m => FakeStream payload -> m [payload]
getFakeStreamContents = liftIO . readIORef


pull :: MonadIO m => FakeStream payload -> m (Maybe payload)
pull stream = do
  liftIO . atomicModifyIORef' stream $ \case
    [] -> ([], Nothing)
    hd : tl -> (tl, Just hd)
