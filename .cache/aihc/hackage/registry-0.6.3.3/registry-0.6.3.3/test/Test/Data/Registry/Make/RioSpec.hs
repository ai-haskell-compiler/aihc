{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Make.RioSpec where

import Control.Monad.Trans.Resource
import Data.IORef
import Data.Map as M hiding (singleton)
import Data.Registry as Rio
import Protolude hiding (C1, D1)
import Test.Tasty.Extensions

test_fmap = test "a cached value can still be mapped over" $ do
  -- count the number of creation for the counter
  creations <- liftIO (newIORef (0 :: Int))

  -- create a singleton counter starting at 1
  let counter = Rio.singleton $ liftIO $ do
        c <- newIORef (1 :: Int)
        modifyIORef creations (+ 1)
        pure c

  -- make the counter once, then make the counter start at 2
  c1 <- liftIO (newIORef (2 :: Int))
  ref <- runRio (counter >> (counter $> c1))

  -- the counter starts at 2
  incremented <- liftIO . readIORef $ ref
  incremented === 2

  -- the counter has been created only once
  created <- liftIO . readIORef $ creations
  created === 1

test_singletons_with_specialization = test "some effectful components can be cached to become singletons in some contexts" $ do
  let registry =
        singletons $
          specialize @(Rio Storage3) (valTo @Rio (DatabaseConfig "localhost" 9876)) $
            funTo @Rio newApp
              <: funTo @Rio newBusinessLogic
              <: funTo @Rio Storage3
              <: funTo @Rio Storage2
              <: funTo @Rio Storage1
              <: funTo @Rio newConnectionPool
              <: fun newCounter
              <: valTo @Rio (DatabaseConfig "localhost" 5432)

  withRegistryM @App registry $ \App {..} -> do
    let db1 = getDatabaseConfig $ getStorage1ConnectionPool storage1
    let db2 = getDatabaseConfig $ getStorage2ConnectionPool storage2
    let db3 = getDatabaseConfig $ getStorage3ConnectionPool storage3

    db1 === db2
    db2 /== db3
    db3 === DatabaseConfig "localhost" 9876

    n <- readCounter counter
    n
      === [ ("create the App", 1),
            ("create the BusinessLogic", 1),
            ("http://localhost:5432", 1),
            ("http://localhost:9876", 1)
          ]

-- * HELPERS

data App = App
  { storage1 :: Storage1,
    storage2 :: Storage2,
    storage3 :: Storage3,
    businessLogic :: BusinessLogic,
    counter :: Counter
  }

newApp :: Counter -> Storage1 -> Storage2 -> Storage3 -> BusinessLogic -> Rio App
newApp counter s1 s2 s3 bl = do
  incrementCounter counter "create the App"
  pure (App s1 s2 s3 bl counter)

newtype ConnectionPool = ConnectionPool {getDatabaseConfig :: DatabaseConfig}

newConnectionPool :: Counter -> DatabaseConfig -> Rio ConnectionPool
newConnectionPool counter config = do
  let key = databaseConfigUrl config

  let create = do
        -- count the number of times that a pool is created with some specific parameters
        incrementCounter counter (databaseConfigUrl config)
        pure (ConnectionPool config)
  let destroy = const $ pure ()

  cacheAt key (snd <$> allocate create destroy)

data DatabaseConfig = DatabaseConfig {host :: Text, port :: Int} deriving (Eq, Show)

databaseConfigUrl :: DatabaseConfig -> Text
databaseConfigUrl (DatabaseConfig h p) = "http://" <> h <> ":" <> show p

newtype Storage1 = Storage1 {getStorage1ConnectionPool :: ConnectionPool}

newtype Storage2 = Storage2 {getStorage2ConnectionPool :: ConnectionPool}

newtype Storage3 = Storage3 {getStorage3ConnectionPool :: ConnectionPool}

newtype Counter = Counter (IORef (Map Text Int))

newtype BusinessLogic = BusinessLogic Storage1

newBusinessLogic :: Counter -> Storage1 -> Storage3 -> Rio BusinessLogic
newBusinessLogic counter storage1 _storage3 = do
  incrementCounter counter "create the BusinessLogic"
  pure $ BusinessLogic storage1

newCounter :: Rio Counter
newCounter = Counter <$> liftIO (newIORef mempty)

incrementCounter :: MonadIO m => Counter -> Text -> m ()
incrementCounter (Counter ref) key = liftIO $ do
  m <- readIORef ref
  let current = fromMaybe 0 (M.lookup key m)
  modifyIORef ref $ pure (M.insert key (current + 1) m)

readCounter :: MonadIO m => Counter -> m (Map Text Int)
readCounter (Counter ref) = liftIO $ readIORef ref
