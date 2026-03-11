{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_HADDOCK prune not-home #-}

{- |
Copyright   : (c) 2018-2022 Tim Emiola
SPDX-License-Identifier: BSD3
Maintainer  : Tim Emiola <tim@emio.la>

Provides a 'Handle' that stores data in Redis.
-}
module KeyedVals.Handle.Redis.Internal (
  -- * Handle creation
  fromConnectInfo,

  -- * Configuration
  readEnvConnectInfo,

  -- * module re-exports
  module KeyedVals.Handle,
) where

import Control.Exception (throwIO)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO, liftIO)
import qualified Data.ByteString as B
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Text as Text
import Data.Text.Encoding (decodeUtf8)
import Database.Redis (
  ConnectInfo (..),
  Connection,
  Redis,
  Reply (..),
  Status (..),
  checkedConnect,
  del,
  disconnect,
  get,
  hdel,
  hget,
  hgetall,
  hkeys,
  hlen,
  hmget,
  hmset,
  hset,
  keys,
  parseConnectInfo,
  runRedis,
  set,
 )
import KeyedVals.Handle
import KeyedVals.Handle.Internal (Handle (..))
import Numeric.Natural (Natural)
import System.ReadEnvVar (lookupEnv, readEnvDef)


-- | Determine 'ConnectInfo' from the environment.
readEnvConnectInfo :: IO (Maybe ConnectInfo)
readEnvConnectInfo = do
  maxConns <- readEnvDef "REDIS_MAX_CONNECTIONS" fallbackMaxConns
  lookupEnv "REDIS_URL" >>= maybe (pure Nothing) (parseLocator maxConns)


-- | Obtain a @ConnectInfo@ from a Redis Url and max connections
parseLocator :: Int -> String -> IO (Maybe ConnectInfo)
parseLocator maxConns l = do
  let parseConnectInfo' = either invalidLocator pure . parseConnectInfo
      setMaxConns x cfg = cfg {connectMaxConnections = x}
  Just . setMaxConns maxConns <$> parseConnectInfo' l


invalidLocator :: String -> IO a
invalidLocator x = throwIO $ userError $ "REDIS connection url: " ++ x ++ " is invalid"


-- | Create a 'Handle'.
fromConnectInfo :: MonadUnliftIO m => ConnectInfo -> m (Handle m)
fromConnectInfo connectInfo = do
  conn <- liftIO $ checkedConnect connectInfo
  pure $
    Handle
      { hClose = hClose' conn
      , hLoadVal = hLoadVal' conn
      , hSaveVal = hSaveVal' conn
      , hLoadKVs = hLoadKVs' conn
      , hSaveKVs = hSaveKVs' conn
      , hUpdateKVs = hUpdateKVs' conn
      , hLoadFrom = hLoadFrom' conn
      , hLoadSlice = hLoadSlice' conn
      , hSaveTo = hSaveTo' conn
      , hDeleteSelected = hDeleteSelected' conn
      , hDeleteSelectedKVs = hDeleteSelectedKVs' conn
      , hCountKVs = hCountKVs' conn
      }


hClose' :: MonadUnliftIO m => Connection -> m ()
hClose' = liftIO . disconnect


hLoadVal' ::
  MonadUnliftIO m =>
  Connection ->
  Key ->
  m (Either HandleErr (Maybe Val))
hLoadVal' conn key = doFetch conn $ get key


hSaveVal' ::
  MonadUnliftIO m =>
  Connection ->
  Key ->
  Val ->
  m (Either HandleErr ())
hSaveVal' conn key value = doStore conn $ set key value


hLoadFrom' ::
  MonadUnliftIO m =>
  Connection ->
  Key ->
  Key ->
  m (Either HandleErr (Maybe Val))
hLoadFrom' conn key dictKey = doFetch conn $ hget key dictKey


hLoadKVs' ::
  MonadUnliftIO m =>
  Connection ->
  Key ->
  m (Either HandleErr ValsByKey)
hLoadKVs' conn key = doFetch conn $ hgetall key <&> fmap Map.fromList


hLoadSlice' ::
  MonadUnliftIO m =>
  Connection ->
  Key ->
  Selection ->
  m (Either HandleErr ValsByKey)
hLoadSlice' conn key m@(Match _) = selectKeysThen hLoadSlice' conn key m
hLoadSlice' conn key (AllOf dictKeys') = do
  let dictKeys = NonEmpty.toList dictKeys'
  doFetch conn (hmget key dictKeys) >>= \case
    Left err -> pure $ Left err
    Right fetched -> do
      let pairedMaybes = zip dictKeys fetched
          mbOf (x, mbY) = mbY >>= \y -> Just (x, y)
      pure $ Right $ Map.fromList $ mapMaybe mbOf pairedMaybes


hCountKVs' ::
  MonadUnliftIO m =>
  Connection ->
  Key ->
  m (Either HandleErr Natural)
hCountKVs' conn key = doFetch conn $ hlen key <&> fmap fromInteger


hSaveTo' ::
  MonadUnliftIO m =>
  Connection ->
  Key ->
  Key ->
  Val ->
  m (Either HandleErr ())
hSaveTo' conn key dictKey value = doStore' conn $ hset key dictKey value


hSaveKVs' ::
  MonadUnliftIO m =>
  Connection ->
  Key ->
  ValsByKey ->
  m (Either HandleErr ())
hSaveKVs' conn key dict = do
  _ <- hDeleteSelected' conn $ AllOf (key :| [])
  hUpdateKVs' conn key dict


hUpdateKVs' ::
  MonadUnliftIO m =>
  Connection ->
  Key ->
  ValsByKey ->
  m (Either HandleErr ())
hUpdateKVs' conn key dict = doStore' conn $ hmset key $ Map.toList dict


hDeleteSelected' ::
  MonadUnliftIO m =>
  Connection ->
  Selection ->
  m (Either HandleErr ())
hDeleteSelected' conn (AllOf ks) = doStore' conn $ del $ NonEmpty.toList ks
hDeleteSelected' conn (Match g) = do
  doFetch conn (keys $ globPattern g) >>= \case
    Left e -> pure $ Left e
    Right [] -> pure $ Right ()
    Right (k : ks) -> hDeleteSelected' conn $ AllOf (k :| ks)


hDeleteSelectedKVs' ::
  MonadUnliftIO m =>
  Connection ->
  Key ->
  Selection ->
  m (Either HandleErr ())
hDeleteSelectedKVs' conn key (AllOf dictKeys) = doStore' conn $ hdel key $ NonEmpty.toList dictKeys
hDeleteSelectedKVs' conn key m@(Match _) = selectKeysThen hDeleteSelectedKVs' conn key m


selectKeysThen ::
  (Monoid b, MonadUnliftIO m) =>
  (Connection -> B.ByteString -> Selection -> m (Either HandleErr b)) ->
  Connection ->
  B.ByteString ->
  Selection ->
  m (Either HandleErr b)
selectKeysThen f conn key selection = do
  (doFetch conn $ hkeys key) >>= \case
    Left e -> pure $ Left e
    Right [] -> pure $ Right mempty
    Right xs -> do
      case (filter (\k -> k `isIn` selection) xs) of
        [] -> pure $ Right mempty
        (k : ks) -> f conn key $ AllOf (k :| ks)


fallbackMaxConns :: Int
fallbackMaxConns = 10


toHandleErr :: Reply -> HandleErr
toHandleErr (Error e) | B.isPrefixOf "WRONGTYPE" e = BadKey
toHandleErr (Error e) = Unanticipated $ decodeUtf8 e
toHandleErr r = Unanticipated $ Text.pack $ show r


doStore ::
  MonadIO m =>
  Connection ->
  Redis (Either Reply Status) ->
  m (Either HandleErr ())
doStore conn action = liftIO $ leftErr $ runRedis conn action


leftErr :: Monad m => m (Either Reply Status) -> m (Either HandleErr ())
leftErr x =
  x >>= \case
    (Left l) -> pure $ Left $ toHandleErr l
    Right Ok -> pure $ Right ()
    Right Pong -> pure $ Right ()
    Right (Status err) -> pure $ Left $ Unanticipated $ Text.pack $ show err


doStore' ::
  MonadIO m =>
  Connection ->
  Redis (Either Reply a) ->
  m (Either HandleErr ())
doStore' conn action = liftIO $ leftErr'' $ runRedis conn action


leftErr'' :: Monad m => m (Either Reply a) -> m (Either HandleErr ())
leftErr'' x =
  x >>= \case
    (Left l) -> pure $ Left $ toHandleErr l
    Right _ -> pure $ Right ()


doFetch ::
  MonadUnliftIO m =>
  Connection ->
  Redis (Either Reply a) ->
  m (Either HandleErr a)
doFetch conn = liftIO . leftErr' . runRedis conn


leftErr' :: Monad m => m (Either Reply a) -> m (Either HandleErr a)
leftErr' = (<&> either (Left . toHandleErr) Right)
