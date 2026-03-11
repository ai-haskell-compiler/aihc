{-# LANGUAGE AllowAmbiguousTypes #-}

{- Cache for Rio values, backed by a MVar -}
module Data.Registry.Internal.Cache where

import Data.Dynamic
import Data.Map as M hiding (singleton)
import Data.Map qualified as M
import Data.Registry.Internal.Reflection (showSingleType)
import Protolude
import Type.Reflection (someTypeRep)

-- * EXPORTED FUNCTIONS

-- | Cache an effectful value with a given text key
--   so that the value is not recreated for the same key
cacheAt :: forall a m. (Typeable a, MonadIO m, MonadReader Cache m) => Text -> m a -> m a
cacheAt = cacheAtKey . Custom

-- | Cache an effectful value by using its type as the cache key
singleton :: forall a m. (Typeable a, MonadIO m, MonadReader Cache m) => m a -> m a
singleton = cacheAtKey Singleton

-- * IMPLEMENTATION

-- | A cache for created values, with a map from
--   the textual representation of a type to various cached values
newtype Cache = Cache (MVar (Map Text Cached))
  deriving (Eq)

-- | Cache for a value of a single type
--   There is at most one singleton and possibly some custom values, indexed by a specific key
data Cached = Cached
  { singletonCached :: Maybe Dynamic,
    customCached :: Map Text Dynamic
  }
  deriving (Show)

-- | An empty cached value (with no cached instances yet)
emptyCached :: Cached
emptyCached = Cached Nothing mempty

-- | Create an empty cache
newCache :: MonadIO m => m Cache
newCache = liftIO $ Cache <$> newMVar mempty

-- | Get the current cache
askCache :: MonadReader Cache m => m Cache
askCache = ask

-- | Type of keys used to cache values
--   A value can either be cached with a specific key, or it is a singleton
data Key
  = Custom Text
  | Singleton
  deriving (Eq, Show, Ord)

-- | Make sure that an effectful value is cached after the first evaluation for a specific key
cacheAtKey :: forall a m. (Typeable a, MonadIO m, MonadReader Cache m) => Key -> m a -> m a
cacheAtKey key action = do
  m <- getCached @a key
  case m of
    Just a ->
      pure a
    Nothing -> do
      a <- action
      setCached key a
      pure a

-- | Get a cached value from the cache
--   This is a IO operation since we access the cache MVar
getCached :: (Typeable a, MonadIO m, MonadReader Cache m) => Key -> m (Maybe a)
getCached key = askCache >>= getCachedValue key

-- | Cache a value at a given key in the cache
--   This is a IO operation since we access the cache MVar
setCached :: forall a m. (Typeable a, MonadIO m, MonadReader Cache m) => Key -> a -> m ()
setCached key a =
  askCache >>= cacheValue
  where
    -- \| Cache a value as a Dynamic value for a given key
    cacheValue :: Cache -> m ()
    cacheValue (Cache ms) = liftIO $
      modifyMVar_ ms $
        \m -> pure (M.alter (cacheDynValue key (toDyn a)) (makeTypeText @a) m)

-- | Retrieve a cached value given its key
getCachedValue :: forall a m. (Typeable a, MonadIO m) => Key -> Cache -> m (Maybe a)
getCachedValue key (Cache ms) = liftIO $ do
  m <- readMVar ms
  let c = lookup (makeTypeText @a) m
  pure $ c >>= getDynValue key >>= fromDynamic @a

-- | Insert a (dynamic) value in the Cached data structure for a specific type of value
cacheDynValue :: Key -> Dynamic -> Maybe Cached -> Maybe Cached
cacheDynValue Singleton dynamic Nothing = Just $ emptyCached {singletonCached = Just dynamic}
cacheDynValue Singleton dynamic (Just cached) = Just $ cached {singletonCached = singletonCached cached <|> Just dynamic}
cacheDynValue (Custom key) dynamic Nothing = Just $ emptyCached {customCached = M.singleton key dynamic}
cacheDynValue (Custom key) dynamic (Just cached) = Just $ cached {customCached = M.insert key dynamic $ customCached cached}

-- | Return the dynamic value cached at a given key
getDynValue :: Key -> Cached -> Maybe Dynamic
getDynValue Singleton (Cached s _) = s
getDynValue (Custom k) (Cached _ m) = M.lookup k m

-- | Return a textual description of a Haskell type
makeTypeText :: forall a. (Typeable a) => Text
makeTypeText = showSingleType $ someTypeRep (Proxy :: Proxy a)
