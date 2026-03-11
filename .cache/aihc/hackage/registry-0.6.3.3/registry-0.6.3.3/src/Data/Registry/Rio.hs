{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}

-- | Utilities for working with resources
module Data.Registry.Rio
  ( module Data.Registry.Rio,
    singleton,
    cacheAt
  )
where

import Control.Monad.Morph
import Control.Monad.Trans.Resource
import Data.Dynamic
import Data.Registry.Internal.Cache
import Data.Registry.Make (make)
import Data.Registry.Registry
import Protolude

-- | This data type provides some support for creating effectful components with resources
--   You can use the regular MonadResource functions like allocate to make sure that resources are cleaned up
--   You can also use the 'cacheAt' function
newtype Rio a = Rio {rioRun :: ReaderT Cache (ResourceT IO) a}
  deriving (Functor, Applicative, Monad, MonadReader Cache, MonadIO, MonadResource, MonadUnliftIO)

-- | Run a Rio action by providing an empty cache and allocating / destroying resources
runRio :: MonadIO m => Rio a -> m a
runRio = liftIO . runResourceT . runCache

-- | Run a Rio action by providing an empty cache and allocating / destroying resources
execRio :: MonadIO m => Rio a -> m (a, Cache)
execRio = liftIO . runResourceT . execCache

-- | Use the value created by a Rio action so that resources are properly allocated and cached
withRio :: MonadIO m => Rio a -> (a -> IO b) -> m b
withRio action f = liftIO . runResourceT $ do
  a <- runCache action
  liftIO $ f a

-- | Use the value created by a Rio action so that resources are properly allocated and cached
--   inside a monad transformer
withRioM :: (MonadResource (m (ResourceT IO)), MFunctor m) => Rio a -> (a -> m IO b) -> m IO b
withRioM action f = hoist runResourceT $ do
  a <- liftResourceT (runCache action)
  hoist lift (f a)

-- | Run a Rio action by providing an empty cache
runCache :: Rio a -> ResourceT IO a
runCache (Rio action) = do
  cache <- liftIO newCache
  runReaderT action cache

-- | Run a Rio action by providing an empty cache, and return the final cache
--   for inspection
execCache :: Rio a -> ResourceT IO (a, Cache)
execCache (Rio action) = do
  cache <- liftIO newCache
  (,cache) <$> runReaderT action cache

-- | Lift a resourceful value into Rio
liftRio :: ResourceT IO a -> Rio a
liftRio = Rio . lift

-- | This function must be used to run services involving resources
--   The value a is created using the registry, used with the function 'f'
--   and all resources are freed at the end
withRegistry :: forall a b ins out m. (Typeable a, MonadIO m, MakeSingletons out) => Registry ins out -> (a -> IO b) -> m b
withRegistry registry f =
  liftIO $ runResourceT (runRegistryT @a registry >>= liftIO . f)

-- | This function works like 'withRegistry' for a higher-order monad, typically `PropertyT IO` when
--   writing property tests with Hedgehog
withRegistryM ::
  forall a b ins out m.
  (Typeable a, MonadResource (m (ResourceT IO)), MFunctor m, MakeSingletons out) =>
  Registry ins out ->
  (a -> m IO b) ->
  m IO b
withRegistryM = withRioM . make @(Rio a) . singletons

-- | Create a function of type a with a given registry
--   Return a ResourceT value to control resource allocation
runRegistryT :: forall a ins out. (Typeable a, MakeSingletons out) => Registry ins out -> ResourceT IO a
runRegistryT = runCache . make @(Rio a) . singletons

-- | Make singletons for all the output types of a registry
--   but only if they not specialized values
singletons :: forall ins out. (MakeSingletons out) => Registry ins out -> Registry ins out
singletons r = _singletonsRegistry $ makeSingletons (startSingletonsRegistry r)

-- | Registry where all Rio values are singletons
newtype SingletonsRegistry (todo :: [Type]) (ins :: [Type]) (out :: [Type]) = SingletonsRegistry {_singletonsRegistry :: Registry ins out}

-- | Prepare a Registry for making singletons
startSingletonsRegistry :: Registry ins out -> SingletonsRegistry out ins out
startSingletonsRegistry = SingletonsRegistry

-- | Prepare a Registry for making singletons on a specific list of types
makeSingletonsRegistry :: forall todo ins out. Registry ins out -> SingletonsRegistry todo ins out
makeSingletonsRegistry = SingletonsRegistry @todo

-- | This typeclass take an existing registry and makes a singleton for each Rio output type
class MakeSingletons ls where
  makeSingletons :: SingletonsRegistry ls ins out -> SingletonsRegistry '[] ins out

-- | If the list of types is empty there is nothing to do
instance MakeSingletons '[] where
  makeSingletons = identity

-- | If the type represents an effectful value, make a singleton for it and recurse on the rest
instance {-# OVERLAPPING #-} (Typeable a, MakeSingletons rest) => MakeSingletons (Rio a : rest) where
  makeSingletons (SingletonsRegistry r) =
    makeSingletons $ SingletonsRegistry @rest (tweakUnspecialized @(Rio a) singleton r)

-- | If the type represents a pure value, make singletons for the rest
instance (MakeSingletons rest) => MakeSingletons (a : rest) where
  makeSingletons (SingletonsRegistry r) = makeSingletons (makeSingletonsRegistry @rest r)
