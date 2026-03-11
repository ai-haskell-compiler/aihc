{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

{-
  This module shows how to define a component filling in
    the role of a typeclass as required by another library.

  For example you might use a library requiring `MonadRandom`.
  How can you define a `RandomGenerator` component letting you use your library?
-}
module Test.Data.Registry.MonadRandomSpec where

import Control.Monad.Random.Class as R
import Control.Monad.Trans.Random.Lazy
import Data.IORef
import qualified Data.List as L
import Data.Registry
import Protolude as P
import System.Random as R
import Test.Tasty.Extensions

-- Let's say you have this function coming from a library
-- It has a MonadRandom constraint but you would like to create a
-- component supporting the generation of random number and you
-- would like to be able to use it to call such a function
useMonadRandom :: R.MonadRandom m => m Int
useMonadRandom = R.getRandom

-- For example this Client component might require for its implementation
-- the `useMonadRandom` function
newtype Client = Client {runClient :: IO Int}

-- | What we see here is that the Client component can be implemented
--   with a RandomGenerator component which will provide a way to call
--   the library function having the MonadRandom constraint
newClient :: RandomGenerator -> Client
newClient RandomGenerator {..} = Client {..}
  where
    runClient :: IO Int
    runClient = runRandom useMonadRandom

-- This is the RandomGenerator component
-- it reuses the RandT monad which "implements" MonadRandom given a specific generator
-- it is defined for a given RandomGen type which we don't need to expose
data RandomGenerator = forall g.
  RandomGen g =>
  RandomGenerator
  { runRandom :: forall a. RandT g IO a -> IO a
  }

-- | Production Random generator component using the global StdGen
newRandomGenerator :: IO RandomGenerator
newRandomGenerator = newStdGen >>= makeRandomGenerator

-- | Random generation is "stateful" in the sense that you get a new
--   generator each time you generate a random value.
--   In this implementation we store this generator with a hidden IORef
--   (which should probably be an MVar if we use the RandomGenerator concurrently)
makeRandomGenerator :: (RandomGen g) => g -> IO RandomGenerator
makeRandomGenerator gen = do
  ref <- newIORef gen
  pure $
    RandomGenerator
      ( \a ->
          do
            g <- readIORef ref
            (r, g') <- runRandT a g
            _ <- writeIORef ref g'
            pure r
      )

-- * We can now define other ways to generate random values

-- | Configuration for generators returning pre-determined values
newtype RandomGeneratorConfig = RandomGeneratorConfig
  { seed :: Int
  }
  deriving (Eq, Show)

-- | All the values for this generator are deterministic and determined by
--   the seed in the configuration
newSeededRandomGenerator :: RandomGeneratorConfig -> IO RandomGenerator
newSeededRandomGenerator (RandomGeneratorConfig aSeed) = do
  makeRandomGenerator (mkStdGen aSeed)

-- | There is only one value for this generator determined by
--   the seed in the configuration
newFixedRandomGenerator :: RandomGeneratorConfig -> RandomGenerator
newFixedRandomGenerator (RandomGeneratorConfig aSeed) =
  RandomGenerator ((fst <$>) . flip runRandT (mkStdGen aSeed))

-- | The registry to use for production looks like this
--   It uses the global StdGen
registryProd =
  funTo @IO newClient
    <: fun newRandomGenerator

-- | And now some tests
test_client_function_with_random_values = test "a function using MonadRandom can be executed with the RandomGenerator component and return random values" $ do
  client <- liftIO $ make @(IO Client) registryProd
  results <- liftIO $ replicateM 10 $ client & runClient

  annotateShow results

  -- if we call the generator several times we should get at least 2 different values
  assert (length (L.nub results) > 2)

test_client_function_with_seeded_values = test "a function using MonadRandom can be executed with the RandomGenerator component and return predetermined values" $ do
  let registry' =
        funTo @IO (newSeededRandomGenerator (RandomGeneratorConfig 1))
          <: registryProd

  client <- liftIO $ make @(IO Client) registry'
  results <- liftIO $ replicateM 10 $ client & runClient

  annotateShow results

  -- every time we call the generator we get different values but the same list
  take 3 results === [-2241774542048937483, 8251698951335059867, 8873074891056462818]

test_client_function_with_fixed_values = test "a function using MonadRandom can be executed with the RandomGenerator component can return always the same value" $ do
  let registry' =
        funTo @IO (newFixedRandomGenerator (RandomGeneratorConfig 1))
          <: registryProd

  client <- liftIO $ make @(IO Client) registry'
  results <- liftIO $ replicateM 10 $ client & runClient

  annotateShow results

  -- every time we call the generator we get the same value
  length (L.nub results) === 1
