-------------------------------------------------------------------------------
-- Layer 2 (mockable IO), as per
-- https://www.parsonsmatt.org/2018/03/22/three_layer_haskell_cake.html
-- 2019 Francesco Ariis GPLv3
-------------------------------------------------------------------------------

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}

module Terminal.Game.Layer.Object.Interface where

import Terminal.Game.Plane
import Terminal.Game.Layer.Object.Primitive

import qualified Control.Concurrent as CC

-------------------------------------------------------------------------------
-- mtl interface for game

type MonadGameIO m = (MonadInput m, MonadTimer m,
                      MonadException m, MonadDisplay m)

data InputHandle = InputHandle
            { ihKeyMVar     :: CC.MVar [Event],
              ihOpenThreads :: [CC.ThreadId] }

class Monad m => MonadInput m where
    startEvents :: TPS -> m InputHandle
    pollEvents  :: CC.MVar [Event] -> m [Event]
    stopEvents :: [CC.ThreadId] -> m ()
    areEventsOver :: m Bool
      -- Why do we need this? For test/narrate purposes. When
      -- we play a game events are never over, but when we
      -- test/narrate, it might be than the stream of [Event]
      -- is exhausted before the state function returns Right.
      -- We do not want to be stuck in an endless loop in that
      -- case.

class Monad m => MonadTimer m where
    getTime :: m Integer     -- to nanoseconds
    sleepABit :: TPS -> m () -- Given TPS, sleep a fracion of a single
                             -- Tick.

-- if a fails, do b (useful for cleaning up)
class Monad m => MonadException m where
    cleanUpErr :: m a -> m b -> m a
    throwExc :: ATGException -> m a

class Monad m => MonadDisplay m where
    setupDisplay :: m ()
    clearDisplay :: m ()
    displaySize :: m (Maybe Dimensions)
    blitPlane :: Maybe Plane -> Plane -> m ()
    shutdownDisplay :: m ()

displaySizeErr :: (MonadDisplay m, MonadException m) => m Dimensions
displaySizeErr = displaySize >>= \case
                   Nothing -> throwExc CannotGetDisplaySize
                   Just d -> return d

