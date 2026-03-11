{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}

-- |
--
-- Module      : SDL.Framerate
-- Copyright   : (c) 2015 Siniša Biđin, 2021 Daniel Firth
-- License     : MIT
-- Maintainer  : dan.firth@homotopic.tech
-- Stability   : experimental
--
-- Bindings to @SDL2_gfx@'s framerate management functionality. These functions
-- should allow you to set, manage and query a target application framerate.
module SDL.Framerate
  ( Framerate,
    Manager (..),
    with,
    manager,
    set,
    delay,
    delay_,
    SDL.Framerate.minimum,
    SDL.Framerate.maximum,
    get,
    count,
    destroyManager,
  )
where

import Control.Exception.Lifted (bracket)
import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Foreign.Marshal.Alloc (free, malloc)
import Foreign.Ptr (Ptr)
import qualified SDL.Raw.Framerate

-- | A framerate manager, counting frames and keeping track of time delays
-- necessary to reach a certain target framerate.
newtype Manager = Manager (Ptr SDL.Raw.Framerate.Manager)
  deriving stock (Eq, Show)

-- | A certain number of frames per second.
type Framerate = Int

-- | Creates a new framerate 'Manager', sets a target 'Framerate' and frees the
-- 'Manager' after the inner computation ends.
--
-- This is the recommended way to create a 'Manager'.
with ::
  (MonadBaseControl IO m, MonadIO m) => Framerate -> (Manager -> m a) -> m a
with fps act =
  bracket manager destroyManager $ \m ->
    set m fps >> act m

-- | Create a new framerate 'Manager' using the default settings.
--
-- You have to take care to call 'destroyManager' yourself. It's recommended to
-- use 'with' instead.
manager :: MonadIO m => m Manager
manager =
  fmap Manager . liftIO $ do
    ptr <- malloc
    SDL.Raw.Framerate.init ptr
    return ptr

-- | The smallest allowed framerate.
minimum :: Framerate
minimum = SDL.Raw.Framerate.FPS_LOWER_LIMIT

-- | The largest allowed framerate.
maximum :: Framerate
maximum = SDL.Raw.Framerate.FPS_UPPER_LIMIT

-- | Set a target framerate and reset delay interpolation.
--
-- Note that the given framerate must be within the allowed range -- otherwise
-- the minimum or maximum allowed framerate is used instead.
set :: MonadIO m => Manager -> Framerate -> m ()
set (Manager ptr) = void . SDL.Raw.Framerate.setFramerate ptr . fromIntegral . min SDL.Framerate.maximum . max SDL.Framerate.minimum

-- | Get the currently set framerate.
get :: MonadIO m => Manager -> m Framerate
get (Manager ptr) = fromIntegral <$> SDL.Raw.Framerate.getFramerate ptr

-- | Returns the framecount. Each time 'delay' is called, a frame is counted.
count :: MonadIO m => Manager -> m Int
count (Manager ptr) = fromIntegral <$> SDL.Raw.Framerate.getFramecount ptr

-- | Generate and apply a delay in order to maintain a constant target
-- framerate.
--
-- This should be called once per rendering loop.
--
-- Delay will automatically be set to zero if the computer cannot keep up (if
-- rendering is too slow). Returns the number of milliseconds since the last
-- time 'delay' was called (possibly zero).
delay :: MonadIO m => Manager -> m Int
delay (Manager ptr) = fromIntegral <$> SDL.Raw.Framerate.framerateDelay ptr

-- | Same as 'delay', but doesn't return the time since it was last called.
delay_ :: MonadIO m => Manager -> m ()
delay_ = void . delay

-- | Frees a framerate manager. Make sure not to use it again after this action.
destroyManager :: MonadIO m => Manager -> m ()
destroyManager (Manager ptr) = liftIO $ free ptr
