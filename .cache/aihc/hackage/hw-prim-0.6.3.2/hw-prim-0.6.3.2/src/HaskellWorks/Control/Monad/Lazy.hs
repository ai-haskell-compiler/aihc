{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Control.Monad.Lazy
  ( interleaveSequenceIO
  , interleaveSequenceM
  , interleaveUnfoldrM
  , interleaveTraverseM
  , interleaveForM
  , forceM
  ) where

import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Unlift

import qualified System.IO.Unsafe as IO

interleaveSequenceIO :: [IO a] -> IO [a]
interleaveSequenceIO []       = return []
interleaveSequenceIO (fa:fas) = do
  a <- fa
  as <- IO.unsafeInterleaveIO (interleaveSequenceIO fas)
  return (a:as)

interleaveSequenceM :: MonadUnliftIO m => [m a] -> m [a]
interleaveSequenceM as = do
  f <- askUnliftIO
  liftIO $ interleaveSequenceIO (fmap (unliftIO f) as)

-- | Generates a lazy list of values that are produced by a given monadic function.
--
-- This function is intended to be like the "standard" 'unfoldrM' except
-- that the list is generated lazily.
interleaveUnfoldrM :: MonadUnliftIO m => (b -> m (Maybe (a, b))) -> b -> m [a]
interleaveUnfoldrM f z = do
  u <- askUnliftIO
  liftIO $ IO.unsafeInterleaveIO (go u z)
  where
    go !u !b = do
      m <- unliftIO u (f b)
      case m of
        Nothing      -> pure []
        Just (!a, b') -> do
          rest <- IO.unsafeInterleaveIO (go u b')
          pure (a:rest)

-- | Traverses the function over the list and produces a lazy list in a
-- monadic context.
--
-- It is intended to be like the "standard" 'traverse' except
-- that the list is generated lazily.
interleaveTraverseM :: MonadUnliftIO m => (a -> m b) -> [a] -> m [b]
interleaveTraverseM f as = do
  u <- askUnliftIO
  liftIO $ IO.unsafeInterleaveIO (go u as)
  where
    go _ [] = pure []
    go !u (v:vs) = do
      !res <- unliftIO u (f v)
      rest <- IO.unsafeInterleaveIO (go u vs)
      pure (res:rest)

interleaveForM :: MonadUnliftIO m => [a] -> (a -> m b) -> m [b]
interleaveForM = flip interleaveTraverseM

forceM :: (Monad m, NFData a) => m a -> m a
forceM = (force <$!>)
