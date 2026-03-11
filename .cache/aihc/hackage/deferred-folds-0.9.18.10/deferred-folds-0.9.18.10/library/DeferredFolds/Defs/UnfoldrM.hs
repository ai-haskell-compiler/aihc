module DeferredFolds.Defs.UnfoldrM where

import DeferredFolds.Prelude
import DeferredFolds.Types

unfoldr :: (Monad m) => Unfoldr a -> UnfoldrM m a
unfoldr (Unfoldr unfoldr) = UnfoldrM $ \stepM ->
  let step input act state = stepM input state >>= act
   in unfoldr step return
