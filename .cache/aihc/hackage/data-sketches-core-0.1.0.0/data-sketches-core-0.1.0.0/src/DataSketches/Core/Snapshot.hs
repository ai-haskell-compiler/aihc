module DataSketches.Core.Snapshot where

import Control.Monad.Primitive

class TakeSnapshot a where
  type Snapshot a
  takeSnapshot :: PrimMonad m => a (PrimState m) -> m (Snapshot a)
