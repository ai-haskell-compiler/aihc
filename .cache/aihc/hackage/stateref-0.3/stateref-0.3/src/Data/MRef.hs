module Data.MRef 
    ( module Data.MRef
    , module Data.MRef.Types
    , module Data.MRef.Instances
    ) where

import Data.MRef.Types
import Data.MRef.Instances

-- |See 'takeMRef'.
takeMRef :: MRef m a -> m a
takeMRef = takeMRef

-- |See 'putMRef'.
putMRef :: MRef m a -> a -> m ()
putMRef = putMRef
