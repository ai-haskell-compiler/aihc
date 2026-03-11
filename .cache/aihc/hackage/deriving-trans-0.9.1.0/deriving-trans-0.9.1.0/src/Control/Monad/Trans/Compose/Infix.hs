module Control.Monad.Trans.Compose.Infix where

import Control.Monad.Trans.Compose
import Control.Monad.Trans.Control

-- | A flipped type-level infix operator for 'ComposeT'.
type (.|>) t2 t1 = ComposeT t1 t2

infixl 1 .|>

-- | A 'flip'ped infix operator for 'runComposeT'.
(./>) :: (forall a. t2 m a -> m (StT t2 a)) -- ^ run @t2@
      -> (forall a. t1 (t2 m) a -> t2 m (StT t1 a)) -- ^ run @t1@
      -> (forall a. (t2 .|> t1) m a -> m (StT t2 (StT t1 a)))
(./>) runT2 runT1 = runComposeT runT1 runT2

infixl 1 ./>

-- | A 'flip'ped infix operator for 'runComposeT''.
(..>) :: (t2 m a -> m a) -- ^ run @t2@
      -> (t1 (t2 m) a -> t2 m a) -- ^ run @t1@
      -> ((t2 .|> t1) m a -> m a)
(..>) runT2 runT1 = runComposeT' runT1 runT2

infixl 1 ..>
