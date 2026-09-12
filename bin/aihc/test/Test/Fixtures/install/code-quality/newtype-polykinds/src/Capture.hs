module Capture where

import Continuation (ContT)
import qualified Continuation

class Capture (m :: * -> *) where
  callCC :: ((a -> m b) -> m a) -> m a

instance forall k (r :: k) (m :: k -> *). Capture (ContT r m) where
  callCC = Continuation.callCC

data Lift (f :: * -> *) a = Lift (f a)

lower :: Lift (Continuation.Constant e) a -> e
lower (Lift (Continuation.Constant e)) = e

failure :: e -> Lift (Continuation.Constant e) a
failure e = Lift (Continuation.Constant e)

class Tag (t :: (* -> *) -> * -> *) where
  tag :: t m a -> t m a

instance Tag t where
  tag x = x

use :: forall (m :: * -> *) a. Continuation.IdentityT m a -> Continuation.IdentityT m a
use = tag

class Action (m :: * -> *) where
  action :: m a -> m a

newtype Lifted (t :: (* -> *) -> * -> *) (m :: * -> *) a = Lifted (t m a)

instance Action (Lifted t m) where
  action x = x

deriving via (Lifted Continuation.IdentityT m)
  instance Action (Continuation.IdentityT m)

raw :: Continuation.Raw Continuation.Unit -> Continuation.Raw Continuation.Unit
raw x = x
