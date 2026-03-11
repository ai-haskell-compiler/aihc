{-
This module could also be part of 'transformers'.
-}
module UniqueLogic.ST.TF.MonadTrans where

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.Trans.Except as ME
import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.Maybe as MM
import qualified Control.Monad.Trans.Identity as MI

import Control.Applicative (Applicative, pure, (<*>), Const(Const))
import Control.Monad (liftM, ap, )
import Data.Monoid (Monoid, )


{- |
Provide the methods that make a transformed monad a monad.
-}
class MT.MonadTrans t => C t where
   point :: Monad m => a -> t m a
   bind :: Monad m => t m a -> (a -> t m b) -> t m b

instance C MI.IdentityT where
   point = return
   bind = (>>=)

instance (Monoid w) => C (MW.WriterT w) where
   point = return
   bind = (>>=)

instance C (ME.ExceptT e) where
   point = return
   bind = (>>=)

instance C MM.MaybeT where
   point = return
   bind = (>>=)


{- |
Build a regular monad for generic monad transformer and monad.
The 'Const' type allows us to force the kind (m :: * -> *)
without using ExplicitKindSignatures.
-}
newtype Wrap t m a = Wrap (Const (t m a) (m a))

wrap :: t m a -> Wrap t m a
wrap = Wrap . Const

unwrap :: Wrap t m a -> t m a
unwrap (Wrap (Const m)) = m

lift :: (C t, Monad m) => m a -> Wrap t m a
lift = wrap . MT.lift


instance (C t, Monad m) => Functor (Wrap t m) where
   fmap = liftM

instance (C t, Monad m) => Applicative (Wrap t m) where
   pure = return
   (<*>) = ap

instance (C t, Monad m) => Monad (Wrap t m) where
   return = wrap . point
   x >>= k  =  wrap $ bind (unwrap x) (unwrap . k)
