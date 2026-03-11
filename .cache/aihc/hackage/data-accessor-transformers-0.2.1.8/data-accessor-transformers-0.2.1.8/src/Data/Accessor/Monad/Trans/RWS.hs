{- | Access helper functions in a Reader-Writer-State monad -}
module Data.Accessor.Monad.Trans.RWS where

import qualified Data.Accessor.Basic as Accessor
import qualified Control.Monad.Trans.RWS as RWS
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.RWS (RWS, runRWS, RWST(runRWST), )
import Data.Monoid (Monoid)


-- * accessors in the form of actions in the RWS monad

set :: (Monad m, Monoid w) => Accessor.T s a -> a -> RWST r w s m ()
set f x = RWS.modify (Accessor.set f x)

get :: (Monad m, Monoid w) => Accessor.T s a -> RWST r w s m a
get f = RWS.gets (Accessor.get f)

modify :: (Monad m, Monoid w) => Accessor.T s a -> (a -> a) -> RWST r w s m ()
modify f g = RWS.modify (Accessor.modify f g)

{- |
Modify a record element and return its old value.
-}
getAndModify :: (Monad m, Monoid w) => Accessor.T s a -> (a -> a) -> RWST r w s m a
getAndModify f g =
   do x <- get f
      modify f g
      return x

{- |
Modify a record element and return its new value.
-}
modifyAndGet :: (Monad m, Monoid w) => Accessor.T s a -> (a -> a) -> RWST r w s m a
modifyAndGet f g =
   do modify f g
      get f



infix 1 %=, %:

{- |
Infix variant of 'set'.
-}
(%=) :: (Monad m, Monoid w) => Accessor.T s a -> a -> RWST r w s m ()
(%=) = set

{- |
Infix variant of 'modify'.
-}
(%:) :: (Monad m, Monoid w) => Accessor.T s a -> (a -> a) -> RWST r w s m ()
(%:) = modify



-- * lift a RWS monadic accessor to an accessor of a parent record

lift :: (Monad m, Monoid w) => Accessor.T s1 s0 -> RWS r w s0 a -> RWST r w s1 m a
lift f m =
   do r <- RWS.ask
      s0 <- get f
      let (a,s1,w) = runRWS m r s0
      set f s1
      RWS.tell w
      return a

liftT :: (Monad m, Monoid w) =>
   Accessor.T s1 s0 -> RWST r w s0 m a -> RWST r w s1 m a
liftT f m =
   do r <- RWS.ask
      s0 <- get f
      (a,s1,w) <- Trans.lift $ runRWST m r s0
      set f s1
      RWS.tell w
      return a
