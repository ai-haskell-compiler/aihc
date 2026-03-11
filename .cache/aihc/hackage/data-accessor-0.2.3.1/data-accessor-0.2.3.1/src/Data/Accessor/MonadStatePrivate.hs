module Data.Accessor.MonadStatePrivate where

import qualified Data.Accessor.Basic as Accessor
import qualified Control.Monad.Trans.State as State
import qualified Control.Monad.Trans.Class as Trans
import Control.Monad.Trans.State (State, runState, StateT(runStateT), )

-- * accessors in the form of actions in the state monad

set :: Monad m => Accessor.T r a -> a -> StateT r m ()
set f x = State.modify (Accessor.set f x)

get :: Monad m => Accessor.T r a -> StateT r m a
get f = State.gets (Accessor.get f)

modify :: Monad m => Accessor.T r a -> (a -> a) -> StateT r m ()
modify f g = State.modify (Accessor.modify f g)

{- |
Modify a record element and return its old value.
-}
getAndModify :: Monad m => Accessor.T r a -> (a -> a) -> StateT r m a
getAndModify f g =
   do x <- get f
      modify f g
      return x

{- |
Modify a record element and return its new value.
-}
modifyAndGet :: Monad m => Accessor.T r a -> (a -> a) -> StateT r m a
modifyAndGet f g =
   do modify f g
      get f



infix 1 %=, %:

{- |
Infix variant of 'set'.
-}
(%=) :: Monad m => Accessor.T r a -> a -> StateT r m ()
(%=) = set

{- |
Infix variant of 'modify'.
-}
(%:) :: Monad m => Accessor.T r a -> (a -> a) -> StateT r m ()
(%:) = modify



-- * lift a state monadic accessor to an accessor of a parent record

lift :: Monad m => Accessor.T r s -> State s a -> StateT r m a
lift f m =
   do s0 <- get f
      let (a,s1) = runState m s0
      set f s1
      return a

liftT :: (Monad m) =>
   Accessor.T r s -> StateT s m a -> StateT r m a
liftT f m =
   do s0 <- get f
      (a,s1) <- Trans.lift $ runStateT m s0
      set f s1
      return a
