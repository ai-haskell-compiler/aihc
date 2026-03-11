{- |
Following an idea by Dominique Devriese:
<http://www.haskell.org/pipermail/libraries/2013-June/020185.html>
-}
{-
How about fancy infix operators like:

do ref <:- a
   b <- Ref.read ref

?
-}
module Data.Ref where

import Data.IORef (newIORef, readIORef, writeIORef, )
import Data.STRef (newSTRef, readSTRef, writeSTRef, )
import Control.Concurrent.STM.TVar (newTVar, readTVar, writeTVar, )
import Control.Concurrent.STM (STM, )
import Control.Monad.ST (ST)
import Control.Monad (liftM)

import qualified Control.Monad.Trans.Class as MT
import qualified Control.Monad.IO.Class as MIO

import qualified Control.Monad.Trans.RWS.Lazy as MRWSL
import qualified Control.Monad.Trans.RWS.Strict as MRWSS
import qualified Control.Monad.Trans.State.Lazy as MSL
import qualified Control.Monad.Trans.State.Strict as MSS
import qualified Control.Monad.Trans.Writer.Lazy as MWL
import qualified Control.Monad.Trans.Writer.Strict as MWS
import qualified Control.Monad.Trans.Cont as MC
import qualified Control.Monad.Trans.Except as MEx
import qualified Control.Monad.Trans.Maybe as MM
import qualified Control.Monad.Trans.Reader as MR
import qualified Control.Monad.Trans.Identity as MI

import qualified Data.Accessor.Basic as Accessor
import Data.Monoid (Monoid)

import Prelude hiding (read)


data T m a = Cons { write :: a -> m (), read :: m a }

modify :: C m => T m a -> (a -> a) -> m ()
modify ref f = write ref . f =<< read ref

focus :: C m => Accessor.T a b -> T m a -> T m b
focus acc ref =
   Cons
      (modify ref . Accessor.set acc)
      (liftM (Accessor.get acc) $ read ref)

newCons :: C m =>
   (a -> m ref) -> (ref -> a -> m ()) -> (ref -> m a) ->
   a -> m (T m a)
newCons nw wr rd = liftM (\r -> Cons (wr r) (rd r)) . nw


class Monad m => C m where
   new :: a -> m (T m a)

instance C IO where
   new = newCons newIORef writeIORef readIORef

instance C (ST s) where
   new = newCons newSTRef writeSTRef readSTRef

instance C STM where
   new = newCons newTVar writeTVar readTVar


{-
mapMonad :: (Monad m, Monad n) => (forall b. m b -> n b) -> T m a -> T n a
mapMonad lft (Cons wr rd) = Cons (lft . wr) (lft rd)
-}

lift :: (Monad m, MT.MonadTrans t) => T m a -> T (t m) a
lift (Cons wr rd) = Cons (MT.lift .wr) (MT.lift rd)

liftIO :: (MIO.MonadIO m) => T IO a -> T m a
liftIO (Cons wr rd) = Cons (MIO.liftIO .wr) (MIO.liftIO rd)

newLifted :: (C m, MT.MonadTrans t) => a -> t m (T (t m) a)
newLifted = MT.lift . liftM lift . new


instance C m => C (MI.IdentityT m) where new = newLifted

instance C m => C (MM.MaybeT m) where new = newLifted
instance (C m) => C (MEx.ExceptT e m) where new = newLifted

instance C m => C (MC.ContT r m) where new = newLifted
instance C m => C (MR.ReaderT r m) where new = newLifted
instance C m => C (MSS.StateT s m) where new = newLifted
instance C m => C (MSL.StateT s m) where new = newLifted
instance (Monoid w, C m) => C (MWS.WriterT w m) where new = newLifted
instance (Monoid w, C m) => C (MWL.WriterT w m) where new = newLifted
instance (Monoid w, C m) => C (MRWSS.RWST r w s m) where new = newLifted
instance (Monoid w, C m) => C (MRWSL.RWST r w s m) where new = newLifted

-- ToDo: another interesting instance would be Wrapper (StateT Vault)
