{-# LANGUAGE Safe #-}
{- |
This module implements something similar to
"Control.Concurrent.PooledIO.InOrder",
but since it is restricted to an 'Applicative' interface
we can implement it without 'unsafeInterleaveIO'.
-}
module Control.Concurrent.PooledIO.Final (
   T, run, runLimited, fork,
   ) where

import qualified Control.Concurrent.PooledIO.Monad as Pool
import Control.DeepSeq (NFData)

import Control.Monad (join)
import Control.Applicative (Applicative, pure, (<*>))
import Data.Functor.Compose (Compose(Compose))


newtype T a = Cons (Compose Pool.T IO a)

instance Functor T where
   fmap f (Cons m) = Cons $ fmap f m

instance Applicative T where
   pure = Cons . pure
   Cons f <*> Cons a = Cons $ f <*> a


{- |
This runs an action parallelly to the starting thread.
Since it is an Applicative Functor and not a Monad,
there are no data dependencies between the actions
and thus all actions in a 'T' can be run parallelly.
Only the 'IO' actions are parallelised
but not the combining function passed to 'liftA2' et.al.
That is, the main work must be done in the 'IO' actions
in order to benefit from parallelisation.
-}
fork :: (NFData a) => IO a -> T a
fork = Cons . Compose . Pool.fork

{- |
'runLimited' with a maximum of @numCapabilites@ threads.
-}
run :: T a -> IO a
run = Pool.withNumCapabilities runLimited

{- |
@runLimited n@ runs several actions in a pool with at most @n@ threads.
-}
runLimited :: Int -> T a -> IO a
runLimited maxThreads (Cons (Compose m)) =
   join $ Pool.runLimited maxThreads m
