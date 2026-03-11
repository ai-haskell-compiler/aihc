module Control.Concurrent.PooledIO.InOrder (
   T, run, runLimited, fork,
   ) where

import qualified Control.Concurrent.PooledIO.Monad as Pool
import Control.DeepSeq (NFData)

import qualified System.Unsafe as Unsafe

import Control.Monad.IO.Class (liftIO)
import Control.Applicative (Applicative, pure, (<*>))


newtype T a = Cons {decons :: Pool.T a}

instance Functor T where
   fmap f (Cons m) = Cons $ fmap f m

instance Applicative T where
   pure = Cons . pure
   Cons f <*> Cons a = Cons $ f <*> a

instance Monad T where
   return = Cons . return
   Cons x >>= k  =  Cons $ decons . k =<< x

{-
The 'complete' MVar makes sure
that we do not run more threads than capabilities.
The 'result' MVar makes sure
that we run an action only after all of its inputs are evaluated.
-}
{- |
'fork' runs an IO action in parallel
while respecting a maximum number of threads.
Evaluating the result of 'T'
waits for the termination of the according thread.

Unfortunately, this means that sometimes threads are bored:

> foo a b = do
>    c <- fork $ f a
>    d <- fork $ g c
>    e <- fork $ h b

Here the execution of @g c@ reserves a thread
but starts with waiting for the evaluation of @c@.
It would be certainly better to execute @h b@ first.
You may relax this problem by moving dependent actions
away from another as much as possible.
It would be optimal to have an @OutOfOrder@ monad,
but this is more difficult to implement.

Although we fork all actions in order,
the fork itself might re-order the actions.
Thus the actions must not rely on a particular order
other than the order imposed by data dependencies.
We enforce with the 'NFData' constraint
that the computation is actually completed
when the thread terminates.

Currently the monad does not handle exceptions.
It's certainly best to use a package with explicit exception handling
like @explicit-exception@ in order to tunnel exception information
from the forked action to the main thread.

Although 'fork' has almost the same type signature as 'liftIO'
we do not define @instance MonadIO InOrder.T@
since this definition would not satisfy the laws required by the 'MonadIO' class.
-}
fork :: (NFData a) => IO a -> T a
fork act =
   Cons $
   liftIO . Unsafe.interleaveIO =<< Pool.fork act

{- |
'runLimited' with a maximum of @numCapabilites@ threads.
-}
run :: T a -> IO a
run = Pool.withNumCapabilities runLimited

{- |
@runLimited n@ runs several actions in a pool with at most @n@ threads.
-}
runLimited :: Int -> T a -> IO a
runLimited maxThreads (Cons m) =
   Pool.runLimited maxThreads m
