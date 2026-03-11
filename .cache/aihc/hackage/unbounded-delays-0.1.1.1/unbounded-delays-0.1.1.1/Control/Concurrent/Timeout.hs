{-# LANGUAGE CPP, DeriveDataTypeable, NoImplicitPrelude #-}

#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif

-- | Wait arbitrarily long for an IO computation to finish.
module Control.Concurrent.Timeout ( timeout, Timeout, timeoutWithPred ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Control.Concurrent       ( forkIOWithUnmask, myThreadId, throwTo, killThread )
import Control.Exception        ( Exception, bracket, handleJust )
import Control.Monad            ( return, (>>), fmap )
import Data.Bool                ( Bool(False), otherwise )
import Data.Eq                  ( Eq, (==) )
import Data.Function            ( (.), const)
import Data.Maybe               ( Maybe(Nothing, Just) )
import Data.Ord                 ( (<) )
import Data.Typeable            ( Typeable )
import Data.Unique              ( Unique, newUnique )
import Prelude                  ( Integer )
import System.IO                ( IO )
import Text.Show                ( Show, show )

#if __GLASGOW_HASKELL__ < 700
import Prelude                  ( fromInteger )
import Control.Monad            ( (>>=), fail )
#endif

#ifdef __HADDOCK_VERSION__
import Data.Int                 ( Int )
import System.IO                ( hGetBuf, hPutBuf, hWaitForInput )
import qualified System.Timeout ( timeout )
#endif

-- from unbounded-delays (this package):
import Control.Concurrent.Thread.Delay ( delay )


-------------------------------------------------------------------------------
-- Long delays and timeouts
-------------------------------------------------------------------------------

{-
The following code was mostly copied from the module System.Timeout in the
package base-4.2.0.0.

(c) The University of Glasgow 2007
-}

newtype Timeout = Timeout Unique deriving (Eq, Typeable)

instance Show Timeout where
    show _ = "<<timeout>>"

instance Exception Timeout

{-|
Like @System.Timeout.'System.Timeout.timeout'@, but not bounded by an 'Int'.
(..)
Wrap an 'IO' computation to time out and return 'Nothing' in case no result is
available within @n@ microseconds (@1\/10^6@ seconds). In case a result is
available before the timeout expires, 'Just' @a@ is returned. A negative timeout
interval means \"wait indefinitely\".

If the computation has not terminated after @n@ microseconds, it is interrupted
by an asynchronous exception. The function passed to @f@ can be used to detect
whether it was interrupted by this timeout or some other exception.

The design of this combinator was guided by the objective that @timeout n (const f)@
should behave exactly the same as @f@ as long as @f@ doesn't time out. This
means that @f@ has the same 'myThreadId' it would have without the timeout
wrapper. Any exceptions @f@ might throw cancel the timeout and propagate further
up. It also possible for @f@ to receive exceptions thrown to it by another
thread.

A tricky implementation detail is the question of how to abort an 'IO'
computation. This combinator relies on asynchronous exceptions internally.  The
technique works very well for computations executing inside of the Haskell
runtime system, but it doesn't work at all for non-Haskell code. Foreign
function calls, for example, cannot be timed out with this combinator simply
because an arbitrary C function cannot receive asynchronous exceptions. When
@timeout@ is used to wrap an FFI call that blocks, no timeout event can be
delivered until the FFI call returns, which pretty much negates the purpose of
the combinator. In practice, however, this limitation is less severe than it may
sound. Standard I\/O functions like 'System.IO.hGetBuf', 'System.IO.hPutBuf',
Network.Socket.accept, or 'System.IO.hWaitForInput' appear to be blocking, but
they really don't because the runtime system uses scheduling mechanisms like
@select(2)@ to perform asynchronous I\/O, so it is possible to interrupt
standard socket I\/O or file I\/O using this combinator.
-}
timeoutWithPred :: Integer -> ((Timeout -> Bool) -> IO α) -> IO (Maybe α)
timeoutWithPred n f
    | n < 0     = fmap Just (f (const False))
    | n == 0    = return Nothing
    | otherwise = do
        pid <- myThreadId
        ex  <- fmap Timeout newUnique
        handleJust (\e -> if e == ex then Just () else Nothing)
                   (\_ -> return Nothing)
                   (bracket (forkIOWithUnmask (\unmask -> unmask (delay n >> throwTo pid ex)))
                            (killThread)
                            (\_ -> fmap Just (f (==ex)))
                   )

{-|
Like 'timeoutWithPred', but does not expose the 'Timeout' exception to the called action.
-}
timeout :: Integer -> IO α -> IO (Maybe α)
timeout n = timeoutWithPred n . const
