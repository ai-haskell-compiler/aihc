{-# LANGUAGE ExistentialQuantification #-}

module Control.Exception
  ( module Control.Exception.Base,
    ExceptionWithContext (..),
    someExceptionContext,
    catchNoPropagate,
    rethrowIO,
    allowInterrupt,
    Handler (..),
    catches,
  )
where

import Control.Exception.Base
import GHC.Base (Applicative (..), Functor (..), (.))
import GHC.IO (catchException)
import GHC.Internal.Exception.Type (ExceptionWithContext (..), someExceptionContext)
import GHC.Prim.IO (IO)
import Prelude (foldr, maybe)

-- | Preserve the supplied context. This runtime does not add backtraces.
rethrowIO :: (Exception e) => ExceptionWithContext e -> IO a
rethrowIO = throwIO

-- | Supply the context without an annotation for the active handler.
catchNoPropagate :: (Exception e) => IO a -> (ExceptionWithContext e -> IO a) -> IO a
catchNoPropagate = catchException

-- | The runtime has no asynchronous exceptions to deliver.
allowInterrupt :: IO ()
allowInterrupt = interruptible (pure ())

-- | A handler for one type of exception. See 'catches'.
data Handler a = forall e. (Exception e) => Handler (e -> IO a)

instance Functor Handler where
  fmap function (Handler handler) = Handler (fmap function . handler)

-- | Run the action. If it raises an exception, run the first handler that
-- accepts the type of that exception. If no handler accepts it, raise the
-- exception again.
catches :: IO a -> [Handler a] -> IO a
catches action handlers = action `catch` catchesHandler handlers

catchesHandler :: [Handler a] -> SomeException -> IO a
catchesHandler handlers exception = foldr tryHandler (throwIO exception) handlers
  where
    tryHandler (Handler handler) rest = maybe rest handler (fromException exception)
