module Control.Exception
  ( module Control.Exception.Base,
    ExceptionWithContext (..),
    someExceptionContext,
    catchNoPropagate,
    rethrowIO,
  )
where

import Control.Exception.Base
import GHC.IO (catchException)
import GHC.Internal.Exception.Type (ExceptionWithContext (..), someExceptionContext)
import GHC.Prim.IO (IO)

-- | Preserve the supplied context. This runtime does not add backtraces.
rethrowIO :: (Exception e) => ExceptionWithContext e -> IO a
rethrowIO = throwIO

-- | Supply the context without an annotation for the active handler.
catchNoPropagate :: (Exception e) => IO a -> (ExceptionWithContext e -> IO a) -> IO a
catchNoPropagate = catchException
