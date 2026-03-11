{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
Module defining the type for exception free I/O.
Exceptional results in SIO must be represented by traditional error codes.

If you want to turn an IO action into 'SIO'
you must convert it to @ExceptionalT IOException SIO a@
by 'ioToExceptionalSIO' (or 'Control.Monad.Trans.liftIO')
and then handle the 'IOException's using 'SyncExc.resolveT'.
-}
module System.IO.Straight (
   SIO, sioToIO, ioToExceptionalSIO, unsafeInterleaveSIO,
   ExceptionalT, IOException,
   ) where

import Control.Monad.Exception.Synchronous
   (Exceptional(Success, Exception), ExceptionalT(ExceptionalT), )
import Control.Exception (IOException, try)
import Control.Monad.IO.Class (MonadIO, liftIO, )
import Control.Applicative (Applicative, )

import System.IO.Unsafe (unsafeInterleaveIO, )


{- |
An I/O action of type 'SIO' cannot skip following SIO actions
as a result of exceptional outcomes like \"File not found\".
However an 'error' can well break the program.
-}
newtype SIO a = SIO (IO a) -- {sioToIO :: IO a}
   deriving (Functor, Applicative, Monad)


sioToIO :: SIO a -> IO a
sioToIO (SIO x) = x

ioToExceptionalSIO :: IO a -> ExceptionalT IOException SIO a
ioToExceptionalSIO =
   ExceptionalT . SIO . fmap (either Exception Success) . try


unsafeInterleaveSIO :: SIO a -> SIO a
unsafeInterleaveSIO (SIO io) = SIO $ unsafeInterleaveIO io

-- helper classes for defining the MonadIO instance of SIO

{- |
Users of the library may define new instances of MonadSIO,
but monads other than SIO may not make the absence of exceptions explicit.
It is important however, that we do not make the method 'toSIO' public,
since this would allow users
the unsafe conversion from @IO@ to @SIO@.

Maybe we should not be so picky about exceptional monads
within exception monad transformers.
A monad like @ExceptionalT e0 (StateT s (Exceptional e1))@
may be useful for distinction
of non-fatal exceptions @e0@ that can maintain the state @s@
and fatal exceptions @e1@ that prevent generation of an updated state.
-}
class Monad m => MonadSIO m where toSIO :: IO a -> m a
instance MonadSIO SIO where toSIO = SIO

class ContainsIOException e where fromIOException :: IOException -> e
instance ContainsIOException IOException where fromIOException = id


instance (MonadSIO m, ContainsIOException e) =>
            MonadIO (ExceptionalT e m) where
   liftIO =
      ExceptionalT . toSIO .
      fmap (either (Exception . fromIOException) Success) . try
