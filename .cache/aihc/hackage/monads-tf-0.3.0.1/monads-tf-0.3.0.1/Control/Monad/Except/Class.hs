{- |
Module      :  Control.Monad.Except.Class
Copyright   :  (c) Michael Weber <michael.weber@post.rwth-aachen.de> 2001,
               (c) Jeff Newbern 2003-2006,
               (c) Andriy Palamarchuk 2006
License     :  BSD-style (see the file LICENSE)

Maintainer  :  ross@soi.city.ac.uk
Stability   :  experimental
Portability :  non-portable (type families)

[Computation type:] Computations which may fail or throw exceptions.

[Binding strategy:] Failure records information about the cause\/location
of the failure. Failure values bypass the bound function,
other values are used as inputs to the bound function.

[Useful for:] Building computations from sequences of functions that may fail
or using exception handling to structure error handling.

[Zero and plus:] Zero is represented by an empty error and the plus operation
executes its second argument if the first fails.

[Example type:] @'Either' 'String' a@

The Error monad (also called the Exception monad).
-}

{-
  Rendered by Michael Weber <mailto:michael.weber@post.rwth-aachen.de>,
  inspired by the Haskell Monad Template Library from
    Andy Gill (<http://web.cecs.pdx.edu/~andy/>)
-}
module Control.Monad.Except.Class (
    MonadError(..),
  ) where

import Control.Monad.Trans.Except (ExceptT)
import qualified Control.Monad.Trans.Except as ExceptT (throwE, catchE)
import Control.Monad.Trans.Identity as Identity
import Control.Monad.Trans.Maybe as Maybe
import Control.Monad.Trans.Reader as Reader
import Control.Monad.Trans.RWS.Lazy as LazyRWS
import Control.Monad.Trans.RWS.Strict as StrictRWS
import Control.Monad.Trans.State.Lazy as LazyState
import Control.Monad.Trans.State.Strict as StrictState
import Control.Monad.Trans.Writer.Lazy as LazyWriter
import Control.Monad.Trans.Writer.Strict as StrictWriter
import Control.Monad.Trans

import qualified Control.Exception

{- |
The strategy of combining computations that can throw exceptions
by bypassing bound functions
from the point an exception is thrown to the point that it is handled.

Is parameterized over the type of error information and
the monad type constructor.
It is common to use @'Data.Either' String@ as the monad type constructor
for an error monad in which error descriptions take the form of strings.
In that case and many other common cases the resulting monad is already defined
as an instance of the 'MonadError' class.
You can also define your own a monad type constructor
other than @'Data.Either' e@, in which case you will have to explicitly define
an instance of the 'MonadError' class.
-}
class (Monad m) => MonadError m where
    type ErrorType m

    -- | Is used within a monadic computation to begin exception processing.
    throwError :: ErrorType m -> m a

    {- |
    A handler function to handle previous errors and return to normal execution.
    A common idiom is:

    > do { action1; action2; action3 } `catchError` handler

    where the @action@ functions can call 'throwError'.
    Note that @handler@ and the do-block must have the same return type.
    -}
    catchError :: m a -> (ErrorType m -> m a) -> m a

instance MonadError IO where
    type ErrorType IO = IOError
    throwError = ioError
    catchError = Control.Exception.catch

-- ---------------------------------------------------------------------------
-- Our parameterizable error monad

instance MonadError (Either e) where
    type ErrorType (Either e) = e
    throwError             = Left
    Left  l `catchError` h = h l
    Right r `catchError` _ = Right r

instance (Monad m) => MonadError (ExceptT e m) where
    type ErrorType (ExceptT e m) = e
    throwError = ExceptT.throwE
    catchError = ExceptT.catchE

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (MonadError m) => MonadError (IdentityT m) where
    type ErrorType (IdentityT m) = ErrorType m
    throwError = lift . throwError
    catchError = Identity.liftCatch catchError

instance (MonadError m) => MonadError (MaybeT m) where
    type ErrorType (MaybeT m) = ErrorType m
    throwError = lift . throwError
    catchError = Maybe.liftCatch catchError

instance (MonadError m) => MonadError (ReaderT r m) where
    type ErrorType (ReaderT r m) = ErrorType m
    throwError = lift . throwError
    catchError = Reader.liftCatch catchError

instance (Monoid w, MonadError m) => MonadError (LazyRWS.RWST r w s m) where
    type ErrorType (LazyRWS.RWST r w s m) = ErrorType m
    throwError = lift . throwError
    catchError = LazyRWS.liftCatch catchError

instance (Monoid w, MonadError m) => MonadError (StrictRWS.RWST r w s m) where
    type ErrorType (StrictRWS.RWST r w s m) = ErrorType m
    throwError = lift . throwError
    catchError = StrictRWS.liftCatch catchError

instance (MonadError m) => MonadError (LazyState.StateT s m) where
    type ErrorType (LazyState.StateT s m) = ErrorType m
    throwError = lift . throwError
    catchError = LazyState.liftCatch catchError

instance (MonadError m) => MonadError (StrictState.StateT s m) where
    type ErrorType (StrictState.StateT s m) = ErrorType m
    throwError = lift . throwError
    catchError = StrictState.liftCatch catchError

instance (Monoid w, MonadError m) => MonadError (LazyWriter.WriterT w m) where
    type ErrorType (LazyWriter.WriterT w m) = ErrorType m
    throwError = lift . throwError
    catchError = LazyWriter.liftCatch catchError

instance (Monoid w, MonadError m) => MonadError (StrictWriter.WriterT w m) where
    type ErrorType (StrictWriter.WriterT w m) = ErrorType m
    throwError = lift . throwError
    catchError = StrictWriter.liftCatch catchError
