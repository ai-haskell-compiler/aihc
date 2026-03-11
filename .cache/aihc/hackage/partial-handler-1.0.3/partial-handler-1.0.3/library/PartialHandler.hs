module PartialHandler where

import Prelude
import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Concurrent
import Data.Maybe
import Data.Monoid
import qualified Data.Semigroup as Sem
import System.IO.Error


-- |
-- A composable exception handler.
newtype PartialHandler a =
  PartialHandler (SomeException -> Maybe (IO a))


instance Functor PartialHandler where
  fmap fn (PartialHandler partialHandlerFn) =
    PartialHandler (fmap (fmap fn) . partialHandlerFn)

instance Applicative PartialHandler where
  pure x =
    PartialHandler (const (pure (pure x)))
  (<*>) (PartialHandler partialHandlerFn1) (PartialHandler partialHandlerFn2) =
    PartialHandler (liftA2 (liftA2 (liftA2 ($))) partialHandlerFn1 partialHandlerFn2)

instance Alternative PartialHandler where
  empty =
    PartialHandler (const Nothing)
  (<|>) (PartialHandler partialHandlerFn1) (PartialHandler partialHandlerFn2) =
    PartialHandler (liftA2 (<|>) partialHandlerFn1 partialHandlerFn2)

instance Sem.Semigroup (PartialHandler a) where
  (<>) =
    (<|>)

instance Monoid (PartialHandler a) where
  mempty = 
    empty
  mappend =
    (<|>)



-- * Totalizers
-------------------------

-- |
-- A function, which handles all exceptions.
-- 
-- Can be used as a parameter to standard functions like 'catch' and 'handle',
-- or to process the result of 'try'.
type TotalHandler a = 
  SomeException -> IO a

-- |
-- Convert a partial handler into a total handler,
-- which throws an error for unhandled cases.
-- In other words, the produced total handler is itself a partial function,
-- so use this only in cases when an unhandled exception should be considered as a bug.
{-# INLINABLE totalize #-}
totalize :: PartialHandler a -> TotalHandler a
totalize (PartialHandler h) =
  \e -> fromMaybe (error $ "Unhandled exception: " <> show e) (h e)

-- |
-- Convert a partial handler into a total handler,
-- which rethrows all unhandled exceptions.
{-# INLINABLE totalizeRethrowing #-}
totalizeRethrowing :: PartialHandler a -> TotalHandler a
totalizeRethrowing (PartialHandler h) =
  \e -> fromMaybe (throwIO e) (h e)

-- |
-- Convert a partial handler into a total handler,
-- which rethrows all unhandled exceptions to the specified thread and 
-- the current thread.
{-# INLINABLE totalizeRethrowingTo #-}
totalizeRethrowingTo :: ThreadId -> PartialHandler a -> TotalHandler a
totalizeRethrowingTo t (PartialHandler h) =
  \e -> fromMaybe (throwTo t e >> throwIO e) (h e)

-- |
-- Convert a partial handler into a total handler,
-- which rethrows all unhandled exceptions to the specified thread,
-- while the current thread continues the execution.
{-# INLINABLE totalizeRethrowingTo_ #-}
totalizeRethrowingTo_ :: ThreadId -> PartialHandler () -> TotalHandler ()
totalizeRethrowingTo_ t (PartialHandler h) =
  \e -> fromMaybe (throwTo t e) (h e)


-- * Standard handlers
-------------------------

-- |
-- A handler of exceptions of a specific type.
{-# INLINABLE typed #-}
typed :: Exception e => (e -> Maybe (IO a)) -> PartialHandler a
typed h =
  PartialHandler $ fromException >=> h
  

-- ** AsyncException handlers
-------------------------

-- |
-- A handler of the 'ThreadKilled' exception.
{-# INLINABLE onThreadKilled #-}
onThreadKilled :: IO a -> PartialHandler a
onThreadKilled handler =
  typed $ \case
    ThreadKilled -> Just handler
    _ -> Nothing

-- ** IOError handlers
-------------------------

-- |
-- A handler of all exceptions of type 'IOError' by their type.
{-# INLINABLE onIOErrorByType #-}
onIOErrorByType :: (IOErrorType -> Maybe (IO a)) -> PartialHandler a
onIOErrorByType handler =
  typed $ handler . ioeGetErrorType

-- |
-- A handler of an 'IOError' exception 
-- with type satisfying the 'isAlreadyExistsError' predicate.
{-# INLINABLE onAlreadyExists #-}
onAlreadyExists :: IO a -> PartialHandler a
onAlreadyExists handler =
  typed $ \e -> if isAlreadyExistsError e then Just handler else Nothing

-- |
-- A handler of an 'IOError' exception 
-- with type satisfying the 'isDoesNotExistError' predicate.
{-# INLINABLE onDoesNotExist #-}
onDoesNotExist :: IO a -> PartialHandler a
onDoesNotExist handler =
  typed $ \e -> if isDoesNotExistError e then Just handler else Nothing

-- |
-- A handler of an 'IOError' exception 
-- with type satisfying the 'isAlreadyInUseError' predicate.
{-# INLINABLE onAlreadyInUse #-}
onAlreadyInUse :: IO a -> PartialHandler a
onAlreadyInUse handler =
  typed $ \e -> if isAlreadyInUseError e then Just handler else Nothing

-- |
-- A handler of an 'IOError' exception 
-- with type satisfying the 'isFullError' predicate.
{-# INLINABLE onFull #-}
onFull :: IO a -> PartialHandler a
onFull handler =
  typed $ \e -> if isFullError e then Just handler else Nothing

-- |
-- A handler of an 'IOError' exception 
-- with type satisfying the 'isEOFError' predicate.
{-# INLINABLE onEOF #-}
onEOF :: IO a -> PartialHandler a
onEOF handler =
  typed $ \e -> if isEOFError e then Just handler else Nothing

-- |
-- A handler of an 'IOError' exception 
-- with type satisfying the 'isIllegalOperation' predicate.
{-# INLINABLE onIllegalOperation #-}
onIllegalOperation :: IO a -> PartialHandler a
onIllegalOperation handler =
  typed $ \e -> if isIllegalOperation e then Just handler else Nothing

-- |
-- A handler of an 'IOError' exception 
-- with type satisfying the 'isPermissionError' predicate.
{-# INLINABLE onPermission #-}
onPermission :: IO a -> PartialHandler a
onPermission handler =
  typed $ \e -> if isPermissionError e then Just handler else Nothing

-- |
-- A handler of an 'IOError' exception 
-- with type satisfying the 'isUserError' predicate.
{-# INLINABLE onUser #-}
onUser :: IO a -> PartialHandler a
onUser handler =
  typed $ \e -> if isUserError e then Just handler else Nothing
