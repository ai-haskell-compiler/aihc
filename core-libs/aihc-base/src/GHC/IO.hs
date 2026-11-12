{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- The argument of mask has a rank-2 type, so const would need an
-- impredicative instantiation. GHC's base uses the lambda too.
{- HLINT ignore "Use const" -}

-- | The 'IO' monad and the exception operations that the rest of @base@
-- builds on. The mask operations are those of GHC. See "Asynchronous
-- exceptions" in docs/exceptions.md.
module GHC.IO
  ( module GHC.Prim.IO,
    unIO,
    ioToST,
    unsafeIOToST,
    unsafeSTToIO,
    FilePath,
    unsafePerformIO,
    unsafeDupablePerformIO,
    unsafeInterleaveIO,
    unsafeDupableInterleaveIO,
    noDuplicate,
    catch,
    catchException,
    catchAny,
    throwIO,
    mask,
    mask_,
    uninterruptibleMask,
    uninterruptibleMask_,
    getMaskingState,
    unsafeUnmask,
    interruptible,
    onException,
    bracket,
    finally,
    evaluate,
  )
where

import GHC.Exception.Type (Exception (..), SomeException (..))
import GHC.IO.Unsafe (unsafeDupableInterleaveIO, unsafeDupablePerformIO, unsafeInterleaveIO, unsafePerformIO)
import GHC.Internal.Base (Maybe (..), Monad (..), String, id)
import GHC.Prim (RealWorld, State#, catch#, getMaskingState#, maskAsyncExceptions#, maskUninterruptible#, noDuplicate#, raiseIO#, seq, unmaskAsyncExceptions#, unsafeCoerce#)
import GHC.Prim.IO

type FilePath = String

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO action) = action

ioToST :: IO a -> ST RealWorld a
ioToST (IO action) = ST action

-- The two conversions coerce the state tokens and not the state function.
-- When one conversion is inside the other, the two coercions of a token
-- then meet after inlining, and the simplifier removes the pair.
unsafeIOToST :: IO a -> ST s a
unsafeIOToST (IO action) =
  ST
    ( \state -> case action (unsafeCoerce# state) of
        (# next, value #) -> (# unsafeCoerce# next, value #)
    )

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO (ST action) =
  IO
    ( \state -> case action (unsafeCoerce# state) of
        (# next, value #) -> (# unsafeCoerce# next, value #)
    )

-- | Claim the evaluations of the thread, and give up an evaluation that
-- another thread does already. See 'noDuplicate#'.
noDuplicate :: IO ()
noDuplicate = IO (\state -> (# noDuplicate# state, () #))

-- | Raise an exception in the @IO@ thread. 'raiseIO#' takes the state
-- token, so each effect before the raise stays before it.
throwIO :: (Exception e) => e -> IO a
throwIO exception = IO (raiseIO# (toException exception))

-- | Catch the exceptions of one type. Other exceptions continue upward.
catchException :: (Exception e) => IO a -> (e -> IO a) -> IO a
catchException (IO action) handler =
  IO (catch# action handler')
  where
    handler' exception =
      case fromException exception of
        Just selected -> unIO (handler selected)
        Nothing -> raiseIO# exception

catch :: (Exception e) => IO a -> (e -> IO a) -> IO a
catch = catchException

-- | Catch every exception.
catchAny :: IO a -> (forall e. (Exception e) => e -> IO a) -> IO a
catchAny (IO action) handler =
  IO (catch# action handler')
  where
    handler' (SomeException exception) = unIO (handler exception)

block :: IO a -> IO a
block (IO action) = IO (maskAsyncExceptions# action)

blockUninterruptible :: IO a -> IO a
blockUninterruptible (IO action) = IO (maskUninterruptible# action)

unblock :: IO a -> IO a
unblock = unsafeUnmask

-- | Run the action with asynchronous exceptions masked. The action gets a
-- function that restores the masking state of the caller.
mask :: ((forall a. IO a -> IO a) -> IO b) -> IO b
mask io = do
  state <- getMaskingState
  case state of
    Unmasked -> block (io unblock)
    MaskedInterruptible -> io block
    MaskedUninterruptible -> io blockUninterruptible

mask_ :: IO a -> IO a
mask_ io = mask (\_ -> io)

uninterruptibleMask :: ((forall a. IO a -> IO a) -> IO b) -> IO b
uninterruptibleMask io = do
  state <- getMaskingState
  case state of
    Unmasked -> blockUninterruptible (io unblock)
    MaskedInterruptible -> blockUninterruptible (io block)
    MaskedUninterruptible -> io blockUninterruptible

uninterruptibleMask_ :: IO a -> IO a
uninterruptibleMask_ io = uninterruptibleMask (\_ -> io)

getMaskingState :: IO MaskingState
getMaskingState =
  IO
    ( \state -> case getMaskingState# state of
        (# next, 0# #) -> (# next, Unmasked #)
        (# next, 1# #) -> (# next, MaskedUninterruptible #)
        (# next, _ #) -> (# next, MaskedInterruptible #)
    )

-- | Run the action unmasked. A queued asynchronous exception is raised at
-- once.
unsafeUnmask :: IO a -> IO a
unsafeUnmask (IO action) = IO (unmaskAsyncExceptions# action)

-- | Let an interruptibly masked thread take a queued exception while the
-- action runs.
interruptible :: IO a -> IO a
interruptible act = do
  state <- getMaskingState
  case state of
    Unmasked -> act
    MaskedInterruptible -> unsafeUnmask act
    MaskedUninterruptible -> act

-- | Run the second action when the first action raises an exception.
onException :: IO a -> IO b -> IO a
onException action cleanup =
  catchException action (\exception -> cleanup >> throwIO (exception :: SomeException))

-- | Acquire a resource, use it, and release it. An asynchronous exception
-- cannot come between the acquisition and the use, or between the use and
-- the release.
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket acquire release use =
  mask
    ( \restore -> do
        resource <- acquire
        result <- restore (use resource) `onException` release resource
        _ <- release resource
        return result
    )

finally :: IO a -> IO b -> IO a
finally action cleanup =
  mask
    ( \restore -> do
        result <- restore action `onException` cleanup
        _ <- cleanup
        return result
    )

evaluate :: a -> IO a
evaluate value =
  IO
    ( \state ->
        seq value (# state, value #)
    )
