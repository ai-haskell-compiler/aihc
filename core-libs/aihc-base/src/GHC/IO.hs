{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The 'IO' monad and the synchronous exception operations that the
-- rest of @base@ builds on. Asynchronous exceptions do not exist in this
-- runtime, so the mask operations only run their argument.
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

import GHC.Base (Maybe (..), Monad (..), String, id)
import GHC.Exception.Type (Exception (..), SomeException (..))
import GHC.IO.Unsafe (unsafeDupableInterleaveIO, unsafeDupablePerformIO, unsafeInterleaveIO, unsafePerformIO)
import GHC.Prim (RealWorld, State#, catch#, raise#, seq, unsafeCoerce#)
import GHC.Prim.IO

type FilePath = String

unIO :: IO a -> State# RealWorld -> (# State# RealWorld, a #)
unIO (IO action) = action

ioToST :: IO a -> ST RealWorld a
ioToST (IO action) = ST action

unsafeIOToST :: IO a -> ST s a
unsafeIOToST (IO action) = ST (unsafeCoerce# action)

unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO (ST action) = IO (unsafeCoerce# action)

-- | Thunks are never entered twice in this runtime.
noDuplicate :: IO ()
noDuplicate = return ()

throwIO :: (Exception e) => e -> IO a
throwIO exception = IO (\_state -> raise# (toException exception))

-- | Catch the exceptions of one type. Other exceptions continue upward.
catchException :: (Exception e) => IO a -> (e -> IO a) -> IO a
catchException (IO action) handler =
  IO (catch# action handler')
  where
    handler' exception =
      case fromException exception of
        Just selected -> unIO (handler selected)
        Nothing -> raise# exception

catch :: (Exception e) => IO a -> (e -> IO a) -> IO a
catch = catchException

-- | Catch every exception.
catchAny :: IO a -> (forall e. (Exception e) => e -> IO a) -> IO a
catchAny (IO action) handler =
  IO (catch# action handler')
  where
    handler' (SomeException exception) = unIO (handler exception)

mask :: ((forall a. IO a -> IO a) -> IO b) -> IO b
mask action = action id

mask_ :: IO a -> IO a
mask_ action = action

uninterruptibleMask :: ((forall a. IO a -> IO a) -> IO b) -> IO b
uninterruptibleMask action = action id

uninterruptibleMask_ :: IO a -> IO a
uninterruptibleMask_ action = action

getMaskingState :: IO MaskingState
getMaskingState = return Unmasked

unsafeUnmask :: IO a -> IO a
unsafeUnmask action = action

interruptible :: IO a -> IO a
interruptible action = action

-- | Run the second action when the first action raises an exception.
onException :: IO a -> IO b -> IO a
onException action cleanup =
  catchException action (\exception -> cleanup >> throwIO (exception :: SomeException))

bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
bracket acquire release use = do
  resource <- acquire
  result <- use resource `onException` release resource
  _ <- release resource
  return result

finally :: IO a -> IO b -> IO a
finally action cleanup = do
  result <- action `onException` cleanup
  _ <- cleanup
  return result

evaluate :: a -> IO a
evaluate value =
  IO
    ( \state ->
        seq value (# state, value #)
    )
