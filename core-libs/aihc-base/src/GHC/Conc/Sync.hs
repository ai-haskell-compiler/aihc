{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Conc.Sync
  ( ThreadId (..),
    forkIO,
    fromThreadId,
    killThread,
    myThreadId,
    showThreadId,
    throwTo,
    yield,
    STM (..),
    TVar (..),
    atomically,
    retry,
    orElse,
    throwSTM,
    catchSTM,
    newTVar,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
    unsafeIOToSTM,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus (..), ap, liftM2)
import GHC.Exception (ErrorCall (..), Exception (..), SomeException)
import GHC.IO (IO (..), catch, throwIO)
import GHC.IO.Exception (AsyncException (ThreadKilled), BlockedIndefinitelyOnSTM (..))
import GHC.Prim
import GHC.Word (Word64 (..))
import Prelude

-- | An opaque green-thread identifier.
data ThreadId = ThreadId ThreadId#

-- | The number of a green thread.
--
-- The runtime gives a different number to each green thread. The numbers start
-- at one, and the main thread has the number one. The runtime does not give the
-- number of a thread to another thread later. The number is not related to an
-- operating-system thread.
fromThreadId :: ThreadId -> Word64
fromThreadId (ThreadId threadId) = W64# (aihcThreadIdNumber# threadId)

-- | Read the number of a green thread.
--
-- GHC uses a foreign call to its runtime for this operation. The aihc foreign
-- interface accepts only C types, and a thread is a runtime object. Thus aihc
-- uses a primitive, and GHC.Prim does not export it.
foreign import prim aihcThreadIdNumber# :: ThreadId# -> Word64#

instance Eq ThreadId where
  left == right = fromThreadId left == fromThreadId right

instance Ord ThreadId where
  compare left right = compare (fromThreadId left) (fromThreadId right)

instance Show ThreadId where
  showsPrec precedence threadId =
    showString "ThreadId " . showsPrec precedence (fromThreadId threadId)

-- | Show a green-thread identifier.
showThreadId :: ThreadId -> String
showThreadId = show

-- | The identifier of the green thread that runs this action.
myThreadId :: IO ThreadId
myThreadId =
  IO
    ( \state ->
        case myThreadId# state of
          (# nextState, threadId #) -> (# nextState, ThreadId threadId #)
    )

-- | Schedule an action on a new green thread.
forkIO :: IO () -> IO ThreadId
forkIO (IO action) =
  IO
    ( \state ->
        -- Explicit GRIN apply does not enter operands, and unpacking the IO
        -- newtype alone does not enter its state transformer.
        seq
          action
          ( case fork# action state of
              (# nextState, threadId #) -> (# nextState, ThreadId threadId #)
          )
    )

-- | Asynchronous exceptions are not supported. The stub does not use either argument.
throwTo :: (Exception e) => ThreadId -> e -> IO ()
throwTo _ _ = throwIO (ErrorCallWithLocation "throwTo: asynchronous exceptions are not supported" "")

-- | Request thread termination. 'throwTo' reports an unsupported operation in this runtime.
killThread :: ThreadId -> IO ()
killThread thread = throwTo thread ThreadKilled

-- | Cooperatively yield to the next runnable green thread.
yield :: IO ()
yield =
  IO
    ( \state ->
        case yield# state of
          nextState -> (# nextState, () #)
    )

newtype STM a = STM (State# RealWorld -> (# State# RealWorld, a #))

data TVar a = TVar (TVar# RealWorld a)

instance Eq (TVar a) where
  TVar left == TVar right = case sameTVar# left right of
    0# -> False
    _ -> True

instance Functor STM where
  fmap function (STM action) = STM $ \state ->
    case action state of
      (# next, value #) -> (# next, function value #)

instance Applicative STM where
  pure value = STM (# ,value #)
  (<*>) = ap

instance Monad STM where
  STM action >>= next = STM $ \state ->
    case action state of
      (# nextState, value #) -> case next value of
        STM continuation -> continuation nextState

instance Alternative STM where
  empty = retry
  (<|>) = orElse

instance MonadPlus STM where
  mzero = retry
  mplus = orElse

instance (Semigroup a) => Semigroup (STM a) where
  (<>) = liftM2 (<>)

instance (Monoid a) => Monoid (STM a) where
  mempty = pure mempty

-- | Retry has a private exception type. catchSTM must let it pass.
data Retry = Retry

instance Show Retry where
  show Retry = "STM retry"

instance Exception Retry

unsafeIOToSTM :: IO a -> STM a
unsafeIOToSTM (IO action) = STM action

stmToIO :: STM a -> IO a
stmToIO (STM action) = IO action

-- | A failed action restores all writes since this savepoint.
transaction :: IO a -> IO a
transaction action = do
  IO (\state -> case stmBegin# state of next -> (# next, () #))
  result <- catch action abort
  IO (\state -> case stmCommit# state of next -> (# next, result #))
  where
    abort :: SomeException -> IO a
    abort exception = do
      IO (\state -> case stmAbort# state of next -> (# next, () #))
      throwIO exception

atomically :: STM a -> IO a
atomically action = do
  active <-
    IO
      ( \state -> case stmActive# state of
          (# next, flag #) -> (# next, case flag of 0# -> False; _ -> True #)
      )
  if active
    then error "atomically: nested transaction"
    else catch (transaction (stmToIO action)) blocked
  where
    blocked Retry = do
      changed <-
        IO
          ( \state -> case stmWait# state of
              (# next, flag #) -> (# next, case flag of 0# -> False; _ -> True #)
          )
      if changed then atomically action else throwIO BlockedIndefinitelyOnSTM

retry :: STM a
retry = unsafeIOToSTM (throwIO Retry)

orElse :: STM a -> STM a -> STM a
orElse first second =
  unsafeIOToSTM
    $ catch (transaction (stmToIO first)) (\Retry -> stmToIO second)

throwSTM :: (Exception e) => e -> STM a
throwSTM exception = unsafeIOToSTM (throwIO exception)

catchSTM :: (Exception e) => STM a -> (e -> STM a) -> STM a
catchSTM action handler =
  unsafeIOToSTM
    $ catch (transaction (stmToIO action)) select
  where
    select exception = case fromException exception of
      Just Retry -> throwIO exception
      Nothing -> case fromException exception of
        Just selected -> stmToIO (handler selected)
        Nothing -> throwIO exception

newTVar :: a -> STM (TVar a)
newTVar value = unsafeIOToSTM (newTVarIO value)

newTVarIO :: a -> IO (TVar a)
newTVarIO value = IO $ \state ->
  case newTVar# value state of
    (# next, variable #) -> (# next, TVar variable #)

readTVar :: TVar a -> STM a
readTVar (TVar variable) = STM (readTVar# variable)

readTVarIO :: TVar a -> IO a
readTVarIO (TVar variable) = IO (readTVarIO# variable)

writeTVar :: TVar a -> a -> STM ()
writeTVar (TVar variable) value = STM $ \state ->
  case writeTVar# variable value state of
    next -> (# next, () #)
