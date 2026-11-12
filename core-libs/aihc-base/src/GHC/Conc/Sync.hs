{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.Conc.Sync
  ( ThreadId (..),
    forkIO,
    fromThreadId,
    killThread,
    labelThread,
    myThreadId,
    showThreadId,
    threadLabel,
    throwTo,
    yield,
    ThreadStatus (..),
    BlockReason (..),
    threadStatus,
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
    getNumCapabilities,
    getNumProcessors,
    numCapabilities,
    par,
    pseq,
    withMVar,
    modifyMVar_,
  )
where

import Aihc.Prim.IO (stmWait#)
import Control.Applicative (Alternative (..))
import Control.Concurrent.MVar (modifyMVar_, withMVar)
import Control.Monad (MonadPlus (..), ap, liftM2)
import GHC.Exception (ErrorCall (..), Exception (..), SomeException)
import GHC.IO (IO (..), catch, mask, throwIO)
import GHC.IO.Exception (AsyncException (ThreadKilled), BlockedIndefinitelyOnMVar (..), BlockedIndefinitelyOnSTM (..))
import GHC.IO.Handle.Text (hPutStrLn)
import GHC.IO.StdHandles (stderr)
import GHC.IO.Unsafe (unsafePerformIO)
import GHC.Prim
import GHC.Types (Int (I#))
import GHC.Word (Word64 (..))
import Prelude

infixr 0 `par`, `pseq`

-- | Return the second argument without a parallel spark.
par :: a -> b -> b
par _ result = result

-- | Evaluate the first argument before the result.
pseq :: a -> b -> b
pseq = seq

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

-- | Schedule an action on a new green thread. As in GHC, an exception that
-- the action does not catch ends only the new thread, and the handler
-- writes it to the standard error.
forkIO :: IO () -> IO ThreadId
forkIO action = forkIOWithoutHandler (action `catch` childHandler)

-- | Report an exception that ends a forked thread. As in GHC, the handler
-- writes nothing for a thread that was killed or blocked forever.
childHandler :: SomeException -> IO ()
childHandler exception
  | Just ThreadKilled <- fromException exception = return ()
  | Just BlockedIndefinitelyOnMVar <- fromException exception = return ()
  | Just BlockedIndefinitelyOnSTM <- fromException exception = return ()
  | otherwise = catch (hPutStrLn stderr (displayException exception)) ignoreException
  where
    ignoreException :: SomeException -> IO ()
    ignoreException _ = return ()

forkIOWithoutHandler :: IO () -> IO ThreadId
forkIOWithoutHandler (IO action) =
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

-- | Raise an exception in a thread. As in GHC, the call returns when the
-- target takes the exception. A target that is masked takes it when it
-- unmasks, or when it waits while it is masked interruptibly.
throwTo :: (Exception e) => ThreadId -> e -> IO ()
throwTo (ThreadId target) exception =
  IO (\state -> case killThread# target (toException exception) state of next -> (# next, () #))

-- | Give a label to a green thread.
--
-- GHC keeps the label in its runtime for debug tools and 'threadLabel'.
-- The aihc runtime has no storage for thread labels. Thus this operation
-- does nothing, and 'threadLabel' does not find the label.
labelThread :: ThreadId -> String -> IO ()
labelThread _ _ = return ()

-- | Read the label of a green thread.
--
-- The aihc runtime has no storage for thread labels, and 'labelThread'
-- does nothing. Thus each thread has no label, and the result is always
-- 'Nothing'. GHC gives the same result for a thread without a label.
threadLabel :: ThreadId -> IO (Maybe String)
threadLabel _ = return Nothing

-- | The reason that a thread cannot run.
data BlockReason
  = -- | The thread waits on an MVar.
    BlockedOnMVar
  | -- | The thread waits for a different thread to evaluate a thunk.
    BlockedOnBlackHole
  | -- | The thread waits until a different thread takes its exception.
    BlockedOnException
  | -- | The thread waits for a change of a transaction variable.
    BlockedOnSTM
  | -- | The thread waits for a foreign call.
    BlockedOnForeignCall
  | -- | The thread waits for a different event, for example IO or a timer.
    BlockedOnOther
  deriving (Eq, Ord, Show)

-- | The state of a thread.
data ThreadStatus
  = -- | The thread runs or can run.
    ThreadRunning
  | -- | The thread ended.
    ThreadFinished
  | -- | The thread waits.
    ThreadBlocked BlockReason
  | -- | The thread ended with an exception that it did not catch.
    ThreadDied
  deriving (Eq, Ord, Show)

-- | Read the state of a thread. The result is a snapshot, because the
-- thread can change its state at the same time.
--
-- 'forkIO' catches each exception of the new thread, thus the thread ends
-- with 'ThreadFinished', as in GHC. The aihc runtime does not record a
-- different end, thus the result is never 'ThreadDied'.
-- On one capability without workers, a transaction that calls 'retry' and
-- has no timer does not wait. Its thread runs the transaction again after
-- the other threads, thus its status is 'ThreadRunning' and not
-- 'ThreadBlocked BlockedOnSTM'.
threadStatus :: ThreadId -> IO ThreadStatus
threadStatus (ThreadId threadId) =
  IO
    ( \state ->
        case aihcThreadStatus# threadId state of
          (# nextState, code #) -> (# nextState, decodeThreadStatus (I# code) #)
    )

-- | Decode the AIHC_BLOCKED_ values of the runtime.
decodeThreadStatus :: Int -> ThreadStatus
decodeThreadStatus code =
  case code of
    0 -> ThreadRunning
    1 -> ThreadBlocked BlockedOnMVar
    2 -> ThreadBlocked BlockedOnBlackHole
    4 -> ThreadBlocked BlockedOnSTM
    5 -> ThreadBlocked BlockedOnException
    6 -> ThreadFinished
    _ -> ThreadBlocked BlockedOnOther

-- | Read the state of a thread from the runtime.
--
-- GHC has the primitive threadStatus#, which gives three values. The aihc
-- runtime gives only one value, thus aihc uses a different primitive.
foreign import prim aihcThreadStatus# :: ThreadId# -> State# RealWorld -> (# State# RealWorld, Int# #)

-- | Raise 'ThreadKilled' in a thread.
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

-- | A transaction whose reads no longer hold runs again. Conflict has a
-- private exception type too, and catchSTM must let it pass.
data Conflict = Conflict

instance Show Conflict where
  show Conflict = "STM conflict"

instance Exception Conflict

unsafeIOToSTM :: IO a -> STM a
unsafeIOToSTM (IO action) = STM action

stmToIO :: STM a -> IO a
stmToIO (STM action) = IO action

-- | A failed action drops all writes since this savepoint. A failure or a
-- commit whose reads no longer hold raises Conflict.
--
-- The begin, the handler, and the commit run masked, so an asynchronous
-- exception cannot leave a transaction open. The action runs with the
-- masking state of the caller.
transaction :: IO a -> IO a
transaction action = mask $ \restore -> do
  IO (\state -> case stmBegin# state of next -> (# next, () #))
  result <- catch (restore action) abort
  committed <-
    IO
      ( \state -> case stmCommit# state of
          (# next, flag #) -> (# next, case flag of 0# -> False; _ -> True #)
      )
  if committed then return result else throwIO Conflict
  where
    abort :: SomeException -> IO a
    abort exception = do
      valid <-
        IO
          ( \state -> case stmAbort# state of
              (# next, flag #) -> (# next, case flag of 0# -> False; _ -> True #)
          )
      if valid then throwIO exception else throwIO Conflict

atomically :: STM a -> IO a
atomically action = do
  active <-
    IO
      ( \state -> case stmActive# state of
          (# next, flag #) -> (# next, case flag of 0# -> False; _ -> True #)
      )
  if active
    then error "atomically: nested transaction"
    else catch (catch (transaction (stmToIO action)) conflict) blocked
  where
    conflict Conflict = atomically action
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
        Just Conflict -> throwIO exception
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

-- | The number of Haskell threads that can run at the same time. The RTS
-- option @-N@ sets this number. The default is one.
getNumCapabilities :: IO Int
getNumCapabilities = capabilityCount

foreign import ccall unsafe "aihc_capability_count"
  capabilityCount :: IO Int

-- | The number of processors of the machine.
getNumProcessors :: IO Int
getNumProcessors = processorCount

foreign import ccall unsafe "aihc_processor_count"
  processorCount :: IO Int

-- | The number of capabilities when the program starts. See
-- 'getNumCapabilities'.
numCapabilities :: Int
numCapabilities = unsafePerformIO getNumCapabilities
{-# NOINLINE numCapabilities #-}
