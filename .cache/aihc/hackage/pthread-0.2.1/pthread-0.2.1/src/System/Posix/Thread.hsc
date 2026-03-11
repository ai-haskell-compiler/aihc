-- | Bindings to the POSIX threads library.
--
-- Requires linking with the @-threaded@ RTS.

{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}

-- see comment on the imports of Data.Int and Data.Word
{-# OPTIONS_GHC -Wno-unused-imports #-}

module System.Posix.Thread
  ( -- * Threads
    create
  , create_
  , createWithAttributes
  , createWithAttributes_
  , exit
  , exit_
  , cancel
  , join
  , detach
  , myThreadId
    -- * Attributes
  , Attributes(..)
  , AttributesMonoid(..)
    -- * Thread local storage
  , Key
  , createKey
  , createKey_
  , deleteKey
  , setSpecific
  , getSpecific
  ) where

import Control.Concurrent (isCurrentThreadBound, rtsSupportsBoundThreads)
import Control.Exception (Exception, bracket_, throwIO)
import Control.Monad (forM_, unless, when)
import Data.Monoid (First(..))
import Data.Semigroup (Semigroup(..))
import Foreign.C.Types
import Foreign.C.Error (Errno(..), errnoToIOError)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (FunPtr, Ptr, castPtr, nullFunPtr, nullPtr)
import Foreign.Storable
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack, callStack, getCallStack, prettySrcLoc)

-- These two might be used depending on what the Key representation expands to.
import Data.Int
import Data.Word

#include <pthread.h>

newtype ThreadId = ThreadId #{type pthread_t}
  deriving (Eq, Ord, Show, Storable)

foreign import capi unsafe "pthread.h" pthread_create
  :: Ptr ThreadId -> Ptr AttributesMonoid -> FunPtr (Ptr a -> IO b) -> Ptr a -> IO CInt

foreign import ccall "wrapper" wrap :: (Ptr a -> IO (Ptr b)) -> IO (FunPtr (Ptr a -> IO (Ptr b)))

-- | Create a new thread.
create :: IO (Ptr a) -> IO ThreadId
create action =
    alloca $ \tidPtr -> do
      unless rtsSupportsBoundThreads (error "Use -threaded RTS.")
      fptr <- wrap $ \_ -> action
      throwIfNonZero_ $ pthread_create tidPtr nullPtr fptr nullPtr
      peek tidPtr

-- | Like 'create', but with an 'IO' computation that returns nothing.
create_ :: IO () -> IO ThreadId
create_ action = create (action >> return nullPtr)

foreign import capi unsafe "pthread.h" pthread_attr_init
  :: Ptr AttributesMonoid -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_destroy
  :: Ptr AttributesMonoid -> IO CInt

-- | Create a new thread.
createWithAttributes
  :: AttributesMonoid
  -> IO (Ptr a) -- ^ Created thread runs this IO computation.
  -> IO ThreadId
createWithAttributes attrs action =
    alloca $ \tidPtr ->
    alloca $ \attrsPtr -> do
      unless rtsSupportsBoundThreads (error "Use -threaded RTS.")
      bracket_
        (throwIfNonZero_ $ pthread_attr_init attrsPtr)
        (throwIfNonZero_ $ pthread_attr_destroy attrsPtr) $ do
          poke attrsPtr attrs
          fptr <- wrap $ \_ -> action
          throwIfNonZero_ $ pthread_create tidPtr attrsPtr fptr nullPtr
          peek tidPtr

-- | Like 'createWithAttributes', but with an 'IO' computation that returns
-- nothing.
createWithAttributes_ :: AttributesMonoid -> IO () -> IO ThreadId
createWithAttributes_ attrs action =
    createWithAttributes attrs (action >> return nullPtr)

foreign import capi safe "pthread.h" pthread_exit :: Ptr a -> IO ()

-- | Terminate calling thread.
exit :: Ptr a -> IO ()
exit = pthread_exit

-- | Like 'exit', but don't return anything.
exit_ :: IO ()
exit_ = exit nullPtr

foreign import capi safe "pthread.h" pthread_cancel :: ThreadId -> IO CInt

-- | Send a cancellation request to a thread.
cancel :: ThreadId -> IO ()
cancel tid = throwIfNonZero_ $ pthread_cancel tid

foreign import capi safe "pthread.h" pthread_join
  :: ThreadId -> Ptr (Ptr a) -> IO CInt

-- | Join with a terminated thread.
join :: ThreadId -> IO (Ptr a)
join tid = alloca $ \ptr -> do
    throwIfNonZero_ $ pthread_join tid ptr
    peek ptr

foreign import capi unsafe "pthread.h" pthread_detach :: ThreadId -> IO CInt

-- | Detach a thread.
detach :: ThreadId -> IO ()
detach tid = throwIfNonZero_ $ pthread_detach tid

foreign import capi unsafe "pthread.h" pthread_self :: IO ThreadId

-- | Obtain ID of the calling thread.
myThreadId :: IO ThreadId
myThreadId = pthread_self

data DetachState = Detached | Joinable
  deriving (Eq, Show)

instance Enum DetachState where
  toEnum #{const PTHREAD_CREATE_DETACHED} = Detached
  toEnum #{const PTHREAD_CREATE_JOINABLE} = Joinable
  toEnum _ = error "Invalid detach state attribute value"

  fromEnum Detached = #{const PTHREAD_CREATE_DETACHED}
  fromEnum Joinable = #{const PTHREAD_CREATE_JOINABLE}

data InheritSched = InheritSched | ExplicitSched
  deriving (Eq, Show)

instance Enum InheritSched where
  toEnum #{const PTHREAD_INHERIT_SCHED} = InheritSched
  toEnum #{const PTHREAD_EXPLICIT_SCHED} = ExplicitSched
  toEnum _ = error "Invalid inherit-scheduler attribute value"

  fromEnum InheritSched = #{const PTHREAD_INHERIT_SCHED}
  fromEnum ExplicitSched = #{const PTHREAD_EXPLICIT_SCHED}

data SchedParam = SchedParam
  { schedPriority :: Int32
  } deriving (Eq, Show)

instance Storable SchedParam where
  sizeOf _ = #{size struct sched_param}
  alignment _ = #{alignment struct sched_param}
  peek ptr =
    SchedParam <$>
      (fromIntegral <$> (#{peek struct sched_param, sched_priority} ptr :: IO CInt))
  poke ptr SchedParam{..} =
    #{poke struct sched_param, sched_priority} ptr (fromIntegral schedPriority :: CInt)

data SchedPolicy = SchedFIFO | SchedRR | SchedOther
  deriving (Eq, Show)

instance Enum SchedPolicy where
  toEnum #{const SCHED_FIFO} = SchedFIFO
  toEnum #{const SCHED_RR} = SchedRR
  toEnum #{const SCHED_OTHER} = SchedOther
  toEnum _ = error "Invalid scheduling policy attribute value"

  fromEnum SchedFIFO = #{const SCHED_FIFO}
  fromEnum SchedRR = #{const SCHED_RR}
  fromEnum SchedOther = #{const SCHED_OTHER}

data Scope = ScopeSystem | ScopeProcess
  deriving (Eq, Show)

instance Enum Scope where
  toEnum #{const PTHREAD_SCOPE_SYSTEM} = ScopeSystem
  toEnum #{const PTHREAD_SCOPE_PROCESS} = ScopeProcess
  toEnum _ = error "Invalid scope attribute value"

  fromEnum ScopeSystem = #{const PTHREAD_SCOPE_SYSTEM}
  fromEnum ScopeProcess = #{const PTHREAD_SCOPE_PROCESS}

-- | Thread attributes.
data Attributes = Attributes
  { detachState :: DetachState
  , guardSize :: CSize
  , inheritSched :: InheritSched
  , schedParam :: SchedParam
  , schedPolicy :: SchedPolicy
  , scope :: Scope
  , stack :: Ptr ()
  , stackSize :: CSize
  } deriving (Generic, Show)

-- | Partial set of thread attributes. Think of it as a diff to apply to the
-- default attributes object.
data AttributesMonoid = AttributesMonoid
  { detachState :: First DetachState
  , guardSize :: First CSize
  , inheritSched :: First InheritSched
  , schedParam :: First SchedParam
  , schedPolicy :: First SchedPolicy
  , scope :: First Scope
  , stack :: First (Ptr ())
  , stackSize :: First CSize
  } deriving (Generic, Show)

instance Semigroup AttributesMonoid where
  (<>) = mappenddefault

instance Monoid AttributesMonoid where
  mempty = memptydefault
  mappend = (<>)

monoidFromAttributes :: Attributes -> AttributesMonoid
monoidFromAttributes Attributes{..} =
    AttributesMonoid
      { detachState = return detachState
      , guardSize = return guardSize
      , inheritSched = return inheritSched
      , schedParam = return schedParam
      , schedPolicy = return schedPolicy
      , scope = return scope
      , stack = return stack
      , stackSize = return stackSize
      }

foreign import capi unsafe "pthread.h" pthread_attr_getdetachstate
  :: Ptr Attributes -> Ptr CInt -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_getguardsize
  :: Ptr Attributes -> Ptr CSize -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_getinheritsched
  :: Ptr Attributes -> Ptr CInt -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_getschedparam
  :: Ptr Attributes -> Ptr SchedParam -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_getschedpolicy
  :: Ptr Attributes -> Ptr CInt -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_getscope
  :: Ptr Attributes -> Ptr CInt -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_getstack
  :: Ptr Attributes -> Ptr (Ptr ()) -> Ptr CSize -> IO CInt

foreign import capi unsafe "pthread.h" pthread_attr_setdetachstate
  :: Ptr AttributesMonoid -> CInt -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_setguardsize
  :: Ptr AttributesMonoid -> CSize -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_setinheritsched
  :: Ptr AttributesMonoid -> CInt -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_setschedparam
  :: Ptr AttributesMonoid -> Ptr SchedParam -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_setschedpolicy
  :: Ptr AttributesMonoid -> CInt -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_setscope
  :: Ptr AttributesMonoid -> CInt -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_setstack
  :: Ptr AttributesMonoid -> Ptr () -> CSize -> IO CInt
foreign import capi unsafe "pthread.h" pthread_attr_setstacksize
  :: Ptr AttributesMonoid -> CSize -> IO CInt

instance Storable AttributesMonoid where
  sizeOf _ = #{size pthread_attr_t}
  alignment _ = #{alignment pthread_attr_t}

  peek attr = monoidFromAttributes <$> peek (castPtr attr)

  poke attr AttributesMonoid{..} = do
      forM_ detachState $ \x ->
        throwIfNonZero_ $ pthread_attr_setdetachstate attr (enum x)
      forM_ guardSize $ \x ->
        throwIfNonZero_ $ pthread_attr_setguardsize attr x
      forM_ inheritSched $ \x ->
        throwIfNonZero_ $ pthread_attr_setinheritsched attr (enum x)
      forM_ schedParam $ \x ->
        throwIfNonZero_ $ alloca $ \sp -> do
          poke sp x
          pthread_attr_setschedparam attr sp
      forM_ schedPolicy $ \x ->
        throwIfNonZero_ $ pthread_attr_setschedpolicy attr (enum x)
      forM_ scope $ \x ->
        throwIfNonZero_ $ pthread_attr_setscope attr (enum x)
      case (stack, stackSize) of
        (First (Just x), First (Just y)) ->
          throwIfNonZero_ $ pthread_attr_setstack attr x y
        (First Nothing, First (Just y)) ->
          throwIfNonZero_ $ pthread_attr_setstacksize attr y
        _ -> return ()
    where
      enum :: Enum a => a -> CInt
      enum = fromIntegral . fromEnum

instance Storable Attributes where
  sizeOf _ = #{size pthread_attr_t}
  alignment _ = #{alignment pthread_attr_t}

  peek attr = do
      detachState <- enum <$> alloca (\x -> pthread_attr_getdetachstate attr x >> peek x)
      guardSize <- alloca (\x -> pthread_attr_getguardsize attr x >> peek x)
      inheritSched <- enum <$> alloca (\x -> pthread_attr_getinheritsched attr x >> peek x)
      schedParam <- alloca (\x -> pthread_attr_getschedparam attr x >> peek x)
      schedPolicy <- enum <$> alloca (\x -> pthread_attr_getschedpolicy attr x >> peek x)
      scope <- enum <$> alloca (\x -> pthread_attr_getscope attr x >> peek x)
      (stack, stackSize) <-
        alloca $ \x -> alloca $ \y -> do
          _ <- pthread_attr_getstack attr x y
          (,) <$> peek x <*> peek y
      return Attributes{..}
    where
      enum :: Enum a => CInt -> a
      enum = toEnum . fromIntegral

  poke attr Attributes{..} = poke (castPtr attr) (monoidFromAttributes Attributes{..})

-- | Opaque objects used to locate thread-specific data.
newtype Key = Key #{type pthread_key_t}
  deriving (Eq, Ord, Show, Storable)
-- We check in cbits/checks.c that the size of pthread_key_t fits unsigned int.

foreign import capi unsafe "pthread.h"
   pthread_key_create :: Ptr Key -> FunPtr (Ptr a -> IO ()) -> IO CInt

-- | Thread-specific data key creation.
createKey
  :: FunPtr (Ptr a -> IO ()) -- ^ Finalizer
  -> IO Key
createKey destructor = alloca $ \keyPtr -> do
    throwIfNonZero_ $ pthread_key_create keyPtr destructor
    peek keyPtr

-- | Like 'createKey', but with no finalizer.
createKey_ :: IO Key
createKey_ = createKey nullFunPtr

foreign import capi unsafe "pthread.h"
   pthread_key_delete :: Key -> IO CInt

-- | Thread-specific data key deletion.
deleteKey :: Key -> IO ()
deleteKey k = throwIfNonZero_ $ pthread_key_delete k

foreign import capi unsafe "pthread.h"
   pthread_setspecific :: Key -> Ptr a -> IO CInt

-- | Associate a thread-specific /value/ with a /key/ obtained via a previous
-- call to 'createKey'.
setSpecific :: Key -> Ptr a -> IO ()
setSpecific k v = do
    checkBoundness
    throwIfNonZero_ $ pthread_setspecific k v

foreign import capi unsafe "pthread.h"
   pthread_getspecific :: Key -> IO (Ptr a)

-- | Return the value currently bound to the specified key on behalf of the
-- calling thread.
getSpecific :: Key -> IO (Ptr a)
getSpecific k = do
    checkBoundness
    pthread_getspecific k

data ThreadNotBound = ThreadNotBound

instance Exception ThreadNotBound

instance Show ThreadNotBound where
  show _ = "Calling thread is not bound"

-- | Yields an error if the calling thread is not bound.
checkBoundness :: IO ()
checkBoundness = when rtsSupportsBoundThreads $ do
    bound <- isCurrentThreadBound
    unless bound $ throwIO ThreadNotBound

-- | Yields an error if the passed integer is not zero.
throwIfNonZero_ :: HasCallStack => IO CInt -> IO ()
throwIfNonZero_ m = m >>= \rc -> when (rc /= 0) $
    ioError (errnoToIOError (prettySrcLoc loc) (Errno rc) Nothing Nothing)
  where
    (_, loc):_ = getCallStack callStack
