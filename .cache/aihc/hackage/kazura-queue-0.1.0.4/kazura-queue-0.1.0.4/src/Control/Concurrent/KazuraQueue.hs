{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies  #-}

-- | KazuraQueue is the fast queue implementation inspired by unagi-chan.

module Control.Concurrent.KazuraQueue
    ( Queue
    , newQueue
    , readQueue
    , readQueueWithoutMask
    , tryReadQueue
    , tryReadQueueWithoutMask
    , writeQueue
    , writeQueueWithoutMask
    , lengthQueue
    , lengthQueue'
    ) where

import           Control.Concurrent.WVar (WCached, WTicket, WVar)
import qualified Control.Concurrent.WVar as WVar

import qualified Control.Concurrent      as CC
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception       as E
import qualified Control.Monad           as M
import           Control.Monad.Primitive (RealWorld)

import qualified Data.Atomics         as Atm
import qualified Data.Atomics.Counter as Atm
import           Data.Bits            ((.&.))
import qualified Data.Bits            as Bits
import           Data.IORef           (IORef)
import qualified Data.IORef           as Ref
import qualified Data.Primitive.Array as Arr

--------------------------------
-- constants and its utilities

{-# INLINE bufferLength #-}
bufferLength :: Int
bufferLength = 64

{-# INLINE logBufferLength #-}
logBufferLength :: Int
logBufferLength = 6

{-# INLINE divModBufferLength #-}
divModBufferLength :: Int -> (Int,Int)
divModBufferLength n = d `seq` m `seq` (d,m)
  where
    d = n `Bits.unsafeShiftR` logBufferLength
    m = n .&. (bufferLength - 1)

--------------------------------
-- Queue

-- | Type of a Queue. /a/ is the type of an item in the Queue.
data Queue a = Queue
    { queueWriteStream  :: {-# UNPACK #-} !(IORef (Stream a))
    , queueWriteCounter :: {-# UNPACK #-} !Atm.AtomicCounter
    , queueReadStream   :: {-# UNPACK #-} !(IORef (Stream a))
    , queueReadState    :: {-# UNPACK #-} !(WVar (ReadState a))
    , queueNoneTicket   ::                !(Atm.Ticket (Item a))
    }

data ReadState a = ReadState
    { rsCounter :: {-# UNPACK #-} !Atm.AtomicCounter
    , rsLimit   :: {-# UNPACK #-} !StreamIndex
    }

type Buffer a = Arr.MutableArray RealWorld (Item a)

type BufferSource a = IO (Buffer a)

data Item a =
      Item a
    | None
    | Wait {-# UNPACK #-} !(MVar a)
    | Done

data Stream a = Stream
    { streamBuffer :: {-# UNPACK #-} !(Buffer a)
    , streamNext   :: {-# UNPACK #-} !(IORef (NextStream a))
    , streamOffset :: {-# UNPACK #-} !StreamIndex
    }

data NextStream a =
      NextStream {-# UNPACK #-} !(Stream a)
    | NextSource                !(BufferSource a)

type StreamIndex = Int
type BufferIndex = Int

------------------------------

newBufferSource :: IO (BufferSource a)
newBufferSource = do
    arr <- Arr.newArray bufferLength None
    return (Arr.cloneMutableArray arr 0 bufferLength)

newReadState :: StreamIndex -> IO (WVar (ReadState a))
newReadState strIdx = do
    rcounter <- Atm.newCounter strIdx
    WVar.newWVar ReadState
        { rsCounter = rcounter
        , rsLimit   = strIdx
        }

-- | Create a new empty 'Queue'.
newQueue :: IO (Queue a)
newQueue = do
    bufSrc     <- newBufferSource
    buf        <- bufSrc
    noneTicket <- Atm.readArrayElem buf 0
    next       <- Ref.newIORef $ NextSource bufSrc
    let stream = Stream buf next initialOffset
    wstream    <- Ref.newIORef stream
    wcounter   <- Atm.newCounter initialIndex
    rstream    <- Ref.newIORef stream
    rsvar      <- newReadState initialIndex
    return Queue
        { queueWriteStream  = wstream
        , queueWriteCounter = wcounter
        , queueReadStream   = rstream
        , queueReadState    = rsvar
        , queueNoneTicket   = noneTicket
        }
  where
    -- for test of counter overflow
    initialOffset = maxBound - 3
    initialIndex  = initialOffset - 1

----------------------------------------------------------

{-# INLINE waitItem #-}
waitItem :: Buffer a -> BufferIndex -> IO ()
waitItem buf bufIdx = do
    ticket <- Atm.readArrayElem buf bufIdx
    case Atm.peekTicket ticket of
        None    -> do
            mv <- MVar.newEmptyMVar
            (_ret, next) <- Atm.casArrayElem buf bufIdx ticket $! Wait mv
            case Atm.peekTicket next of
                None     -> error "impossible case waitItem"
                Wait mv' -> M.void $ MVar.readMVar mv'
                _        -> return ()
        Wait mv -> M.void $ MVar.readMVar mv
        _       -> return ()

{-# INLINE writeItem #-}
writeItem :: Buffer a -> BufferIndex -> Atm.Ticket (Item a) -> a -> IO ()
writeItem buf bufIdx ticket a = do
    (suc, next) <- Atm.casArrayElem buf bufIdx ticket (Item a)
    M.unless suc $ case Atm.peekTicket next of
        Wait mv -> do
            Arr.writeArray buf bufIdx $ Item a
            MVar.putMVar mv a
        _ -> error "impossible case writeItem"

----------------------------------------------------------

-- | Read an item from the 'Queue'.
{-# INLINE readQueue #-}
readQueue :: Queue a -> IO a
readQueue = E.mask_ . readQueueWithoutMask

-- | Non-masked version of 'readQueue'.
--   It is not safe for asynchronous exception.
{-# INLINE readQueueWithoutMask #-}
readQueueWithoutMask :: Queue a -> IO a
readQueueWithoutMask queue@(Queue _ _ _ rsvar _) =
    WVar.cacheWVar rsvar >>= readQueueRaw queue

readQueueRaw :: Queue a -> WCached (ReadState a) -> IO a
readQueueRaw queue rswc0 = do
    rstr0 <- Ref.readIORef rstrRef
    strIdx <- Atm.incrCounter 1 rcounter
    if rlimit0 `gteIndex` strIdx
        then readStream rstrRef rstr0 strIdx
        else do
            rswt1 <- extendReadStreamWithLock rstr0 rswc0 True True
            let rswc1 = rswc0 { WVar.cachedTicket = rswt1 }
            readQueueRaw queue rswc1
  where
    rstrRef = queueReadStream queue
    rswt0 = WVar.cachedTicket rswc0
    (ReadState rcounter rlimit0) = WVar.readWTicket rswt0

-- | Try to read an item from the 'Queue'. It never blocks.
--
--   Note: It decreases "length" of 'Queue' even when it returns Nothing.
--     In such case, "length" will be lower than 0.
{-# INLINE tryReadQueue #-}
tryReadQueue :: Queue a -> IO (Maybe a)
tryReadQueue = E.mask_ . tryReadQueueWithoutMask

-- | Non-masked version of 'tryReadQueue'.
--   It is not safe for asynchronous exception.
{-# INLINE tryReadQueueWithoutMask #-}
tryReadQueueWithoutMask :: Queue a -> IO (Maybe a)
tryReadQueueWithoutMask queue@(Queue _ _ _ rsvar _) =
    WVar.cacheWVar rsvar >>= tryReadQueueRaw queue

tryReadQueueRaw :: Queue a -> WCached (ReadState a) -> IO (Maybe a)
tryReadQueueRaw queue rswc0 = do
    rstr0 <- Ref.readIORef rstrRef
    strIdx <- Atm.incrCounter 1 rcounter
    if rlimit0 `gteIndex` strIdx
        then Just <$> readStream rstrRef rstr0 strIdx
        else do
            rswt1 <- extendReadStreamWithLock rstr0 rswc0 False False
            let rswc1 = rswc0 { WVar.cachedTicket = rswt1 }
                (ReadState _ rlimit1) = WVar.readWTicket rswt1
            if rlimit1 /= rlimit0
                then tryReadQueueRaw queue rswc1
                else return Nothing
  where
    rstrRef = queueReadStream queue
    rswt0 = WVar.cachedTicket rswc0
    (ReadState rcounter rlimit0) = WVar.readWTicket rswt0

{-# INLINE readStream #-}
readStream :: IORef (Stream a) -> Stream a -> StreamIndex -> IO a
readStream rstrRef rstr0 strIdx = do
    (bufIdx, rstr1) <- targetStream rstr0 strIdx
    M.when (bufIdx == 0) $ Ref.writeIORef rstrRef rstr1
    let buf = streamBuffer rstr1
    item <- Arr.readArray buf bufIdx
    Arr.writeArray buf bufIdx Done
    case item of
        Item a -> return a
        _      -> error "impossible case readQueue"

extendReadStreamWithLock ::
       Stream a
    -> WCached (ReadState a)
    -> Bool
    -> Bool
    -> IO (WTicket (ReadState a))
extendReadStreamWithLock rstr0 rswc0 waitLock waitWrite = do
    (suc, rswt1) <- WVar.tryTakeWCached rswc0
    let rstate1 = WVar.readWTicket rswt1
    if suc
        then do
            rstate2 <- extendReadStream rstate1 rstr0 waitWrite
                `E.onException` WVar.putWCached rswc0 rstate1
            WVar.putWCached rswc0 rstate2
        else do
            let rswc1 = rswc0 { WVar.cachedTicket = rswt1 }
            if waitLock
                then WVar.readFreshWCached rswc1
                else do
                    rswc2 <- WVar.recacheWCached rswc1
                    return $ WVar.cachedTicket rswc2

{-# INLINE extendReadStream #-}
extendReadStream :: ReadState a -> Stream a -> Bool -> IO (ReadState a)
extendReadStream rstate0 rstr0 waitWrite = do
    (rlimitNext1, rstr1) <- searchStreamReadLimit rstr0 rlimitNext0
    if rlimitNext0 /= rlimitNext1
        then newRState rlimitNext1
        else if waitWrite
            then do
                let (Stream buf1 _ offset1) = rstr1
                    bufIdx1 = rlimitNext1 - offset1
                waitItem buf1 bufIdx1
                (rlimitNext2, _) <- searchStreamReadLimit rstr1 rlimitNext1
                newRState rlimitNext2
            else return rstate0
  where
    rlimit0 = rsLimit rstate0
    rlimitNext0 = rlimit0 + 1
    newRState rlimitNext = do
        rcounter <- Atm.newCounter rlimit0
        return rstate0
            { rsCounter = rcounter
            , rsLimit   = rlimitNext - 1
            }

-- | Write an item to the 'Queue'.
-- The item is evaluated (WHNF) before actual queueing.
writeQueue :: Queue a -> a -> IO ()
writeQueue queue = E.mask_ . writeQueueRaw queue

-- | Non-masked version of 'writeQueue'.
--   It is not safe for asynchronous exception.
{-# INLINE writeQueueRaw #-}
writeQueueWithoutMask :: Queue a -> a -> IO ()
writeQueueWithoutMask = writeQueueRaw

writeQueueRaw :: Queue a -> a -> IO ()
writeQueueRaw (Queue wstrRef wcounter _ _ noneTicket) a = do
    wstr0 <- Ref.readIORef wstrRef
    strIdx <- Atm.incrCounter 1 wcounter
    (bufIdx, wstr1) <- targetStream wstr0 strIdx
    writeItem (streamBuffer wstr1) bufIdx noneTicket a
    M.when (bufIdx == 0) $ Ref.writeIORef wstrRef wstr1

{-# INLINE targetStream #-}
targetStream :: Stream a -> StreamIndex -> IO (BufferIndex, Stream a)
targetStream str0@(Stream _ _ offset) strIdx = do
    let (strNum, bufIdx) = divModBufferLength $ strIdx - offset
    str1 <- getStream strNum bufIdx str0
    return (bufIdx, str1)
  where
    {-# INLINE getStream #-}
    getStream 0 _      strA = return strA
    getStream n bufIdx strA = do
        strB <- waitNextStream strA bufIdx
        getStream (n-1) bufIdx strB

{-# NOINLINE waitNextStream #-}
waitNextStream :: Stream a -> Int -> IO (Stream a)
waitNextStream (Stream _ nextStrRef offset) = go
  where
    {-# INLINE go #-}
    go wait = do
        ticket <- Atm.readForCAS nextStrRef
        case Atm.peekTicket ticket of
            NextStream strNext -> return strNext
            nextSrc@(NextSource bufSrc)
                | wait > 0  -> do
                    CC.yield
                    go (wait - 1)
                | otherwise -> do
                    newBuf <- bufSrc
                    newNext <- Ref.newIORef nextSrc
                    let nextStrCand = NextStream Stream
                            { streamBuffer = newBuf
                            , streamNext   = newNext
                            , streamOffset = offset + bufferLength
                            }
                    (_, next) <- Atm.casIORef nextStrRef ticket nextStrCand
                    case Atm.peekTicket next of
                        NextStream nextStr -> return nextStr
                        NextSource _       -> go 1

-- | Search 'Stream' and return 'StreamIndex' and its 'Stream'
--     of the oldest unavailable Item.
{-# INLINE searchStreamReadLimit #-}
searchStreamReadLimit :: Stream a -> StreamIndex -> IO (StreamIndex, Stream a)
searchStreamReadLimit baseStr strIdx =
    go (strIdx - streamOffset baseStr) baseStr
  where
    {-# INLINE go #-}
    go bufIdx stream@(Stream buf _ offset) = do
        ret <- searchBufferReadLimit buf bufIdx
        case ret of
            Just retBufIdx -> return (offset + retBufIdx, stream)
            Nothing        -> waitNextStream stream 0 >>= go 0

-- | Search 'Buffer' and return 'BufferIndex'
--     of the oldest unavailable Item.
--   If all Item in the Buffer is ready, return Nothing.
{-# INLINE searchBufferReadLimit #-}
searchBufferReadLimit :: Buffer a -> BufferIndex -> IO (Maybe BufferIndex)
searchBufferReadLimit buf = go
  where
    {-# INLINE go #-}
    go bufIdx
        | idxIsOutOfBuf = return Nothing
        | otherwise = do
            item <- Arr.readArray buf bufIdx
            case item of
                None   -> return $ Just bufIdx
                Wait _ -> return $ Just bufIdx
                _      -> go $ bufIdx + 1
      where
        idxIsOutOfBuf = bufIdx >= bufferLength

-- | Get the length of the items in the 'Queue'.
--
--   Caution: It returns the value which is lower than 0
--     when the Queue is empty and some threads are waiting for new value.
lengthQueue :: Queue a -> IO Int
lengthQueue (Queue _ wcounter _ rsvar _) = do
    rs <- WVar.readWVar rsvar
    wcount <- Atm.readCounter wcounter
    rcount <- Atm.readCounter $ rsCounter rs
    return $ wcount - rcount

-- | Non-minus version of 'lengthQueue'.
lengthQueue' :: Queue a -> IO Int
lengthQueue' queue = f <$> lengthQueue queue
  where
    f i | i > 0     = i
        | otherwise = 0

{-# INLINE gteIndex #-}
gteIndex :: Int -> Int -> Bool
gteIndex a b | a - b < 0 = False
             | otherwise = True
