-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2007 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@haskell.org
-- Portability :  portable (H98 + FFI)
--
-- IConv wrapper layer
--
-----------------------------------------------------------------------------
module Codec.Text.IConv.Internal (

  -- * The iconv state monad
  IConv,
  run,
  InitStatus(..),
  unsafeInterleave,
  unsafeLiftIO,
  finalise,

  -- * The buisness
  iconv,
  Status(..),

  -- * Buffer management
  -- ** Input buffer
  pushInputBuffer,
  inputBufferSize,
  inputBufferEmpty,
  inputPosition,
  replaceInputBuffer,

  -- ** Output buffer
  newOutputBuffer,
  popOutputBuffer,
  outputBufferBytesAvailable,
  outputBufferFull,

  -- * Debugging
--  consistencyCheck,
  dump,
  trace
  ) where

import Foreign hiding (unsafePerformIO)
import Foreign.C
import qualified Data.ByteString.Internal as S
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import System.IO (hPutStrLn, stderr)
import Control.Exception (assert)
import Control.Applicative
import Control.Monad (ap)

import Prelude hiding (length)


pushInputBuffer :: S.ByteString -> IConv ()
pushInputBuffer (S.PS inBuffer' inOffset' inLength') = do

  -- must not push a new input buffer if the last one is not used up
  inAvail <- gets inLength
  assert (inAvail == 0) $ return ()

  -- now set the available input buffer ptr and length
  modify $ \bufs -> bufs {
    inBuffer = inBuffer',
    inOffset = inOffset',
    inLength = inLength'
  }


inputBufferEmpty :: IConv Bool
inputBufferEmpty = gets ((==0) . inLength)


inputBufferSize :: IConv Int
inputBufferSize = gets inLength


inputPosition :: IConv Int
inputPosition = gets inTotal


replaceInputBuffer :: (S.ByteString -> S.ByteString) -> IConv ()
replaceInputBuffer replace =
  modify $ \bufs ->
    case replace (S.PS (inBuffer bufs) (inOffset bufs) (inLength bufs)) of
      S.PS inBuffer' inOffset' inLength' ->
        bufs {
          inBuffer = inBuffer',
          inOffset = inOffset',
          inLength = inLength'
        }


newOutputBuffer :: Int -> IConv ()
newOutputBuffer size = do

  --must not push a new buffer if there is still data in the old one
  outAvail <- gets outLength
  assert (outAvail == 0) $ return ()
  -- Note that there may still be free space in the output buffer, that's ok,
  -- you might not want to bother completely filling the output buffer say if
  -- there's only a few free bytes left.

  -- now set the available output buffer ptr and length
  outBuffer' <- unsafeLiftIO $ S.mallocByteString size
  modify $ \bufs -> bufs {
      outBuffer = outBuffer',
      outOffset = 0,
      outLength = 0,
      outFree   = size
    }


-- get that part of the output buffer that is currently full
-- (might be 0, use outputBufferBytesAvailable to check)
-- this may leave some space remaining in the buffer
popOutputBuffer :: IConv S.ByteString
popOutputBuffer = do

  bufs <- get

  -- there really should be something to pop, otherwise it's silly
  assert (outLength bufs > 0) $ return ()

  modify $ \buf -> buf {
      outOffset = outOffset bufs + outLength bufs,
      outLength = 0
    }

  return (S.PS (outBuffer bufs) (outOffset bufs) (outLength bufs))


-- this is the number of bytes available in the output buffer
outputBufferBytesAvailable :: IConv Int
outputBufferBytesAvailable = gets outLength


-- you only need to supply a new buffer when there is no more output buffer
-- space remaining
outputBufferFull :: IConv Bool
outputBufferFull = gets ((==0) . outFree)


----------------------------
-- IConv buffer layout
--

data Buffers = Buffers {
    inBuffer  :: {-# UNPACK #-} !(ForeignPtr Word8), -- ^ Current input buffer
    inOffset  :: {-# UNPACK #-} !Int,                -- ^ Current read offset
    inLength  :: {-# UNPACK #-} !Int,                -- ^ Input bytes left
    inTotal   :: {-# UNPACK #-} !Int,                -- ^ Total read offset
    outBuffer :: {-# UNPACK #-} !(ForeignPtr Word8), -- ^ Current output buffer
    outOffset :: {-# UNPACK #-} !Int,                -- ^ Base out offset
    outLength :: {-# UNPACK #-} !Int,                -- ^ Available output bytes
    outFree   :: {-# UNPACK #-} !Int                 -- ^ Free output space
  } deriving Show

nullBuffers :: Buffers
nullBuffers = Buffers S.nullForeignPtr 0 0 0 S.nullForeignPtr 0 0 0

{-
 - For the output buffer we have this setup:
 -
 - +-------------+-------------+----------+
 - |### poped ###|** current **|   free   |
 - +-------------+-------------+----------+
 -  \           / \           / \         /
 -    outOffset     outLength     outFree
 -
 - The output buffer is allocated by us and pointer to by the outBuf ForeignPtr.
 - An initial prefix of the buffer that we have already poped/yielded. This bit
 - is immutable, it's already been handed out to the caller, we cannot touch it.
 - When we yield we increment the outOffset. The next part of the buffer between
 - outBuf + outOffset and outBuf + outOffset + outLength is the current bit that
 - has had output data written into it but we have not yet yielded it to the
 - caller. Finally, we have the free part of the buffer. This is the bit we
 - provide to iconv to be filled. When it is written to, we increase the
 - outLength and decrease the outLeft by the number of bytes written.

 - The input buffer layout is much simpler, it's basically just a bytestring:
 -
 - +------------+------------+
 - |### done ###|  remaining |
 - +------------+------------+
 -  \          / \          /
 -    inOffset     inLength
 -
 - So when we iconv we increase the inOffset and decrease the inLength by the
 - number of bytes read.
 -}


----------------------------
-- IConv monad
--

newtype IConv a = I {
    unI :: ConversionDescriptor
        -> Buffers
        -> IO (Buffers, a)
  }

instance Functor IConv where
  fmap f a = a >>= returnI . f

instance Applicative IConv where
  pure  = returnI
  (<*>) = ap

instance Monad IConv where
  (>>=)  = bindI
--  m >>= f = (m `bindI` \a -> consistencyCheck `thenI` returnI a) `bindI` f
  (>>)   = thenI
  return = returnI

returnI :: a -> IConv a
returnI a = I $ \_ bufs -> return (bufs, a)
{-# INLINE returnI #-}

bindI :: IConv a -> (a -> IConv b) -> IConv b
bindI m f = I $ \cd bufs -> do
  (bufs', a) <- unI m cd bufs
  unI (f a) cd bufs'
{-# INLINE bindI #-}

thenI :: IConv a -> IConv b -> IConv b
thenI m f = I $ \cd bufs -> do
  (bufs', _) <- unI m cd bufs
  unI f cd bufs'
{-# INLINE thenI #-}

data InitStatus = InitOk | UnsupportedConversion | UnexpectedInitError Errno

{-# NOINLINE run #-}
run :: String -> String -> (InitStatus -> IConv a) -> a
run from to m = unsafePerformIO $ do
  ptr <- withCString from $ \fromPtr ->
         withCString to $ \toPtr ->
           c_iconv_open toPtr fromPtr -- note arg reversal

  (cd, status) <- if ptrToIntPtr ptr /= (-1)
                    then do cd <- newForeignPtr c_iconv_close ptr
                            return (cd, InitOk)
                    else do errno <- getErrno
                            cd <- newForeignPtr_ nullPtr
                            if errno == eINVAL
                              then return (cd, UnsupportedConversion)
                              else return (cd, UnexpectedInitError errno)
  (_,a) <- unI (m status) (ConversionDescriptor cd) nullBuffers
  return a

unsafeLiftIO :: IO a -> IConv a
unsafeLiftIO m = I $ \_ bufs -> do
  a <- m
  return (bufs, a)

-- It's unsafe because we discard the values here, so if you mutate anything
-- between running this and forcing the result then you'll get an inconsistent
-- iconv state.
unsafeInterleave :: IConv a -> IConv a
unsafeInterleave m = I $ \cd st -> do
  res <- unsafeInterleaveIO (unI m cd st)
  return (st, snd res)

get :: IConv Buffers
get = I $ \_ buf -> return (buf, buf)

gets :: (Buffers -> a) -> IConv a
gets getter = I $ \_ buf -> return (buf, getter buf)

modify :: (Buffers -> Buffers) -> IConv ()
modify change = I $ \_ buf -> return (change buf, ())

----------------------------
-- Debug stuff
--

trace :: String -> IConv ()
trace = unsafeLiftIO . hPutStrLn stderr


dump :: IConv ()
dump = do
  bufs <- get
  unsafeLiftIO $ hPutStrLn stderr $ show bufs

----------------------------
-- iconv wrapper layer
--

data Status =
         InputEmpty
       | OutputFull
       | IncompleteChar
       | InvalidChar
       | UnexpectedError Errno

iconv :: IConv Status
iconv = I $ \(ConversionDescriptor cdfptr) bufs ->
  assert (outFree bufs > 0) $
  --TODO: optimise all this allocation
  withForeignPtr cdfptr                   $ \cdPtr ->
  withForeignPtr (inBuffer bufs)          $ \inBufPtr ->
  with (inBufPtr `plusPtr` inOffset bufs) $ \inBufPtrPtr ->
  with (fromIntegral (inLength bufs))     $ \inLengthPtr ->
  withForeignPtr (outBuffer bufs)         $ \outBufPtr ->
  let outBufPtr' = outBufPtr `plusPtr` (outOffset bufs + outLength bufs) in
  with outBufPtr'                         $ \outBufPtrPtr ->
  with (fromIntegral (outFree bufs))      $ \outFreePtr -> do

    result <- c_iconv cdPtr inBufPtrPtr inLengthPtr outBufPtrPtr outFreePtr
    inLength' <- fromIntegral `fmap` peek inLengthPtr
    outFree'  <- fromIntegral `fmap` peek outFreePtr
    let inByteCount   = inLength bufs - inLength'
        outByteCount  = outFree bufs  - outFree'
        bufs' = bufs {
            inOffset  = inOffset bufs + inByteCount,
            inLength  = inLength',
            inTotal   = inTotal bufs   + inByteCount,
            outLength = outLength bufs + outByteCount,
            outFree   = outFree'
          }
    if result /= errVal
      then return (bufs', InputEmpty)
      else do errno <- getErrno
              case () of
                _ | errno == e2BIG  -> return (bufs', OutputFull)
                  | errno == eINVAL -> return (bufs', IncompleteChar)
                  | errno == eILSEQ -> return (bufs', InvalidChar)
                  | otherwise       -> return (bufs', UnexpectedError errno)

  where errVal :: CSize
        errVal = (-1)   -- (size_t)(-1)

-- | This never needs to be used as the iconv descriptor will be released
-- automatically when no longer needed, however this can be used to release
-- it early. Only use this when you can guarantee that the iconv will no
-- longer be needed, for example if an error occurs or if the input stream
-- ends.
--
finalise :: IConv ()
finalise = I $ \(ConversionDescriptor cd) bufs -> do
  finalizeForeignPtr cd
  return (bufs, ())


----------------------
-- The foreign imports

newtype ConversionDescriptor = ConversionDescriptor (ForeignPtr ConversionDescriptor) -- iconv_t

foreign import ccall unsafe "hsiconv.h hs_wrap_iconv_open"
  c_iconv_open :: CString  -- to code
               -> CString  -- from code
               -> IO (Ptr ConversionDescriptor)

foreign import ccall unsafe "hsiconv.h hs_wrap_iconv"
  c_iconv :: Ptr ConversionDescriptor
          -> Ptr (Ptr CChar)  -- in buf
          -> Ptr CSize        -- in buf bytes left
          -> Ptr (Ptr CChar)  -- out buf
          -> Ptr CSize        -- out buf bytes left
          -> IO CSize

foreign import ccall unsafe "hsiconv.h &hs_wrap_iconv_close"
  c_iconv_close :: FinalizerPtr ConversionDescriptor
