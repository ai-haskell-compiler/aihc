{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}

module Data.Store.Core
    ( -- * Core Types
      Poke(..), PokeException(..), pokeException
    , Peek(..), PeekResult(..), PeekException(..), peekException, tooManyBytes
    , PokeState, pokeStatePtr
    , PeekState, peekStateEndPtr
    , Offset
      -- * Encode ByteString
    , unsafeEncodeWith
      -- * Decode ByteString
    , decodeWith
    , decodeExWith, decodeExPortionWith
    , decodeIOWith, decodeIOPortionWith
    , decodeIOWithFromPtr, decodeIOPortionWithFromPtr
      -- * Storable
    , pokeStorable, peekStorable, peekStorableTy
      -- * ForeignPtr
    , pokeFromForeignPtr, peekToPlainForeignPtr, pokeFromPtr
      -- * ByteArray
    , pokeFromByteArray, peekToByteArray
      -- * Creation of PokeState / PeekState
    , unsafeMakePokeState, unsafeMakePeekState, maybeAlignmentBufferSize
    ) where

import           Control.Applicative
import           Control.Exception (Exception(..), throwIO, try)
import           Control.Monad (when)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Primitive (PrimMonad (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Internal as BS
import           Data.Monoid ((<>))
import           Data.Primitive.ByteArray (ByteArray, MutableByteArray(..), newByteArray, unsafeFreezeByteArray)
import qualified Data.Text as T
import           Data.Typeable
import           Data.Word
import           Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import           Foreign.Ptr
import           Foreign.Storable as Storable
import           GHC.Exts (unsafeCoerce#)
import           GHC.Prim (RealWorld, ByteArray#, copyByteArrayToAddr#, copyAddrToByteArray#)
import           GHC.Ptr (Ptr(..))
import           GHC.Types (IO(..), Int(..))
import           Prelude
import           System.IO.Unsafe (unsafePerformIO)

#if MIN_VERSION_base(4,9,0)
import qualified Control.Monad.Fail as Fail
#endif

#if ALIGNED_MEMORY
import           Foreign.Marshal.Alloc (allocaBytesAligned)
#endif

------------------------------------------------------------------------
-- Helpful Type Synonyms

-- | How far into the given Ptr to look
type Offset = Int

------------------------------------------------------------------------
-- Poke monad

-- | 'Poke' actions are useful for building sequential serializers.
--
-- They are actions which write values to bytes into memory specified by
-- a 'Ptr' base. The 'Applicative' and 'Monad' instances make it easy to
-- write serializations, by keeping track of the 'Offset' of the current
-- byte. They allow you to chain 'Poke' action such that subsequent
-- 'Poke's write into subsequent portions of the output.
newtype Poke a = Poke
    { runPoke :: PokeState -> Offset -> IO (Offset, a)
      -- ^ Run the 'Poke' action, with the 'Ptr' to the buffer where
      -- data is poked, and the current 'Offset'. The result is the new
      -- offset, along with a return value.
      --
      -- May throw a 'PokeException', though this should be avoided when
      -- possible.  They usually indicate a programming error.
    }
    deriving Functor

instance Applicative Poke where
    pure x = Poke $ \_ptr offset -> pure (offset, x)
    {-# INLINE pure #-}
    Poke f <*> Poke g = Poke $ \ptr offset1 -> do
        (offset2, f') <- f ptr offset1
        (offset3, g') <- g ptr offset2
        return (offset3, f' g')
    {-# INLINE (<*>) #-}
    Poke f *> Poke g = Poke $ \ptr offset1 -> do
        (offset2, _) <- f ptr offset1
        g ptr offset2
    {-# INLINE (*>) #-}

instance Monad Poke where
    return = pure
    {-# INLINE return #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}
    Poke x >>= f = Poke $ \ptr offset1 -> do
        (offset2, x') <- x ptr offset1
        runPoke (f x') ptr offset2
    {-# INLINE (>>=) #-}
#if !(MIN_VERSION_base(4,13,0))
    fail = pokeException . T.pack
    {-# INLINE fail #-}
#endif

#if MIN_VERSION_base(4,9,0)
instance Fail.MonadFail Poke where
    fail = pokeException . T.pack
    {-# INLINE fail #-}
#endif

instance MonadIO Poke where
    liftIO f = Poke $ \_ offset -> (offset, ) <$> f
    {-# INLINE liftIO #-}

-- | Holds a 'pokeStatePtr', which is passed in to each 'Poke' action.
-- If the package is built with the 'force-alignment' flag, this also
-- has a hidden 'Ptr' field, which is used as scratch space during
-- unaligned writes.
#if ALIGNED_MEMORY
data PokeState = PokeState
    { pokeStatePtr :: {-# UNPACK #-} !(Ptr Word8)
    , pokeStateAlignPtr :: {-# UNPACK #-} !(Ptr Word8)
    }
#else
newtype PokeState = PokeState
    { pokeStatePtr :: Ptr Word8
    }
#endif

-- | Make a 'PokeState' from a buffer pointer.
--
-- The first argument is a pointer to the memory to write to. The second
-- argument is an IO action which is invoked if the store-core package
-- was built with the @force-alignment@ flag. The action should yield a
-- pointer to scratch memory as large as 'maybeAlignmentBufferSize'.
--
-- Since 0.4.2
unsafeMakePokeState :: Ptr Word8 -- ^ pokeStatePtr
                    -> IO (Ptr Word8) -- ^ action to produce pokeStateAlignPtr
                    -> IO PokeState
#if ALIGNED_MEMORY
unsafeMakePokeState ptr f = PokeState ptr <$> f
#else
unsafeMakePokeState ptr _ = return $ PokeState ptr
#endif

-- | Exception thrown while running 'poke'. Note that other types of
-- exceptions could also be thrown. Invocations of 'fail' in the 'Poke'
-- monad causes this exception to be thrown.
--
-- 'PokeException's are not expected to occur in ordinary circumstances,
-- and usually indicate a programming error.
data PokeException = PokeException
    { pokeExByteIndex :: Offset
    , pokeExMessage :: T.Text
    }
    deriving (Eq, Show, Typeable)

instance Exception PokeException where
#if MIN_VERSION_base(4,8,0)
    displayException (PokeException offset msg) =
        "Exception while poking, at byte index " ++
        show offset ++
        " : " ++
        T.unpack msg
#endif

-- | Throws a 'PokeException'. These should be avoided when possible,
-- they usually indicate a programming error.
pokeException :: T.Text -> Poke a
pokeException msg = Poke $ \_ off -> throwIO (PokeException off msg)

------------------------------------------------------------------------
-- Peek monad

-- | 'Peek' actions are useful for building sequential deserializers.
--
-- They are actions which read from memory and construct values from it.
-- The 'Applicative' and 'Monad' instances make it easy to chain these
-- together to get more complicated deserializers. This machinery keeps
-- track of the current 'Ptr' and end-of-buffer 'Ptr'.
newtype Peek a = Peek
    { runPeek :: PeekState -> Ptr Word8 -> IO (PeekResult a)
      -- ^ Run the 'Peek' action, with a 'Ptr' to the end of the buffer
      -- where data is poked, and a 'Ptr' to the current position. The
      -- result is the 'Ptr', along with a return value.
      --
      -- May throw a 'PeekException' if the memory contains invalid
      -- values.
    } deriving (Functor)

-- | A result of a 'Peek' action containing the current 'Ptr' and a return value.
data PeekResult a = PeekResult {-# UNPACK #-} !(Ptr Word8) !a
    deriving (Functor)

instance Applicative Peek where
    pure x = Peek (\_ ptr -> return $ PeekResult ptr x)
    {-# INLINE pure #-}
    Peek f <*> Peek g = Peek $ \end ptr1 -> do
        PeekResult ptr2 f' <- f end ptr1
        PeekResult ptr3 g' <- g end ptr2
        return $ PeekResult ptr3 (f' g')
    {-# INLINE (<*>) #-}
    Peek f *> Peek g = Peek $ \end ptr1 -> do
        PeekResult ptr2 _ <- f end ptr1
        g end ptr2
    {-# INLINE (*>) #-}

instance Monad Peek where
    return = pure
    {-# INLINE return #-}
    (>>) = (*>)
    {-# INLINE (>>) #-}
    Peek x >>= f = Peek $ \end ptr1 -> do
        PeekResult ptr2 x' <- x end ptr1
        runPeek (f x') end ptr2
    {-# INLINE (>>=) #-}
#if !(MIN_VERSION_base(4,13,0))
    fail = peekException . T.pack
    {-# INLINE fail #-}
#endif

#if MIN_VERSION_base(4,9,0)
instance Fail.MonadFail Peek where
    fail = peekException . T.pack
    {-# INLINE fail #-}
#endif

instance PrimMonad Peek where
    type PrimState Peek = RealWorld
    primitive action = Peek $ \_ ptr -> do
        x <- primitive (unsafeCoerce# action)
        return $ PeekResult ptr x
    {-# INLINE primitive #-}

instance MonadIO Peek where
    liftIO f = Peek $ \_ ptr -> PeekResult ptr <$> f
    {-# INLINE liftIO #-}

-- | Holds a 'peekStatePtr', which is passed in to each 'Peek' action.
-- If the package is built with the 'force-alignment' flag, this also
-- has a hidden 'Ptr' field, which is used as scratch space during
-- unaligned reads.
#if ALIGNED_MEMORY
data PeekState = PeekState
    { peekStateEndPtr :: {-# UNPACK #-} !(Ptr Word8)
    , peekStateAlignPtr :: {-# UNPACK #-} !(Ptr Word8)
    }
#else
newtype PeekState = PeekState
    { peekStateEndPtr :: Ptr Word8 }
#endif

-- | Make a 'PeekState' from a buffer pointer.
--
-- The first argument is a pointer to the memory to write to. The second
-- argument is an IO action which is invoked if the store-core package
-- was built with the @force-alignment@ flag. The action should yield a
-- pointer to scratch memory as large as 'maybeAlignmentBufferSize'.
--
-- Since 0.4.2
unsafeMakePeekState :: Ptr Word8 -- ^ peekStateEndPtr
                    -> IO (Ptr Word8) -- ^ action to produce peekStateAlignPtr
                    -> IO PeekState
#if ALIGNED_MEMORY
unsafeMakePeekState ptr f = PeekState ptr <$> f
#else
unsafeMakePeekState ptr _ = return $ PeekState ptr
#endif

-- | Exception thrown while running 'peek'. Note that other types of
-- exceptions can also be thrown. Invocations of 'fail' in the 'Poke'
-- monad causes this exception to be thrown.
--
-- 'PeekException' is thrown when the data being decoded is invalid.
data PeekException = PeekException
    { peekExBytesFromEnd :: Offset
    , peekExMessage :: T.Text
    } deriving (Eq, Show, Typeable)

instance Exception PeekException where
#if MIN_VERSION_base(4,8,0)
    displayException (PeekException offset msg) =
        "Exception while peeking, " ++
        show offset ++
        " bytes from end: " ++
        T.unpack msg
#endif

-- | Throws a 'PeekException'.
peekException :: T.Text -> Peek a
peekException msg = Peek $ \ps ptr -> throwIO (PeekException (peekStateEndPtr ps `minusPtr` ptr) msg)

-- | Throws a 'PeekException' about an attempt to read too many bytes.
tooManyBytes :: Int -> Int -> String -> IO void
tooManyBytes needed remaining ty =
    throwIO $ PeekException remaining $ T.pack $
        "Attempted to read too many bytes for " ++
        ty ++
        ". Needed " ++
        show needed ++ ", but only " ++
        show remaining ++ " remain."

-- | Throws a 'PeekException' about an attempt to read a negative number of bytes.
--
-- This can happen when we read invalid data -- the length tag is
-- basically random in this case.
negativeBytes :: Int -> Int -> String -> IO void
negativeBytes needed remaining ty =
    throwIO $ PeekException remaining $ T.pack $
        "Attempted to read negative number of bytes for " ++
        ty ++
        ". Tried to read " ++
        show needed ++ ".  This probably means that we're trying to read invalid data."

------------------------------------------------------------------------
-- Decoding and encoding ByteStrings


-- | Given a 'Poke' and its length, uses it to fill a 'ByteString'
--
-- This function is unsafe because the provided length must exactly
-- match the number of bytes used by the 'Poke'. It will throw
-- 'PokeException' errors when the buffer is under or overshot. However,
-- in the case of overshooting the buffer, memory corruption and
-- segfaults may occur.
unsafeEncodeWith :: Poke () -> Int -> ByteString
unsafeEncodeWith f l =
    BS.unsafeCreate l $ \ptr -> do
#if ALIGNED_MEMORY
    allocaBytesAligned alignBufferSize 8 $ \aptr -> do
#endif
        let ps = PokeState
                { pokeStatePtr = ptr
#if ALIGNED_MEMORY
                , pokeStateAlignPtr = aptr
#endif
                }
        (o, ()) <- runPoke f ps 0
        checkOffset o l

#if ALIGNED_MEMORY
alignBufferSize :: Int
alignBufferSize = 32
#endif

-- | If store-core is built with the @force-alignment@ flag, then this
-- will be a 'Just' value indicating the amount of memory that is
-- expected in the alignment buffer used by 'PeekState' and 'PokeState'.
-- Currently this will either be @Just 32@ or @Nothing@.
maybeAlignmentBufferSize :: Maybe Int
maybeAlignmentBufferSize =
#if ALIGNED_MEMORY
  Just alignBufferSize
#else
  Nothing
#endif

-- | Checks if the offset matches the expected length, and throw a
-- 'PokeException' otherwise.
checkOffset :: Int -> Int -> IO ()
checkOffset o l
    | o > l = throwIO $ PokeException o $ T.pack $
        "encode overshot end of " ++
        show l ++
        " byte long buffer"
    | o < l = throwIO $ PokeException o $ T.pack $
        "encode undershot end of " <>
        show l <>
        " byte long buffer"
    | otherwise = return ()

-- | Decodes a value from a 'ByteString', potentially throwing
-- exceptions, and taking a 'Peek' to run. It is an exception to not
-- consume all input.
decodeWith :: Peek a -> ByteString -> Either PeekException a
decodeWith mypeek = unsafePerformIO . try . decodeIOWith mypeek

-- | Decodes a value from a 'ByteString', potentially throwing
-- exceptions, and taking a 'Peek' to run. It is an exception to not
-- consume all input.
decodeExWith :: Peek a -> ByteString -> a
decodeExWith f = unsafePerformIO . decodeIOWith f

-- | Similar to 'decodeExWith', but it allows there to be more of the
-- buffer remaining. The 'Offset' of the buffer contents immediately
-- after the decoded value is returned.
decodeExPortionWith :: Peek a -> ByteString -> (Offset, a)
decodeExPortionWith f = unsafePerformIO . decodeIOPortionWith f

-- | Decodes a value from a 'ByteString', potentially throwing
-- exceptions, and taking a 'Peek' to run. It is an exception to not
-- consume all input.
decodeIOWith :: Peek a -> ByteString -> IO a
decodeIOWith mypeek (BS.PS x s len) =
    withForeignPtr x $ \ptr0 ->
        let ptr = ptr0 `plusPtr` s
        in decodeIOWithFromPtr mypeek ptr len

-- | Similar to 'decodeExPortionWith', but runs in the 'IO' monad.
decodeIOPortionWith :: Peek a -> ByteString -> IO (Offset, a)
decodeIOPortionWith mypeek (BS.PS x s len) =
    withForeignPtr x $ \ptr0 ->
        let ptr = ptr0 `plusPtr` s
        in decodeIOPortionWithFromPtr mypeek ptr len

-- | Like 'decodeIOWith', but using 'Ptr' and length instead of a
-- 'ByteString'.
decodeIOWithFromPtr :: Peek a -> Ptr Word8 -> Int -> IO a
decodeIOWithFromPtr mypeek ptr len = do
    (offset, x) <- decodeIOPortionWithFromPtr mypeek ptr len
    if len /= offset
       then throwIO $ PeekException (len - offset) "Didn't consume all input."
       else return x

-- | Like 'decodeIOPortionWith', but using 'Ptr' and length instead of a 'ByteString'.
decodeIOPortionWithFromPtr :: Peek a -> Ptr Word8 -> Int -> IO (Offset, a)
decodeIOPortionWithFromPtr mypeek ptr len =
    let end = ptr `plusPtr` len
        remaining = end `minusPtr` ptr
    in do PeekResult ptr2 x' <-
#if ALIGNED_MEMORY
              allocaBytesAligned alignBufferSize 8 $ \aptr -> do
                  runPeek mypeek (PeekState end aptr) ptr
#else
              runPeek mypeek (PeekState end) ptr
#endif
          if len > remaining -- Do not perform the check on the new pointer, since it could have overflowed
              then throwIO $ PeekException (end `minusPtr` ptr2) "Overshot end of buffer"
              else return (ptr2 `minusPtr` ptr, x')

------------------------------------------------------------------------
-- Utilities for defining 'Store' instances based on 'Storable'

-- | A 'poke' implementation based on an instance of 'Storable'.
pokeStorable :: Storable a => a -> Poke ()
pokeStorable x = Poke $ \ps offset -> do
    let targetPtr = pokeStatePtr ps `plusPtr` offset
#if ALIGNED_MEMORY
    -- If necessary, poke into the scratch buffer, and copy the results
    -- to the output buffer.
    let bufStart = pokeStateAlignPtr ps
        alignStart = alignPtr (pokeStateAlignPtr ps) (alignment x)
        sz = sizeOf x
    if targetPtr == alignPtr targetPtr (alignment x)
        -- If we luck out and the output is already aligned, just poke it
        -- directly.
        then poke targetPtr x
        else (if (alignStart `plusPtr` sz) < (bufStart `plusPtr` alignBufferSize)
            then do
                poke (castPtr alignStart) x
                BS.memcpy (castPtr targetPtr) alignStart sz
            else do
                allocaBytesAligned sz (alignment x) $ \tempPtr -> do
                    poke tempPtr x
                    BS.memcpy (castPtr targetPtr) (castPtr tempPtr) sz)
#else
    poke targetPtr x
#endif
    let !newOffset = offset + sizeOf x
    return (newOffset, ())
{-# INLINE pokeStorable #-}

-- | A 'peek' implementation based on an instance of 'Storable' and
-- 'Typeable'.
peekStorable :: forall a. (Storable a, Typeable a) => Peek a
peekStorable = peekStorableTy (show (typeRep (Proxy :: Proxy a)))
{-# INLINE peekStorable #-}

-- | A 'peek' implementation based on an instance of 'Storable'. Use
-- this if the type is not 'Typeable'.
peekStorableTy :: forall a. Storable a => String -> Peek a
peekStorableTy ty = Peek $ \ps ptr -> do
    let ptr' = ptr `plusPtr` sz
        sz = sizeOf (undefined :: a)
        remaining = peekStateEndPtr ps `minusPtr` ptr
    when (sz > remaining) $ -- Do not perform the check on the new pointer, since it could have overflowed
        tooManyBytes sz remaining ty
#if ALIGNED_MEMORY
    let bufStart = peekStateAlignPtr ps
        alignStart = alignPtr (peekStateAlignPtr ps) alignAmount
        alignAmount = alignment (undefined :: a)
    x <- if ptr == alignPtr ptr alignAmount
        then Storable.peek (castPtr ptr)
        else (if (alignStart `plusPtr` sz) < (bufStart `plusPtr` alignBufferSize)
            then do
                BS.memcpy (castPtr alignStart) ptr sz
                Storable.peek (castPtr alignStart)
            else do
                allocaBytesAligned sz alignAmount $ \tempPtr -> do
                    BS.memcpy tempPtr (castPtr ptr) sz
                    Storable.peek (castPtr tempPtr))
#else
    x <- Storable.peek (castPtr ptr)
#endif
    return $ PeekResult ptr' x
{-# INLINE peekStorableTy #-}

------------------------------------------------------------------------
-- Utilities for implementing 'Store' instances via memcpy

-- | Copy a section of memory, based on a 'ForeignPtr', to the output.
-- Note that this operation is unsafe, the offset and length parameters
-- are not checked.
pokeFromForeignPtr :: ForeignPtr a -> Int -> Int -> Poke ()
pokeFromForeignPtr sourceFp sourceOffset len =
    Poke $ \targetState targetOffset -> do
        let targetPtr = pokeStatePtr targetState
        withForeignPtr sourceFp $ \sourcePtr ->
            BS.memcpy (targetPtr `plusPtr` targetOffset)
                      (sourcePtr `plusPtr` sourceOffset)
                      len
        let !newOffset = targetOffset + len
        return (newOffset, ())

-- | Allocate a plain ForeignPtr (no finalizers), of the specified
-- length and fill it with bytes from the input.
peekToPlainForeignPtr :: String -> Int -> Peek (ForeignPtr a)
peekToPlainForeignPtr ty len =
    Peek $ \ps sourcePtr -> do
        let ptr2 = sourcePtr `plusPtr` len
            remaining = peekStateEndPtr ps `minusPtr` sourcePtr
        when (len > remaining) $ -- Do not perform the check on the new pointer, since it could have overflowed
            tooManyBytes len remaining ty
        when (len < 0) $
            negativeBytes len remaining ty
        fp <- BS.mallocByteString len
        withForeignPtr fp $ \targetPtr ->
            BS.memcpy targetPtr (castPtr sourcePtr) len
        return $ PeekResult ptr2 (castForeignPtr fp)

-- | Copy a section of memory, based on a 'Ptr', to the output. Note
-- that this operation is unsafe, because the offset and length
-- parameters are not checked.
pokeFromPtr :: Ptr a -> Int -> Int -> Poke ()
pokeFromPtr sourcePtr sourceOffset len =
    Poke $ \targetState targetOffset -> do
        let targetPtr = pokeStatePtr targetState
        BS.memcpy (targetPtr `plusPtr` targetOffset)
                  (sourcePtr `plusPtr` sourceOffset)
                  len
        let !newOffset = targetOffset + len
        return (newOffset, ())

-- | Copy a section of memory, based on a 'ByteArray#', to the output.
-- Note that this operation is unsafe, because the offset and length
-- parameters are not checked.
pokeFromByteArray :: ByteArray# -> Int -> Int -> Poke ()
pokeFromByteArray sourceArr sourceOffset len =
    Poke $ \targetState targetOffset -> do
        let target = (pokeStatePtr targetState) `plusPtr` targetOffset
        copyByteArrayToAddr sourceArr sourceOffset target len
        let !newOffset = targetOffset + len
        return (newOffset, ())

-- | Allocate a ByteArray of the specified length and fill it with bytes
-- from the input.
peekToByteArray :: String -> Int -> Peek ByteArray
peekToByteArray ty len =
    Peek $ \ps sourcePtr -> do
        let ptr2 = sourcePtr `plusPtr` len
            remaining = peekStateEndPtr ps `minusPtr` sourcePtr
        when (len > remaining) $ -- Do not perform the check on the new pointer, since it could have overflowed
            tooManyBytes len remaining ty
        when (len < 0) $
            negativeBytes len remaining ty
        marr <- newByteArray len
        copyAddrToByteArray sourcePtr marr 0 len
        x <- unsafeFreezeByteArray marr
        return $ PeekResult ptr2 x

-- | Wrapper around @copyByteArrayToAddr#@ primop.
copyByteArrayToAddr :: ByteArray# -> Int -> Ptr a -> Int -> IO ()
copyByteArrayToAddr arr (I# offset) (Ptr addr) (I# len) =
    IO (\s -> (# copyByteArrayToAddr# arr offset addr len s, () #))
{-# INLINE copyByteArrayToAddr  #-}

-- | Wrapper around @copyAddrToByteArray#@ primop.
copyAddrToByteArray :: Ptr a -> MutableByteArray (PrimState IO) -> Int -> Int -> IO ()
copyAddrToByteArray (Ptr addr) (MutableByteArray arr) (I# offset) (I# len) =
    IO (\s -> (# copyAddrToByteArray# addr arr offset len s, () #))
{-# INLINE copyAddrToByteArray  #-}
