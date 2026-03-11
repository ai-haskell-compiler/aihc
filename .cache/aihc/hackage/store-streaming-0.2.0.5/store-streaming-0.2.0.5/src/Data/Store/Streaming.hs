{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-|
Module: Data.Store.Streaming
Description: A thin streaming layer that uses 'Store' for serialisation.

For efficiency reasons, 'Store' does not provide facilities for
incrementally consuming input.  In order to avoid partial input, this
module introduces 'Message's that wrap values of instances of 'Store'.

In addition to the serialisation of a value, the serialised message
also contains the length of the serialisation.  This way, instead of
consuming input incrementally, more input can be demanded before
serialisation is attempted in the first place.

Each message starts with a fixed magic number, in order to detect
(randomly) invalid data.

-}
module Data.Store.Streaming
       ( -- * 'Message's to stream data using 'Store' for serialisation.
         Message (..)
         -- * Encoding 'Message's
       , encodeMessage
         -- * Decoding 'Message's
       , PeekMessage
       , FillByteBuffer
       , peekMessage
       , decodeMessage
       , peekMessageBS
       , decodeMessageBS
#ifndef mingw32_HOST_OS
       , ReadMoreData(..)
       , peekMessageFd
       , decodeMessageFd
#endif
         -- * Conduits for encoding and decoding
       , conduitEncode
       , conduitDecode
       ) where

import           Control.Exception (throwIO)
import           Control.Monad (unless)
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.ByteString (ByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as C
import           Data.Store
import           Data.Store.Core (decodeIOWithFromPtr, unsafeEncodeWith)
import           Data.Store.Internal (getSize)
import qualified Data.Text as T
import           Data.Word
import           Foreign.Ptr
import           Prelude
import           System.IO.ByteBuffer (ByteBuffer)
import qualified System.IO.ByteBuffer as BB
import           Control.Monad.Trans.Free.Church (FT, iterTM, wrap)
import           Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import           Control.Monad.Trans.Class (lift)
import           System.Posix.Types (Fd(..))
import           GHC.Conc (threadWaitRead)
import           Data.Store.Streaming.Internal

-- | If @a@ is an instance of 'Store', @Message a@ can be serialised
-- and deserialised in a streaming fashion.
newtype Message a = Message { fromMessage :: a } deriving (Eq, Show)

-- | Encode a 'Message' to a 'ByteString'.
encodeMessage :: Store a => Message a -> ByteString
encodeMessage (Message x) =
    unsafeEncodeWith pokeFunc totalLength
  where
    bodyLength = getSize x
    totalLength = headerLength + bodyLength
    pokeFunc = do
        poke messageMagic
        poke bodyLength
        poke x

-- | The result of peeking at the next message can either be a
-- successfully deserialised object, or a request for more input.
type PeekMessage i m a = FT ((->) i) m a

needMoreInput :: PeekMessage i m i
needMoreInput = wrap return

-- | Given some sort of input, fills the 'ByteBuffer' with it.
--
-- The 'Int' is how many bytes we'd like: this is useful when the filling
-- function is 'fillFromFd', where we can specify a max size.
type FillByteBuffer i m = ByteBuffer -> Int -> i -> m ()

-- | Decode a value, given a 'Ptr' and the number of bytes that make
-- up the encoded message.
decodeFromPtr :: (MonadIO m, Store a) => Ptr Word8 -> Int -> m a
decodeFromPtr ptr n = liftIO $ decodeIOWithFromPtr peek ptr n

peekSized :: (MonadIO m, Store a) => FillByteBuffer i m -> ByteBuffer -> Int -> PeekMessage i m a
peekSized fill bb n = go
  where
    go = do
      mbPtr <- BB.unsafeConsume bb n
      case mbPtr of
        Left needed -> do
          inp <- needMoreInput
          lift (fill bb needed inp)
          go
        Right ptr -> decodeFromPtr ptr n

-- | Read and check the magic number from a 'ByteBuffer'
peekMessageMagic :: MonadIO m => FillByteBuffer i m -> ByteBuffer -> PeekMessage i m ()
peekMessageMagic fill bb =
    peekSized fill bb magicLength >>= \case
      mm | mm == messageMagic -> return ()
      mm -> liftIO . throwIO $ PeekException 0 . T.pack $
          "Wrong message magic, " ++ show mm

-- | Decode a 'SizeTag' from a 'ByteBuffer'.
peekMessageSizeTag :: MonadIO m => FillByteBuffer i m -> ByteBuffer -> PeekMessage i m SizeTag
peekMessageSizeTag fill bb = peekSized fill bb sizeTagLength

-- | Decode some object from a 'ByteBuffer', by first reading its
-- header, and then the actual data.
peekMessage :: (MonadIO m, Store a) => FillByteBuffer i m -> ByteBuffer -> PeekMessage i m (Message a)
peekMessage fill bb =
  fmap Message $ do
    peekMessageMagic fill bb
    peekMessageSizeTag fill bb >>= peekSized fill bb

-- | Decode a 'Message' from a 'ByteBuffer' and an action that can get
-- additional inputs to refill the buffer when necessary.
--
-- The only conditions under which this function will give 'Nothing',
-- is when the 'ByteBuffer' contains zero bytes, and refilling yields
-- 'Nothing'.  If there is some data available, but not enough to
-- decode the whole 'Message', a 'PeekException' will be thrown.
decodeMessage :: (Store a, MonadIO m) => FillByteBuffer i m -> ByteBuffer -> m (Maybe i) -> m (Maybe (Message a))
decodeMessage fill bb getInp =
  maybeDecode (peekMessageMagic fill bb) >>= \case
    Just () -> maybeDecode (peekMessageSizeTag fill bb >>= peekSized fill bb) >>= \case
      Just x -> return (Just (Message x))
      Nothing -> do
        -- We have already read the message magic, so a failure to
        -- read the whole message means we have an incomplete message.
        available <- BB.availableBytes bb
        liftIO $ throwIO $ PeekException available $ T.pack
          "Data.Store.Streaming.decodeMessage: could not get enough bytes to decode message"
    Nothing -> do
      available <- BB.availableBytes bb
      -- At this point, we have not consumed anything yet, so if bb is
      -- empty, there simply was no message to read.
      unless (available == 0) $ liftIO $ throwIO $ PeekException available $ T.pack
        "Data.Store.Streaming.decodeMessage: could not get enough bytes to decode message"
      return Nothing
  where
    maybeDecode m = runMaybeT (iterTM (\consumeInp -> consumeInp =<< MaybeT getInp) m)

-- | Decode some 'Message' from a 'ByteBuffer', by first reading its
-- header, and then the actual 'Message'.
peekMessageBS :: (MonadIO m, Store a) => ByteBuffer -> PeekMessage ByteString m (Message a)
peekMessageBS = peekMessage (\bb _ bs -> BB.copyByteString bb bs)

decodeMessageBS :: (MonadIO m, Store a)
            => ByteBuffer -> m (Maybe ByteString) -> m (Maybe (Message a))
decodeMessageBS = decodeMessage (\bb _ bs -> BB.copyByteString bb bs)

#ifndef mingw32_HOST_OS

-- | We use this type as a more descriptive unit to signal that more input
-- should be read from the Fd.
--
-- This data-type is only available on POSIX systems (essentially, non-windows)
data ReadMoreData = ReadMoreData
  deriving (Eq, Show)

-- | Peeks a message from a _non blocking_ 'Fd'.
--
-- This function is only available on POSIX systems (essentially, non-windows)
peekMessageFd :: (MonadIO m, MonadFail m, Store a) => ByteBuffer -> Fd -> PeekMessage ReadMoreData m (Message a)
peekMessageFd bb fd =
  peekMessage (\bb_ needed ReadMoreData -> do _ <- BB.fillFromFd bb_ fd needed; return ()) bb

-- | Decodes all the message using 'registerFd' to find out when a 'Socket' is
-- ready for reading.
--
-- This function is only available on POSIX systems (essentially, non-windows)
decodeMessageFd :: (MonadIO m, MonadFail m, Store a) => ByteBuffer -> Fd -> m (Message a)
decodeMessageFd bb fd = do
  mbMsg <- decodeMessage
    (\bb_ needed ReadMoreData -> do _ <- BB.fillFromFd bb_ fd needed; return ()) bb
    (liftIO (threadWaitRead fd) >> return (Just ReadMoreData))
  case mbMsg of
    Just msg -> return msg
    Nothing -> liftIO (fail "decodeMessageFd: impossible: got Nothing")

#endif

-- | Conduit for encoding 'Message's to 'ByteString's.
conduitEncode
  :: (Monad m, Store a)
  => C.Conduit (Message a) m ByteString
  -- ^ NOTE: ignore the conduit deprecation warning. Otherwise
  -- incompatible with old conduit versions
conduitEncode = C.map encodeMessage

-- | Conduit for decoding 'Message's from 'ByteString's.
conduitDecode :: (MonadResource m, Store a)
              => Maybe Int
              -- ^ Initial length of the 'ByteBuffer' used for
              -- buffering the incoming 'ByteString's.  If 'Nothing',
              -- use the default value of 4MB.
              -> C.Conduit ByteString m (Message a)
              -- ^ NOTE: ignore the conduit deprecation
              -- warning. Otherwise incompatible with old conduit
              -- versions.
conduitDecode bufSize =
    C.bracketP
      (BB.new bufSize)
      BB.free
      go
  where
    go buffer = do
        mmessage <- decodeMessageBS buffer C.await
        case mmessage of
            Nothing -> return ()
            Just message -> C.yield message >> go buffer
