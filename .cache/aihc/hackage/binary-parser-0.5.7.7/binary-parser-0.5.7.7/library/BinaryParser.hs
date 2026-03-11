{-# LANGUAGE CPP #-}

module BinaryParser
  ( BinaryParser,
    run,
    failure,
    byte,
    matchingByte,
    bytesOfSize,
    bytesWhile,
    unitOfSize,
    unitOfBytes,
    unitWhile,
    remainders,
    fold,
    endOfInput,
    sized,

    -- * Extras
    storableOfSize,
    beWord16,
    leWord16,
    beWord32,
    leWord32,
    beWord64,
    leWord64,
    asciiIntegral,
  )
where

import BinaryParser.Prelude hiding (fold)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Internal as ByteString
import qualified Data.ByteString.Unsafe as ByteString

-- |
-- A highly-efficient parser specialised for strict 'ByteString's.
--
-- Supports the roll-back and alternative branching
-- on the basis of the 'Alternative' interface.
--
-- Does not generate fancy error-messages,
-- which contributes to its efficiency.
newtype BinaryParser a
  = BinaryParser (ByteString -> Either Text (a, ByteString))
  deriving
    (Functor, Applicative, Alternative, Monad, MonadPlus, MonadError Text)
    via (StateT ByteString (Except Text))

type role BinaryParser representational

instance MonadFail BinaryParser where
  fail = failure . fromString

-- |
-- Apply a parser to bytes.
{-# INLINE run #-}
run :: BinaryParser a -> ByteString -> Either Text a
run (BinaryParser parser) input =
  fmap fst $ parser input

-- |
-- Fail with a message.
{-# INLINE failure #-}
failure :: Text -> BinaryParser a
failure text =
  BinaryParser (const (Left text))

-- |
-- Consume a single byte.
{-# INLINE byte #-}
byte :: BinaryParser Word8
byte =
  BinaryParser $ \remainders ->
    if ByteString.null remainders
      then Left "End of input"
      else Right (ByteString.unsafeHead remainders, ByteString.unsafeDrop 1 remainders)

-- |
-- Consume a single byte, which satisfies the predicate.
{-# INLINE matchingByte #-}
matchingByte :: (Word8 -> Either Text a) -> BinaryParser a
matchingByte matcher =
  BinaryParser $ \remainders ->
    case ByteString.uncons remainders of
      Nothing -> Left "End of input"
      Just (head, tail) ->
        case matcher head of
          Right result -> Right (result, tail)
          Left error -> Left error

-- |
-- Consume an amount of bytes.
{-# INLINE bytesOfSize #-}
bytesOfSize :: Int -> BinaryParser ByteString
bytesOfSize size =
  BinaryParser $ \remainders ->
    if ByteString.length remainders >= size
      then Right (ByteString.unsafeTake size remainders, ByteString.unsafeDrop size remainders)
      else Left "End of input"

-- |
-- Consume multiple bytes, which satisfy the predicate.
{-# INLINE bytesWhile #-}
bytesWhile :: (Word8 -> Bool) -> BinaryParser ByteString
bytesWhile predicate =
  BinaryParser $ \remainders ->
    Right (ByteString.span predicate remainders)

-- |
-- Skip an amount of bytes.
{-# INLINE unitOfSize #-}
unitOfSize :: Int -> BinaryParser ()
unitOfSize size =
  BinaryParser $ \remainders ->
    if ByteString.length remainders >= size
      then Right ((), ByteString.unsafeDrop size remainders)
      else Left "End of input"

-- |
-- Skip specific bytes, while failing if they don't match.
{-# INLINE unitOfBytes #-}
unitOfBytes :: ByteString -> BinaryParser ()
unitOfBytes bytes =
  BinaryParser $ \remainders ->
    if ByteString.isPrefixOf bytes remainders
      then Right ((), ByteString.unsafeDrop (ByteString.length bytes) remainders)
      else Left "Bytes don't match"

-- |
-- Skip bytes, which satisfy the predicate.
{-# INLINE unitWhile #-}
unitWhile :: (Word8 -> Bool) -> BinaryParser ()
unitWhile predicate =
  BinaryParser $ \remainders ->
    Right ((), ByteString.dropWhile predicate remainders)

-- |
-- Consume all the remaining bytes.
{-# INLINE remainders #-}
remainders :: BinaryParser ByteString
remainders =
  BinaryParser $ \remainders -> Right (remainders, ByteString.empty)

-- |
-- Fail if the input hasn't ended.
{-# INLINE endOfInput #-}
endOfInput :: BinaryParser ()
endOfInput =
  BinaryParser $ \case
    "" -> Right ((), ByteString.empty)
    _ -> Left "Not the end of input"

-- |
-- Left-fold the bytes, terminating before the byte,
-- on which the step function returns Nothing.
{-# INLINE fold #-}
fold :: (a -> Word8 -> Maybe a) -> a -> BinaryParser a
fold step init =
  BinaryParser $ Right . loop init
  where
    loop !accumulator remainders =
      case ByteString.uncons remainders of
        Nothing -> (accumulator, remainders)
        Just (head, tail) ->
          case step accumulator head of
            Just newAccumulator ->
              loop newAccumulator tail
            Nothing -> (accumulator, remainders)

-- |
-- Run a subparser passing it a chunk of the current input of the specified size.
{-# INLINE sized #-}
sized :: Int -> BinaryParser a -> BinaryParser a
sized size (BinaryParser parser) =
  BinaryParser $ \remainders ->
    if ByteString.length remainders >= size
      then
        parser (ByteString.unsafeTake size remainders)
          & fmap (\result -> (fst result, ByteString.unsafeDrop size remainders))
      else Left "End of input"

-- |
-- Storable value of the given amount of bytes.
{-# INLINE storableOfSize #-}
storableOfSize :: (Storable a) => Int -> BinaryParser a
storableOfSize size =
  BinaryParser $ \(ByteString.PS payloadFP offset length) ->
    if length >= size
      then
        let result =
              unsafeDupablePerformIO $ withForeignPtr payloadFP $ \ptr -> peekByteOff (castPtr ptr) offset
            newRemainder =
              ByteString.PS payloadFP (offset + size) (length - size)
         in Right (result, newRemainder)
      else Left "End of input"

-- | Big-endian word of 2 bytes.
{-# INLINE beWord16 #-}
beWord16 :: BinaryParser Word16
#ifdef WORDS_BIGENDIAN
beWord16 =
  storableOfSize 2
#else
beWord16 =
  byteSwap16 <$> storableOfSize 2
#endif

-- | Little-endian word of 2 bytes.
{-# INLINE leWord16 #-}
leWord16 :: BinaryParser Word16
#ifdef WORDS_BIGENDIAN
leWord16 =
  byteSwap16 <$> storableOfSize 2
#else
leWord16 =
  storableOfSize 2
#endif

-- | Big-endian word of 4 bytes.
{-# INLINE beWord32 #-}
beWord32 :: BinaryParser Word32
#ifdef WORDS_BIGENDIAN
beWord32 =
  storableOfSize 4
#else
beWord32 =
  byteSwap32 <$> storableOfSize 4
#endif

-- | Little-endian word of 4 bytes.
{-# INLINE leWord32 #-}
leWord32 :: BinaryParser Word32
#ifdef WORDS_BIGENDIAN
leWord32 =
  byteSwap32 <$> storableOfSize 4
#else
leWord32 =
  storableOfSize 4
#endif

-- | Big-endian word of 8 bytes.
{-# INLINE beWord64 #-}
beWord64 :: BinaryParser Word64
#ifdef WORDS_BIGENDIAN
beWord64 =
  storableOfSize 8
#else
beWord64 =
  byteSwap64 <$> storableOfSize 8
#endif

-- | Little-endian word of 8 bytes.
{-# INLINE leWord64 #-}
leWord64 :: BinaryParser Word64
#ifdef WORDS_BIGENDIAN
leWord64 =
  byteSwap64 <$> storableOfSize 8
#else
leWord64 =
  storableOfSize 8
#endif

-- |
-- Integral number encoded in ASCII.
{-# INLINE asciiIntegral #-}
asciiIntegral :: (Integral a) => BinaryParser a
asciiIntegral =
  do
    firstDigit <- matchingByte byteDigit
    fold step firstDigit
  where
    byteDigit byte =
      case byte - 48 of
        subtracted ->
          if subtracted <= 9
            then Right (fromIntegral subtracted)
            else Left "Not an ASCII decimal byte"
    step state byte =
      case byteDigit byte of
        Right digit -> Just (state * 10 + digit)
        _ -> Nothing
