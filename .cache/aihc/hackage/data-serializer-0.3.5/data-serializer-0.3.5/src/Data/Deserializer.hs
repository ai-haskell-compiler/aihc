{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Deserialization monad and deserializable types.
module Data.Deserializer
  (
  -- * Deserialization monad
    Deserializer(..)
  , BinaryDeserializer(..)
  , CerealDeserializer(..)
  -- ** Binary word parsing
  , word16H
  , word32H
  , word64H
  , word
  , wordL
  , wordB
  , wordH
  , int8
  , int16
  , int16L
  , int16B
  , int16H
  , int32
  , int32L
  , int32B
  , int32H
  , int64
  , int64L
  , int64B
  , int64H
  , int
  , intL
  , intB
  , intH
  -- ** Parsing combinators
  , module Text.Parser.Combinators
  , label
  , module Text.Parser.LookAhead
  -- ** Endian deserializers
  , LittleEndianDeserializer(..)
  , BigEndianDeserializer(..)
  , deserializeIn
  , deserializeH
  -- ** Default deserializer
  , Deserialized(..)
  , isDeserialized
  , isMalformed
  , maybeDeserialized
  , defaultDeserializer
  -- * Deserializable types
  , Deserializable(..)
  , getIn
  , getL
  , getB
  , getH
  , deserializeBytes
  , deserializeBytesAs
  , deserializeByteString
  , deserializeByteStringAs
  , deserializeLazyByteString
  , deserializeLazyByteStringAs
  , fromBytes
  , fromBytesAs
  , fromByteString
  , fromByteStringAs
  , fromLazyByteString
  , fromLazyByteStringAs
  , RestDeserializable(..)
  ) where

import Prelude hiding (take)
import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Data (Data)
import Data.Proxy (Proxy(..))
import Data.Endian (Endian(..), swapEndian)
import Data.Word
import Data.Int
import Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Builder as BB
import qualified Data.Binary.Get as B
import qualified Data.Binary.Get.Internal as B
import qualified Data.Serialize.Get as S
import Data.List.Split (splitOn)
import Text.Parser.Combinators
import Text.Parser.LookAhead
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (Applicative(..), (<$>), (<$), (<*>), (*>))
#endif
import Control.Applicative (Alternative, (<|>))
import Control.Monad (unless)

-- | Deserialization monad.
class (Monad μ, Parsing μ) ⇒ Deserializer μ where
  {-# MINIMAL ensure, take, chunk, isolate #-}
  -- | Default byte order of the deserializer.
  endian ∷ Proxy μ → Endian
#ifdef WORDS_BIGENDIAN
  endian _ = BigEndian
#else
  endian _ = LittleEndian
#endif
  -- | Deserialze a byte.
  word8 ∷ μ Word8
  word8 = BS.unsafeHead <$> take 1 <?> "word8"
  {-# INLINE word8 #-}
  -- | Deserialize an unsigned 16-bit integer in default byte order.
  word16 ∷ μ Word16
  word16 = getIn (endian (Proxy ∷ Proxy μ))
  {-# INLINE word16 #-}
  -- | Deserialize an unsigned 32-bit integer in default byte order.
  word32 ∷ μ Word32
  word32 = getIn (endian (Proxy ∷ Proxy μ))
  {-# INLINE word32 #-}
  -- | Deserialize an unsigned 64-bit integer in default byte order.
  word64 ∷ μ Word64
  word64 = getIn (endian (Proxy ∷ Proxy μ))
  {-# INLINE word64 #-}
  -- | Deserialize an unsigned 16-bit integer in little endian.
  word16L ∷ μ Word16
  word16L = (<?> "word16")
          $ do bs ← take 2
               let l = BS.unsafeIndex bs 0
                   h = BS.unsafeIndex bs 1
               return $ shiftL (fromIntegral h) 8 .|. fromIntegral l
  -- | Deserialize an unsigned 16-bit integer in big endian.
  word16B ∷ μ Word16
  word16B = swapEndian <$> word16L
  -- | Deserialize an unsigned 32-bit integer in little endian.
  word32L ∷ μ Word32
  word32L = (<?> "word32")
          $ do bs ← take 4
               let o₀ = BS.unsafeIndex bs 0
                   o₁ = BS.unsafeIndex bs 1
                   o₂ = BS.unsafeIndex bs 2
                   o₃ = BS.unsafeIndex bs 3
               return  $  shiftL (fromIntegral o₃) 24
                      .|. shiftL (fromIntegral o₂) 16
                      .|. shiftL (fromIntegral o₁) 8
                      .|. fromIntegral o₀
  -- | Deserialize an unsigned 32-bit integer in big endian.
  word32B ∷ μ Word32
  word32B = swapEndian <$> word32L
  -- | Deserialize an unsigned 64-bit integer in little endian.
  word64L ∷ μ Word64
  word64L = (<?> "word64")
          $ do bs ← take 8
               let o₀ = BS.unsafeIndex bs 0
                   o₁ = BS.unsafeIndex bs 1
                   o₂ = BS.unsafeIndex bs 2
                   o₃ = BS.unsafeIndex bs 3
                   o₄ = BS.unsafeIndex bs 4
                   o₅ = BS.unsafeIndex bs 5
                   o₆ = BS.unsafeIndex bs 6
                   o₇ = BS.unsafeIndex bs 7
               return  $  shiftL (fromIntegral o₇) 56
                      .|. shiftL (fromIntegral o₆) 48
                      .|. shiftL (fromIntegral o₅) 40
                      .|. shiftL (fromIntegral o₄) 32
                      .|. shiftL (fromIntegral o₃) 24
                      .|. shiftL (fromIntegral o₂) 16
                      .|. shiftL (fromIntegral o₁) 8
                      .|. fromIntegral o₀
  -- | Deserialize an unsigned 64-bit integer in big endian.
  word64B ∷ μ Word64
  word64B = swapEndian <$> word64L
  -- | 'satisfy' /p/ deserializes a byte that satisfies the predicate /p/,
  --   failing otherwise.
  satisfy ∷ (Word8 → Bool) → μ Word8
  satisfy p = do w ← word8
                 if p w then return w
                        else unexpected (show w)
  {-# INLINE satisfy #-}
  -- | Deserialize the specified byte value, failing on any other input.
  byte ∷ Word8 → μ Word8
  byte w = (<?> "byte " ++ show w)
         $ do w' ← word8
              if w' == w then return w'
                         else unexpected (show w')
  {-# INLINE byte #-}
  -- | 'notByte' /c/ deserializes any byte that is not equal to /c/, failing
  --   if /c/ is encountered.
  notByte ∷ Word8 → μ Word8
  notByte w = (<?> "not byte " ++ show w)
            $ do w' ← word8
                 if w' == w then unexpected (show w')
                            else return w'
  {-# INLINE notByte #-}
  -- | 'bytes' /bs/ deserializes a sequence of bytes given by /bs/, failing
  --   on any other input.
  bytes ∷ BS.ByteString → μ BS.ByteString
  bytes bs = (<?> "bytes " ++ show (BS.unpack bs))
           $ do bs' ← take (BS.length bs)
                if bs' == bs then return bs'
                             else unexpected (show $ BS.unpack bs')
  {-# INLINE bytes #-}
  -- | Skip exactly the given number of bytes.
  skip ∷ Int → μ ()
  skip n | n <= 0    = pure ()
         | otherwise = word8 *> skip (n - 1)
  -- | 'ensure' /n/ checks that the input has at least /n/ more bytes and
  --   returns a portion of the input of length greater or equal to /n/
  --   (without consuming it).
  ensure ∷ Int → μ BS.ByteString
  -- | 'ensure_' /n/ fails if the input has less than /n/ more bytes.
  ensure_ ∷ Int → μ ()
  ensure_ = (() <$) . ensure
  {-# INLINE ensure_ #-}
  -- | 'check' /n/ returns 'True' if the input has at least /n/ more bytes.
  check ∷ Int → μ Bool
  check n = True <$ ensure_ n <|> return False
  {-# INLINE check #-}
  -- | Consume exactly the given number of bytes.
  take ∷ Int → μ BS.ByteString
  -- | Consume a portion of the input (the size of the returned
  --   'BS.ByteString' is implementation dependent). Empty result means that
  --    the 'eof' is reached.
  chunk ∷ μ BS.ByteString
  -- | 'isolate' /n/ /d/ feeds the next /n/ bytes to the deserializer /d/.
  --   If /d/ consumes less or more that /n/ bytes, 'isolate' will fail.
  isolate ∷ Int → μ α → μ α

-- | A wrapper around the 'B.Get' monad (to avoid orphan instances).
newtype BinaryDeserializer α =
          BinaryDeserializer { binaryDeserializer ∷ B.Get α }
          deriving (Typeable, Generic, Functor, Applicative,
                    Alternative, Monad)

instance Parsing BinaryDeserializer where
  try p = p
  {-# INLINE try #-}
  p <?> l = BinaryDeserializer (B.label l (binaryDeserializer p))
  {-# INLINE (<?>) #-}
  skipMany p = ((True <$ p) <|> pure False) >>= \case
                 True  → skipMany p
                 False → return ()
  unexpected = BinaryDeserializer . fail
  {-# INLINE unexpected #-}
  eof = BinaryDeserializer
      $ B.isEmpty >>= \case
          True  → return ()
          False → fail "Parsing.eof"
  {-# INLINABLE eof #-}
  notFollowedBy p = BinaryDeserializer
                  $ (B.lookAheadE (Left <$> (binaryDeserializer p)) <|>
                     pure (Right ())) >>= \case
                      Left e  → fail (show e)
                      Right _ → return ()
  {-# INLINABLE notFollowedBy #-}

instance LookAheadParsing BinaryDeserializer where
  lookAhead = BinaryDeserializer . B.lookAhead . binaryDeserializer
  {-# INLINE lookAhead #-}

instance Deserializer BinaryDeserializer where
  word8 = BinaryDeserializer B.getWord8
  {-# INLINE word8 #-}
  word16L = BinaryDeserializer B.getWord16le
  {-# INLINE word16L #-}
  word16B = BinaryDeserializer B.getWord16be
  {-# INLINE word16B #-}
  word32L = BinaryDeserializer B.getWord32le
  {-# INLINE word32L #-}
  word32B = BinaryDeserializer B.getWord32be
  {-# INLINE word32B #-}
  word64L = BinaryDeserializer B.getWord64le
  {-# INLINE word64L #-}
  word64B = BinaryDeserializer B.getWord64be
  {-# INLINE word64B #-}
  skip = BinaryDeserializer . B.skip
  {-# INLINE skip #-}
  ensure n = BinaryDeserializer (B.ensureN n *> B.get)
  {-# INLINE ensure #-}
  ensure_ = BinaryDeserializer . B.ensureN
  {-# INLINE ensure_ #-}
  take = BinaryDeserializer . B.getByteString
  {-# INLINE take #-}
  chunk = BinaryDeserializer
        $ do bs ← B.get
             if BS.null bs
             then do
               e ← B.isEmpty
               if e
               then return bs
               else B.get
             else
               return bs
  {-# INLINABLE chunk #-}
  isolate n d = BinaryDeserializer $ B.isolate n (binaryDeserializer d)
  {-# INLINE isolate #-}

-- | A wrapper around the 'S.Get' monad (to avoid orphan instances).
newtype CerealDeserializer α =
          CerealDeserializer { cerealDeserializer ∷ S.Get α }
          deriving (Typeable, Generic,  Functor, Applicative,
                    Alternative, Monad)

instance Parsing CerealDeserializer where
  try p = p
  {-# INLINE try #-}
  p <?> l = CerealDeserializer (S.label l (cerealDeserializer p))
  {-# INLINE (<?>) #-}
  skipMany p = ((True <$ p) <|> pure False) >>= \case
                 True  → skipMany p
                 False → return ()
  unexpected = CerealDeserializer . fail
  {-# INLINE unexpected #-}
  eof = CerealDeserializer
      $ ((False <$ S.lookAheadM (Nothing <$ S.getWord8)) <|>
         pure True) >>= \case
          True  → return ()
          False → fail "Parsing.eof"
  {-# INLINABLE eof #-}
  notFollowedBy p = CerealDeserializer
                  $ (S.lookAheadE (Left <$> (cerealDeserializer p)) <|>
                     pure (Right ())) >>= \case
                      Left e  → fail (show e)
                      Right _ → return ()
  {-# INLINABLE notFollowedBy #-}

instance LookAheadParsing CerealDeserializer where
  lookAhead = CerealDeserializer . S.lookAhead . cerealDeserializer
  {-# INLINE lookAhead #-}

instance Deserializer CerealDeserializer where
  word8 = CerealDeserializer S.getWord8
  {-# INLINE word8 #-}
  word16L = CerealDeserializer S.getWord16le
  {-# INLINE word16L #-}
  word16B = CerealDeserializer S.getWord16be
  {-# INLINE word16B #-}
  word32L = CerealDeserializer S.getWord32le
  {-# INLINE word32L #-}
  word32B = CerealDeserializer S.getWord32be
  {-# INLINE word32B #-}
  word64L = CerealDeserializer S.getWord64le
  {-# INLINE word64L #-}
  word64B = CerealDeserializer S.getWord64be
  {-# INLINE word64B #-}
  skip = CerealDeserializer . S.skip
  {-# INLINE skip #-}
  ensure = CerealDeserializer . S.ensure
  {-# INLINE ensure #-}
  take = CerealDeserializer . S.getBytes
  {-# INLINE take #-}
  chunk = CerealDeserializer
        $ (<|> pure BS.empty)
        $ do bs ← S.ensure 1
             S.uncheckedSkip (BS.length bs)
             return bs
  {-# INLINE chunk #-}
  isolate n d = CerealDeserializer (S.isolate n (cerealDeserializer d))
  {-# INLINE isolate #-}

-- | Deserialize an unsigned 16-bit integer in host byte order.
word16H ∷ Deserializer μ ⇒ μ Word16
#ifdef WORDS_BIGENDIAN
word16H = word16B
#else
word16H = word16L
#endif
{-# INLINE word16H #-}

-- | Deserialize an unsigned 32-bit integer in host byte order.
word32H ∷ Deserializer μ ⇒ μ Word32
#ifdef WORDS_BIGENDIAN
word32H = word32B
#else
word32H = word32L
#endif
{-# INLINE word32H #-}

-- | Deserialize an unsigned 64-bit integer in host byte order.
word64H ∷ Deserializer μ ⇒ μ Word64
#ifdef WORDS_BIGENDIAN
word64H = word64B
#else
word64H = word64L
#endif
{-# INLINE word64H #-}

-- | Deserialize an unsigned native-sized integer in serializer default
--   byte order.
word ∷ Deserializer μ ⇒ μ Word
#if WORD_SIZE_IN_BITS == 32
word = fromIntegral <$> word32
#else
word = fromIntegral <$> word64
#endif
{-# INLINE word #-}

-- | Deserialize an unsigned native-sized integer in little endian.
wordL ∷ Deserializer μ ⇒ μ Word
#if WORD_SIZE_IN_BITS == 32
wordL = fromIntegral <$> word32L
#else
wordL = fromIntegral <$> word64L
#endif
{-# INLINE wordL #-}

-- | Deserialize an unsigned native-sized integer in big endian.
wordB ∷ Deserializer μ ⇒ μ Word
#if WORD_SIZE_IN_BITS == 32
wordB = fromIntegral <$> word32B
#else
wordB = fromIntegral <$> word64B
#endif
{-# INLINE wordB #-}

-- | Deserialize an unsigned native-sized integer in host byte order.
wordH ∷ Deserializer μ ⇒ μ Word
#ifdef WORDS_BIGENDIAN
wordH = wordB
#else
wordH = wordL
#endif
{-# INLINE wordH #-}

-- | Deserialize a signed 8-bit integer.
int8 ∷ Deserializer μ ⇒ μ Int8
int8 = fromIntegral <$> word8
{-# INLINE int8 #-}

-- | Deserialize a signed 16-bit integer in serializer default byte order.
int16 ∷ Deserializer μ ⇒ μ Int16
int16 = fromIntegral <$> word16
{-# INLINE int16 #-}

-- | Deserialize a signed 16-bit integer in little endian.
int16L ∷ Deserializer μ ⇒ μ Int16
int16L = fromIntegral <$> word16L
{-# INLINE int16L #-}

-- | Deserialize a signed 16-bit integer in big endian.
int16B ∷ Deserializer μ ⇒ μ Int16
int16B = fromIntegral <$> word16B
{-# INLINE int16B #-}

-- | Deserialize a signed 16-bit integer in host byte order.
int16H ∷ Deserializer μ ⇒ μ Int16
int16H = fromIntegral <$> word16H
{-# INLINE int16H #-}

-- | Deserialize a signed 32-bit integer in serializer default byte order.
int32 ∷ Deserializer μ ⇒ μ Int32
int32 = fromIntegral <$> word32
{-# INLINE int32 #-}

-- | Deserialize a signed 32-bit integer in little endian.
int32L ∷ Deserializer μ ⇒ μ Int32
int32L = fromIntegral <$> word32L
{-# INLINE int32L #-}

-- | Deserialize a signed 32-bit integer in big endian.
int32B ∷ Deserializer μ ⇒ μ Int32
int32B = fromIntegral <$> word32B
{-# INLINE int32B #-}

-- | Deserialize a signed 32-bit integer in host byte order.
int32H ∷ Deserializer μ ⇒ μ Int32
int32H = fromIntegral <$> word32H
{-# INLINE int32H #-}

-- | Deserialize a signed 64-bit integer in serializer default byte order.
int64 ∷ Deserializer μ ⇒ μ Int64
int64 = fromIntegral <$> word64
{-# INLINE int64 #-}

-- | Deserialize a signed 64-bit integer in little endian.
int64L ∷ Deserializer μ ⇒ μ Int64
int64L = fromIntegral <$> word64L
{-# INLINE int64L #-}

-- | Deserialize a signed 64-bit integer in big endian.
int64B ∷ Deserializer μ ⇒ μ Int64
int64B = fromIntegral <$> word64B
{-# INLINE int64B #-}

-- | Deserialize a signed 64-bit integer in host byte order.
int64H ∷ Deserializer μ ⇒ μ Int64
int64H = fromIntegral <$> word64H
{-# INLINE int64H #-}

-- | Deserialize a signed native-sized integer in serializer default byte
--   order.
int ∷ Deserializer μ ⇒ μ Int
#if WORD_SIZE_IN_BITS == 32
int = fromIntegral <$> int32
#else
int = fromIntegral <$> int64
#endif
{-# INLINE int #-}

-- | Deserialize a signed native-sized integer in little endian.
intL ∷ Deserializer μ ⇒ μ Int
#if WORD_SIZE_IN_BITS == 32
intL = fromIntegral <$> int32L
#else
intL = fromIntegral <$> int64L
#endif
{-# INLINE intL #-}

-- | Deserialize a signed native-sized integer in big endian.
intB ∷ Deserializer μ ⇒ μ Int
#if WORD_SIZE_IN_BITS == 32
intB = fromIntegral <$> int32B
#else
intB = fromIntegral <$> int64B
#endif
{-# INLINE intB #-}

-- | Deserialize a signed native-sized integer in host byte order.
intH ∷ Deserializer μ ⇒ μ Int
#if WORD_SIZE_IN_BITS == 32
intH = fromIntegral <$> int32H
#else
intH = fromIntegral <$> int64H
#endif
{-# INLINE intH #-}

-- | A shorthand for 'flip' ('<?>').
label ∷ Parsing μ ⇒ String → μ α → μ α
label = flip (<?>)
{-# INLINE label #-}

-- | Deserializer wrapper with little endian default byte order.
newtype LittleEndianDeserializer μ α =
          LittleEndianDeserializer { deserializeL ∷ μ α }
          deriving (Typeable, Data, Generic, Functor, Applicative,
                    Alternative, Monad, Parsing, LookAheadParsing)

instance Deserializer μ ⇒ Deserializer (LittleEndianDeserializer μ) where
  endian _ = LittleEndian
  {-# INLINE endian #-}
  word8 = LittleEndianDeserializer word8
  {-# INLINE word8 #-}
  word16 = LittleEndianDeserializer word16L
  {-# INLINE word16 #-}
  word32 = LittleEndianDeserializer word32L
  {-# INLINE word32 #-}
  word64 = LittleEndianDeserializer word64L
  {-# INLINE word64 #-}
  word16L = LittleEndianDeserializer word16L
  {-# INLINE word16L #-}
  word16B = LittleEndianDeserializer word16B
  {-# INLINE word16B #-}
  word32L = LittleEndianDeserializer word32L
  {-# INLINE word32L #-}
  word32B = LittleEndianDeserializer word32B
  {-# INLINE word32B #-}
  word64L = LittleEndianDeserializer word64L
  {-# INLINE word64L #-}
  word64B = LittleEndianDeserializer word64B
  {-# INLINE word64B #-}
  satisfy = LittleEndianDeserializer . satisfy
  {-# INLINE satisfy #-}
  byte = LittleEndianDeserializer . byte
  {-# INLINE byte #-}
  notByte = LittleEndianDeserializer . notByte
  {-# INLINE notByte #-}
  bytes = LittleEndianDeserializer . bytes
  {-# INLINE bytes #-}
  skip = LittleEndianDeserializer . skip
  {-# INLINE skip #-}
  ensure = LittleEndianDeserializer . ensure
  {-# INLINE ensure #-}
  ensure_ = LittleEndianDeserializer . ensure_
  {-# INLINE ensure_ #-}
  take = LittleEndianDeserializer . take
  {-# INLINE take #-}
  chunk = LittleEndianDeserializer chunk
  {-# INLINE chunk #-}
  isolate n = LittleEndianDeserializer . isolate n . deserializeL
  {-# INLINE isolate #-}

-- | Deserializer wrapper with big endian default byte order.
newtype BigEndianDeserializer μ α =
          BigEndianDeserializer { deserializeB ∷ μ α }
          deriving (Typeable, Data, Generic, Functor, Applicative,
                    Alternative, Monad, Parsing, LookAheadParsing)

instance Deserializer μ ⇒ Deserializer (BigEndianDeserializer μ) where
  endian _ = BigEndian
  {-# INLINE endian #-}
  word8 = BigEndianDeserializer word8
  {-# INLINE word8 #-}
  word16 = BigEndianDeserializer word16B
  {-# INLINE word16 #-}
  word32 = BigEndianDeserializer word32B
  {-# INLINE word32 #-}
  word64 = BigEndianDeserializer word64B
  {-# INLINE word64 #-}
  word16L = BigEndianDeserializer word16L
  {-# INLINE word16L #-}
  word16B = BigEndianDeserializer word16B
  {-# INLINE word16B #-}
  word32L = BigEndianDeserializer word32L
  {-# INLINE word32L #-}
  word32B = BigEndianDeserializer word32B
  {-# INLINE word32B #-}
  word64L = BigEndianDeserializer word64L
  {-# INLINE word64L #-}
  word64B = BigEndianDeserializer word64B
  {-# INLINE word64B #-}
  satisfy = BigEndianDeserializer . satisfy
  {-# INLINE satisfy #-}
  byte = BigEndianDeserializer . byte
  {-# INLINE byte #-}
  notByte = BigEndianDeserializer . notByte
  {-# INLINE notByte #-}
  bytes = BigEndianDeserializer . bytes
  {-# INLINE bytes #-}
  skip = BigEndianDeserializer . skip
  {-# INLINE skip #-}
  ensure = BigEndianDeserializer . ensure
  {-# INLINE ensure #-}
  ensure_ = BigEndianDeserializer . ensure_
  {-# INLINE ensure_ #-}
  take = BigEndianDeserializer . take
  {-# INLINE take #-}
  chunk = BigEndianDeserializer chunk
  {-# INLINE chunk #-}
  isolate n = BigEndianDeserializer . isolate n . deserializeB
  {-# INLINE isolate #-}

-- | Force the default byte order.
deserializeIn ∷ Deserializer μ
              ⇒ Endian → (∀ μ' . (Deserializer μ') ⇒ μ' α) → μ α
deserializeIn LittleEndian d = deserializeL d
deserializeIn BigEndian    d = deserializeB d
{-# INLINE deserializeIn #-}

-- | Force the default byte order to be the host byte order.
deserializeH ∷ Deserializer μ ⇒ (∀ μ' . (Deserializer μ') ⇒ μ' α) → μ α
#ifdef WORDS_BIGENDIAN
deserializeH d = deserializeB d
#else
deserializeH d = deserializeL d
#endif
{-# INLINE deserializeH #-}

-- | Deserialization result.
data Deserialized α = Deserialized α
                    | Malformed [String] String

-- | Map 'Deserialized' to 'True' and 'Malformed' to 'False'.
isDeserialized ∷ Deserialized α → Bool
isDeserialized (Deserialized _) = True
isDeserialized (Malformed _ _)  = False

-- | Map 'Deserialized' to 'False' and 'Malformed' to 'True'.
isMalformed ∷ Deserialized α → Bool
isMalformed (Deserialized _) = False
isMalformed (Malformed _ _)  = True

-- | Map 'Deserialized' values to 'Just' and 'Malformed' to 'Nothing'.
maybeDeserialized ∷ Deserialized α → Maybe α
maybeDeserialized (Deserialized a) = Just a
maybeDeserialized (Malformed _ _)  = Nothing

-- | Deserialize a 'LBS.ByteString' via the default deserializer.
defaultDeserializer ∷ (∀ μ . Deserializer μ ⇒ μ α) → LBS.ByteString
                    → Deserialized α
defaultDeserializer m i = case B.runGetOrFail (binaryDeserializer m) i of
  Left (_, _, e) → case splitOn "\n" e of
    []  → Malformed [] e
    [_] → Malformed [] e
    es  → Malformed (init es) (last es)
  Right (_, _, a) →
    Deserialized a

-- | Deserializable type. 'get' must not rely on 'eof'.
class Deserializable α where
  get ∷ Deserializer μ ⇒ μ α

instance Deserializable Bool where
  get = do w ← word8
           case w of
             0 → return False
             1 → return True
             _ → unexpected (show w)

instance Deserializable Word8 where
  get = word8
  {-# INLINE get #-}

instance Deserializable Word16 where
  get = word16
  {-# INLINE get #-}

instance Deserializable Word32 where
  get = word32
  {-# INLINE get #-}

instance Deserializable Word64 where
  get = word64
  {-# INLINE get #-}

instance Deserializable Word where
  get = word
  {-# INLINE get #-}

instance Deserializable Int8 where
  get = int8
  {-# INLINE get #-}

instance Deserializable Int16 where
  get = int16
  {-# INLINE get #-}

instance Deserializable Int32 where
  get = int32
  {-# INLINE get #-}

instance Deserializable Int64 where
  get = int64
  {-# INLINE get #-}

instance Deserializable Int where
  get = int
  {-# INLINE get #-}

instance (Deserializable α, Deserializable β) ⇒ Deserializable (α, β) where
  get = (,) <$> get <*> get
  {-# INLINE get #-}

instance Deserializable α ⇒ Deserializable (Maybe α) where
  get = do w ← word8
           case w of
             0 → return Nothing
             1 → Just <$> get
             _ → unexpected (show w)

instance (Deserializable α, Deserializable β)
         ⇒ Deserializable (Either α β) where
  get = do w ← word8
           case w of
             0 → Left <$> get
             1 → Right <$> get
             _ → unexpected (show w)

instance Deserializable BS.ByteString where
  get = do l ← int <?> "length"
           unless (l >= 0) $ unexpected "negative length"
           take l <?> "contents"
  {-# INLINABLE get #-}

instance Deserializable SBS.ShortByteString where
  get = SBS.toShort <$> get
  {-# INLINE get #-}

-- | Deserialize a value using the provided default byte order.
getIn ∷ (Deserializer μ, Deserializable α) ⇒ Endian → μ α
getIn e = deserializeIn e get
{-# INLINE getIn #-}

-- | Deserialize a value using little endian as the default byte order.
getL ∷ (Deserializer μ, Deserializable α) ⇒ μ α
getL = deserializeL get
{-# INLINE getL #-}

-- | Deserialize a value using big endian as the default byte order.
getB ∷ (Deserializer μ, Deserializable α) ⇒ μ α
getB = deserializeB get
{-# INLINE getB #-}

-- | Deserialize a value using host byte order as the default byte order.
getH ∷ (Deserializer μ, Deserializable α) ⇒ μ α
#ifdef WORDS_BIGENDIAN
getH = getB
#else
getH = getL
#endif
{-# INLINE getH #-}

-- | Deserialize a value of type @α@ from a list of bytes via
-- the 'defaultDeserializer'.
deserializeBytes ∷ Deserializable α ⇒ [Word8] → Deserialized α
deserializeBytes = defaultDeserializer get . LBS.pack
{-# INLINE deserializeBytes #-}

-- | Provide a hint for the type system when using 'deserializeBytes'.
deserializeBytesAs ∷ Deserializable α ⇒ p α → [Word8] → Deserialized α
deserializeBytesAs _ = deserializeBytes
{-# INLINE deserializeBytesAs #-}

-- | Deserialize a value of type @α@ from a 'BS.ByteString' via
-- the 'defaultDeserializer'.
deserializeByteString ∷ Deserializable α
                      ⇒ BS.ByteString → Deserialized α
deserializeByteString = defaultDeserializer get . LBS.fromStrict
{-# INLINE deserializeByteString #-}

-- | Provide a hint for the type system when using 'deserializeByteString'.
deserializeByteStringAs ∷ Deserializable α
                        ⇒ p α → BS.ByteString → Deserialized α
deserializeByteStringAs _ = deserializeByteString
{-# INLINE deserializeByteStringAs #-}

-- | Deserialize a value of type @α@ from a 'LBS.ByteString' via
-- the 'defaultDeserializer'.
deserializeLazyByteString ∷ Deserializable α
                          ⇒ LBS.ByteString → Deserialized α
deserializeLazyByteString = defaultDeserializer get
{-# INLINE deserializeLazyByteString #-}

-- | Provide a hint for the type system when using
-- 'deserializeLazyByteString'.
deserializeLazyByteStringAs ∷ Deserializable α
                            ⇒ p α → LBS.ByteString → Deserialized α
deserializeLazyByteStringAs _ = deserializeLazyByteString
{-# INLINE deserializeLazyByteStringAs #-}

-- | A shorthand for @'maybeDeserialized' . 'deserializeBytes'@.
fromBytes ∷ Deserializable α ⇒ [Word8] → Maybe α
fromBytes = maybeDeserialized . deserializeBytes
{-# INLINE fromBytes #-}

-- | Provide a hint for the type system when using 'fromBytes'
fromBytesAs ∷ Deserializable α ⇒ p α → [Word8] → Maybe α
fromBytesAs _ = fromBytes
{-# INLINE fromBytesAs #-}

-- | A shorthand for @'maybeDeserialized' . 'deserializeByteString'@.
fromByteString ∷ Deserializable α ⇒ BS.ByteString → Maybe α
fromByteString = maybeDeserialized . deserializeByteString
{-# INLINE fromByteString #-}

-- | Provide a hint for the type system when using 'fromByteString'
fromByteStringAs ∷ Deserializable α ⇒ p α → BS.ByteString → Maybe α
fromByteStringAs _ = fromByteString
{-# INLINE fromByteStringAs #-}

-- | A shorthand for @'maybeDeserialized' . 'deserializeLazyByteString'@.
fromLazyByteString ∷ Deserializable α ⇒ LBS.ByteString → Maybe α
fromLazyByteString = maybeDeserialized . deserializeLazyByteString
{-# INLINE fromLazyByteString #-}

-- | Provide a hint for the type system when using 'fromLazyByteString'
fromLazyByteStringAs ∷ Deserializable α ⇒ p α → LBS.ByteString → Maybe α
fromLazyByteStringAs _ = fromLazyByteString
{-# INLINE fromLazyByteStringAs #-}

-- | Deserializable type. 'getRest' must consume all the remaining input
--   or fail.
class RestDeserializable α where
  getRest ∷ Deserializer μ ⇒ μ α

instance RestDeserializable BS.ByteString where
  getRest = go []
    where go acc = do bs ← chunk
                      if BS.null bs then return $ BS.concat $ reverse acc
                                    else go (bs : acc)

instance RestDeserializable SBS.ShortByteString where
  getRest = SBS.toShort <$> getRest
  {-# INLINE getRest #-}

instance RestDeserializable LBS.ByteString where
  getRest = go []
    where go acc = do bs ← chunk
                      if BS.null bs then return $ LBS.fromChunks $ reverse acc
                                    else go (bs : acc)

instance RestDeserializable BB.Builder where
  getRest = BB.lazyByteString <$> getRest
  {-# INLINE getRest #-}

instance (RestDeserializable α, RestDeserializable β)
         ⇒ RestDeserializable (Either α β) where
  getRest = word8 >>= \case
              0 → Left <$> getRest
              1 → Right <$> getRest
              w → unexpected (show w)
  {-# INLINABLE getRest #-}

instance (Deserializable α, RestDeserializable β)
         ⇒ RestDeserializable (α, β) where
  getRest = (,) <$> get <*> getRest
  {-# INLINE getRest #-}

instance Deserializable α ⇒ RestDeserializable [α] where
  getRest = ([] <$ eof) <|> ((:) <$> get <*> getRest)
