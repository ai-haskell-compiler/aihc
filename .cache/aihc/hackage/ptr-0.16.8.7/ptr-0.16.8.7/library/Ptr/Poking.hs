module Ptr.Poking where

import qualified Data.ByteString.Internal as B
import qualified Data.List as List
import qualified Data.Vector.Generic as GenericVector
import qualified Ptr.IO as A
import qualified Ptr.List as List
import qualified Ptr.Poke as C
import qualified Ptr.PokeAndPeek as D
import qualified Ptr.PokeIO as E
import Ptr.Prelude hiding (length)

-- |
-- An efficiently composable unmaterialised specification of how to populate a pointer.
--
-- Once composed it can be materialized into a specific data-structure like ByteString or
-- to directly populate a pointer in some low-level API.
data Poking
  = -- |
    --  * Amount of bytes the encoded data will occupy.
    --  * Exception-free action, which populates the pointer to the encoded data.
    Poking !Int (Ptr Word8 -> IO ())

instance Semigroup Poking where
  {-# INLINEABLE (<>) #-}
  (<>) (Poking space1 action1) (Poking space2 action2) =
    Poking (space1 + space2) action
    where
      action =
        if space1 < 2048 || space2 < 2048
          then E.sequentially space1 action1 action2
          else E.concurrently space1 action1 action2

instance Monoid Poking where
  {-# INLINE mempty #-}
  mempty =
    Poking 0 (const (pure ()))
  {-# INLINE mappend #-}
  mappend =
    (<>)

instance IsString Poking where
  fromString string = Poking (List.length string) io
    where
      io ptr = foldM_ step ptr string
        where
          step ptr char = A.pokeWord8 ptr (fromIntegral (ord char)) $> plusPtr ptr 1

{-# INLINE null #-}
null :: Poking -> Bool
null =
  (== 0) . length

{-# INLINE length #-}
length :: Poking -> Int
length (Poking size _) =
  size

{-# INLINE word8 #-}
word8 :: Word8 -> Poking
word8 x =
  Poking 1 (flip A.pokeWord8 x)

{-# INLINE leWord16 #-}
leWord16 :: Word16 -> Poking
leWord16 x =
  Poking 2 (flip A.pokeLEWord16 x)

{-# INLINE leWord32 #-}
leWord32 :: Word32 -> Poking
leWord32 x =
  Poking 4 (flip A.pokeLEWord32 x)

{-# INLINE leWord64 #-}
leWord64 :: Word64 -> Poking
leWord64 x =
  Poking 8 (flip A.pokeLEWord64 x)

{-# INLINE beWord16 #-}
beWord16 :: Word16 -> Poking
beWord16 x =
  Poking 2 (flip A.pokeBEWord16 x)

{-# INLINE beWord32 #-}
beWord32 :: Word32 -> Poking
beWord32 x =
  Poking 4 (flip A.pokeBEWord32 x)

{-# INLINE beWord64 #-}
beWord64 :: Word64 -> Poking
beWord64 x =
  Poking 8 (flip A.pokeBEWord64 x)

{-# INLINE bytes #-}
bytes :: ByteString -> Poking
bytes (B.PS bytesFPtr offset length) =
  Poking length (\ptr -> withForeignPtr bytesFPtr (\bytesPtr -> copyBytes ptr (plusPtr bytesPtr offset) length))

{-# INLINE poke #-}
poke :: C.Poke input -> input -> Poking
poke (C.Poke space poke) input =
  Poking space (\ptr -> poke ptr input)

{-# INLINE pokeAndPeek #-}
pokeAndPeek :: D.PokeAndPeek input output -> input -> Poking
pokeAndPeek (D.PokeAndPeek space poke _) input =
  Poking space (\ptr -> poke ptr input)

-- | Unsigned ASCII integral
{-# INLINE asciiIntegral #-}
asciiIntegral :: (Integral a) => a -> Poking
asciiIntegral = \case
  0 -> word8 48
  x ->
    let reverseDigits = List.reverseDigits 10 x
        size = List.length reverseDigits
        action = E.reverseAsciiDigits (pred size) reverseDigits
     in Poking size action

{-# INLINE asciiChar #-}
asciiChar :: Char -> Poking
asciiChar =
  word8 . fromIntegral . ord

{-# INLINEABLE asciiPaddedAndTrimmedIntegral #-}
asciiPaddedAndTrimmedIntegral :: (Integral a) => Int -> a -> Poking
asciiPaddedAndTrimmedIntegral !length !integral =
  if length > 0
    then
      if integral >= 0
        then case quotRem integral 10 of
          (quot, rem) ->
            asciiPaddedAndTrimmedIntegral (pred length) quot
              <> word8 (48 + fromIntegral rem)
        else stimes length (word8 48)
    else mempty

{-# INLINEABLE asciiUtcTimeInIso8601 #-}
{-
2017-02-01T05:03:58Z
-}
asciiUtcTimeInIso8601 :: UTCTime -> Poking
asciiUtcTimeInIso8601 utcTime =
  asciiPaddedAndTrimmedIntegral 4 year
    <> word8 45
    <> asciiPaddedAndTrimmedIntegral 2 month
    <> word8 45
    <> asciiPaddedAndTrimmedIntegral 2 day
    <> word8 84
    <> asciiPaddedAndTrimmedIntegral 2 hour
    <> word8 58
    <> asciiPaddedAndTrimmedIntegral 2 minute
    <> word8 58
    <> asciiPaddedAndTrimmedIntegral 2 (round second)
    <> word8 90
  where
    LocalTime date (TimeOfDay hour minute second) = utcToLocalTime utc utcTime
    (year, month, day) = toGregorian date

{-# INLINE list #-}
list :: (element -> Poking) -> [element] -> Poking
list element =
  loop mempty
  where
    loop state =
      \case
        head : tail -> loop (state <> word8 1 <> element head) tail
        _ -> state <> word8 0

{-# INLINEABLE vector #-}
vector :: (GenericVector.Vector vector element) => (element -> Poking) -> vector element -> Poking
vector element vectorValue =
  Poking byteSize io
  where
    byteSize =
      GenericVector.foldl' step 0 vectorValue
      where
        step !byteSize elementValue =
          case element elementValue of
            Poking elementByteSize _ -> byteSize + elementByteSize
    io ptr =
      GenericVector.foldM'_ step ptr vectorValue
      where
        step ptr elementValue =
          case element elementValue of
            Poking elementByteSize elementIO -> do
              elementIO ptr
              return (plusPtr ptr elementByteSize)

{-# INLINEABLE intercalateVector #-}
intercalateVector :: (GenericVector.Vector vector element) => (element -> Poking) -> Poking -> vector element -> Poking
intercalateVector element (Poking separatorLength separatorIo) vectorValue = Poking length io
  where
    length = GenericVector.foldl' step 0 vectorValue + ((GenericVector.length vectorValue - 1) * separatorLength)
      where
        step length elementValue = case element elementValue of
          Poking elementLength _ -> length + elementLength
    indexIsLast =
      let lastIndex = pred (GenericVector.length vectorValue)
       in (== lastIndex)
    io ptr = GenericVector.ifoldM'_ step ptr vectorValue
      where
        step ptr index elementValue = case element elementValue of
          Poking elementLength elementIo ->
            if indexIsLast index
              then elementIo ptr $> ptr
              else
                let ptrAfterElement = plusPtr ptr elementLength
                 in elementIo ptr *> separatorIo ptrAfterElement $> plusPtr ptrAfterElement separatorLength
