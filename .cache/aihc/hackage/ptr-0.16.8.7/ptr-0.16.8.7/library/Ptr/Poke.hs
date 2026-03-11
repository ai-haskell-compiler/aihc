module Ptr.Poke where

import qualified Data.Vector as F
import qualified Ptr.PokeAndPeek as B
import Ptr.Prelude

-- |
-- Specification of a sized and errorless writing action to a pointer.
data Poke input
  = Poke !Int !(Ptr Word8 -> input -> IO ())

instance Contravariant Poke where
  {-# INLINE contramap #-}
  contramap fn (Poke size io) =
    Poke size (\ptr input -> io ptr (fn input))

instance Divisible Poke where
  {-# INLINE conquer #-}
  conquer =
    Poke 0 (\_ _ -> pure ())
  {-# INLINE divide #-}
  divide fn (Poke size1 io1) (Poke size2 io2) =
    Poke (size1 + size2) (\ptr input -> case fn input of (input1, input2) -> io1 ptr input1 *> io2 (plusPtr ptr size1) input2)

{-# INLINE word8 #-}
word8 :: Poke Word8
word8 =
  pokeAndPeek B.word8

{-# INLINE leWord16 #-}
leWord16 :: Poke Word16
leWord16 =
  pokeAndPeek B.leWord16

{-# INLINE leWord32 #-}
leWord32 :: Poke Word32
leWord32 =
  pokeAndPeek B.leWord32

{-# INLINE leWord64 #-}
leWord64 :: Poke Word64
leWord64 =
  pokeAndPeek B.leWord64

{-# INLINE beWord16 #-}
beWord16 :: Poke Word16
beWord16 =
  pokeAndPeek B.beWord16

{-# INLINE beWord32 #-}
beWord32 :: Poke Word32
beWord32 =
  pokeAndPeek B.beWord32

{-# INLINE beWord64 #-}
beWord64 :: Poke Word64
beWord64 =
  pokeAndPeek B.beWord64

{-# INLINE bytes #-}
bytes :: Int -> Poke ByteString
bytes amount =
  pokeAndPeek (B.bytes amount)

{-# INLINE pokeAndPeek #-}
pokeAndPeek :: B.PokeAndPeek input output -> Poke input
pokeAndPeek (B.PokeAndPeek size io _) =
  Poke size io

{-# INLINE asciiChar #-}
asciiChar :: Poke Char
asciiChar =
  contramap (fromIntegral . ord) word8

{-# INLINE asciiDigit #-}
asciiDigit :: Poke Word8
asciiDigit =
  contramap (+ 48) word8

{-# INLINE asciiHexDigit #-}
asciiHexDigit :: Poke Word8
asciiHexDigit =
  contramap (\n -> if n < 10 then 48 + n else 55 + n) word8

{-# INLINE vector #-}
vector :: Int -> Poke element -> Poke (F.Vector element)
vector vectorSize (Poke elementByteSize elementIO) =
  Poke vectorByteSize vectorIO
  where
    vectorByteSize =
      vectorSize * elementByteSize
    vectorIO =
      F.foldM'_ step
      where
        step ptr element =
          elementIO ptr element $> plusPtr ptr elementByteSize
