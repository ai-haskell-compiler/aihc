module Ptr.PokeAndPeek where

import qualified Ptr.IO as A
import Ptr.Prelude

-- |
-- Encoder and decoder of the same binary representation.
--
-- You can compose both the covariant and contravariant parameters of PokeAndPeek
-- using Applicative and Profunctor. E.g.,
--
-- >word8AndWord32 :: PokeAndPeek (Word8, Word32) (Word8, Word32)
-- >word8AndWord32 =
-- >  (,) <$> lmap fst word8 <*> lmap snd beWord32
data PokeAndPeek input output
  = PokeAndPeek !Int (Ptr Word8 -> input -> IO ()) (Ptr Word8 -> IO output)

-- |
-- A codec, which encodes and decodes the same type. E.g.,
--
-- >word8AndWord32 :: InvPokeAndPeek (Word8, Word32)
-- >word8AndWord32 =
-- >  (,) <$> lmap fst word8 <*> lmap snd beWord32
type InvPokeAndPeek value =
  PokeAndPeek value value

instance Profunctor PokeAndPeek where
  {-# INLINE dimap #-}
  dimap fn1 fn2 (PokeAndPeek size poke peek) =
    PokeAndPeek size (\ptr -> poke ptr . fn1) (\ptr -> fmap fn2 (peek ptr))

instance Functor (PokeAndPeek input) where
  {-# INLINE fmap #-}
  fmap fn (PokeAndPeek size poke peek) =
    PokeAndPeek size poke (fmap fn . peek)

instance Applicative (PokeAndPeek input) where
  {-# INLINE pure #-}
  pure x =
    PokeAndPeek 0 (\_ _ -> pure ()) (\_ -> pure x)
  {-# INLINE (<*>) #-}
  (<*>) (PokeAndPeek leftSize leftPoke leftPeek) (PokeAndPeek rightSize rightPoke rightPeek) =
    PokeAndPeek (leftSize + rightSize) poke peek
    where
      poke ptr input =
        leftPoke ptr input *> rightPoke (plusPtr ptr leftSize) input
      peek ptr =
        leftPeek ptr <*> rightPeek (plusPtr ptr leftSize)

{-# INLINE word8 #-}
word8 :: InvPokeAndPeek Word8
word8 =
  {-# SCC "word8" #-}
  PokeAndPeek 1 A.pokeWord8 A.peekWord8

{-# INLINE leWord16 #-}
leWord16 :: InvPokeAndPeek Word16
leWord16 =
  {-# SCC "leWord16" #-}
  PokeAndPeek 2 A.pokeLEWord16 A.peekLEWord16

{-# INLINE leWord32 #-}
leWord32 :: InvPokeAndPeek Word32
leWord32 =
  {-# SCC "leWord32" #-}
  PokeAndPeek 4 A.pokeLEWord32 A.peekLEWord32

{-# INLINE leWord64 #-}
leWord64 :: InvPokeAndPeek Word64
leWord64 =
  {-# SCC "leWord64" #-}
  PokeAndPeek 8 A.pokeLEWord64 A.peekLEWord64

{-# INLINE beWord16 #-}
beWord16 :: InvPokeAndPeek Word16
beWord16 =
  {-# SCC "beWord16" #-}
  PokeAndPeek 2 A.pokeBEWord16 A.peekBEWord16

{-# INLINE beWord32 #-}
beWord32 :: InvPokeAndPeek Word32
beWord32 =
  {-# SCC "beWord32" #-}
  PokeAndPeek 4 A.pokeBEWord32 A.peekBEWord32

{-# INLINE beWord64 #-}
beWord64 :: InvPokeAndPeek Word64
beWord64 =
  {-# SCC "beWord64" #-}
  PokeAndPeek 8 A.pokeBEWord64 A.peekBEWord64

{-# INLINE bytes #-}
bytes :: Int -> InvPokeAndPeek ByteString
bytes amount =
  {-# SCC "bytes" #-}
  PokeAndPeek amount (\ptr -> A.pokeBytesTrimming ptr amount) (\ptr -> A.peekBytes ptr amount)
