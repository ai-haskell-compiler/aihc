module Ptr.Peek where

import qualified Ptr.IO as A
import qualified Ptr.Parse as C
import qualified Ptr.ParseUnbound as D
import qualified Ptr.PokeAndPeek as B
import Ptr.Prelude hiding (take)

data Peek output
  = Peek {-# UNPACK #-} !Int !(Ptr Word8 -> IO output)

instance Functor Peek where
  {-# INLINE fmap #-}
  fmap fn (Peek size io) =
    Peek size (fmap fn . io)

instance Applicative Peek where
  {-# INLINE pure #-}
  pure x =
    Peek 0 (const (pure x))
  {-# INLINE (<*>) #-}
  (<*>) (Peek leftSize leftIO) (Peek rightSize rightIO) =
    Peek (leftSize + rightSize) io
    where
      io ptr =
        leftIO ptr <*> rightIO (plusPtr ptr leftSize)

-------------------------

{-# INLINE int8 #-}
int8 :: Peek Int8
int8 =
  {-# SCC "int8" #-}
  Peek 1 A.peekStorable

-------------------------

{-# INLINE beInt16 #-}
beInt16 :: Peek Int16
beInt16 =
  {-# SCC "beInt16" #-}
  Peek 2 A.peekBEInt16

{-# INLINE beInt32 #-}
beInt32 :: Peek Int32
beInt32 =
  {-# SCC "beInt32" #-}
  Peek 4 A.peekBEInt32

{-# INLINE beInt64 #-}
beInt64 :: Peek Int64
beInt64 =
  {-# SCC "beInt64" #-}
  Peek 8 A.peekBEInt64

-------------------------

{-# INLINE leInt16 #-}
leInt16 :: Peek Int16
leInt16 =
  {-# SCC "leInt16" #-}
  Peek 2 A.peekLEInt16

{-# INLINE leInt32 #-}
leInt32 :: Peek Int32
leInt32 =
  {-# SCC "leInt32" #-}
  Peek 4 A.peekLEInt32

{-# INLINE leInt64 #-}
leInt64 :: Peek Int64
leInt64 =
  {-# SCC "leInt64" #-}
  Peek 8 A.peekLEInt64

-------------------------

{-# INLINE word8 #-}
word8 :: Peek Word8
word8 =
  {-# SCC "word8" #-}
  Peek 1 A.peekWord8

-------------------------

{-# INLINE beWord16 #-}
beWord16 :: Peek Word16
beWord16 =
  {-# SCC "beWord16" #-}
  Peek 2 A.peekBEWord16

{-# INLINE beWord32 #-}
beWord32 :: Peek Word32
beWord32 =
  {-# SCC "beWord32" #-}
  Peek 4 A.peekBEWord32

{-# INLINE beWord64 #-}
beWord64 :: Peek Word64
beWord64 =
  {-# SCC "beWord64" #-}
  Peek 8 A.peekBEWord64

-------------------------

{-# INLINE leWord16 #-}
leWord16 :: Peek Word16
leWord16 =
  {-# SCC "leWord16" #-}
  Peek 2 A.peekLEWord16

{-# INLINE leWord32 #-}
leWord32 :: Peek Word32
leWord32 =
  {-# SCC "leWord32" #-}
  Peek 4 A.peekLEWord32

{-# INLINE leWord64 #-}
leWord64 :: Peek Word64
leWord64 =
  {-# SCC "leWord64" #-}
  Peek 8 A.peekLEWord64

-------------------------

{-# INLINE bytes #-}
bytes :: Int -> Peek ByteString
bytes !amount =
  {-# SCC "bytes" #-}
  Peek amount (\ptr -> A.peekBytes ptr amount)

{-# INLINE shortByteString #-}
shortByteString :: Int -> Peek ShortByteString
shortByteString !amount =
  {-# SCC "shortByteString" #-}
  Peek amount (\ptr -> A.peekShortByteString ptr amount)

{-# INLINE pokeAndPeek #-}
pokeAndPeek :: B.PokeAndPeek input output -> Peek output
pokeAndPeek (B.PokeAndPeek size _ io) =
  {-# SCC "pokeAndPeek" #-}
  Peek size io

-- |
-- Given the length of the data and a specification of its sequential consumption,
-- produces Peek, which results in Just the successfully taken value,
-- or Nothing, if the specified length of data wasn't enough.
{-# INLINE parse #-}
parse :: Int -> C.Parse a -> (Int -> a) -> (Text -> a) -> Peek a
parse amount (C.Parse parseIO) eoi error =
  {-# SCC "parse" #-}
  Peek amount $ \ptr ->
    parseIO amount ptr (return . eoi) (return . error) (\result _ _ -> return result)

-- |
-- Given the length of the data and a specification of its sequential consumption,
-- produces Peek, which results in Just the successfully taken value,
-- or Nothing, if the specified length of data wasn't enough.
{-# INLINE parseUnbound #-}
parseUnbound :: Int -> D.ParseUnbound a -> (Int -> a) -> (Text -> a) -> Peek a
parseUnbound sizeBound (D.ParseUnbound parseIO) eoi error =
  {-# SCC "parse" #-}
  Peek sizeBound $ \ptr ->
    parseIO
      ptr
      (return . error)
      ( \result size ->
          if size <= sizeBound
            then return (eoi (size - sizeBound))
            else return result
      )

-- |
-- A standard idiom, where a header specifies the length of the body.
--
-- Produces Peek, which itself produces another Peek, which is the same as the result of the 'parse' function.
{-# INLINE peekAmountAndParse #-}
peekAmountAndParse :: Peek Int -> C.Parse a -> (Int -> a) -> (Text -> a) -> Peek (Peek a)
peekAmountAndParse peekAmount parse_ eoi error =
  {-# SCC "peekAmountAndParse" #-}
  flip fmap peekAmount $ \amount ->
    parse amount parse_ eoi error
