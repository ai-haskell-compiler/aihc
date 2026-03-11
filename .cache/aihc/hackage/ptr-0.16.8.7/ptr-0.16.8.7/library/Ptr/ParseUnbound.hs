module Ptr.ParseUnbound where

import qualified Data.ByteString.Char8 as B
import qualified Ptr.IO as D
import qualified Ptr.PokeAndPeek as A
import Ptr.Prelude hiding (peek, take)
import qualified Ptr.Prelude as C

-- |
-- Unbound parser, whose peeking action decides how much input to consume,
-- and merely informs the executor about how many bytes it consumed.
newtype ParseUnbound output
  = ParseUnbound (Ptr Word8 -> forall result. (Text -> IO result) -> (output -> Int -> IO result) -> IO result)

deriving instance Functor ParseUnbound

instance Applicative ParseUnbound where
  pure x =
    ParseUnbound (\ptr _ succeed -> succeed x 0)
  {-# INLINE (<*>) #-}
  (<*>) (ParseUnbound left) (ParseUnbound right) =
    ParseUnbound $ \ptr fail succeed ->
      left ptr fail $ \leftOutput leftSize ->
        right (plusPtr ptr leftSize) fail $ \rightOutput rightSize ->
          succeed (leftOutput rightOutput) (leftSize + rightSize)

instance Monad ParseUnbound where
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) (ParseUnbound left) rightK =
    ParseUnbound $ \ptr fail succeed ->
      left ptr fail $ \leftOutput leftSize ->
        case rightK leftOutput of
          ParseUnbound right ->
            right (plusPtr ptr leftSize) fail succeed

{-# INLINE fail #-}
fail :: Text -> ParseUnbound output
fail message =
  ParseUnbound $ \_ fail _ -> fail message

{-# INLINE io #-}
io :: Int -> (Ptr Word8 -> IO output) -> ParseUnbound output
io !size ptrIO =
  {-# SCC "io" #-}
  ParseUnbound $ \ptr fail succeed -> do
    !result <- ptrIO ptr
    succeed result size

{-# INLINE pokeAndPeek #-}
pokeAndPeek :: A.PokeAndPeek input output -> ParseUnbound output
pokeAndPeek (A.PokeAndPeek size _ ptrIO) =
  ParseUnbound $ \ptr fail succeed -> do
    !result <- ptrIO ptr
    succeed result size

{-# INLINE word8 #-}
word8 :: ParseUnbound Word8
word8 =
  {-# SCC "word8" #-}
  io 1 D.peekWord8

{-# INLINE beWord16 #-}
beWord16 :: ParseUnbound Word16
beWord16 =
  {-# SCC "beWord16" #-}
  io 2 D.peekBEWord16

{-# INLINE beWord32 #-}
beWord32 :: ParseUnbound Word32
beWord32 =
  {-# SCC "beWord32" #-}
  io 4 D.peekBEWord32

{-# INLINE beWord64 #-}
beWord64 :: ParseUnbound Word64
beWord64 =
  {-# SCC "beWord64" #-}
  io 8 D.peekBEWord64

{-# INLINE bytes #-}
bytes :: Int -> ParseUnbound ByteString
bytes amount =
  {-# SCC "bytes" #-}
  io amount (\ptr -> D.peekBytes ptr amount)

{-# INLINE nullTerminatedBytes #-}
nullTerminatedBytes :: ParseUnbound ByteString
nullTerminatedBytes =
  {-# SCC "nullTerminatedBytes" #-}
  ParseUnbound $ \ !ptr fail succeed -> do
    !bytes <- B.packCString (castPtr ptr)
    succeed bytes $! succ (B.length bytes)

{-# INLINE nullTerminatedShortByteString #-}
nullTerminatedShortByteString :: ParseUnbound ShortByteString
nullTerminatedShortByteString =
  {-# SCC "nullTerminatedShortByteString" #-}
  ParseUnbound $ \ !ptr fail succeed ->
    D.peekNullTerminatedShortByteString ptr $ \ !length create ->
      do
        !bytes <- create
        succeed bytes length

{-# INLINE bytesWhile #-}
bytesWhile :: (Word8 -> Bool) -> ParseUnbound ByteString
bytesWhile predicate =
  {-# SCC "bytesWhile" #-}
  ParseUnbound $ \ptr fail succeed ->
    let iterate !i =
          do
            byte <- C.peek (plusPtr ptr i)
            if predicate byte
              then iterate (succ i)
              else do
                bytes <- B.packCStringLen (castPtr ptr, i)
                succeed bytes i
     in iterate 0

{-# INLINE skipWhile #-}
skipWhile :: (Word8 -> Bool) -> ParseUnbound ()
skipWhile predicate =
  {-# SCC "skipWhile" #-}
  ParseUnbound $ \ptr fail succeed ->
    let iterate !i =
          do
            byte <- C.peek (plusPtr ptr i)
            if predicate byte
              then iterate (succ i)
              else succeed () i
     in iterate 0

{-# INLINE foldWhile #-}
foldWhile :: (Word8 -> Bool) -> (state -> Word8 -> state) -> state -> ParseUnbound state
foldWhile predicate step start =
  {-# SCC "foldWhile" #-}
  ParseUnbound $ \ptr fail succeed ->
    let iterate !state !i =
          do
            byte <- C.peek (plusPtr ptr i)
            if predicate byte
              then iterate (step state byte) (succ i)
              else succeed state i
     in iterate start 0

-- |
-- Unsigned integral number encoded in ASCII.
{-# INLINE unsignedASCIIIntegral #-}
unsignedASCIIIntegral :: (Integral a) => ParseUnbound a
unsignedASCIIIntegral =
  {-# SCC "unsignedASCIIIntegral" #-}
  foldWhile byteIsDigit step 0
  where
    byteIsDigit byte =
      byte - 48 <= 9
    step !state !byte =
      state * 10 + fromIntegral byte - 48
