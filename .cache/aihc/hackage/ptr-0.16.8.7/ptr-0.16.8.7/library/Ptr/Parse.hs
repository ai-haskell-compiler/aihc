module Ptr.Parse where

import qualified Data.ByteString.Char8 as B
import qualified Ptr.IO as D
import qualified Ptr.PokeAndPeek as A
import Ptr.Prelude hiding (peek, take)
import qualified Ptr.Prelude as C

newtype Parse output
  = Parse (Int -> Ptr Word8 -> forall result. (Int -> IO result) -> (Text -> IO result) -> (output -> Int -> Ptr Word8 -> IO result) -> IO result)

deriving instance Functor Parse

instance Applicative Parse where
  pure x =
    Parse (\availableAmount ptr _ _ succeed -> succeed x availableAmount ptr)
  {-# INLINE (<*>) #-}
  (<*>) (Parse left) (Parse right) =
    Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
      left availableAmount ptr failWithEOI failWithMessage $ \leftOutput !leftAvailableAmount !leftPtr ->
        right leftAvailableAmount leftPtr failWithEOI failWithMessage $ \rightOutput !rightAvailableAmount !rightPtr ->
          succeed (leftOutput rightOutput) rightAvailableAmount rightPtr

instance Alternative Parse where
  empty =
    Parse (\_ _ failWithEOI _ _ -> failWithEOI 0)
  {-# INLINE (<|>) #-}
  (<|>) (Parse left) (Parse right) =
    Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
      left
        availableAmount
        ptr
        (\_ -> right availableAmount ptr failWithEOI failWithMessage succeed)
        failWithMessage
        succeed

instance Monad Parse where
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) (Parse left) rightK =
    Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
      left availableAmount ptr failWithEOI failWithMessage $ \leftOutput !leftAvailableAmount !leftPtr ->
        case rightK leftOutput of
          Parse right ->
            right leftAvailableAmount leftPtr failWithEOI failWithMessage succeed

instance MonadPlus Parse where
  mzero = empty
  mplus = (<|>)

instance MonadIO Parse where
  {-# INLINE liftIO #-}
  liftIO io =
    Parse $ \availableAmount ptr _ _ succeed -> io >>= \output -> succeed output availableAmount ptr

{-# INLINE fail #-}
fail :: Text -> Parse output
fail message =
  Parse $ \_ _ _ failWithMessage _ -> failWithMessage message

{-# INLINE io #-}
io :: Int -> (Ptr Word8 -> IO output) -> Parse output
io !requiredAmount ptrIO =
  {-# SCC "io" #-}
  Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
    if availableAmount >= requiredAmount
      then do
        !result <- ptrIO ptr
        succeed result (availableAmount - requiredAmount) (plusPtr ptr requiredAmount)
      else failWithEOI (requiredAmount - availableAmount)

{-# INLINE mapInIO #-}
mapInIO :: (output -> IO newOutput) -> Parse output -> Parse newOutput
mapInIO io (Parse parseIO) =
  Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
    parseIO
      availableAmount
      ptr
      failWithEOI
      failWithMessage
      (\output newAvailableAmount newPtr -> io output >>= \newOutput -> succeed newOutput newAvailableAmount newPtr)

{-# INLINE pokeAndPeek #-}
pokeAndPeek :: A.PokeAndPeek input output -> Parse output
pokeAndPeek (A.PokeAndPeek requiredAmount _ ptrIO) =
  Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
    if availableAmount >= requiredAmount
      then do
        !result <- ptrIO ptr
        succeed result (availableAmount - requiredAmount) (plusPtr ptr requiredAmount)
      else failWithEOI (requiredAmount - availableAmount)

{-# INLINE limiting #-}
limiting :: Int -> Parse output -> Parse output
limiting limitAmount (Parse io) =
  Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
    if availableAmount >= limitAmount
      then io limitAmount ptr failWithEOI failWithMessage succeed
      else failWithEOI (limitAmount - availableAmount)

-- |
-- Decode the remaining bytes, whithout moving the parser's cursor.
-- Useful for debugging.
{-# INLINE peekRemainders #-}
peekRemainders :: Parse ByteString
peekRemainders =
  {-# SCC "peekRemainders" #-}
  Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed -> do
    !bytes <- D.peekBytes ptr availableAmount
    succeed bytes availableAmount ptr

{-# INLINE word8 #-}
word8 :: Parse Word8
word8 =
  {-# SCC "word8" #-}
  io 1 D.peekWord8

{-# INLINE beWord16 #-}
beWord16 :: Parse Word16
beWord16 =
  {-# SCC "beWord16" #-}
  io 2 D.peekBEWord16

{-# INLINE beWord32 #-}
beWord32 :: Parse Word32
beWord32 =
  {-# SCC "beWord32" #-}
  io 4 D.peekBEWord32

{-# INLINE beWord64 #-}
beWord64 :: Parse Word64
beWord64 =
  {-# SCC "beWord64" #-}
  io 8 D.peekBEWord64

{-# INLINE bytes #-}
bytes :: Int -> Parse ByteString
bytes amount =
  {-# SCC "bytes" #-}
  io amount (\ptr -> D.peekBytes ptr amount)

{-# INLINE allBytes #-}
allBytes :: Parse ByteString
allBytes =
  {-# SCC "allBytes" #-}
  Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed -> do
    !bytes <- D.peekBytes ptr availableAmount
    succeed bytes 0 (plusPtr ptr availableAmount)

{-# INLINE nullTerminatedBytes #-}
nullTerminatedBytes :: Parse ByteString
nullTerminatedBytes =
  {-# SCC "nullTerminatedBytes" #-}
  Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed -> do
    !bytes <- B.packCString (castPtr ptr)
    case succ (B.length bytes) of
      consumedAmount ->
        if consumedAmount <= availableAmount
          then succeed bytes (availableAmount - consumedAmount) (plusPtr ptr consumedAmount)
          else failWithEOI (consumedAmount - availableAmount)

{-# INLINE nullTerminatedShortByteString #-}
nullTerminatedShortByteString :: Parse ShortByteString
nullTerminatedShortByteString =
  {-# SCC "nullTerminatedShortByteString" #-}
  Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
    D.peekNullTerminatedShortByteString ptr $ \length create ->
      if length <= availableAmount
        then do
          !result <- create
          succeed result (availableAmount - length) (plusPtr ptr length)
        else failWithEOI (length - availableAmount)

{-# INLINE bytesWhile #-}
bytesWhile :: (Word8 -> Bool) -> Parse ByteString
bytesWhile predicate =
  {-# SCC "bytesWhile" #-}
  Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
    let iterate !availableAmount !unconsumedAmount !currentPtr =
          if unconsumedAmount > 0
            then do
              byte <- C.peek currentPtr
              if predicate byte
                then iterate availableAmount (pred unconsumedAmount) (plusPtr currentPtr 1)
                else do
                  bytes <- B.packCStringLen (castPtr ptr, availableAmount - unconsumedAmount)
                  succeed bytes unconsumedAmount currentPtr
            else do
              bytes <- B.packCStringLen (castPtr ptr, availableAmount - unconsumedAmount)
              succeed bytes unconsumedAmount currentPtr
     in iterate availableAmount availableAmount ptr

{-# INLINE skipWhile #-}
skipWhile :: (Word8 -> Bool) -> Parse ()
skipWhile predicate =
  {-# SCC "skipWhile" #-}
  Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
    let iterate !availableAmount !unconsumedAmount !ptr =
          if unconsumedAmount > 0
            then do
              byte <- C.peek ptr
              if predicate byte
                then iterate availableAmount (pred unconsumedAmount) (plusPtr ptr 1)
                else succeed () unconsumedAmount ptr
            else succeed () unconsumedAmount ptr
     in iterate availableAmount availableAmount ptr

{-# INLINE foldWhile #-}
foldWhile :: (Word8 -> Bool) -> (state -> Word8 -> state) -> state -> Parse state
foldWhile predicate step start =
  {-# SCC "foldWhile" #-}
  Parse $ \ !availableAmount !ptr failWithEOI failWithMessage succeed ->
    let iterate !state !unconsumedAmount !ptr =
          if unconsumedAmount > 0
            then do
              byte <- C.peek ptr
              if predicate byte
                then iterate (step state byte) (pred unconsumedAmount) (plusPtr ptr 1)
                else succeed state unconsumedAmount ptr
            else failWithEOI 0
     in iterate start availableAmount ptr

-- |
-- Unsigned integral number encoded in ASCII.
{-# INLINE unsignedASCIIIntegral #-}
unsignedASCIIIntegral :: (Integral a) => Parse a
unsignedASCIIIntegral =
  {-# SCC "unsignedASCIIIntegral" #-}
  foldWhile byteIsDigit step 0
  where
    byteIsDigit byte =
      byte - 48 <= 9
    step !state !byte =
      state * 10 + fromIntegral byte - 48
