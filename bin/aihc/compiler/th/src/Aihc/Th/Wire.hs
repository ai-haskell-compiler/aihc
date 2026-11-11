{-# LANGUAGE OverloadedStrings #-}

-- | The messages between a splice and the compiler.
--
-- A splice runs in a WebAssembly instance, and its result is a value of
-- the Template Haskell syntax tree. The module @Aihc.TH.Splice@ of the
-- @template-haskell@ package encodes that value as a tree of constructor
-- names, and this module decodes the tree. That module gives the encoding.
-- The reply to a request of a splice has the same encoding, and this
-- module encodes it.
--
-- The message comes from code that the compiler does not trust. The decoder
-- reads only the bytes of the message, and it rejects a message that is
-- not complete or that has more bytes than its tree.
module Aihc.Th.Wire
  ( Tree (..),
    decodeTree,
    encodeTree,
  )
where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, ord)
import Data.Ratio (denominator, numerator, (%))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Text.Read (readMaybe)

-- | A value of an algebraic data type or a primitive value.
data Tree
  = -- | A constructor and its fields.
    TreeNode !Text ![Tree]
  | TreeInteger !Integer
  | TreeChar !Char
  | TreeString !Text
  | TreeRatio !Rational
  deriving (Eq, Show)

-- | The bytes of a tree, in the encoding that the decoder reads.
encodeTree :: Tree -> ByteString
encodeTree = BL.toStrict . Builder.toLazyByteString . go
  where
    go tree =
      case tree of
        TreeNode name fields -> Builder.word8 0 <> text name <> count (length fields) <> foldMap go fields
        TreeInteger number -> Builder.word8 1 <> text (T.pack (show number))
        TreeChar char -> Builder.word8 2 <> count (ord char)
        TreeString string -> Builder.word8 3 <> text string
        TreeRatio ratio -> Builder.word8 4 <> text (T.pack (show (numerator ratio))) <> text (T.pack (show (denominator ratio)))
    text string =
      let bytes = TE.encodeUtf8 string
       in count (BS.length bytes) <> Builder.byteString bytes
    count number
      | number < 128 = Builder.word8 (fromIntegral number)
      | otherwise = Builder.word8 (fromIntegral ((number .&. 127) .|. 128)) <> count (number `shiftR` 7)

-- | A decoder gives a value and the bytes after it.
type Decoder value = ByteString -> Either Text (value, ByteString)

-- | Decode a message that holds one tree.
decodeTree :: ByteString -> Either Text Tree
decodeTree bytes = do
  (result, rest) <- treeDecoder bytes
  if BS.null rest
    then Right result
    else Left "The splice result has bytes after its tree."

treeDecoder :: Decoder Tree
treeDecoder bytes =
  case BS.uncons bytes of
    Nothing -> incomplete
    Just (0, rest) -> do
      (name, afterName) <- textDecoder rest
      (fieldCount, afterCount) <- countDecoder afterName
      (fields, afterFields) <- trees fieldCount afterCount
      Right (TreeNode name fields, afterFields)
    Just (1, rest) -> do
      (number, afterNumber) <- integerDecoder rest
      Right (TreeInteger number, afterNumber)
    Just (2, rest) -> do
      (code, afterCode) <- countDecoder rest
      if code > 0x10FFFF
        then Left "The splice result has a character that is not valid."
        else Right (TreeChar (chr code), afterCode)
    Just (3, rest) -> do
      (string, afterString) <- textDecoder rest
      Right (TreeString string, afterString)
    Just (4, rest) -> do
      (top, afterTop) <- integerDecoder rest
      (bottom, afterBottom) <- integerDecoder afterTop
      if bottom == 0
        then Left "The splice result has a ratio with the denominator zero."
        else Right (TreeRatio (top % bottom), afterBottom)
    Just (tag, _) -> Left ("The splice result has the unknown tag " <> T.pack (show tag) <> ".")

-- | A number of trees. Each tree has one byte or more, so a count that is
-- larger than the number of bytes is not valid.
trees :: Int -> Decoder [Tree]
trees number bytes
  | number > BS.length bytes = incomplete
  | otherwise = go number [] bytes
  where
    go remaining done rest
      | remaining == 0 = Right (reverse done, rest)
      | otherwise = do
          (next, afterNext) <- treeDecoder rest
          go (remaining - 1 :: Int) (next : done) afterNext

integerDecoder :: Decoder Integer
integerDecoder bytes = do
  (digits, rest) <- textDecoder bytes
  case readMaybe (T.unpack digits) of
    Just number -> Right (number, rest)
    Nothing -> Left "The splice result has an integer that is not valid."

textDecoder :: Decoder Text
textDecoder bytes = do
  (size, rest) <- countDecoder bytes
  if size > BS.length rest
    then incomplete
    else case TE.decodeUtf8' (BS.take size rest) of
      Right string -> Right (string, BS.drop size rest)
      Left _ -> Left "The splice result has text that is not valid UTF-8."

-- | An unsigned LEB128 number of 28 bits or less.
countDecoder :: Decoder Int
countDecoder = go (0 :: Int) 0
  where
    go shift number bytes =
      case BS.uncons bytes of
        Nothing -> incomplete
        Just (byte, rest)
          | shift > 21 -> Left "The splice result has a count that is too large."
          | byte >= 128 -> go (shift + 7) (number .|. (fromIntegral (byte .&. 127) `shiftL` shift)) rest
          | otherwise -> Right (number .|. (fromIntegral byte `shiftL` shift), rest)

incomplete :: Either Text value
incomplete = Left "The splice result is not complete."
