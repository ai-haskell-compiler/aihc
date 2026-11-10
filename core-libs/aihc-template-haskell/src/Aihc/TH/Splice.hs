-- | Run a splice in the guest of an engine image.
--
-- The compiler runs a splice in a WebAssembly instance. This module is the
-- part of that work that is in the instance. The compiler makes a closure
-- that applies 'runSplice' to the body of the splice, and the guest runs
-- the closure. The result goes to the compiler as one message.
--
-- The message is a tree. Each value of an algebraic data type is a node
-- with the name of its constructor and one child for each field. Thus, the
-- message has the same constructors as the Template Haskell syntax tree,
-- and this module has no knowledge of them.
--
-- The encoding of a tree is:
--
-- > tree     = 0 text count tree*   a constructor and its fields
-- >          | 1 text               an integer, in decimal
-- >          | 2 count              a character, as its code point
-- >          | 3 text               a string
-- >          | 4 text text          a ratio: numerator, denominator
-- > text     = count byte*          UTF-8, with the number of bytes
-- > count    = an unsigned LEB128 number
module Aihc.TH.Splice
  ( runSplice,
  )
where

import Data.Bits (shiftR, (.&.), (.|.))
import Data.Char (ord)
import Data.Data (ConstrRep (..), Data, cast, constrRep, gmapQ, showConstr, toConstr)
import Data.Ratio (denominator, numerator)
import Data.Word (Word8)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Ptr (Ptr)
import Language.Haskell.TH.Syntax (Q, runQ)

-- | Send a message to the host.
foreign import ccall unsafe "aihc_engine_send" engineSend :: Ptr Word8 -> Int -> IO ()

-- | Run a splice and send its result to the host.
runSplice :: (Data a) => Q a -> IO ()
runSplice splice = do
  result <- runQ splice
  withArrayLen (encodeTree result []) (flip engineSend)

-- | The bytes of a value, before the bytes that follow it.
encodeTree :: (Data a) => a -> [Word8] -> [Word8]
encodeTree value =
  case cast value of
    Just string -> (3 :) . encodeText string
    Nothing ->
      case constrRep constructor of
        AlgConstr _ -> (0 :) . encodeText (showConstr constructor) . encodeCount (length fields) . foldr (.) id fields
        IntConstr number -> (1 :) . encodeText (show number)
        CharConstr char -> (2 :) . encodeCount (ord char)
        FloatConstr ratio -> (4 :) . encodeText (show (numerator ratio)) . encodeText (show (denominator ratio))
  where
    constructor = toConstr value
    fields = gmapQ encodeTree value

encodeText :: String -> [Word8] -> [Word8]
encodeText string rest = encodeCount (length bytes) (bytes ++ rest)
  where
    bytes = concatMap utf8 string

encodeCount :: Int -> [Word8] -> [Word8]
encodeCount number rest
  | number < 128 = fromIntegral number : rest
  | otherwise = fromIntegral ((number .&. 127) .|. 128) : encodeCount (number `shiftR` 7) rest

-- | The UTF-8 bytes of a character.
utf8 :: Char -> [Word8]
utf8 char
  | code < 0x80 = [fromIntegral code]
  | code < 0x800 = [lead 0xC0 6, continuation 0]
  | code < 0x10000 = [lead 0xE0 12, continuation 6, continuation 0]
  | otherwise = [lead 0xF0 18, continuation 12, continuation 6, continuation 0]
  where
    code = ord char
    lead prefix shift = fromIntegral (prefix .|. (code `shiftR` shift))
    continuation shift = fromIntegral (0x80 .|. ((code `shiftR` shift) .&. 0x3F))
