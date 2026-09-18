-- The character routines of text reach C through @size_t@, which is four
-- bytes on wasm32 while an @Int@ is eight, so a count that crosses that
-- boundary can wrap. @Data.Text.length@ is @negate . measureOff maxBound@
-- and measures against exactly such a count.
--
-- Nothing here prints a 'Data.Text.Text' or a 'String' built from one:
-- 'Data.Text.unpack' reaches @noDuplicate#@, which the native runtime does
-- not implement. Byte lists say the same thing.
module Main where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

bytes :: T.Text -> [Word]
bytes = map fromIntegral . B.unpack . TE.encodeUtf8

main :: IO ()
main = do
  let hello = T.pack "hello"
      -- naive cafe lambda, with the accented letters and the lambda outside
      -- ASCII, so that a character count and a byte count differ.
      wide = T.pack "na\239ve caf\233 \955"
      ascii = TE.decodeUtf8 (B.pack [0x41, 0x42, 0x43])
  print (T.length hello, T.length ascii, T.length wide)
  print (B.length (TE.encodeUtf8 ascii), B.length (TE.encodeUtf8 wide))
  print (bytes (T.take 4 wide))
  print (bytes (T.drop 6 wide))
  print (bytes (T.reverse hello))
  print (T.splitAt 2 hello == (T.pack "he", T.pack "llo"))
  print (T.isInfixOf (T.pack "caf") wide, T.isInfixOf (T.pack "fac") wide)
