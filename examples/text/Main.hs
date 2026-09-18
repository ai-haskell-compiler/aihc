-- The character routines of text reach C through @size_t@, which is four
-- bytes on wasm32 while an @Int@ is eight, so a count that crosses that
-- boundary can wrap. @Data.Text.length@ is @negate . measureOff maxBound@
-- and measures against exactly such a count.
--
-- A 'Data.Text.Text' that holds no ASCII prints through @show@; one that
-- does prints as its UTF-8 bytes, because aihc's @showLitChar@ does not
-- escape a character above @\\DEL@ the way GHC's does, and this example is
-- diffed against GHC. Byte lists also keep the output ASCII, so it does not
-- depend on the encoding of the standard output handle either.
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
  -- length is negate . measureOff maxBound, so it is the one that needs the
  -- bound it measures against to survive the trip through size_t.
  print (T.length hello, T.length ascii, T.length wide)
  print (B.length (TE.encodeUtf8 ascii), B.length (TE.encodeUtf8 wide))
  print (ascii, T.splitAt 2 hello, T.reverse hello)
  print (bytes (T.take 4 wide))
  print (bytes (T.drop 6 wide))
  print (T.isInfixOf (T.pack "caf") wide, T.isInfixOf (T.pack "fac") wide)
  print (T.all (< '\x80') hello, T.all (< '\x80') wide)
