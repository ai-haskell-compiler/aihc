module Text.PortableLines.ByteString.Lazy
    ( lines8
    ) where

import Prelude as P hiding (lines)

import Data.ByteString.Lazy (ByteString)

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8

-- | Like the 'L8.lines' function from Data.ByteString.Lazy.Char8, but treat the
-- @\"\\r\\n\"@ and @\"\\r\"@ sequences as newlines too, not just @\"\\n\"@.
--
-- Input is assumed to be in ASCII or an ASCII-compatible encoding (at least
-- with respect to newline characters).  For example, UTF-8 is fine, but UTF-16
-- is not.
lines8 :: ByteString -> [ByteString]
lines8 str | L.null str = []
           | otherwise  = let (line, rest) = breakNewline8 str
                           in line : lines8 rest

breakNewline8 :: ByteString -> (ByteString, ByteString)
breakNewline8 str =
    case L.break (\c -> c == 13 || c == 10) str of
        (line, rest) | L.null rest                -> (line, rest)
                     | L.length rest >= 2 &&
                       rest `L.index` 0 == 13 &&
                       rest `L.index` 1 == 10     -> (line, L.drop 2 rest)
                     | otherwise                  -> (line, L.tail rest)
