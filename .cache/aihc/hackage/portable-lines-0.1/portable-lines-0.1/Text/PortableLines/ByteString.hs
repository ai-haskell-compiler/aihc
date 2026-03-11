module Text.PortableLines.ByteString
    ( lines8
    ) where

import Prelude as P hiding (lines)

import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Unsafe (unsafeIndex, unsafeTail, unsafeDrop)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

-- | Like the 'B8.lines' function from Data.ByteString.Char8, but treat the
-- @\"\\r\\n\"@ and @\"\\r\"@ sequences as newlines too, not just @\"\\n\"@.
--
-- Input is assumed to be in ASCII or an ASCII-compatible encoding (at least
-- with respect to newline characters).  For example, UTF-8 is fine, but UTF-16
-- is not.
lines8 :: ByteString -> [ByteString]
lines8 str | B.null str = []
           | otherwise  = let (line, rest) = breakNewline8 str
                           in line : lines8 rest

breakNewline8 :: ByteString -> (ByteString, ByteString)
breakNewline8 str =
    case B.break (\c -> c == 13 || c == 10) str of
        (line, rest) | B.null rest                      -> (line, rest)
                     | B.length rest >= 2 &&
                       rest `unsafeIndex` 0 == 13 &&
                       rest `unsafeIndex` 1 == 10       -> (line, unsafeDrop 2 rest)
                     | otherwise                        -> (line, unsafeTail rest)
