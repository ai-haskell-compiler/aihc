module Text.PortableLines
    ( lines
    ) where

import Prelude hiding (lines)
import qualified Prelude as P

-- | Like the 'P.lines' function from Prelude, but treat the @\"\\r\\n\"@ and
--   @\"\\r\"@ sequences as newlines too, not just @\"\\n\"@.
lines :: String -> [String]
lines []  = []
lines str = let (a, str') = breakNewline str
             in a : lines str'

breakNewline :: String -> (String, String)
breakNewline []       = ([], [])
breakNewline (x : xs) =
    case x of
        '\n' -> ([], xs)
        '\r' -> ([], case xs of
                         ('\n' : xs') -> xs'
                         _            -> xs)
                -- The reason for the weird case expression instead of just a
                -- ('\r' : '\n' : s) pattern is for better laziness.
                -- Otherwise, lines ("hello\r" ++ undefined) would fail to
                -- completely yield the first line.  If we see a '\r', we know
                -- the line has ended, so don't force the next character
                -- immediately.
        _    -> let (line, rest) = breakNewline xs
                 in (x : line, rest)
