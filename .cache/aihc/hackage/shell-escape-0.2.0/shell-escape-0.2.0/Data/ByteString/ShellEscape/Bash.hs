
module Data.ByteString.ShellEscape.Bash where

import Data.Maybe
import Data.Char
import Text.Printf
import Data.ByteString (ByteString)

import qualified Data.Vector as Vector

import Data.ByteString.ShellEscape.Escape
import qualified Data.ByteString.ShellEscape.Put as Put
import Data.ByteString.ShellEscape.EscapeVector

{-| A Bash escaped 'ByteString'. The strings are wrapped in @$\'...\'@ if any
    bytes within them must be escaped; otherwise, they are left as is.
    Newlines and other control characters are represented as ANSI escape
    sequences. High bytes are represented as hex codes. Thus Bash escaped
    strings will always fit on one line and never contain non-ASCII bytes.
 -}
newtype Bash                 =  Bash (EscapeVector EscapingMode)
 deriving (Eq, Ord, Show)

{-| Construct a Bash escaped intermediate form.
 -}
bash                        ::  ByteString -> Bash
bash                         =  escape

instance Escape Bash where
  escape                     =  Bash . escWith classify
  unescape (Bash v)          =  stripEsc v
  bytes (Bash v) | literal v =  stripEsc v
                 | otherwise =  interpretEsc v renderANSI' end (begin, Literal)
   where
    literal                  =  isNothing . Vector.find ((/= Literal) . snd)
    begin                    =  [      Put.putString "$'"]
    end                      =  const (Put.putChar '\'')
    renderANSI' _ (c, e)     =  (renderANSI c, e)


{-| Bash escaping modes.
 -}
data EscapingMode            =  ANSIHex | ANSIBackslash | Literal | Quoted
 deriving (Eq, Ord, Show)

renderANSI c =
  case classify c of
    Literal                 ->  Put.putChar     c
    Quoted                  ->  Put.putChar     c
    ANSIHex                 ->  Put.putString $ hexify c
    ANSIBackslash           ->  Put.putString $ backslashify c

backslashify                ::  Char -> String
backslashify c               =  (take 2 . drop 1 . show) c

hexify                      ::  Char -> String
hexify                       =  printf "\\x%02X" . ord

classify                    ::  Char -> EscapingMode
classify c | c <= '\ACK'     =  ANSIHex
           | c <= '\r'       =  ANSIBackslash
           | c <= '\US'      =  ANSIHex
           | c <= '&'        =  Quoted
           | c == '\''       =  ANSIBackslash
           | c <= '+'        =  Quoted
           | c <= '9'        =  Literal
           | c <= '?'        =  Quoted
           | c <= 'Z'        =  Literal
           | c == '['        =  Quoted
           | c == '\\'       =  ANSIBackslash
           | c <= ']'        =  Quoted
           | c == '_'        =  Literal
           | c <= '`'        =  Quoted
           | c <= 'z'        =  Literal
           | c <= '~'        =  Quoted
           | c == '\DEL'     =  ANSIHex
           | otherwise       =  ANSIHex

