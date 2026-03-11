
module Data.ByteString.ShellEscape.Sh where

import Data.ByteString (ByteString)

import Data.ByteString.ShellEscape.Escape
import qualified Data.ByteString.ShellEscape.Put as Put
import Data.ByteString.ShellEscape.EscapeVector


{-| A Bourne Shell escaped 'ByteString'. An oddity of Bourne shell escaping is
    the absence of escape codes for newline and other ASCII control
    characters. These bytes are simply placed literally in single quotes; the
    effect is that a Bourne Shell escaped string may cover several lines and
    contain non-ASCII bytes. Runs of bytes that must be escaped are wrapped in
    @\'...\'@; bytes that are acceptable as literals in Bourne Shell are left
    as is.
 -}
newtype Sh                   =  Sh (EscapeVector EscapingMode)
 deriving (Eq, Ord, Show)

{-| Construct a Bourne Shell escaped intermediate form.
 -}
sh                          ::  ByteString -> Sh
sh                           =  escape

instance Escape Sh where
  escape                     =  Sh . escWith classify
  unescape (Sh v)            =  stripEsc v
  bytes (Sh v)               =  interpretEsc v act finish ([], Literal)
   where
    finish Quote             =  Put.putChar '\''
    finish Backslash         =  Put.putChar '\\'
    finish Literal           =  return ()


{-| Accept the present escaping mode and desired escaping mode and yield an
    action and the resulting mode.
 -}
act :: EscapingMode -> (Char, EscapingMode) -> (Put.Put, EscapingMode)
act Quote (c, Quote)         =  (Put.putChar c                 , Quote)
act Quote (c, Literal)       =  (Put.putChar c                 , Quote)
act Quote (c, Backslash)     =  (Put.putString ['\'', '\\', c] , Literal)
act Backslash (c, Backslash) =  (Put.putChar c                 , Literal)
act Backslash (c, Quote)     =  (Put.putString ['\\', '\'', c] , Quote)
act Backslash (c, Literal)   =  (Put.putString ['\\', c]       , Literal)
act Literal (c, Literal)     =  (Put.putChar c                 , Literal)
act Literal (c, Backslash)   =  (Put.putString ['\\', c]       , Literal)
act Literal (c, Quote)       =  (Put.putString ['\'', c]       , Quote)

classify                    ::  Char -> EscapingMode
classify c | c <= '&'        =  Quote           --  0x00..0x26
           | c == '\''       =  Backslash       --  0x27
           | c <= ','        =  Quote           --  0x28..0x2c
           | c <= '9'        =  Literal         --  0x2d..0x39
           | c <= '?'        =  Quote           --  0x3a..0x3f
           | c <= 'Z'        =  Literal         --  0x40..0x5a
           | c <= '^'        =  Quote           --  0x5b..0x5e
           | c == '_'        =  Literal         --  0x5f
           | c == '`'        =  Quote           --  0x60
           | c <= 'z'        =  Literal         --  0x61..0x7a
           | c <= '\DEL'     =  Quote           --  0x7b..0x7f
           | otherwise       =  Quote           --  0x80..0xff

{-| Bourne Shell escaping modes. 
 -}
data EscapingMode            =  Backslash | Literal | Quote
 deriving (Eq, Ord, Show)

