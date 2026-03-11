module Regex.Internal.CharSets
  ( digit
  , word
  , space
  , ascii
  , asciiAlpha
  , asciiUpper
  , asciiLower
  ) where

import Regex.Internal.CharSet (CharSet)
import qualified Regex.Internal.CharSet as CS

-- | ASCII digits. @\'0\'..\'9\'@. Agrees with 'Data.Char.isDigit'.
digit :: CharSet
digit = CS.fromRange ('0','9')

-- | ASCII alphabet, digits and underscore.
-- @\'A\'..\'Z\',\'a\'..\'z\',\'0\'..\'9\',\'_\'@.
word :: CharSet
word = asciiUpper <> asciiLower <> digit <> CS.singleton '_'

-- | Unicode space characters and the control characters
-- @\'\\t\',\'\\n\',\'\\r\',\'\\f\',\'\\v\'@.
-- Agrees with 'Data.Char.isSpace'.
space :: CharSet
space = CS.fromList "\t\n\r\f\v"
     <> CS.fromList "\x0020\x00A0\x1680\x202F\x205F\x3000"
     <> CS.fromRange ('\x2000','\x200A')

-- | ASCII @Char@s. @\'\\0\'..\'\\127\'@. Agrees with 'Data.Char.isAscii'.
ascii :: CharSet
ascii = CS.fromRange ('\0','\127')

-- | ASCII alphabet. @\'A\'..\'Z\',\'a\'..\'z\'@.
asciiAlpha :: CharSet
asciiAlpha = asciiUpper <> asciiLower

-- | ASCII uppercase @Char@s. @\'A\'..\'Z\'@. Agrees with
-- 'Data.Char.isAsciiUpper'.
asciiUpper :: CharSet
asciiUpper = CS.fromRange ('A','Z')

-- | ASCII lowercase @Char@s. @\'a\'..\'z\'@. Agrees with
-- 'Data.Char.isAsciiLower'.
asciiLower :: CharSet
asciiLower = CS.fromRange ('a','z')
