-- | This module provides pattern synonyms for 'Char' in [Basic Latin block](https://compart.com/en/unicode/block/U+0000) (range @'\x00'-'\x7f'@).
--
-- The pattern names are inspired by Unicode (letter and digits) and PostScript names (symbols),
-- e.g. @/@ is 'SLASH', not @SOLIDUS@.
--
-- This module is designed to be imported qualified
--
-- @
-- import "Data.Char.Patterns" as C
--
-- hello = [C.'LOWER_H', C.'LOWER_E', C.'LOWER_L', C.'LOWER_L', C.'LOWER_O']
-- @
--
-- but can also be used unqualified as well.
--
module Data.Char.Patterns where

-------------------------------------------------------------------------------
-- * Control characters, x00-x1f
-------------------------------------------------------------------------------

-- | Null character (NUL)
pattern NUL :: Char
pattern NUL = '\x00'

-- | Start of Heading (SOH)
pattern SOH :: Char
pattern SOH = '\x01'

-- | Start of Text (STX)
pattern STX :: Char
pattern STX = '\x02'

-- | End of Text (ETX)
pattern ETX :: Char
pattern ETX = '\x03'

-- | End of Transmission (EOT)
pattern EOT :: Char
pattern EOT = '\x04'

-- | Enquiry (ENQ)
pattern ENQ :: Char
pattern ENQ = '\x05'

-- | Acknowledge (ACK)
pattern ACK :: Char
pattern ACK = '\x06'

-- | Alert (BEL)
pattern BEL :: Char
pattern BEL = '\x07'

-- | Backspace (BS)
pattern BS :: Char
pattern BS = '\x08'

-- | Character tabulation (TAB)
pattern TAB :: Char
pattern TAB = '\x09'

-- | End of line, line feed (LF)
pattern LF :: Char
pattern LF = '\x0a'

-- | Line tabulation, vertical tab (VT)
pattern VT :: Char
pattern VT = '\x0b'

-- | Form feed (FF)
pattern FF :: Char
pattern FF = '\x0c'

-- | Carriage Return (CR)
pattern CR :: Char
pattern CR = '\x0d'

-- | Locking-Shift One, Shift Out (SO)
pattern SO :: Char
pattern SO = '\x0e'

-- | Locking-Shift Zero, Shift In (SI)
pattern SI :: Char
pattern SI = '\x0f'

-- | Data Link Escape (DLE)
pattern DLE :: Char
pattern DLE = '\x10'

-- | Device Control One, XON (DC1)
pattern DC1 :: Char
pattern DC1 = '\x11'

-- | Device Control Two (DC2)
pattern DC2 :: Char
pattern DC2 = '\x12'

-- | Device Control Three, XOFF (DC3)
pattern DC3 :: Char
pattern DC3 = '\x13'

-- | Device Control Four (DC4)
pattern DC4 :: Char
pattern DC4 = '\x14'

-- | Negative Acknowledge (NAK)
pattern NAK :: Char
pattern NAK = '\x15'

-- | Syncronous Idle (SYN)
pattern SYN :: Char
pattern SYN = '\x16'

-- | End of Transmission Block (ETB)
pattern ETB :: Char
pattern ETB = '\x17'

-- | Cancel (CAN)
pattern CAN :: Char
pattern CAN = '\x18'

-- | End of Medium (EOM)
pattern EOM :: Char
pattern EOM = '\x19'

-- | Substitute (SUB)
pattern SUB :: Char
pattern SUB = '\x1a'

-- | Escape (ESC)
pattern ESC :: Char
pattern ESC = '\x1b'

-- | File Separator (FS)
pattern FS :: Char
pattern FS = '\x1c'

-- | Group Separator (GS)
pattern GS :: Char
pattern GS = '\x1d'

-- | Information Separator One, Record Separator (RS)
pattern RS :: Char
pattern RS = '\x1e'

-- | Information Separator Two, Unit Separator (US)
pattern US :: Char
pattern US = '\x1f'

-------------------------------------------------------------------------------
-- * Symbols 1, x20-x2f
-------------------------------------------------------------------------------

-- | Space (SP), @' '@.
pattern SPACE :: Char
pattern SPACE = '\x20'

-- | Exclamation mark, @!@
pattern EXCLAM :: Char
pattern EXCLAM = '\x21'

-- | Quotation mark, double quote, @"@
pattern DOUBLE_QUOTE :: Char
pattern DOUBLE_QUOTE = '\x22'

-- | Number sign, @#@
pattern NUMBER :: Char
pattern NUMBER = '\x23'

-- | Dollar sign, @$@
pattern DOLLAR :: Char
pattern DOLLAR = '\x24'

-- | Percent sign, @%@
pattern PERCENT :: Char
pattern PERCENT = '\x25'

-- | Ampersand, @&@
pattern AMPERSAND :: Char
pattern AMPERSAND = '\x26'

-- | Apostrophe, single quote, @'@
pattern SINGLE_QUOTE :: Char
pattern SINGLE_QUOTE = '\x27'

-- | Left parenthesis, @(@
pattern LEFT_PAREN :: Char
pattern LEFT_PAREN = '\x28'

-- | Right parenthesis, @)@
pattern RIGHT_PAREN :: Char
pattern RIGHT_PAREN = '\x29'

-- | Asterisk, @*@
pattern ASTERISK :: Char
pattern ASTERISK = '\x2a'

-- | Plus sign, @+@
pattern PLUS :: Char
pattern PLUS = '\x2b'

-- | Comma, @,@
pattern COMMA :: Char
pattern COMMA = '\x2c'

-- | Hyphen-minus, @-@
pattern HYPHEN :: Char
pattern HYPHEN = '\x2d'

-- | Full stop, period, @.@
pattern PERIOD :: Char
pattern PERIOD = '\x2e'

-- | Solidus, slash, @/@
pattern SLASH :: Char
pattern SLASH = '\x2f'

-------------------------------------------------------------------------------
-- * Digits, x30-x39
-------------------------------------------------------------------------------

-- | Digit 0, @0@
pattern DIGIT_0 :: Char
pattern DIGIT_0 = '\x30'

-- | Digit 1, @1@
pattern DIGIT_1 :: Char
pattern DIGIT_1 = '\x31'

-- | Digit 2, @2@
pattern DIGIT_2 :: Char
pattern DIGIT_2 = '\x32'

-- | Digit 3, @3@
pattern DIGIT_3 :: Char
pattern DIGIT_3 = '\x33'

-- | Digit 4, @4@
pattern DIGIT_4 :: Char
pattern DIGIT_4 = '\x34'

-- | Digit 5, @5@
pattern DIGIT_5 :: Char
pattern DIGIT_5 = '\x35'

-- | Digit 6, @6@
pattern DIGIT_6 :: Char
pattern DIGIT_6 = '\x36'

-- | Digit 7, @7@
pattern DIGIT_7 :: Char
pattern DIGIT_7 = '\x37'

-- | Digit 8, @8@
pattern DIGIT_8 :: Char
pattern DIGIT_8 = '\x38'

-- | Digit 9, @9@
pattern DIGIT_9 :: Char
pattern DIGIT_9 = '\x39'

-------------------------------------------------------------------------------
-- * Symbols 2, x3a-x40
-------------------------------------------------------------------------------

-- | Colon, @:@
pattern COLON :: Char
pattern COLON = '\x3a'

-- | Semicolon, @;@
pattern SEMICOLON :: Char
pattern SEMICOLON = '\x3b'

-- | Less-than sign, @<@
pattern LESS :: Char
pattern LESS = '\x3c'

-- | Equals sign, @=@
pattern EQUAL :: Char
pattern EQUAL = '\x3d'

-- | Greater-than sign, @>@
pattern GREATER :: Char
pattern GREATER = '\x3e'

-- | Question mark, @?@
pattern QUESTION :: Char
pattern QUESTION = '\x3f'

-- | Commercial At, @\@@
pattern AT :: Char
pattern AT = '\x40'

-------------------------------------------------------------------------------
-- * Upper case letters, x41-x5a
-------------------------------------------------------------------------------

-- | Latin small letter A, @A@
pattern UPPER_A :: Char
pattern UPPER_A = '\x41'

-- | Latin small letter B, @B@
pattern UPPER_B :: Char
pattern UPPER_B = '\x42'

-- | Latin small letter C, @C@
pattern UPPER_C :: Char
pattern UPPER_C = '\x43'

-- | Latin small letter D, @D@
pattern UPPER_D :: Char
pattern UPPER_D = '\x44'

-- | Latin small letter E, @E@
pattern UPPER_E :: Char
pattern UPPER_E = '\x45'

-- | Latin small letter F, @F@
pattern UPPER_F :: Char
pattern UPPER_F = '\x46'

-- | Latin small letter G, @G@
pattern UPPER_G :: Char
pattern UPPER_G = '\x47'

-- | Latin small letter H, @H@
pattern UPPER_H :: Char
pattern UPPER_H = '\x48'

-- | Latin small letter I, @I@
pattern UPPER_I :: Char
pattern UPPER_I = '\x49'

-- | Latin small letter J, @J@
pattern UPPER_J :: Char
pattern UPPER_J = '\x4a'

-- | Latin small letter K, @K@
pattern UPPER_K :: Char
pattern UPPER_K = '\x4b'

-- | Latin small letter L, @L@
pattern UPPER_L :: Char
pattern UPPER_L = '\x4c'

-- | Latin small letter M, @M@
pattern UPPER_M :: Char
pattern UPPER_M = '\x4d'

-- | Latin small letter N, @N@
pattern UPPER_N :: Char
pattern UPPER_N = '\x4e'

-- | Latin small letter O, @O@
pattern UPPER_O :: Char
pattern UPPER_O = '\x4f'

-- | Latin small letter P, @P@
pattern UPPER_P :: Char
pattern UPPER_P = '\x50'

-- | Latin small letter Q, @Q@
pattern UPPER_Q :: Char
pattern UPPER_Q = '\x51'

-- | Latin small letter R, @R@
pattern UPPER_R :: Char
pattern UPPER_R = '\x52'

-- | Latin small letter S, @S@
pattern UPPER_S :: Char
pattern UPPER_S = '\x53'

-- | Latin small letter T, @T@
pattern UPPER_T :: Char
pattern UPPER_T = '\x54'

-- | Latin small letter U, @U@
pattern UPPER_U :: Char
pattern UPPER_U = '\x55'

-- | Latin small letter V, @V@
pattern UPPER_V :: Char
pattern UPPER_V = '\x56'

-- | Latin small letter W, @W@
pattern UPPER_W :: Char
pattern UPPER_W = '\x57'

-- | Latin small letter X, @X@
pattern UPPER_X :: Char
pattern UPPER_X = '\x58'

-- | Latin small letter Y, @Y@
pattern UPPER_Y :: Char
pattern UPPER_Y = '\x59'

-- | Latin small letter Z, @Z@
pattern UPPER_Z :: Char
pattern UPPER_Z = '\x5a'

-------------------------------------------------------------------------------
-- * Symbols 3, x5b-x60
-------------------------------------------------------------------------------

-- | Left square bracket, @[@
pattern LEFT_SQUARE :: Char
pattern LEFT_SQUARE = '\x5b'

-- | Reverse solidus, backslash, @\\@
pattern BACKSLASH :: Char
pattern BACKSLASH = '\x5c'

-- | Right square bracket, @]@
pattern RIGHT_SQUARE :: Char
pattern RIGHT_SQUARE = '\x5d'

-- | Circumflex accent, @^@
pattern CIRCUM :: Char
pattern CIRCUM = '\x5e'

-- | Low line, underscore, @_@
pattern UNDERSCORE :: Char
pattern UNDERSCORE = '\x5f'

-- | Grave accent, @`@
pattern GRAVE :: Char
pattern GRAVE = '\x60'

-------------------------------------------------------------------------------
-- * Lower case letters, x61-x7a
-------------------------------------------------------------------------------

-- | Latin small letter A, @a@
pattern LOWER_A :: Char
pattern LOWER_A = '\x61'

-- | Latin small letter B, @b@
pattern LOWER_B :: Char
pattern LOWER_B = '\x62'

-- | Latin small letter C, @c@
pattern LOWER_C :: Char
pattern LOWER_C = '\x63'

-- | Latin small letter D, @d@
pattern LOWER_D :: Char
pattern LOWER_D = '\x64'

-- | Latin small letter E, @e@
pattern LOWER_E :: Char
pattern LOWER_E = '\x65'

-- | Latin small letter F, @f@
pattern LOWER_F :: Char
pattern LOWER_F = '\x66'

-- | Latin small letter G, @g@
pattern LOWER_G :: Char
pattern LOWER_G = '\x67'

-- | Latin small letter H, @h@
pattern LOWER_H :: Char
pattern LOWER_H = '\x68'

-- | Latin small letter I, @i@
pattern LOWER_I :: Char
pattern LOWER_I = '\x69'

-- | Latin small letter J, @j@
pattern LOWER_J :: Char
pattern LOWER_J = '\x6a'

-- | Latin small letter K, @k@
pattern LOWER_K :: Char
pattern LOWER_K = '\x6b'

-- | Latin small letter L, @l@
pattern LOWER_L :: Char
pattern LOWER_L = '\x6c'

-- | Latin small letter M, @m@
pattern LOWER_M :: Char
pattern LOWER_M = '\x6d'

-- | Latin small letter N, @n@
pattern LOWER_N :: Char
pattern LOWER_N = '\x6e'

-- | Latin small letter O, @o@
pattern LOWER_O :: Char
pattern LOWER_O = '\x6f'

-- | Latin small letter P, @p@
pattern LOWER_P :: Char
pattern LOWER_P = '\x70'

-- | Latin small letter Q, @q@
pattern LOWER_Q :: Char
pattern LOWER_Q = '\x71'

-- | Latin small letter R, @r@
pattern LOWER_R :: Char
pattern LOWER_R = '\x72'

-- | Latin small letter S, @s@
pattern LOWER_S :: Char
pattern LOWER_S = '\x73'

-- | Latin small letter T, @t@
pattern LOWER_T :: Char
pattern LOWER_T = '\x74'

-- | Latin small letter U, @u@
pattern LOWER_U :: Char
pattern LOWER_U = '\x75'

-- | Latin small letter V, @v@
pattern LOWER_V :: Char
pattern LOWER_V = '\x76'

-- | Latin small letter W, @w@
pattern LOWER_W :: Char
pattern LOWER_W = '\x77'

-- | Latin small letter X, @x@
pattern LOWER_X :: Char
pattern LOWER_X = '\x78'

-- | Latin small letter Y, @y@
pattern LOWER_Y :: Char
pattern LOWER_Y = '\x79'

-- | Latin small letter Z, @z@
pattern LOWER_Z :: Char
pattern LOWER_Z = '\x7a'

-------------------------------------------------------------------------------
-- * Symbols 5, x7b-x7f
-------------------------------------------------------------------------------

-- | Left curly bracket, @{@
pattern LEFT_CURLY :: Char
pattern LEFT_CURLY = '\x7b'

-- | Vertical line, vecrtical bar, @|@
pattern BAR :: Char
pattern BAR = '\x7c'

-- | Right curly bracket, @}@
pattern RIGHT_CURLY :: Char
pattern RIGHT_CURLY = '\x7d'

-- | Tilde, @~@
pattern TILDE :: Char
pattern TILDE = '\x7e'

-- | Delete (DEL)
pattern DEL :: Char
pattern DEL = '\x7f'
