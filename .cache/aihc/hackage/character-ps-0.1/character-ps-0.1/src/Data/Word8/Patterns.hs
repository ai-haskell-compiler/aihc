-- | This module provides pattern synonyms for 'Word8' in [Basic Latin block](https://compart.com/en/unicode/block/U+0000) (range @0x00-0x7f@).
--
-- The pattern names are inspired by Unicode (letter and digits) and PostScript names (symbols),
-- e.g. @/@ is 'SLASH', not @SOLIDUS@.
--
-- This module is designed to be imported qualified
--
-- @
-- import "Data.Word8.Patterns" as W8
--
-- hello = [W8.'LOWER_H', W8.'LOWER_E', W8.'LOWER_L', W8.'LOWER_L', W8.'LOWER_O']
-- @
--
-- but can also be used unqualified as well.
--
module Data.Word8.Patterns where

import Data.Word (Word8)

-------------------------------------------------------------------------------
-- * Control characters, x00-x1f
-------------------------------------------------------------------------------

-- | Null character (NUL)
pattern NUL :: Word8
pattern NUL = 0x00

-- | Start of Heading (SOH)
pattern SOH :: Word8
pattern SOH = 0x01

-- | Start of Text (STX)
pattern STX :: Word8
pattern STX = 0x02

-- | End of Text (ETX)
pattern ETX :: Word8
pattern ETX = 0x03

-- | End of Transmission (EOT)
pattern EOT :: Word8
pattern EOT = 0x04

-- | Enquiry (ENQ)
pattern ENQ :: Word8
pattern ENQ = 0x05

-- | Acknowledge (ACK)
pattern ACK :: Word8
pattern ACK = 0x06

-- | Alert (BEL)
pattern BEL :: Word8
pattern BEL = 0x07

-- | Backspace (BS)
pattern BS :: Word8
pattern BS = 0x08

-- | Character tabulation (TAB)
pattern TAB :: Word8
pattern TAB = 0x09

-- | End of line, line feed (LF)
pattern LF :: Word8
pattern LF = 0x0a

-- | Line tabulation, vertical tab (VT)
pattern VT :: Word8
pattern VT = 0x0b

-- | Form feed (FF)
pattern FF :: Word8
pattern FF = 0x0c

-- | Carriage Return (CR)
pattern CR :: Word8
pattern CR = 0x0d

-- | Locking-Shift One, Shift Out (SO)
pattern SO :: Word8
pattern SO = 0x0e

-- | Locking-Shift Zero, Shift In (SI)
pattern SI :: Word8
pattern SI = 0x0f

-- | Data Link Escape (DLE)
pattern DLE :: Word8
pattern DLE = 0x10

-- | Device Control One, XON (DC1)
pattern DC1 :: Word8
pattern DC1 = 0x11

-- | Device Control Two (DC2)
pattern DC2 :: Word8
pattern DC2 = 0x12

-- | Device Control Three, XOFF (DC3)
pattern DC3 :: Word8
pattern DC3 = 0x13

-- | Device Control Four (DC4)
pattern DC4 :: Word8
pattern DC4 = 0x14

-- | Negative Acknowledge (NAK)
pattern NAK :: Word8
pattern NAK = 0x15

-- | Syncronous Idle (SYN)
pattern SYN :: Word8
pattern SYN = 0x16

-- | End of Transmission Block (ETB)
pattern ETB :: Word8
pattern ETB = 0x17

-- | Cancel (CAN)
pattern CAN :: Word8
pattern CAN = 0x18

-- | End of Medium (EOM)
pattern EOM :: Word8
pattern EOM = 0x19

-- | Substitute (SUB)
pattern SUB :: Word8
pattern SUB = 0x1a

-- | Escape (ESC)
pattern ESC :: Word8
pattern ESC = 0x1b

-- | File Separator (FS)
pattern FS :: Word8
pattern FS = 0x1c

-- | Group Separator (GS)
pattern GS :: Word8
pattern GS = 0x1d

-- | Information Separator One, Record Separator (RS)
pattern RS :: Word8
pattern RS = 0x1e

-- | Information Separator Two, Unit Separator (US)
pattern US :: Word8
pattern US = 0x1f

-------------------------------------------------------------------------------
-- * Symbols 1, x20-x2f
-------------------------------------------------------------------------------

-- | Space (SP), @' '@.
pattern SPACE :: Word8
pattern SPACE = 0x20

-- | Exclamation mark, @!@
pattern EXCLAM :: Word8
pattern EXCLAM = 0x21

-- | Quotation mark, double quote, @"@
pattern DOUBLE_QUOTE :: Word8
pattern DOUBLE_QUOTE = 0x22

-- | Number sign, @#@
pattern NUMBER :: Word8
pattern NUMBER = 0x23

-- | Dollar sign, @$@
pattern DOLLAR :: Word8
pattern DOLLAR = 0x24

-- | Percent sign, @%@
pattern PERCENT :: Word8
pattern PERCENT = 0x25

-- | Ampersand, @&@
pattern AMPERSAND :: Word8
pattern AMPERSAND = 0x26

-- | Apostrophe, single quote, @'@
pattern SINGLE_QUOTE :: Word8
pattern SINGLE_QUOTE = 0x27

-- | Left parenthesis, @(@
pattern LEFT_PAREN :: Word8
pattern LEFT_PAREN = 0x28

-- | Right parenthesis, @)@
pattern RIGHT_PAREN :: Word8
pattern RIGHT_PAREN = 0x29

-- | Asterisk, @*@
pattern ASTERISK :: Word8
pattern ASTERISK = 0x2a

-- | Plus sign, @+@
pattern PLUS :: Word8
pattern PLUS = 0x2b

-- | Comma, @,@
pattern COMMA :: Word8
pattern COMMA = 0x2c

-- | Hyphen-minus, @-@
pattern HYPHEN :: Word8
pattern HYPHEN = 0x2d

-- | Full stop, period, @.@
pattern PERIOD :: Word8
pattern PERIOD = 0x2e

-- | Solidus, slash, @/@
pattern SLASH :: Word8
pattern SLASH = 0x2f

-------------------------------------------------------------------------------
-- * Digits, x30-x39
-------------------------------------------------------------------------------

-- | Digit 0, @0@
pattern DIGIT_0 :: Word8
pattern DIGIT_0 = 0x30

-- | Digit 1, @1@
pattern DIGIT_1 :: Word8
pattern DIGIT_1 = 0x31

-- | Digit 2, @2@
pattern DIGIT_2 :: Word8
pattern DIGIT_2 = 0x32

-- | Digit 3, @3@
pattern DIGIT_3 :: Word8
pattern DIGIT_3 = 0x33

-- | Digit 4, @4@
pattern DIGIT_4 :: Word8
pattern DIGIT_4 = 0x34

-- | Digit 5, @5@
pattern DIGIT_5 :: Word8
pattern DIGIT_5 = 0x35

-- | Digit 6, @6@
pattern DIGIT_6 :: Word8
pattern DIGIT_6 = 0x36

-- | Digit 7, @7@
pattern DIGIT_7 :: Word8
pattern DIGIT_7 = 0x37

-- | Digit 8, @8@
pattern DIGIT_8 :: Word8
pattern DIGIT_8 = 0x38

-- | Digit 9, @9@
pattern DIGIT_9 :: Word8
pattern DIGIT_9 = 0x39

-------------------------------------------------------------------------------
-- * Symbols 2, x3a-x40
-------------------------------------------------------------------------------

-- | Colon, @:@
pattern COLON :: Word8
pattern COLON = 0x3a

-- | Semicolon, @;@
pattern SEMICOLON :: Word8
pattern SEMICOLON = 0x3b

-- | Less-than sign, @<@
pattern LESS :: Word8
pattern LESS = 0x3c

-- | Equals sign, @=@
pattern EQUAL :: Word8
pattern EQUAL = 0x3d

-- | Greater-than sign, @>@
pattern GREATER :: Word8
pattern GREATER = 0x3e

-- | Question mark, @?@
pattern QUESTION :: Word8
pattern QUESTION = 0x3f

-- | Commercial At, @\@@
pattern AT :: Word8
pattern AT = 0x40

-------------------------------------------------------------------------------
-- * Upper case letters, x41-x5a
-------------------------------------------------------------------------------

-- | Latin small letter A, @A@
pattern UPPER_A :: Word8
pattern UPPER_A = 0x41

-- | Latin small letter B, @B@
pattern UPPER_B :: Word8
pattern UPPER_B = 0x42

-- | Latin small letter C, @C@
pattern UPPER_C :: Word8
pattern UPPER_C = 0x43

-- | Latin small letter D, @D@
pattern UPPER_D :: Word8
pattern UPPER_D = 0x44

-- | Latin small letter E, @E@
pattern UPPER_E :: Word8
pattern UPPER_E = 0x45

-- | Latin small letter F, @F@
pattern UPPER_F :: Word8
pattern UPPER_F = 0x46

-- | Latin small letter G, @G@
pattern UPPER_G :: Word8
pattern UPPER_G = 0x47

-- | Latin small letter H, @H@
pattern UPPER_H :: Word8
pattern UPPER_H = 0x48

-- | Latin small letter I, @I@
pattern UPPER_I :: Word8
pattern UPPER_I = 0x49

-- | Latin small letter J, @J@
pattern UPPER_J :: Word8
pattern UPPER_J = 0x4a

-- | Latin small letter K, @K@
pattern UPPER_K :: Word8
pattern UPPER_K = 0x4b

-- | Latin small letter L, @L@
pattern UPPER_L :: Word8
pattern UPPER_L = 0x4c

-- | Latin small letter M, @M@
pattern UPPER_M :: Word8
pattern UPPER_M = 0x4d

-- | Latin small letter N, @N@
pattern UPPER_N :: Word8
pattern UPPER_N = 0x4e

-- | Latin small letter O, @O@
pattern UPPER_O :: Word8
pattern UPPER_O = 0x4f

-- | Latin small letter P, @P@
pattern UPPER_P :: Word8
pattern UPPER_P = 0x50

-- | Latin small letter Q, @Q@
pattern UPPER_Q :: Word8
pattern UPPER_Q = 0x51

-- | Latin small letter R, @R@
pattern UPPER_R :: Word8
pattern UPPER_R = 0x52

-- | Latin small letter S, @S@
pattern UPPER_S :: Word8
pattern UPPER_S = 0x53

-- | Latin small letter T, @T@
pattern UPPER_T :: Word8
pattern UPPER_T = 0x54

-- | Latin small letter U, @U@
pattern UPPER_U :: Word8
pattern UPPER_U = 0x55

-- | Latin small letter V, @V@
pattern UPPER_V :: Word8
pattern UPPER_V = 0x56

-- | Latin small letter W, @W@
pattern UPPER_W :: Word8
pattern UPPER_W = 0x57

-- | Latin small letter X, @X@
pattern UPPER_X :: Word8
pattern UPPER_X = 0x58

-- | Latin small letter Y, @Y@
pattern UPPER_Y :: Word8
pattern UPPER_Y = 0x59

-- | Latin small letter Z, @Z@
pattern UPPER_Z :: Word8
pattern UPPER_Z = 0x5a

-------------------------------------------------------------------------------
-- * Symbols 3, x5b-x60
-------------------------------------------------------------------------------

-- | Left square bracket, @[@
pattern LEFT_SQUARE :: Word8
pattern LEFT_SQUARE = 0x5b

-- | Reverse solidus, backslash, @\\@
pattern BACKSLASH :: Word8
pattern BACKSLASH = 0x5c

-- | Right square bracket, @]@
pattern RIGHT_SQUARE :: Word8
pattern RIGHT_SQUARE = 0x5d

-- | Circumflex accent, @^@
pattern CIRCUM :: Word8
pattern CIRCUM = 0x5e

-- | Low line, underscore, @_@
pattern UNDERSCORE :: Word8
pattern UNDERSCORE = 0x5f

-- | Grave accent, @`@
pattern GRAVE :: Word8
pattern GRAVE = 0x60

-------------------------------------------------------------------------------
-- * Lower case letters, x61-x7a
-------------------------------------------------------------------------------

-- | Latin small letter A, @a@
pattern LOWER_A :: Word8
pattern LOWER_A = 0x61

-- | Latin small letter B, @b@
pattern LOWER_B :: Word8
pattern LOWER_B = 0x62

-- | Latin small letter C, @c@
pattern LOWER_C :: Word8
pattern LOWER_C = 0x63

-- | Latin small letter D, @d@
pattern LOWER_D :: Word8
pattern LOWER_D = 0x64

-- | Latin small letter E, @e@
pattern LOWER_E :: Word8
pattern LOWER_E = 0x65

-- | Latin small letter F, @f@
pattern LOWER_F :: Word8
pattern LOWER_F = 0x66

-- | Latin small letter G, @g@
pattern LOWER_G :: Word8
pattern LOWER_G = 0x67

-- | Latin small letter H, @h@
pattern LOWER_H :: Word8
pattern LOWER_H = 0x68

-- | Latin small letter I, @i@
pattern LOWER_I :: Word8
pattern LOWER_I = 0x69

-- | Latin small letter J, @j@
pattern LOWER_J :: Word8
pattern LOWER_J = 0x6a

-- | Latin small letter K, @k@
pattern LOWER_K :: Word8
pattern LOWER_K = 0x6b

-- | Latin small letter L, @l@
pattern LOWER_L :: Word8
pattern LOWER_L = 0x6c

-- | Latin small letter M, @m@
pattern LOWER_M :: Word8
pattern LOWER_M = 0x6d

-- | Latin small letter N, @n@
pattern LOWER_N :: Word8
pattern LOWER_N = 0x6e

-- | Latin small letter O, @o@
pattern LOWER_O :: Word8
pattern LOWER_O = 0x6f

-- | Latin small letter P, @p@
pattern LOWER_P :: Word8
pattern LOWER_P = 0x70

-- | Latin small letter Q, @q@
pattern LOWER_Q :: Word8
pattern LOWER_Q = 0x71

-- | Latin small letter R, @r@
pattern LOWER_R :: Word8
pattern LOWER_R = 0x72

-- | Latin small letter S, @s@
pattern LOWER_S :: Word8
pattern LOWER_S = 0x73

-- | Latin small letter T, @t@
pattern LOWER_T :: Word8
pattern LOWER_T = 0x74

-- | Latin small letter U, @u@
pattern LOWER_U :: Word8
pattern LOWER_U = 0x75

-- | Latin small letter V, @v@
pattern LOWER_V :: Word8
pattern LOWER_V = 0x76

-- | Latin small letter W, @w@
pattern LOWER_W :: Word8
pattern LOWER_W = 0x77

-- | Latin small letter X, @x@
pattern LOWER_X :: Word8
pattern LOWER_X = 0x78

-- | Latin small letter Y, @y@
pattern LOWER_Y :: Word8
pattern LOWER_Y = 0x79

-- | Latin small letter Z, @z@
pattern LOWER_Z :: Word8
pattern LOWER_Z = 0x7a

-------------------------------------------------------------------------------
-- * Symbols 5, x7b-x7f
-------------------------------------------------------------------------------

-- | Left curly bracket, @{@
pattern LEFT_CURLY :: Word8
pattern LEFT_CURLY = 0x7b

-- | Vertical line, vecrtical bar, @|@
pattern BAR :: Word8
pattern BAR = 0x7c

-- | Right curly bracket, @}@
pattern RIGHT_CURLY :: Word8
pattern RIGHT_CURLY = 0x7d

-- | Tilde, @~@
pattern TILDE :: Word8
pattern TILDE = 0x7e

-- | Delete (DEL)
pattern DEL :: Word8
pattern DEL = 0x7f
