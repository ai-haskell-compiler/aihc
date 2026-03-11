-- | This module provides pattern synonyms for 'Word16' in [Basic Latin block](https://compart.com/en/unicode/block/U+0000) (range @0x00-0x7f@).
--
-- The pattern names are inspired by Unicode (letter and digits) and PostScript names (symbols),
-- e.g. @/@ is 'SLASH', not @SOLIDUS@.
--
-- This module is designed to be imported qualified
--
-- @
-- import "Data.Word16.Patterns" as W16
--
-- hello = [W16.'LOWER_H', W16.'LOWER_E', W16.'LOWER_L', W16.'LOWER_L', W16.'LOWER_O']
-- @
--
-- but can also be used unqualified as well.
--
module Data.Word16.Patterns where

import Data.Word (Word16)

-------------------------------------------------------------------------------
-- * Control characters, x00-x1f
-------------------------------------------------------------------------------

-- | Null character (NUL)
pattern NUL :: Word16
pattern NUL = 0x00

-- | Start of Heading (SOH)
pattern SOH :: Word16
pattern SOH = 0x01

-- | Start of Text (STX)
pattern STX :: Word16
pattern STX = 0x02

-- | End of Text (ETX)
pattern ETX :: Word16
pattern ETX = 0x03

-- | End of Transmission (EOT)
pattern EOT :: Word16
pattern EOT = 0x04

-- | Enquiry (ENQ)
pattern ENQ :: Word16
pattern ENQ = 0x05

-- | Acknowledge (ACK)
pattern ACK :: Word16
pattern ACK = 0x06

-- | Alert (BEL)
pattern BEL :: Word16
pattern BEL = 0x07

-- | Backspace (BS)
pattern BS :: Word16
pattern BS = 0x08

-- | Character tabulation (TAB)
pattern TAB :: Word16
pattern TAB = 0x09

-- | End of line, line feed (LF)
pattern LF :: Word16
pattern LF = 0x0a

-- | Line tabulation, vertical tab (VT)
pattern VT :: Word16
pattern VT = 0x0b

-- | Form feed (FF)
pattern FF :: Word16
pattern FF = 0x0c

-- | Carriage Return (CR)
pattern CR :: Word16
pattern CR = 0x0d

-- | Locking-Shift One, Shift Out (SO)
pattern SO :: Word16
pattern SO = 0x0e

-- | Locking-Shift Zero, Shift In (SI)
pattern SI :: Word16
pattern SI = 0x0f

-- | Data Link Escape (DLE)
pattern DLE :: Word16
pattern DLE = 0x10

-- | Device Control One, XON (DC1)
pattern DC1 :: Word16
pattern DC1 = 0x11

-- | Device Control Two (DC2)
pattern DC2 :: Word16
pattern DC2 = 0x12

-- | Device Control Three, XOFF (DC3)
pattern DC3 :: Word16
pattern DC3 = 0x13

-- | Device Control Four (DC4)
pattern DC4 :: Word16
pattern DC4 = 0x14

-- | Negative Acknowledge (NAK)
pattern NAK :: Word16
pattern NAK = 0x15

-- | Syncronous Idle (SYN)
pattern SYN :: Word16
pattern SYN = 0x16

-- | End of Transmission Block (ETB)
pattern ETB :: Word16
pattern ETB = 0x17

-- | Cancel (CAN)
pattern CAN :: Word16
pattern CAN = 0x18

-- | End of Medium (EOM)
pattern EOM :: Word16
pattern EOM = 0x19

-- | Substitute (SUB)
pattern SUB :: Word16
pattern SUB = 0x1a

-- | Escape (ESC)
pattern ESC :: Word16
pattern ESC = 0x1b

-- | File Separator (FS)
pattern FS :: Word16
pattern FS = 0x1c

-- | Group Separator (GS)
pattern GS :: Word16
pattern GS = 0x1d

-- | Information Separator One, Record Separator (RS)
pattern RS :: Word16
pattern RS = 0x1e

-- | Information Separator Two, Unit Separator (US)
pattern US :: Word16
pattern US = 0x1f

-------------------------------------------------------------------------------
-- * Symbols 1, x20-x2f
-------------------------------------------------------------------------------

-- | Space (SP), @' '@.
pattern SPACE :: Word16
pattern SPACE = 0x20

-- | Exclamation mark, @!@
pattern EXCLAM :: Word16
pattern EXCLAM = 0x21

-- | Quotation mark, double quote, @"@
pattern DOUBLE_QUOTE :: Word16
pattern DOUBLE_QUOTE = 0x22

-- | Number sign, @#@
pattern NUMBER :: Word16
pattern NUMBER = 0x23

-- | Dollar sign, @$@
pattern DOLLAR :: Word16
pattern DOLLAR = 0x24

-- | Percent sign, @%@
pattern PERCENT :: Word16
pattern PERCENT = 0x25

-- | Ampersand, @&@
pattern AMPERSAND :: Word16
pattern AMPERSAND = 0x26

-- | Apostrophe, single quote, @'@
pattern SINGLE_QUOTE :: Word16
pattern SINGLE_QUOTE = 0x27

-- | Left parenthesis, @(@
pattern LEFT_PAREN :: Word16
pattern LEFT_PAREN = 0x28

-- | Right parenthesis, @)@
pattern RIGHT_PAREN :: Word16
pattern RIGHT_PAREN = 0x29

-- | Asterisk, @*@
pattern ASTERISK :: Word16
pattern ASTERISK = 0x2a

-- | Plus sign, @+@
pattern PLUS :: Word16
pattern PLUS = 0x2b

-- | Comma, @,@
pattern COMMA :: Word16
pattern COMMA = 0x2c

-- | Hyphen-minus, @-@
pattern HYPHEN :: Word16
pattern HYPHEN = 0x2d

-- | Full stop, period, @.@
pattern PERIOD :: Word16
pattern PERIOD = 0x2e

-- | Solidus, slash, @/@
pattern SLASH :: Word16
pattern SLASH = 0x2f

-------------------------------------------------------------------------------
-- * Digits, x30-x39
-------------------------------------------------------------------------------

-- | Digit 0, @0@
pattern DIGIT_0 :: Word16
pattern DIGIT_0 = 0x30

-- | Digit 1, @1@
pattern DIGIT_1 :: Word16
pattern DIGIT_1 = 0x31

-- | Digit 2, @2@
pattern DIGIT_2 :: Word16
pattern DIGIT_2 = 0x32

-- | Digit 3, @3@
pattern DIGIT_3 :: Word16
pattern DIGIT_3 = 0x33

-- | Digit 4, @4@
pattern DIGIT_4 :: Word16
pattern DIGIT_4 = 0x34

-- | Digit 5, @5@
pattern DIGIT_5 :: Word16
pattern DIGIT_5 = 0x35

-- | Digit 6, @6@
pattern DIGIT_6 :: Word16
pattern DIGIT_6 = 0x36

-- | Digit 7, @7@
pattern DIGIT_7 :: Word16
pattern DIGIT_7 = 0x37

-- | Digit 8, @8@
pattern DIGIT_8 :: Word16
pattern DIGIT_8 = 0x38

-- | Digit 9, @9@
pattern DIGIT_9 :: Word16
pattern DIGIT_9 = 0x39

-------------------------------------------------------------------------------
-- * Symbols 2, x3a-x40
-------------------------------------------------------------------------------

-- | Colon, @:@
pattern COLON :: Word16
pattern COLON = 0x3a

-- | Semicolon, @;@
pattern SEMICOLON :: Word16
pattern SEMICOLON = 0x3b

-- | Less-than sign, @<@
pattern LESS :: Word16
pattern LESS = 0x3c

-- | Equals sign, @=@
pattern EQUAL :: Word16
pattern EQUAL = 0x3d

-- | Greater-than sign, @>@
pattern GREATER :: Word16
pattern GREATER = 0x3e

-- | Question mark, @?@
pattern QUESTION :: Word16
pattern QUESTION = 0x3f

-- | Commercial At, @\@@
pattern AT :: Word16
pattern AT = 0x40

-------------------------------------------------------------------------------
-- * Upper case letters, x41-x5a
-------------------------------------------------------------------------------

-- | Latin small letter A, @A@
pattern UPPER_A :: Word16
pattern UPPER_A = 0x41

-- | Latin small letter B, @B@
pattern UPPER_B :: Word16
pattern UPPER_B = 0x42

-- | Latin small letter C, @C@
pattern UPPER_C :: Word16
pattern UPPER_C = 0x43

-- | Latin small letter D, @D@
pattern UPPER_D :: Word16
pattern UPPER_D = 0x44

-- | Latin small letter E, @E@
pattern UPPER_E :: Word16
pattern UPPER_E = 0x45

-- | Latin small letter F, @F@
pattern UPPER_F :: Word16
pattern UPPER_F = 0x46

-- | Latin small letter G, @G@
pattern UPPER_G :: Word16
pattern UPPER_G = 0x47

-- | Latin small letter H, @H@
pattern UPPER_H :: Word16
pattern UPPER_H = 0x48

-- | Latin small letter I, @I@
pattern UPPER_I :: Word16
pattern UPPER_I = 0x49

-- | Latin small letter J, @J@
pattern UPPER_J :: Word16
pattern UPPER_J = 0x4a

-- | Latin small letter K, @K@
pattern UPPER_K :: Word16
pattern UPPER_K = 0x4b

-- | Latin small letter L, @L@
pattern UPPER_L :: Word16
pattern UPPER_L = 0x4c

-- | Latin small letter M, @M@
pattern UPPER_M :: Word16
pattern UPPER_M = 0x4d

-- | Latin small letter N, @N@
pattern UPPER_N :: Word16
pattern UPPER_N = 0x4e

-- | Latin small letter O, @O@
pattern UPPER_O :: Word16
pattern UPPER_O = 0x4f

-- | Latin small letter P, @P@
pattern UPPER_P :: Word16
pattern UPPER_P = 0x50

-- | Latin small letter Q, @Q@
pattern UPPER_Q :: Word16
pattern UPPER_Q = 0x51

-- | Latin small letter R, @R@
pattern UPPER_R :: Word16
pattern UPPER_R = 0x52

-- | Latin small letter S, @S@
pattern UPPER_S :: Word16
pattern UPPER_S = 0x53

-- | Latin small letter T, @T@
pattern UPPER_T :: Word16
pattern UPPER_T = 0x54

-- | Latin small letter U, @U@
pattern UPPER_U :: Word16
pattern UPPER_U = 0x55

-- | Latin small letter V, @V@
pattern UPPER_V :: Word16
pattern UPPER_V = 0x56

-- | Latin small letter W, @W@
pattern UPPER_W :: Word16
pattern UPPER_W = 0x57

-- | Latin small letter X, @X@
pattern UPPER_X :: Word16
pattern UPPER_X = 0x58

-- | Latin small letter Y, @Y@
pattern UPPER_Y :: Word16
pattern UPPER_Y = 0x59

-- | Latin small letter Z, @Z@
pattern UPPER_Z :: Word16
pattern UPPER_Z = 0x5a

-------------------------------------------------------------------------------
-- * Symbols 3, x5b-x60
-------------------------------------------------------------------------------

-- | Left square bracket, @[@
pattern LEFT_SQUARE :: Word16
pattern LEFT_SQUARE = 0x5b

-- | Reverse solidus, backslash, @\\@
pattern BACKSLASH :: Word16
pattern BACKSLASH = 0x5c

-- | Right square bracket, @]@
pattern RIGHT_SQUARE :: Word16
pattern RIGHT_SQUARE = 0x5d

-- | Circumflex accent, @^@
pattern CIRCUM :: Word16
pattern CIRCUM = 0x5e

-- | Low line, underscore, @_@
pattern UNDERSCORE :: Word16
pattern UNDERSCORE = 0x5f

-- | Grave accent, @`@
pattern GRAVE :: Word16
pattern GRAVE = 0x60

-------------------------------------------------------------------------------
-- * Lower case letters, x61-x7a
-------------------------------------------------------------------------------

-- | Latin small letter A, @a@
pattern LOWER_A :: Word16
pattern LOWER_A = 0x61

-- | Latin small letter B, @b@
pattern LOWER_B :: Word16
pattern LOWER_B = 0x62

-- | Latin small letter C, @c@
pattern LOWER_C :: Word16
pattern LOWER_C = 0x63

-- | Latin small letter D, @d@
pattern LOWER_D :: Word16
pattern LOWER_D = 0x64

-- | Latin small letter E, @e@
pattern LOWER_E :: Word16
pattern LOWER_E = 0x65

-- | Latin small letter F, @f@
pattern LOWER_F :: Word16
pattern LOWER_F = 0x66

-- | Latin small letter G, @g@
pattern LOWER_G :: Word16
pattern LOWER_G = 0x67

-- | Latin small letter H, @h@
pattern LOWER_H :: Word16
pattern LOWER_H = 0x68

-- | Latin small letter I, @i@
pattern LOWER_I :: Word16
pattern LOWER_I = 0x69

-- | Latin small letter J, @j@
pattern LOWER_J :: Word16
pattern LOWER_J = 0x6a

-- | Latin small letter K, @k@
pattern LOWER_K :: Word16
pattern LOWER_K = 0x6b

-- | Latin small letter L, @l@
pattern LOWER_L :: Word16
pattern LOWER_L = 0x6c

-- | Latin small letter M, @m@
pattern LOWER_M :: Word16
pattern LOWER_M = 0x6d

-- | Latin small letter N, @n@
pattern LOWER_N :: Word16
pattern LOWER_N = 0x6e

-- | Latin small letter O, @o@
pattern LOWER_O :: Word16
pattern LOWER_O = 0x6f

-- | Latin small letter P, @p@
pattern LOWER_P :: Word16
pattern LOWER_P = 0x70

-- | Latin small letter Q, @q@
pattern LOWER_Q :: Word16
pattern LOWER_Q = 0x71

-- | Latin small letter R, @r@
pattern LOWER_R :: Word16
pattern LOWER_R = 0x72

-- | Latin small letter S, @s@
pattern LOWER_S :: Word16
pattern LOWER_S = 0x73

-- | Latin small letter T, @t@
pattern LOWER_T :: Word16
pattern LOWER_T = 0x74

-- | Latin small letter U, @u@
pattern LOWER_U :: Word16
pattern LOWER_U = 0x75

-- | Latin small letter V, @v@
pattern LOWER_V :: Word16
pattern LOWER_V = 0x76

-- | Latin small letter W, @w@
pattern LOWER_W :: Word16
pattern LOWER_W = 0x77

-- | Latin small letter X, @x@
pattern LOWER_X :: Word16
pattern LOWER_X = 0x78

-- | Latin small letter Y, @y@
pattern LOWER_Y :: Word16
pattern LOWER_Y = 0x79

-- | Latin small letter Z, @z@
pattern LOWER_Z :: Word16
pattern LOWER_Z = 0x7a

-------------------------------------------------------------------------------
-- * Symbols 5, x7b-x7f
-------------------------------------------------------------------------------

-- | Left curly bracket, @{@
pattern LEFT_CURLY :: Word16
pattern LEFT_CURLY = 0x7b

-- | Vertical line, vecrtical bar, @|@
pattern BAR :: Word16
pattern BAR = 0x7c

-- | Right curly bracket, @}@
pattern RIGHT_CURLY :: Word16
pattern RIGHT_CURLY = 0x7d

-- | Tilde, @~@
pattern TILDE :: Word16
pattern TILDE = 0x7e

-- | Delete (DEL)
pattern DEL :: Word16
pattern DEL = 0x7f
