{-# LANGUAGE RecordWildCards #-}

module Text.Format.ArgFmt
  ( FmtAlign(..)
  , FmtSign(..)
  , FmtNumSep(..)
  , ArgFmt(..)
  , prettyArgFmt
  , formatText
  , formatNumber
  ) where

import           Control.Arrow
import           Data.Char          (isDigit)
import qualified Data.List          as L
import           Numeric

import           Text.Format.ArgKey
import           Text.Format.Error


-- | A data type indicates how to align arguments
--
data FmtAlign = AlignNone     -- ^ Not specified, equivalent to 'AlignLeft'
                              -- unless number's sign aware enabled.
              | AlignLeft     -- ^ Forces the argument to be left-aligned
                              -- within the available space.
              | AlignRight    -- ^ Forces the field to be right-aligned within
                              -- the available space.
              | AlignCenter   -- ^ Forces the field to be centered within the
                              -- available space.
              | AlignSign     -- ^ Number specified, forces the padding to be
                              -- placed after the sign (if any) but before
                              -- the digits.
              deriving (Show, Eq)


-- | A data type indicates how to show number's sign
--
data FmtSign = SignNone      -- ^ Not specified, equivalent to 'SignMinus'
                             -- for signed numbers.
             | SignPlus      -- ^ Sign should be used for both positive as well
                             -- as negative numbers.
             | SignMinus     -- ^ Sign should be used only for negative numbers
             | SignSpace     -- ^ A leading space should be used on positive
                             -- numbers, and a minus sign on negative numbers.
             deriving (Show, Eq)


-- | A data type indicates number separator
--
-- e.g. 20,200,101  20_200_202
data FmtNumSep = NumSepNone   -- ^ Don't separate number
               | NumSepDash   -- ^ Use dash as number separator
               | NumSepComma  -- ^ Use comma as number separator
               deriving (Show, Eq)


{-| A data type indicates how to format an argument.

==== The format syntax

  @
  fmt       -> [[pad] align][sign]["#"]["0"][width][sep]["." precision][specs]
  pad       -> char
  align     -> "\<" | "\>" | "^" | "="
  sign      -> "+" | "-" | " "
  width     -> int | ("{" key "}")
  sep       -> "_" | ","
  precision -> int | ("{" key "}")
  specs     -> chars
  key       -> \<see 'ArgKey'\>
  @

  * @#@ will cause the "alternate form" to be used for integers.

      The alternate format is defined differently for different types.

      * add 0b prefix for binary
      * add 0o prefix for octal
      * add 0x prefix for hexadecimal

  * @with@ indicates minimum with of the field

      If omitted, the field width will be determined by the content.

      When align is omitted, preceding width by a zero character enables
      sign-aware zero-padding for numbers.
      This is equivalent to a pad of 0 with an align of =.

  * @precision@ indicates how many digits should be displayed after the decimal
    point for a floating point number, or maximum width for other types.

      When precision is omitted

      * preceding dot is present, indicates precision is 0
      * preceding dot is omitted too, indicates precision not set,
        default value (i.e. 6 for floating point numbers, 0 for others)
        will be used.

  * @specs@ indicates type specified options

      When specs is omitted, the default specs will be used.
      The default specs is defined differently from different types.

  Examples

    >>> read "*<30s" :: ArgFmt
    >>> read "<10.20s" :: ArgFmt
    >>> read "0=10_.20d" :: ArgFmt
    >>> read "#010_.20b" :: ArgFmt

==== String specs

  @
  s
  default         s
  @

==== Integer specs

  @
  b               binary format integer
  c               char point ('Char' will be trasformed by 'Char.ord' first)
  d               decimal format integer
  o               octal format integer
  x               hex format integer (use lower-case letters)
  X               hex format integer (use upper-case letters)
  default         d
  @

==== Floating point number specs

  @
  e               exponent notation, see 'Numeric.showEFloat'
  E               same as "e", but use upper-case 'E' as separator
  f               fixed-point notation see 'Numeric.showFFloat'
  F               same as "f", but converts nan to NAN and inf to INF
  g               general format, see 'Numeric.showGFloat'
  G               same as "g", but use upper-case 'E' as separator and
                  converts nan to NAN and inf to INF
  %               percentage, same as "f" except multiplies 100 first and
                  followed by a percent sign
  default         g
  @

See 'Text.Format.FormatArg' to learn how to define specs for your own types.
-}
data ArgFmt = ArgFmt { fmtAlign     :: FmtAlign
                     , fmtPad       :: Char
                     , fmtSign      :: FmtSign
                     , fmtAlternate :: Bool
                     , fmtSignAware :: Bool
                     , fmtWidth     :: Either Int ArgKey
                     , fmtNumSep    :: FmtNumSep
                     , fmtPrecision :: Either Int ArgKey
                     , fmtSpecs     :: String
                     , fmtRaw       :: String -- ^ When reading from a string,
                                              -- different strings may produce
                                              -- the same 'ArgFmt', this field
                                              -- keeps the original string here
                                              -- in case it is use later.
                     } deriving (Show, Eq)

instance Read ArgFmt where
  readsPrec _ cs =
      let (fmtAlign, fmtPad, cs1) = parseAlign cs
          (fmtSign, cs2) = parseSign cs1
          (fmtAlternate, cs3) = parseAlternate cs2
          (fmtSignAware, cs4) = parseSignAware cs3
          (fmtWidth, cs5) = parseWidth cs4
          (fmtNumSep, cs6) = parseNumSep cs5
          (fmtPrecision, fmtSpecs) = parsePrecision cs6
          fmtRaw = cs
      in [ (ArgFmt{..}, "") ]
    where
      stack :: String -> String
      stack = reverse . (`drop` (reverse cs)) . length

      parseAlign (c : '<' : cs) = (AlignLeft, c, cs)
      parseAlign (c : '>' : cs) = (AlignRight, c, cs)
      parseAlign (c : '^' : cs) = (AlignCenter, c, cs)
      parseAlign (c : '=' : cs) = (AlignSign, c, cs)
      parseAlign ('<' : cs)     = (AlignLeft, ' ', cs)
      parseAlign ('>' : cs)     = (AlignRight,' ', cs)
      parseAlign ('^' : cs)     = (AlignCenter, ' ', cs)
      parseAlign ('=' : cs)     = (AlignSign,' ', cs)
      parseAlign cs             = (AlignNone, ' ', cs)

      parseSign ('+' : cs) = (SignPlus, cs)
      parseSign ('-' : cs) = (SignMinus, cs)
      parseSign (' ' : cs) = (SignSpace, cs)
      parseSign cs         = (SignNone, cs)

      parseAlternate ('#' : cs) = (True, cs)
      parseAlternate cs         = (False, cs)

      parseSignAware ('0' : cs) = (True, cs)
      parseSignAware cs         = (False, cs)

      parseWidth ('{' : cs) =
        case L.break (== '}') cs of
          (ks, '}' : cs1) -> (Right (read ks), cs1)
          _               -> errorCloseTag $ stack cs
      parseWidth cs         =
        case L.break (not . isDigit) cs of
          ("", cs1) -> (Left 0, cs)
          (ds, cs1) -> (Left (read ds), cs1)

      parseNumSep ('_' : cs) = (NumSepDash, cs)
      parseNumSep (',' : cs) = (NumSepComma, cs)
      parseNumSep cs         = (NumSepNone, cs)

      parsePrecision ('.' : '{' : cs) =
        case L.break (== '}') cs of
          (ks, '}' : cs1) -> (Right (read ks), cs1)
          _               -> errorCloseTag $ stack cs
      parsePrecision ('.' : cs)       =
        case L.break (not . isDigit) cs of
          ("", cs1) -> (Left 0, cs)
          (ds, cs1) -> (Left (read ds), cs1)
      parsePrecision cs               = (Left (-1), cs)


prettyFmtAlign :: FmtAlign -> String
prettyFmtAlign AlignNone   = ""
prettyFmtAlign AlignLeft   = "<"
prettyFmtAlign AlignRight  = ">"
prettyFmtAlign AlignCenter = "^"
prettyFmtAlign AlignSign   = "="


prettyFmtSign :: FmtSign -> String
prettyFmtSign SignNone  = ""
prettyFmtSign SignPlus  = "+"
prettyFmtSign SignMinus = "-"
prettyFmtSign SignSpace = " "


prettyFmtNumSep :: FmtNumSep -> String
prettyFmtNumSep NumSepNone  = ""
prettyFmtNumSep NumSepDash  = "_"
prettyFmtNumSep NumSepComma = ","


-- | Pretty showing 'ArgFmt'
--
-- Note: Don't create 'ArgFmt' manually, 'read' from a string, see 'ArgFmt'.
prettyArgFmt :: ArgFmt -> String
prettyArgFmt (ArgFmt{fmtRaw=raw@(_:_)}) = raw
prettyArgFmt (ArgFmt{..}) = concat $
    [pad, align, sign, alternate, aware, width, sep, ".", precision, fmtSpecs]
  where
    showInt i = if i == 0 then "" else show i
    pad = if fmtAlign == AlignNone then "" else [fmtPad]
    align = prettyFmtAlign fmtAlign
    sign = prettyFmtSign fmtSign
    alternate = if fmtAlternate then "#" else ""
    aware = if fmtSignAware then "0" else ""
    width = either showInt (('{' :) . (++ "}") . show) fmtWidth
    sep = prettyFmtNumSep fmtNumSep
    precision = either showInt (('{' :) . (++ "}") . show) fmtPrecision


formatText :: ArgFmt -> ShowS
formatText fmt@ArgFmt{fmtWidth=(Left minw), fmtPrecision=(Left maxw)} cs
    | padw > 0 = pad (fmtAlign fmt) padw (fmtPad fmt)
    | otherwise = cs1
  where
    cs1 = if maxw > 0 && maxw < length cs then take maxw cs else cs
    padw = minw - (length cs1)

    pad :: FmtAlign -> Int -> Char -> String
    pad AlignNone   n c = pad AlignLeft n c
    pad AlignLeft   n c = cs1 ++ replicate n c
    pad AlignRight  n c = replicate n c ++ cs1
    pad AlignCenter n c =
      let ln = div n 2
      in replicate ln c ++ cs1 ++ (replicate (n - ln) c)
formatText fmt _ = errorNoParse $ prettyArgFmt fmt


formatNumber :: ArgFmt -> Bool -> Int -> Maybe Char -> ShowS
formatNumber fmt signed sepWidth flag cs = uncurry (++) $
    pad (fmtAlign fmt) (fmtPad fmt) (fmtSignAware fmt) $
      second (seperate (fmtNumSep fmt) sepWidth) $
        sign (fmtSign fmt) signed $
          alternate (fmtAlternate fmt) flag cs
  where
    pad :: FmtAlign -> Char -> Bool -> (String, String) -> (String, String)
    pad AlignNone   c  False (ps, cs) = (ps, cs)
    pad AlignNone   c  True  (ps, cs) = pad AlignSign '0' False (ps, cs)
    pad AlignLeft   c  _     (ps, cs) = (ps, cs ++ replicate (padw ps cs) c)
    pad AlignRight  c  _     (ps, cs) = (replicate (padw ps cs) c ++ ps, cs)
    pad AlignSign   c  True  (ps, cs) = pad AlignSign c False (ps, cs)
    pad AlignSign  '0' False (ps, cs) =
      let cs1 = fixSep (fmtNumSep fmt) sepWidth (padw ps cs) cs
      in (ps, cs1)
    pad AlignSign   c  False (ps, cs) = (ps, replicate (padw ps cs) c ++ cs)
    pad AlignCenter c  _     (ps, cs) =
      let n = padw ps cs; ln = div n 2
      in (replicate ln c ++ ps, cs ++ (replicate (n - ln) c))

    padw :: String -> String -> Int
    padw ps cs = max 0 $
      case fmtWidth fmt of Left n -> n - (length ps) - (length cs); _ -> 0

    fixSep :: FmtNumSep -> Int -> Int -> String -> String
    fixSep NumSepNone _ n cs = replicate n '0' ++ cs
    fixSep sep        w n cs =
      let sepChar = if sep == NumSepComma then ',' else '_'
          (cs1, cs2) = L.break (== sepChar) cs
          (wc, r) = divMod (n + length cs1) (w + 1)
          n1 = n - wc + if r > 0 then 0 else 1
       in rseperate sepChar w (reverse cs1 ++ replicate n1 '0') ++ cs2

    seperate :: FmtNumSep -> Int -> String -> String
    seperate NumSepNone _ cs = cs
    seperate sep        w cs =
      let sepChar = if sep == NumSepComma then ',' else '_'
          (cs1, cs2) = L.break (== '.') cs
      in rseperate sepChar w $ reverse cs1

    rseperate :: Char -> Int -> String -> String
    rseperate sep w cs =
      case L.splitAt w cs of
        (cs1, "")  -> reverse cs1
        (cs1, cs2) -> rseperate sep w cs2 ++ (sep : reverse cs1)

    sign :: FmtSign -> Bool -> (String, String) -> (String, String)
    sign SignNone  True (ps, cs)       = sign SignMinus True (ps, cs)
    sign SignPlus  True (ps, '-' : cs) = ('-' : ps, cs)
    sign SignPlus  True (ps, '+' : cs) = ('+' : ps, cs)
    sign SignPlus  True (ps, cs)       = ('+' : ps, cs)
    sign SignMinus True (ps, '-': cs)  = ('-' : ps, cs)
    sign SignSpace True (ps, '+' : cs) = (' ' : ps, cs)
    sign SignSpace True (ps, '-' : cs) = ('-' : ps, cs)
    sign SignSpace True (ps, cs)       = (' ' : ps, cs)
    sign _         _    (ps, '+' : cs) = (ps, cs)
    sign _         _    (ps, '-' : cs) = (ps, cs)
    sign _         _    (ps, cs)       = (ps, cs)

    alternate :: Bool -> Maybe Char -> String -> (String, String)
    alternate True (Just c) cs = (['0', c], cs)
    alternate _    _        cs = ("", cs)
