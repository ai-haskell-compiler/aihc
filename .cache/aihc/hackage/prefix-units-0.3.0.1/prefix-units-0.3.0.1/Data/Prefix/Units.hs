{-

Copyright 2012, 2014, 2015, Google Inc.
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
copyright notice, this list of conditions and the following disclaimer
in the documentation and/or other materials provided with the
distribution.
    * Neither the name of Google Inc. nor the names of its
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{- | Definitions and functions for parsing and formatting prefix units.

This module defines the type 'Unit' and associated functions for
parsing numbers containing a prefix unit (e.g. @100M@) into
corespondingly scaled values (for the above example, @100000000@), and
for formatting numbers.

The units definition is taken from the man page @units(7)@ and the web
sites <http://physics.nist.gov/cuu/Units/prefixes.html> and
<http://physics.nist.gov/cuu/Units/binary.html>.

Since a give prefix unit (e.g. @m@) can be interpreted in different
ways, the module offers various ways to interpret this:

* in a binary context (e.g. when talking about memory), this will be
  interpreted as 2^20 (see 'ParseBinary')

* in a SI context dealing with multiples, this will be intepreted as
  10^3 (see 'ParseKMGT')

* in an exact parsing mode, this will be interpreded as the \"milli\"
  prefix, i.e. 10^-3 (see 'ParseExact')

The different parsing mode are offered as different contexts will have
different \"natural\" units, and always forcing precise parsing (which
also implies case-sensitivity) will lead to confusing user interfaces.

The internal calculations when converting values are done via the
'Rational' type (with arbitrary precision), and precision loss happens
only at the last step of converting to the target type; for
float\/doubles this is 'fromRational', for integral types this is
'round'.

A few examples are given below:

>>> showValue FormatBinary 2048
"2.0Ki"
>>> showValue FormatSiAll 0.0001
"100.0u"
>>> showValue (FormatFixed Mebi) 1048576
"1Mi"
>>> parseValue ParseExact "2.5Ki"::Either String Double
Right 2560.0
>>> parseValue ParseBinary "2M"::Either String Int
Right 2097152
>>> parseValue ParseExact "2ki"
Left "Unrecognised unit 'ki'"

The failure in the last example is due to the fact that 'ParseExact'
is case-sensitive.

-}

module Data.Prefix.Units
  (
  -- * Basic definitions
  -- ** Types
  Unit(..)
  , RationalConvertible(..)
  , siUnits
  , siSupraunitary
  , siKMGT
  , binaryUnits
  , siBase
  , binaryBase
  -- ** Unit-related functions
  , unitMultiplier
  , unitName
  , unitSymbol
  , fancySymbol
  -- * Formatting functions
  , FormatMode(..)
  , recommendedUnit
  , formatValue
  , showValue
  -- * Parsing functions
  , ParseMode(..)
  , parseSymbol
  , parseValue
  -- * Low-level generic functions
  , unitRange
  -- ** Parsing
  , ParseOptions(..)
  , parseExactSymbol
  , parseBinarySymbol
  , parseKMGTSymbol
  , parseGeneric
  -- ** Formatting
  , showValueWith
  ) where

import Control.Monad (liftM)
import Data.Char (toUpper)
import Data.List (intercalate)

import Data.Prefix.Units.Compat ()

default ()

-- | The unit type.
data Unit = Quecto
          | Ronto
          | Yocto
          | Zepto
          | Atto
          | Femto
          | Pico
          | Nano
          | Micro
          | Milli
          | Centi
          | Deci
          | Deka
          | Hecto
          | Kilo
          | Kibi
          | Mega
          | Mebi
          | Giga
          | Gibi
          | Tera
          | Tebi
          | Peta
          | Pebi
          | Exa
          | Exbi
          | Zetta
          | Yotta
          | Ronna
          | Quetta
            deriving (Show, Eq, Enum, Bounded, Ord)

-- | List of all SI units.
siUnits :: [Unit]
siUnits
  = [ Quecto
    , Ronto
    , Yocto
    , Zepto
    , Atto
    , Femto
    , Pico
    , Nano
    , Micro
    , Milli
    , Centi
    , Deci
    , Deka
    , Hecto
    , Kilo
    , Mega
    , Giga
    , Tera
    , Peta
    , Exa
    , Zetta
    , Yotta
    , Ronna
    , Quetta
    ]

-- | List of binary units.
binaryUnits :: [Unit]
binaryUnits = [Kibi, Mebi, Gibi, Tebi, Pebi, Exbi]

-- | List of units which are supraunitary (their multiplier is greater
-- than one).
siSupraunitary :: [Unit]
siSupraunitary = filter (>= Deka) siUnits

-- | List of SI units which are greater or equal to 'Kilo'.
siKMGT :: [Unit]
siKMGT = filter (>= Kilo) siUnits

-- | The base for SI units.
siBase :: Rational
siBase = 10

-- | The base for binary units.
binaryBase :: Rational
binaryBase = 2

-- | Returns the unit scaling \"multiplier\" (which can be either
-- supra- or sub-unitary):
--
-- >>> unitMultiplier Micro
-- 1 % 1000000
-- >>> unitMultiplier Mebi
-- 1048576 % 1
unitMultiplier :: Unit -> Rational
unitMultiplier Quecto = siBase     ^^ (-30 :: Int)
unitMultiplier Ronto  = siBase     ^^ (-27 :: Int)
unitMultiplier Yocto  = siBase     ^^ (-24 :: Int)
unitMultiplier Zepto  = siBase     ^^ (-21 :: Int)
unitMultiplier Atto   = siBase     ^^ (-18 :: Int)
unitMultiplier Femto  = siBase     ^^ (-15 :: Int)
unitMultiplier Pico   = siBase     ^^ (-12 :: Int)
unitMultiplier Nano   = siBase     ^^ ( -9 :: Int)
unitMultiplier Micro  = siBase     ^^ ( -6 :: Int)
unitMultiplier Milli  = siBase     ^^ ( -3 :: Int)
unitMultiplier Centi  = siBase     ^^ ( -2 :: Int)
unitMultiplier Deci   = siBase     ^^ ( -1 :: Int)
unitMultiplier Deka   = siBase     ^^ (  1 :: Int)
unitMultiplier Hecto  = siBase     ^^ (  2 :: Int)
unitMultiplier Kilo   = siBase     ^^ (  3 :: Int)
unitMultiplier Kibi   = binaryBase ^^ ( 10 :: Int)
unitMultiplier Mega   = siBase     ^^ (  6 :: Int)
unitMultiplier Mebi   = binaryBase ^^ ( 20 :: Int)
unitMultiplier Giga   = siBase     ^^ (  9 :: Int)
unitMultiplier Gibi   = binaryBase ^^ ( 30 :: Int)
unitMultiplier Tera   = siBase     ^^ ( 12 :: Int)
unitMultiplier Tebi   = binaryBase ^^ ( 40 :: Int)
unitMultiplier Peta   = siBase     ^^ ( 15 :: Int)
unitMultiplier Pebi   = binaryBase ^^ ( 50 :: Int)
unitMultiplier Exa    = siBase     ^^ ( 18 :: Int)
unitMultiplier Exbi   = binaryBase ^^ ( 60 :: Int)
unitMultiplier Zetta  = siBase     ^^ ( 21 :: Int)
unitMultiplier Yotta  = siBase     ^^ ( 24 :: Int)
unitMultiplier Ronna  = siBase     ^^ ( 27 :: Int)
unitMultiplier Quetta = siBase     ^^ ( 30 :: Int)

-- | Returns the unit full name.
unitName :: Unit -> String
unitName Quecto = "quecto"
unitName Ronto  = "ronto"
unitName Yocto  = "yocto"
unitName Zepto  = "zepto"
unitName Atto   = "atto"
unitName Femto  = "femto"
unitName Pico   = "pico"
unitName Nano   = "nano"
unitName Micro  = "micro"
unitName Milli  = "milli"
unitName Centi  = "centi"
unitName Deci   = "deci"
unitName Deka   = "deka"
unitName Hecto  = "hecto"
unitName Kilo   = "kilo"
unitName Kibi   = "kibi"
unitName Mega   = "mega"
unitName Mebi   = "mebi"
unitName Giga   = "giga"
unitName Gibi   = "gibi"
unitName Tera   = "tera"
unitName Tebi   = "tebi"
unitName Peta   = "peta"
unitName Pebi   = "pebi"
unitName Exa    = "exa"
unitName Exbi   = "exbi"
unitName Zetta  = "zetta"
unitName Yotta  = "yotta"
unitName Ronna  = "ronna"
unitName Quetta = "quetta"

-- | Returns the unit ASCII symbol.
unitSymbol :: Unit -> String
unitSymbol Quecto = "q"
unitSymbol Ronto  = "r"
unitSymbol Yocto  = "y"
unitSymbol Zepto  = "z"
unitSymbol Atto   = "a"
unitSymbol Femto  = "f"
unitSymbol Pico   = "p"
unitSymbol Nano   = "n"
unitSymbol Micro  = "u"
unitSymbol Milli  = "m"
unitSymbol Centi  = "c"
unitSymbol Deci   = "d"
unitSymbol Deka   = "da"
unitSymbol Hecto  = "h"
unitSymbol Kilo   = "k"
unitSymbol Kibi   = "Ki"
unitSymbol Mega   = "M"
unitSymbol Mebi   = "Mi"
unitSymbol Giga   = "G"
unitSymbol Gibi   = "Gi"
unitSymbol Tera   = "T"
unitSymbol Tebi   = "Ti"
unitSymbol Peta   = "P"
unitSymbol Pebi   = "Pi"
unitSymbol Exa    = "E"
unitSymbol Exbi   = "Ei"
unitSymbol Zetta  = "Z"
unitSymbol Yotta  = "Y"
unitSymbol Ronna  = "R"
unitSymbol Quetta = "Q"

-- | Returns the unit symbol, which for the 'Micro' unit is not ASCII.
fancySymbol :: Unit -> String
fancySymbol Micro = "\xb5"
fancySymbol u = unitSymbol u

-- * RationalConvertible

-- | Typeclass for handling values that can be converted to\/from
-- 'Rational'.
class (Real a) => RationalConvertible a where
  -- | Converts the value from Ratioal
  convFromRational :: Rational -> a

instance RationalConvertible Int where
  convFromRational = round

instance RationalConvertible Integer where
  convFromRational = round

instance RationalConvertible Float where
  convFromRational = fromRational

instance RationalConvertible Double where
  convFromRational = fromRational

instance RationalConvertible Rational where
  convFromRational = id

-- | Scales a given value to be represented in the desired unit's scale.
scaleToUnit :: (RationalConvertible a) => a -> Unit -> a
scaleToUnit val = convFromRational . (rational_val /) . unitMultiplier
  where rational_val = toRational val

-- | Scales a given value to units from a given unit's scale.
scaleFromUnit :: (RationalConvertible a) => a -> Unit -> a
scaleFromUnit val = convFromRational . (rational_val *) . unitMultiplier
  where rational_val = toRational val

-- * Formatting functionality

-- | Defines the formatting modes.
data FormatMode
  = FormatSiAll          -- ^ Formats the value using any SI unit.
  | FormatSiSupraunitary -- ^ Formats the value using supraunitary SI
                         -- units only (which means that e.g. @0.001@
                         -- will remain as such instead of being
                         -- formatted as @1m@).
  | FormatSiKMGT         -- ^ Formats the value using units greater or
                         -- equal to 'Kilo'.
  | FormatBinary         -- ^ Formats the value using binary units.
  | FormatUnscaled       -- ^ Formats the value as it is, without
                         -- scaling.
  | FormatFixed Unit     -- ^ Formats the value using the given unit.
    deriving (Show)

-- | The available units range for various format modes.
unitRange :: FormatMode -> Either Unit [Unit]
unitRange FormatSiAll          = Right siUnits
unitRange FormatSiSupraunitary = Right siSupraunitary
unitRange FormatSiKMGT         = Right siKMGT
unitRange FormatBinary         = Right binaryUnits
unitRange FormatUnscaled       = Right []
unitRange (FormatFixed u)      = Left u

-- | Whether a given value should be scaled (in auto-scaling modes) or
-- not.
shouldScale :: (Num a, Ord a) => a -> Bool
-- FIXME: this is not nice at all: we hardcode the set [1, 10) instead
-- of having it naturally follow from a base unit or similar.
shouldScale val = val < 1 || val >= 10

-- | Computes the recommended unit for displaying a given value. The
-- simple algorithm uses the first unit for which we have a
-- supraunitary representation. In case we don't find any such value
-- (e.g. for a zero value), then 'Nothing' is returned. For
-- `FormatFixed`, we always select the given unit, irrespective of the
-- value.
recommendedUnit :: (Real a) => FormatMode -> a -> Maybe Unit
recommendedUnit fmt val =
  case unitRange fmt of
    Left u -> Just u
    Right range ->
      if shouldScale val
        then foldr (\u a -> if ratv / unitMultiplier u >= 1 then Just u else a)
               Nothing $ reverse range
        else Nothing
      where ratv = Prelude.toRational val

-- | Computes the scaled value and unit for a given value
formatValue :: (RationalConvertible a) =>
               FormatMode      -- ^ The desired 'FormatMode'
            -> a               -- ^ The value to format
            -> (a, Maybe Unit) -- ^ Scaled value and optional unit
formatValue fmt val =
  let inverter = if val < 0
                   then negate
                   else id
      val' = inverter val
      unit = recommendedUnit fmt val'
      scaled = maybe val' (scaleToUnit val') unit
  in (inverter scaled, unit)

-- | Simple helper to generate the full string representation of an
-- integral value.
showValueWith :: (RationalConvertible a, Show a) =>
                 (Unit -> String)  -- ^ Function to convert the
                                   -- (optional) unit into a
                                   -- string, e.g. 'unitSymbol' or
                                   -- 'fancySymbol'
              -> FormatMode        -- ^ The desired format mode
              -> a                 -- ^ The value to show
              -> String            -- ^ Resulting string
showValueWith symbfn fmt val =
  let (scaled, unit) = formatValue fmt val
  in show scaled ++ maybe "" symbfn unit

-- | Generates a final string representation of a value.
showValue :: (RationalConvertible a, Show a) =>
             FormatMode    -- ^ The desired format mode.
          -> a             -- ^ The value to show
          -> String        -- ^ Resulting string
showValue = showValueWith unitSymbol

-- * Parsing functionality

-- | Error message for unknown unit.
unknownUnit :: String -> Either String Unit
unknownUnit unit = Left $ "Unrecognised unit '" ++ unit ++ "'"

-- | Parses a symbol in the exact mode. See 'ParseExact' for details.
parseExactSymbol :: String -> Either String Unit
parseExactSymbol "q"  = Right Quecto
parseExactSymbol "r"  = Right Ronto
parseExactSymbol "y"  = Right Yocto
parseExactSymbol "z"  = Right Zepto
parseExactSymbol "a"  = Right Atto
parseExactSymbol "f"  = Right Femto
parseExactSymbol "p"  = Right Pico
parseExactSymbol "n"  = Right Nano
parseExactSymbol "u"  = Right Micro
parseExactSymbol "m"  = Right Milli
parseExactSymbol "c"  = Right Centi
parseExactSymbol "d"  = Right Deci
parseExactSymbol "da" = Right Deka
parseExactSymbol "h"  = Right Hecto
parseExactSymbol "k"  = Right Kilo
parseExactSymbol "Ki" = Right Kibi
parseExactSymbol "M"  = Right Mega
parseExactSymbol "Mi" = Right Mebi
parseExactSymbol "G"  = Right Giga
parseExactSymbol "Gi" = Right Gibi
parseExactSymbol "T"  = Right Tera
parseExactSymbol "Ti" = Right Tebi
parseExactSymbol "P"  = Right Peta
parseExactSymbol "Pi" = Right Pebi
parseExactSymbol "E"  = Right Exa
parseExactSymbol "Ei" = Right Exbi
parseExactSymbol "Z"  = Right Zetta
parseExactSymbol "Y"  = Right Yotta
parseExactSymbol "R"  = Right Ronna
parseExactSymbol "Q"  = Right Quetta
parseExactSymbol unit = unknownUnit unit

-- | Helper for 'parseBinarySymbol' which only deals with upper-case
-- strings.
helperParseBinary :: String -> Maybe Unit
helperParseBinary "K"  = Just Kibi
helperParseBinary "KI" = Just Kibi
helperParseBinary "M"  = Just Mebi
helperParseBinary "MI" = Just Mebi
helperParseBinary "G"  = Just Gibi
helperParseBinary "GI" = Just Gibi
helperParseBinary "T"  = Just Tebi
helperParseBinary "TI" = Just Tebi
helperParseBinary "P"  = Just Pebi
helperParseBinary "PI" = Just Pebi
helperParseBinary "E"  = Just Exbi
helperParseBinary "EI" = Just Exbi
helperParseBinary _    = Nothing

-- | Parses a binary symbol. See 'ParseBinary' for details.
parseBinarySymbol :: String -> Either String Unit
parseBinarySymbol symbol =
  maybe (unknownUnit symbol) Right . helperParseBinary . upperSym $ symbol

-- | Helper for 'parseKMGTSymbol' which only deals with upper-case strings.
helperParseKMGT :: String -> Either String Unit
helperParseKMGT "K"  = Right Kilo
helperParseKMGT "KI" = Right Kibi
helperParseKMGT "M"  = Right Mega
helperParseKMGT "MI" = Right Mebi
helperParseKMGT "G"  = Right Giga
helperParseKMGT "GI" = Right Gibi
helperParseKMGT "T"  = Right Tera
helperParseKMGT "TI" = Right Tebi
helperParseKMGT "P"  = Right Peta
helperParseKMGT "PI" = Right Pebi
helperParseKMGT "E"  = Right Exa
helperParseKMGT "EI" = Right Exbi
helperParseKMGT "Z"  = Right Zetta
helperParseKMGT "Y"  = Right Yotta
helperParseKMGT "R"  = Right Ronna
helperParseKMGT "Q"  = Right Quetta
-- FIXME: error message will contain upper-case version of the symbol
helperParseKMGT symbol = unknownUnit symbol

-- | Parses the given symbol as one of the \"big\" units (kilo/kibi
-- and above). This allows the parsing to be case-insensitive.
parseKMGTSymbol :: String -> Either String Unit
parseKMGTSymbol = helperParseKMGT . upperSym

-- | Defines available parse modes.
data ParseMode
  = ParseExact   -- ^ Exact parser mode. This mode is fully
                 -- case-sensitive.
  | ParseKMGT    -- ^ Parses only units bigger than 'Kilo',
                 -- respectively 'Kibi' (for binary units). This
                 -- allows the parser to be case-insensitive.
  | ParseBinary  -- ^ Parses binary units only. In this mode, both the
                 -- exact and shortened forms are accepted (e.g. both
                 -- \"k\" and \"ki\" will be converted into
                 -- 'Kibi'). Furthermore, the parsing is
                 -- case-insensitive.
    deriving (Show, Enum, Bounded)

-- | Defines unit handling mode on parse.
data ParseOptions
  = UnitRequired     -- ^ Requires that the input string
                     -- has a unit.
  | UnitDefault Unit -- ^ If unit is missing, use a
                     -- default one.
  | UnitOptional     -- ^ The unit is optional, a missing
                     -- one means the value is not
                     -- scaled.
    deriving (Show)

-- | Parses a unit from a string. The exact parsing mode determines
-- the rules for parsing and the range of possible units.
parseSymbol :: ParseMode -> String -> Either String Unit
parseSymbol ParseExact = parseExactSymbol
parseSymbol ParseKMGT = parseKMGTSymbol
parseSymbol ParseBinary = parseBinarySymbol

-- | Main parse routine.
parseValue :: (Read a, RationalConvertible a) =>
                ParseMode       -- ^ The desired parse mode
             -> String          -- ^ String to be parsed
             -> Either String a -- ^ Either a Left error message, or
                                -- a Right parsed value
parseValue = parseGeneric UnitOptional []

-- | Validate a parsed unit using a given valid options list.
validUnit :: [Unit] -> Unit -> Either String Unit
validUnit [] unit = Right unit
validUnit ulist unit =
  if unit `notElem` ulist
     then Left $ "Unit '" ++ show unit ++ "' not part of the accepted" ++
            " unit list (" ++ intercalate ", " (map show ulist) ++ ")"
    else Right unit

-- | Parses a string unit depending on the various options and modes.
processUnit :: ParseOptions
            -> ParseMode
            -> [Unit]
            -> String
            -> Either String (Maybe Unit)
processUnit UnitRequired    _ _ "" =
  Left "An unit is required but the input string lacks one"
processUnit (UnitDefault u) _ _ "" = Right $ Just u
processUnit  UnitOptional   _ _ "" = Right   Nothing
processUnit _ pmode valid_units unit_suffix =
  liftM Just (parseSymbol pmode unit_suffix >>= validUnit valid_units)

-- | Low-level parse routine. Takes two function arguments which fix
-- the initial and final conversion, a parse mode and the string to be
-- parsed.
parseGeneric :: (Read a, RationalConvertible a) =>
                ParseOptions    -- ^ Unit options
             -> [Unit]          -- ^ Optional list of valid units
             -> ParseMode       -- ^ The desired parse mode
             -> String          -- ^ String to be parsed
             -> Either String a
parseGeneric popts valid_units pmode str =
  case reads str of
    [(v, suffix)] ->
      let unit_suffix = dropWhile (== ' ') suffix
          unit = processUnit popts pmode valid_units unit_suffix
      in maybe v (scaleFromUnit v) `fmap` unit
    _ -> Left $ "Can't parse string '" ++ str ++ "'"

-- | Converts a string to upper-case.
upperSym :: String -> String
upperSym = map toUpper
