{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module : Data.PHPSession.ImplicitConv.ConvBool
-- Copyright: (c) 2014 Edward Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Non-coerced and coerced rule sets for converting 'PHPSessionValue' objects to 'Bool'.
-- 
module Data.PHPSession.ImplicitConv.ConvBool (
    -- * Non-coerced from 'PHPSessionValue'
    boolDefaultFalseFrom,
    boolDefaultTrueFrom,
    -- * Type coercion from 'PHPSessionValue'
    boolFromPHPLooseComparisonWithTrue,
    boolFromPHPLooseComparisonWithTrueNullable,
    boolFromReducedLooseCoercionSafe,
    boolFromReducedLooseCoercion,
    boolFromReducedLooseCoercionNullableSafe,
    boolFromReducedLooseCoercionNullable,
    boolFromESBooleanCoercionRules,
    boolFromPerlBooleanCoercionRules,
    boolFromPythonBooleanCoercionRules,
    boolFromLuaBooleanCoercionRules
) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.PHPSession.Types

-- | Returns the value of a boolean value as a 'Bool', or returns False as the default
-- 'Bool' value if the 'PHPSessionValue' is not a boolean type. This function is
-- similar to the \"strict comparison\" operator against the boolean @/TRUE/@ value.
--
boolDefaultFalseFrom :: PHPSessionValue -> Bool
boolDefaultFalseFrom var =
  case var of
    PHPSessionValueBool b -> b
    _ -> False

-- | Returns the value of a boolean value as a 'Bool', or returns True as the default
-- 'Bool' value if the 'PHPSessionValue' is not a boolean type.
-- 
boolDefaultTrueFrom :: PHPSessionValue -> Bool
boolDefaultTrueFrom var =
  case var of
    PHPSessionValueBool b -> b
    _ -> True

-- | Coerces a 'PHPSessionValue' to a 'Bool' based on PHP's conversion rules to
-- convert arbitrary values to boolean for evaluation. This function's behaviour
-- is also reminiscent of PHP's \"loose comparison\" operator against boolean
-- @/TRUE/@.
-- <http://php.net/manual/language.types.boolean.php>
-- 
-- Values that result in 'True' are @/TRUE/@, non-zero numbers, non-empty strings
-- other than \"0\", objects, and non-empty arrays.
-- 
-- A conversion documented in the PHP manual involving @SimpleXML@ objects that
-- coerce to @/FALSE/@ instead of @/TRUE/@ when created with empty tags is not
-- implemented by this function, as SimpleXML objects cannot be serialized directly
-- and so don't have a valid serializable form to convert from in any case.
--
-- Because of the range of valid values of the \"loose comparison\" operator, this
-- function and variations based on other dynamically typed languages is provided 
-- mainly for circumstances where they may be the best fit to map particular data
-- values to a boolean truth value system. 'boolFromReducedLooseCoercionSafe'
-- provides a significantly reduced subset of valid values to coerce from for the
-- purpose of determining a \"confirmation\" value, as opposed to simply determining
-- if a value exists or not.
--
boolFromPHPLooseComparisonWithTrue :: PHPSessionValue -> Bool
boolFromPHPLooseComparisonWithTrue var =
  case var of
    PHPSessionValueBool b -> b
    PHPSessionValueArray arr ->
      case arr of
        [] -> False
        _  -> True
    PHPSessionValueFloat (Left i) -> (i /= 0)
    PHPSessionValueFloat (Right f) -> (f /= 0.0)
    PHPSessionValueInt i -> (i /= 0)
    PHPSessionValueNull -> False
    PHPSessionValueString str ->
      case str of
        ""  -> False
        "0" -> False
        _   -> True
    PHPSessionValueObject _ _ -> True
    PHPSessionValueObjectSerializeable _ _ -> True
    PHPSessionValueMisc _ _ -> True

-- | A version of 'boolFromPHPLooseComparisonWithTrue' where @/NULL/@ returns 'Nothing'
-- and all other inputs returns 'Just' with the boolean result.
boolFromPHPLooseComparisonWithTrueNullable :: PHPSessionValue -> Maybe Bool
boolFromPHPLooseComparisonWithTrueNullable var =
  case var of
    PHPSessionValueNull -> Nothing
    _ | var /= PHPSessionValueNull ->
      Just $ boolFromPHPLooseComparisonWithTrue var


-- | Coerces a 'PHPSessionValue' to a 'Bool' through a reduced subset of valid values.
-- 'boolFromReducedLooseCoercionSafe' and 'boolFromReducedLooseCoercion' are
-- functions intended for testing a variable for a boolean \"confirmation\" from a
-- limited number of probable representations.
--
-- Values that result in 'True' are @/TRUE/@, non-zero numbers, \"1\" and \"-1\".
--
-- Values that result in 'False' are @/FALSE/@, zero numbers, @/NULL/@, \"0\" and \"\".
--
-- Values such as arrays, objects and arbitrary strings, are invalid with this
-- function and return an error. In the case of 'boolFromReducedLooseCoercionSafe'
-- this returns a 'Left' with the error message.
--
boolFromReducedLooseCoercionSafe :: PHPSessionValue -> Either String Bool
boolFromReducedLooseCoercionSafe var =
  case var of
    PHPSessionValueArray _ -> mismatchError var "Bool"
    PHPSessionValueBool b -> Right b
    PHPSessionValueFloat (Left i) -> Right (i /= 0)
    PHPSessionValueFloat (Right f) -> Right (f /= 0)
    PHPSessionValueInt i -> Right (i /= 0)
    PHPSessionValueNull -> Right False
    PHPSessionValueObject _ _ -> mismatchError var "Bool"
    PHPSessionValueObjectSerializeable _ _ -> mismatchError var "Bool"
    PHPSessionValueString str ->
      case str of
        "1"  -> Right True
        "-1" -> Right True
        "0"  -> Right False
        ""   -> Right False
        _    -> mismatchError var "Bool"
    PHPSessionValueMisc _ _ -> mismatchError var "Bool"

mismatchError v totype =
  Left $ "Type mismatch converting from (" ++ show v ++ ") to " ++ totype


-- | A version of 'boolFromReducedLooseCoercionSafe' that may throw exceptions.
--
boolFromReducedLooseCoercion :: PHPSessionValue -> Bool
boolFromReducedLooseCoercion var =
    case boolFromReducedLooseCoercionSafe var of
      Left message -> error message
      Right var' -> var'

-- | A version of 'boolFromReducedLooseCoercionSafe' where @/NULL/@ returns 'Right' 'Nothing'
-- and all other inputs returns 'Right' 'Just' with the boolean result. May return 'Left'
-- with an error message if given invalid values.
--
boolFromReducedLooseCoercionNullableSafe :: PHPSessionValue -> Either String (Maybe Bool)
boolFromReducedLooseCoercionNullableSafe var =
  case var of
    PHPSessionValueNull -> Right Nothing
    _ | var /= PHPSessionValueNull ->
      case boolFromReducedLooseCoercionSafe var of
        Left message -> Left message
        Right result -> Right (Just result)

-- | A version of 'boolFromReducedLooseCoercion' where @/NULL/@ returns 'Nothing'
-- and all other inputs returns 'Just' with the boolean result. May throw an
-- exception on invalid values.
--
boolFromReducedLooseCoercionNullable :: PHPSessionValue -> Maybe Bool
boolFromReducedLooseCoercionNullable var =
  case var of
    PHPSessionValueNull -> Nothing
    _ | var /= PHPSessionValueNull ->
      Just $ boolFromReducedLooseCoercion var


-- | Alternative boolean coercion based on the JS/ES boolean conversion rules. 
-- A particular distinction of the coercion behaviour of this function compared
-- to the behaviour of other functions is that floating point NaN returns
-- 'False' instead of 'True'. Values that return 'False' are @/NAN/@, 0,
-- the empty string \"\", @/NULL/@ and @/FALSE/@
--
-- PHP objects, arrays and objects implementing Serializable always return 'True'.
--
-- JS/ES's boolean conversion is described in section 9.2 of the ECMA 262 standard.
--
boolFromESBooleanCoercionRules :: PHPSessionValue -> Bool
boolFromESBooleanCoercionRules var =
  case var of
    PHPSessionValueArray arr -> True
    PHPSessionValueBool b -> b
    PHPSessionValueFloat (Left i) -> i /= 0
    PHPSessionValueFloat (Right f) ->
      case f of
        0 -> False
        _ | isNaN f -> False
        _ -> True
    PHPSessionValueInt i -> i /= 0
    PHPSessionValueNull -> False
    PHPSessionValueObject _ arr -> True
    PHPSessionValueObjectSerializeable _ bstr -> True
    PHPSessionValueString str ->
      case str of
        ""  -> False
        _   -> True
    PHPSessionValueMisc _ _ -> True

-- | Alternative boolean coercion based on non-overloaded Perl 5 rules. 
-- Values that return 'False' are the empty string \"\", the string containing
-- \"0\", 0, @/NULL/@ and @/FALSE/@.
-- 
-- PHP objects, arrays and objects implementing Serializable always return 'True'. 
--
-- Perl's boolean coercion rules are described in the section \"Truth and Falsehood\"
-- of \"perlsyn\".
-- <http://perldoc.perl.org/perlsyn.html#Truth-and-Falsehood>
--
boolFromPerlBooleanCoercionRules :: PHPSessionValue -> Bool
boolFromPerlBooleanCoercionRules var =
  case var of
    PHPSessionValueArray arr -> True
    PHPSessionValueBool b -> b
    PHPSessionValueFloat (Left i) -> i /= 0
    PHPSessionValueFloat (Right f) -> f /= 0
    PHPSessionValueInt i -> i /= 0
    PHPSessionValueNull -> False
    PHPSessionValueObject _ arr -> True
    PHPSessionValueObjectSerializeable _ bstr -> True
    PHPSessionValueString str ->
      case str of
        "0" -> False
        ""  -> False
        _   -> True
    PHPSessionValueMisc _ _ -> True

-- | Alternative boolean coercion based on non-overloaded Python rules. 
-- Values that return 'False' are the empty array @/[]/@, the empty string
-- \"\", 0, @/NULL/@ and @/FALSE/@.
-- 
-- PHP objects and objects implementing Serializable always return 'True'. 
--
-- Python's boolean conversion rules are described in \"Truth Value Testing\"
-- in section 3.1 of Python 2.5's Library Reference.
-- <https://docs.python.org/release/2.5.2/lib/truth.html>
--
boolFromPythonBooleanCoercionRules :: PHPSessionValue -> Bool
boolFromPythonBooleanCoercionRules var =
  case var of
    PHPSessionValueArray arr ->
      case arr of
        [] -> False
        _  -> True
    PHPSessionValueBool b -> b
    PHPSessionValueFloat (Left i) -> i /= 0
    PHPSessionValueFloat (Right f) -> f /= 0
    PHPSessionValueInt i -> i /= 0
    PHPSessionValueNull -> False
    PHPSessionValueObject _ arr -> True
    PHPSessionValueObjectSerializeable _ bstr -> True
    PHPSessionValueString str ->
      case str of
        "" -> False
        _  -> True
    PHPSessionValueMisc _ _ -> True

-- | Alternative boolean coercion based on Lua rules, all values are 'True' except
-- @/NULL/@ and @/FALSE/@.
-- 
-- For Lua's conversion rules, refer to the passage in section 2.4.4 \"Control
-- Structures\" of Lua 5.1's Manual.
-- <http://www.lua.org/manual/5.1/manual.html>
--
boolFromLuaBooleanCoercionRules :: PHPSessionValue -> Bool
boolFromLuaBooleanCoercionRules var =
  case var of
    PHPSessionValueArray _ -> True
    PHPSessionValueBool b  -> b
    PHPSessionValueFloat _ -> True
    PHPSessionValueInt i   -> True
    PHPSessionValueNull    -> False
    PHPSessionValueObject _ _ -> True
    PHPSessionValueObjectSerializeable _ _ -> True
    PHPSessionValueString str -> True
    PHPSessionValueMisc _ _   -> True


