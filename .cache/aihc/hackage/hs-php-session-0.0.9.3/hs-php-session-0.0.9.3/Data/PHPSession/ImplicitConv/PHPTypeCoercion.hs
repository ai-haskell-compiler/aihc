{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}
-- |
-- Module : Data.PHPSession.ImplicitConv.PHPTypeCoercion
-- Copyright: (c) 2014 Edward Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Functions for performing conversion from 'PHPSessionValue' objects to Haskell
-- types while using a subset of the implicit PHP type coercion behaviour. Some
-- of the differences from the implicit type conversion found in PHP are noted:
--
-- * Conversions that are documented as undefined behaviour in the PHP manual
--   will throw definite exceptions with these functions.
--
-- * A significant difference from PHP's conversion rules is that @/NULL/@
--   cannot be directly converted to any data type except to 'Bool', otherwise
--   @/NULL/@ has to be handled by using the type @'Maybe' a@ to capture
--   nullable values.
--
-- * Objects that implement Serializable are not convertible to any other type
--   at all as their value systems are not directly interpretable in a meaningful
--   manner.
--
-- * Numbers can be converted to strings, but only strings that satisfy
--   @reads str = [(value, \"\")]@ can be converted back to numbers.
--
-- * Arrays and objects to string conversions which would normally be coerced
--   to the simple strings \"Array\" and \"Object\" in PHP, are simply considered
--   errors with these conversion functions.
--
module Data.PHPSession.ImplicitConv.PHPTypeCoercion (
    -- * Convert from 'PHPSessionValue'
    convFromPHPImplicit,
    convFromPHPImplicitSafe,
    -- * Type classes
    ConversionFromPHPImplicitValueOrMismatch(..)
) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.PHPSession.Types
import Data.PHPSession.ImplicitConv.ConvBool
import Data.Int (Int32, Int64)
import Data.List as L (foldl')
import Data.PHPSession.Conv

class ConversionFromPHPImplicitValueOrMismatch b where
  convFromPHPImplicitOM :: PHPSessionValue -> Either String b


isScalarForArray v =
  case v of
    PHPSessionValueBool _     -> True
    PHPSessionValueFloat _    -> True
    PHPSessionValueInt _      -> True
    PHPSessionValueString _   -> True
    PHPSessionValueNull       -> False
    PHPSessionValueObjectSerializeable _ _ -> False
    PHPSessionValueMisc _ _   -> False
    PHPSessionValueObject _ _ -> False
    PHPSessionValueArray _    -> False
  
--
-- Refer to the following reference on array conversion at: 
-- <http://php.net/manual/en/language.types.array.php#language.types.array.casting>
--
instance ConversionFromPHPImplicitValueOrMismatch [(PHPSessionValue,PHPSessionValue)] where
  -- Not converted to array:
  -- * PHPSessionValueNull (differs from PHP: PHP creates empty array)
  -- * PHPSessionValueObjectSerializeable
  -- * PHPSessionValueMisc
  convFromPHPImplicitOM v | isScalarForArray v = arrayScalarAsValue v
  convFromPHPImplicitOM (PHPSessionValueObject _cls arr) = Right arr
  convFromPHPImplicitOM (PHPSessionValueArray arr)       = Right arr
  convFromPHPImplicitOM v = mismatchError v "[(PHPSessionValue,PHPSessionValue)]"


arrayScalarAsValue :: PHPSessionValue -> Either String [(PHPSessionValue,PHPSessionValue)]
arrayScalarAsValue r =
  Right ([(PHPSessionValueInt 0, r)])

convArrayScalarLeftSide :: ConversionFromPHPImplicitValueOrMismatch a => PHPSessionValue -> Either String [(a,PHPSessionValue)]
convArrayScalarLeftSide r = 
  case convFromPHPImplicitOM (PHPSessionValueInt 0) of
    Left message -> Left message
    Right index ->
      Right ([(index, r)])

convArrayListLeftSide :: ConversionFromPHPImplicitValueOrMismatch a => [(PHPSessionValue,PHPSessionValue)] -> Either String [(a,PHPSessionValue)]
convArrayListLeftSide vars =
  case L.foldl' ontoTuple (Right []) vars of
    Left str -> Left str
    Right lst -> Right $ reverse lst
  where
    ontoTuple (Left str) _ = Left str
    ontoTuple (Right lst) (l,r) = 
      let l' = convFromPHPImplicitOM l
       in case l' of
            Left errl -> Left errl
            Right l'' ->
              Right ((l'',r) : lst)

instance ConversionFromPHPImplicitValueOrMismatch b => ConversionFromPHPImplicitValueOrMismatch [(PHPSessionValue,b)] where
  -- Not converted to array:
  -- * PHPSessionValueNull (differs from PHP: PHP creates empty array)
  -- * PHPSessionValueObjectSerializeable
  -- * PHPSessionValueMisc
  convFromPHPImplicitOM v | isScalarForArray v = convArrayScalarRightSide v convFromPHPImplicitOM
  convFromPHPImplicitOM (PHPSessionValueObject _cls arr) = convArrayListRightSide arr convFromPHPImplicitOM
  convFromPHPImplicitOM (PHPSessionValueArray arr)       = convArrayListRightSide arr convFromPHPImplicitOM
  convFromPHPImplicitOM v = mismatchError v "ConversionFromPHPValueOrMismatch b => ConversionFromPHPValueOrMismatch [(PHPSessionValue,b)]"


-- convArrayScalarRightSide :: ConversionFromPHPImplicitValueOrMismatch b => PHPSessionValue -> Either String [(PHPSessionValue,b)]
convArrayScalarRightSide r conv =
  case conv r of
    Left message -> Left message
    Right r' -> Right ([(PHPSessionValueInt 0, r')])

-- convArrayListRightSide :: ConversionFromPHPImplicitValueOrMismatch b => [(PHPSessionValue,PHPSessionValue)] -> Either String [(PHPSessionValue,b)]
convArrayListRightSide vars conv =
  case L.foldl' ontoTuple (Right []) vars of
    Left str -> Left str
    Right lst -> Right $ reverse lst
  where
    ontoTuple (Left str) _ = Left str
    ontoTuple (Right lst) (l,r) = 
      case conv r of
        Left errr -> Left errr
        Right r'' ->
          Right $ (l,r'') : lst


instance (ConversionFromPHPImplicitValueOrMismatch a, ConversionFromPHPImplicitValueOrMismatch b) => ConversionFromPHPImplicitValueOrMismatch [(a,b)] where
  -- Not converted to array:
  -- * PHPSessionValueNull (differs from PHP: PHP creates empty array)
  -- * PHPSessionValueObjectSerializeable
  -- * PHPSessionValueMisc
  convFromPHPImplicitOM v | isScalarForArray v = convArrayScalarBothSides v convFromPHPImplicitOM
  convFromPHPImplicitOM (PHPSessionValueObject _cls arr) = convArrayListBothSides arr convFromPHPImplicitOM
  convFromPHPImplicitOM (PHPSessionValueArray arr)       = convArrayListBothSides arr convFromPHPImplicitOM
  convFromPHPImplicitOM v = mismatchError v "(ConversionFromPHPValueOrMismatch a, ConversionFromPHPValueOrMismatch b) => ConversionFromPHPValueOrMismatch [(a,b)]"


convArrayScalarBothSides
  :: ConversionFromPHPImplicitValueOrMismatch t1 =>
     t -> (t -> Either String t2) -> Either String [(t1, t2)]
convArrayScalarBothSides r conv = 
  case convFromPHPImplicitOM (PHPSessionValueInt 0) of
    Left message -> Left message
    Right index ->
      case conv r of
        Left message' -> Left message'
        Right r' -> Right ([(index, r')])

-- convArrayListBothSides :: (ConversionFromPHPImplicitValueOrMismatch a, ConversionFromPHPImplicitValueOrMismatch b) => [(PHPSessionValue,PHPSessionValue)] -> Either String [(a,b)]
convArrayListBothSides vars conv = 
  case L.foldl' ontoTuple (Right []) vars of
    Left str -> Left str
    Right lst -> Right $ reverse lst
  where
    ontoTuple (Left str) _ = Left str
    ontoTuple (Right lst) (l,r) = 
      case convFromPHPImplicitOM l of
        Left errl -> Left errl
        Right l'' ->
          case conv r of
            Left errr -> Left errr
            Right r'' ->
              Right ((l'',r'') : lst)

{-
-- Sequenceable: 
instance ConversionFromPHPImplicitValueOrMismatch [PHPSessionValue] where
  -- Not converted to array:
  -- * PHPSessionValueNull (differs from PHP: PHP creates empty array)
  -- * PHPSessionValueObjectSerializeable
  -- * PHPSessionValueMisc
  convFromPHPImplicitOM v | isScalarForArray v = Right [v]
  convFromPHPImplicitOM (PHPSessionValueObject _cls arr) = Right $ map (\(_,a) -> a) arr
  convFromPHPImplicitOM (PHPSessionValueArray arr) = Right $ map (\(_,a) -> a) arr
  convFromPHPImplicitOM v = mismatchError v "[PHPSessionValue]"
-}

-- Refer to Data.PHPSession.ImplicitConv.ConvBool for the boolean implicit
-- conversion.
--
instance ConversionFromPHPImplicitValueOrMismatch Bool where
  convFromPHPImplicitOM var =
    Right $ boolFromPHPLooseComparisonWithTrue var

-- Refer to the following reference for floating point conversion rules at
-- <http://php.net/manual/languages.types.float.php#language.types.float.casting>
--
instance ConversionFromPHPImplicitValueOrMismatch Double where
  convFromPHPImplicitOM (PHPSessionValueFloat (Right var)) = (Right) var
  convFromPHPImplicitOM (PHPSessionValueFloat (Left  var)) = (Right . fromIntegral) var
  convFromPHPImplicitOM (PHPSessionValueString str) =
    case reads str' of
      [(val,"")] -> Right val
      [] -> let v = PHPSessionValueString str
             in mismatchError v "Double"
    where
      str' = LBS.unpack str
  convFromPHPImplicitOM var =
    let intvar = convFromPHPImplicitOM var :: Either String Int
     in case intvar of
          Left message -> Left message
          Right intvar' ->
            (Right . fromIntegral) intvar'


-- Refer to the following reference for integer conversion rules:
-- <http://php.net/manual/en/language.types.integer.php#language.types.integer.casting>
--
instance ConversionFromPHPImplicitValueOrMismatch Int where
  -- Not converted to Int:
  -- * PHPSessionValueArray
  -- * PHPSessionValueObject
  -- * PHPSessionValueNull
  -- * PHPSessionValueObjectSerializeable
  -- * PHPSessionValueMisc
  convFromPHPImplicitOM (PHPSessionValueBool b) = Right $ if b then 1 else 0
  convFromPHPImplicitOM (PHPSessionValueFloat lr) =
    Right $ case lr of
              Left i -> i
              Right f -> floor f
  convFromPHPImplicitOM (PHPSessionValueInt val) = Right $ fromIntegral val
  convFromPHPImplicitOM (PHPSessionValueString str) =
    case reads str' of
      [(val,"")] -> Right $ fromIntegral val
      [] ->
        case reads str' of
          [(valdbl,"")] -> Right $ floor valdbl
          [] -> let v = PHPSessionValueString str
                 in mismatchError v "Int"
    where str' = LBS.unpack str
  convFromPHPImplicitOM v = mismatchError v "Int"
instance ConversionFromPHPImplicitValueOrMismatch Int32 where
  convFromPHPImplicitOM var =
    let var' = convFromPHPImplicitOM var :: Either String Int
     in case var' of
          Left message -> Left message
          Right int -> Right $ fromIntegral int
instance ConversionFromPHPImplicitValueOrMismatch Int64 where
  convFromPHPImplicitOM var =
    let var' = convFromPHPImplicitOM var :: Either String Int
     in case var' of
          Left message -> Left message
          Right int -> Right $ fromIntegral int


instance ConversionFromPHPImplicitValueOrMismatch (PHPSessionClassName, [(PHPSessionValue,PHPSessionValue)]) where
  -- Not converted to Object:
  -- * PHPSessionValueObjectSerializeable
  -- * PHPSessionValueMisc
  convFromPHPImplicitOM (PHPSessionValueArray arr) = Right (phpStdClass,arr)
  convFromPHPImplicitOM (PHPSessionValueBool b)  = Right (phpStdClass,[(phpScalarMember, PHPSessionValueBool b)])
  convFromPHPImplicitOM (PHPSessionValueFloat a) = Right (phpStdClass,[(phpScalarMember, PHPSessionValueFloat a)])
  convFromPHPImplicitOM (PHPSessionValueInt i)   = Right (phpStdClass,[(phpScalarMember, PHPSessionValueInt i)])
  convFromPHPImplicitOM (PHPSessionValueNull)    = Right (phpStdClass,[])
  convFromPHPImplicitOM (PHPSessionValueObject cls arr) = Right (cls,arr)
  convFromPHPImplicitOM (PHPSessionValueString a) = Right (phpStdClass,[(phpScalarMember, PHPSessionValueString a)])
  convFromPHPImplicitOM v = mismatchError v "(PHPSessionClassName, [(PHPSessionValue,PHPSessionValue)])"

phpStdClass = PHPSessionClassName "stdClass"
phpScalarMember = PHPSessionValueString "scalar"

instance ConversionFromPHPImplicitValueOrMismatch (PHPSessionClassName, LBS.ByteString) where
  -- Not converted to (and from) objects implementing Serializable:
  -- * PHPSessionValueArray
  -- * PHPSessionValueBool
  -- * PHPSessionValueFloat,
  -- * PHPSessionValueInt
  -- * PHPSessionValueNull
  -- * PHPSessionValueObject
  -- * PHPSessionValueString,
  -- * PHPSessionValueMisc
  convFromPHPImplicitOM (PHPSessionValueObjectSerializeable cls arr) = Right (cls,arr)
  convFromPHPImplicitOM v = mismatchError v "(PHPSessionClassName, LBS.ByteString)"

--
--
instance ConversionFromPHPImplicitValueOrMismatch LBS.ByteString where
  -- Not converted to string:
  -- * PHPSessionValueArray
  -- * PHPSessionValueObject
  -- * PHPSessionValueObjectSerializeable
  -- * PHPSessionValueNull (differs from PHP: PHP creates empty string "")
  -- * PHPSessionValueMisc
  convFromPHPImplicitOM (PHPSessionValueBool b) =
     case b of
       True  -> Right "1"
       False -> Right ""
  convFromPHPImplicitOM (PHPSessionValueFloat a) =
    case a of
      Left i  -> (Right . LBS.pack . show) i
      Right f -> (Right . LBS.pack . show) f
  convFromPHPImplicitOM (PHPSessionValueInt i) =
    (Right . LBS.pack . show) i
  -- Unused: convFromPHPImplicitOM (PHPSessionValueNull) = Right ""
  convFromPHPImplicitOM (PHPSessionValueString var) = Right var
  convFromPHPImplicitOM v = mismatchError v "ByteString"

instance ConversionFromPHPImplicitValueOrMismatch BS.ByteString where
  convFromPHPImplicitOM var =
    let var' = convFromPHPImplicitOM var :: Either String LBS.ByteString
     in case var' of
          Left message -> Left message
          Right str -> Right (BS.concat $ LBS.toChunks str)

instance ConversionFromPHPImplicitValueOrMismatch a => ConversionFromPHPImplicitValueOrMismatch (Maybe a) where
  convFromPHPImplicitOM PHPSessionValueNull = Right Nothing
  convFromPHPImplicitOM v =
    case convFromPHPImplicitOM v of
      Left s -> Left s
      Right b' -> Right (Just b')
  

mismatchError v totype =
  Left $ "Type mismatch converting from (" ++ show v ++ ") to " ++ totype

-- | 'convFromPHPImplicit' and 'convFromPHPImplicitSafe' are functions that convert
-- values stored as 'PHPSessionValue' into appropriate Haskell types depending on
-- the desired type cast or inferred. Unlike the 'convFrom' and 'convFromSafe' 
-- functions provided in "Data.PHPSession.Conv", functions provided in this module 
-- perform type coercion based on a significant number of conversion rules to
-- satisfy the type cast or inferred.
--
-- The example @arrayOfPHPStrings@ definition given in the example documented in
-- "Data.PHPSession.Conv" can be evaluated to @[(0,\"Hello\"),(1,\"World\")]@.
-- 
-- >>> convFromPHPImplicit arrayOfPHPStrings :: [(Int,LBS.ByteString)]
-- [(0,"Hello"),(1,"World")]
--
-- However, if the desired type signature is changed:
--
-- >>> convFromPHPImplicit arrayOfPHPStrings :: [(LBS.ByteString,LBS.ByteString)]
-- [("0","Hello"),("1","World")]
--
-- Where there is the possibility that the value being sought may be @/NULL/@, the
-- type should be @('Maybe' a)@.
--
convFromPHPImplicit :: ConversionFromPHPImplicitValueOrMismatch b => PHPSessionValue -> b
convFromPHPImplicit var =
    case convFromPHPImplicitOM var of
      Left message -> error message
      Right var' -> var'

-- | 'convFromPHPImplicitSafe' is a version of 'convFromPHPImplicit' that returns a
-- 'Left' with an error message instead of throwing a run time exception.
--
convFromPHPImplicitSafe
  :: ConversionFromPHPImplicitValueOrMismatch b =>
     PHPSessionValue -> Either String b
convFromPHPImplicitSafe var = convFromPHPImplicitOM var
