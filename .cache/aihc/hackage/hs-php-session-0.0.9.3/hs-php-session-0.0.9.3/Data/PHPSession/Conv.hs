{-# LANGUAGE FlexibleInstances #-}
-- |
-- Module : Data.PHPSession.Conv
-- Copyright: (c) 2014 Edward Blake
-- License: BSD-style
-- Maintainer: Edward L. Blake <edwardlblake@gmail.com>
-- Stability: experimental
-- Portability: portable
--
-- Non-coerced translation between 'PHPSessionValue' and various Haskell types.
-- 'convTo' provide convenient translation from native types to 'PHPSessionValue',
-- while translation from 'PHPSessionValue' to native types is provided through
-- 'convFrom' and 'convFromSafe'.
-- 
module Data.PHPSession.Conv (
    -- * Convert to 'PHPSessionValue'
    convTo,
    -- * Convert from 'PHPSessionValue'
    convFrom,
    convFromSafe,
    -- * Type classes
    ConversionToPHPValue(..),
    ConversionFromPHPValueOrMismatch(..)
) where

import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.PHPSession.Types
import Data.Int (Int32, Int64)
import Data.List as L (foldl')

-- | 'convTo' is a convenience function that converts natively typed
-- values to 'PHPSessionValue', with the resulting PHP type determined
-- by the type cast or inferred.
--
-- > arrayOfPHPStrings :: PHPSessionValue
-- > arrayOfPHPStrings =
-- >   let str1 = "Hello" :: BS.ByteString
-- >       str2 = "World"
-- >    in convTo [(0 :: Int, str1), (1, str2)]
--
-- In the above example code, the @OverloadedStrings@ language extension is assumed.
--
convTo :: ConversionToPHPValue a => a -> PHPSessionValue
convTo val = convTo' val

class ConversionToPHPValue a where
  convTo' :: a -> PHPSessionValue

class ConversionFromPHPValueOrMismatch b where
  convFromOM :: PHPSessionValue -> Either String b

instance ConversionToPHPValue PHPSessionValue where
  convTo' var = var

instance ConversionToPHPValue [(PHPSessionValue,PHPSessionValue)] where
  convTo' var = PHPSessionValueArray var

instance (ConversionToPHPValue a, ConversionToPHPValue b) => ConversionToPHPValue [(a,b)] where
  convTo' var = PHPSessionValueArray $ map (\(al,ar) -> (convTo' al, convTo' ar)) var

instance ConversionToPHPValue Bool where
  convTo' var = PHPSessionValueBool var

instance ConversionToPHPValue Double where
  convTo' var = PHPSessionValueFloat (Right var)

instance ConversionToPHPValue Int where
  convTo' var = PHPSessionValueInt var

instance ConversionToPHPValue Int32 where
  convTo' var = PHPSessionValueInt (fromIntegral var)

instance ConversionToPHPValue Int64 where
  convTo' var = PHPSessionValueInt (fromIntegral var)

instance ConversionToPHPValue a => ConversionToPHPValue (Maybe a) where
  convTo' Nothing = PHPSessionValueNull
  convTo' (Just var) = convTo' var

instance ConversionToPHPValue (PHPSessionClassName, [(PHPSessionValue,PHPSessionValue)]) where
  convTo' (cls,arr) = PHPSessionValueObject cls arr

instance ConversionToPHPValue (PHPSessionClassName, LBS.ByteString) where
  convTo' (cls,arr) = PHPSessionValueObjectSerializeable cls arr

instance ConversionToPHPValue LBS.ByteString where
  convTo' var = PHPSessionValueString var

instance ConversionToPHPValue BS.ByteString where
  convTo' var = PHPSessionValueString (LBS.fromChunks [var])

instance ConversionFromPHPValueOrMismatch [(PHPSessionValue,PHPSessionValue)] where
  convFromOM (PHPSessionValueArray var) = Right var
  convFromOM v = mismatchError v "[(PHPSessionValue,PHPSessionValue)]"

instance ConversionFromPHPValueOrMismatch a => ConversionFromPHPValueOrMismatch [(a,PHPSessionValue)] where
  convFromOM (PHPSessionValueArray vars) =
    case L.foldl' ontoTuple (Right []) vars of
      Left str -> Left str
      Right lst -> Right $ reverse lst
    where
      ontoTuple (Left str) _ = Left str
      ontoTuple (Right lst) (l,r) = 
        let l' = convFromOM l
         in case l' of
              Left errl -> Left errl
              Right l'' ->
                Right ((l'',r) : lst)
  convFromOM v = mismatchError v "ConversionFromPHPValueOrMismatch a => ConversionFromPHPValueOrMismatch [(a,PHPSessionValue)]"

instance ConversionFromPHPValueOrMismatch b => ConversionFromPHPValueOrMismatch [(PHPSessionValue,b)] where
  convFromOM (PHPSessionValueArray vars) =
    convArrayRightSide vars convFromOM
  convFromOM v = mismatchError v "ConversionFromPHPValueOrMismatch b => ConversionFromPHPValueOrMismatch [(PHPSessionValue,b)]"

convArrayRightSide vars conv =
  case L.foldl' ontoTuple (Right []) vars of
    Left str -> Left str
    Right lst -> Right $ reverse lst
  where
    ontoTuple (Left str) _ = Left str
    ontoTuple (Right lst) (l,r) = 
      case conv r of
        Left errr -> Left errr
        Right r'' ->
          Right $ (l, r'') : lst

instance (ConversionFromPHPValueOrMismatch a, ConversionFromPHPValueOrMismatch b) => ConversionFromPHPValueOrMismatch [(a,b)] where
  convFromOM (PHPSessionValueArray vars) =
    convArrayBothSides vars convFromOM
  convFromOM v = mismatchError v "(ConversionFromPHPValueOrMismatch a, ConversionFromPHPValueOrMismatch b) => ConversionFromPHPValueOrMismatch [(a,b)]"

convArrayBothSides vars conv =
  case L.foldl' ontoTuple (Right []) vars of
    Left str -> Left str
    Right lst -> Right $ reverse lst
  where
    ontoTuple (Left str) _ = Left str
    ontoTuple (Right lst) (l,r) = 
      let l' = convFromOM l
          r' = conv r
       in case l' of
            Left errl -> Left errl
            Right l'' ->
              case r' of
                Left errr -> Left errr
                Right r'' ->
                  Right ((l'', r'') : lst)


instance ConversionFromPHPValueOrMismatch Bool where
  convFromOM (PHPSessionValueBool var) = Right var
  convFromOM v = mismatchError v "Bool"

instance ConversionFromPHPValueOrMismatch Double where
  convFromOM (PHPSessionValueFloat (Left  var)) = Right $ fromIntegral var
  convFromOM (PHPSessionValueFloat (Right var)) = Right var
  convFromOM v = mismatchError v "Double"

instance ConversionFromPHPValueOrMismatch Int where
  convFromOM (PHPSessionValueInt var) = Right var
  convFromOM v = mismatchError v "Int"

instance ConversionFromPHPValueOrMismatch Int32 where
  convFromOM (PHPSessionValueInt var) = Right $ fromIntegral var
  convFromOM v = mismatchError v "Int32"

instance ConversionFromPHPValueOrMismatch Int64 where
  convFromOM (PHPSessionValueInt var) = Right $ fromIntegral var
  convFromOM v = mismatchError v "Int64"

instance Integral n => ConversionFromPHPValueOrMismatch (Either n Double) where
  convFromOM (PHPSessionValueFloat (Right var)) = (Right . Right) var
  convFromOM (PHPSessionValueFloat (Left  var)) = (Right . Right . fromIntegral) var
  convFromOM (PHPSessionValueInt var)           = (Right . Left  . fromIntegral) var
  convFromOM v = mismatchError v "Integral n => Either n Double"

instance Integral n => ConversionFromPHPValueOrMismatch (Either n LBS.ByteString) where
  convFromOM (PHPSessionValueString var) = (Right . Right) var
  convFromOM (PHPSessionValueInt var)    = (Right . Left . fromIntegral) var
  convFromOM v = mismatchError v "Integral n => Either n ByteString"

instance ConversionFromPHPValueOrMismatch (Either Double LBS.ByteString) where
  convFromOM (PHPSessionValueString var)        = (Right . Right) var
  convFromOM (PHPSessionValueFloat (Right var)) = (Right . Left ) var
  convFromOM (PHPSessionValueFloat (Left  var)) = (Right . Left . fromIntegral) var
  convFromOM v = mismatchError v "Either Double ByteString"

instance Integral n => ConversionFromPHPValueOrMismatch (Either n BS.ByteString) where
  convFromOM (PHPSessionValueString var) = (Right . Right . BS.concat . LBS.toChunks) var
  convFromOM (PHPSessionValueInt var)    = (Right . Left . fromIntegral) var
  convFromOM v = mismatchError v "Integral n => Either n ByteString"

instance ConversionFromPHPValueOrMismatch (Either Double BS.ByteString) where
  convFromOM (PHPSessionValueString var)        = (Right . Right . BS.concat . LBS.toChunks) var
  convFromOM (PHPSessionValueFloat (Right var)) = (Right . Left ) var
  convFromOM (PHPSessionValueFloat (Left  var)) = (Right . Left . fromIntegral) var
  convFromOM v = mismatchError v "Either Double ByteString"

instance ConversionFromPHPValueOrMismatch (PHPSessionClassName, [(PHPSessionValue,PHPSessionValue)]) where
  convFromOM (PHPSessionValueObject cls arr) = Right (cls,arr)
  convFromOM v = mismatchError v "(PHPSessionClassName, [(PHPSessionValue,PHPSessionValue)])"

instance ConversionFromPHPValueOrMismatch (PHPSessionClassName, LBS.ByteString) where
  convFromOM (PHPSessionValueObjectSerializeable cls arr) = Right (cls,arr)
  convFromOM v = mismatchError v "(PHPSessionClassName, ByteString)"

instance ConversionFromPHPValueOrMismatch LBS.ByteString where
  convFromOM (PHPSessionValueString var) = Right var
  convFromOM v = mismatchError v "ByteString"

instance ConversionFromPHPValueOrMismatch BS.ByteString where
  convFromOM (PHPSessionValueString var) = (Right . BS.concat . LBS.toChunks) var
  convFromOM v = mismatchError v "ByteString"

instance ConversionFromPHPValueOrMismatch a => ConversionFromPHPValueOrMismatch (Maybe a) where
  convFromOM PHPSessionValueNull = Right Nothing
  convFromOM v =
    case convFromOM v of
      Left s -> Left s
      Right b' -> Right (Just b')


mismatchError v totype =
  Left $ "Type mismatch converting from (" ++ show v ++ ") to " ++ totype

-- | 'convFrom' and 'convFromSafe' are convenience functions that translate PHP
-- values stored as 'PHPSessionValue' into appropriate Haskell types depending on
-- the desired type cast or inferred. Functions provided in this module provide
-- non-coerced type translations and so will either carry on the translation or
-- signal the fact that the attempted conversion will alter the type of the
-- value. For situations where altering value types is expected, alternative
-- conversion functions with similar type signatures are provided in modules
-- within /Data.PHPSession.ImplicitConv/.
--
-- The example arrayOfPHPStrings definition given in 'convTo' can be reverted back
-- to Haskell types, which evaluates to @[(0,\"Hello\"),(1,\"World\")]@.
-- 
-- >>> convFrom arrayOfPHPStrings :: [(Int,LBS.ByteString)]
-- [(0,"Hello"),(1,"World")]
--
-- However, if the desired type signature is changed to a completely different type,
-- then a runtime exception is thrown:
--
-- >>> convFrom arrayOfPHPStrings :: [(Int,Int)]
-- *** Exception: Type mismatch converting from (PHPSessionValueString "Hello") to Int
--
-- Where there is the possibility that the value being sought may be @/NULL/@, the
-- type should be @('Maybe' a)@.
--
convFrom :: ConversionFromPHPValueOrMismatch b => PHPSessionValue -> b
convFrom var =
    case convFromOM var of
      Left message -> error message
      Right var' -> var'

-- | A version of 'convFrom' which returns a 'Left' with an error message instead 
-- of throwing an exception.
--
convFromSafe
  :: ConversionFromPHPValueOrMismatch b =>
     PHPSessionValue -> Either String b
convFromSafe var = convFromOM var

