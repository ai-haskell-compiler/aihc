-- |
-- Module      : Text.LDAP.Data
-- Copyright   : 2014 Kei Hibino
-- License     : BSD3
--
-- Maintainer  : ex8k.hibino@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
module Text.LDAP.Data
       (
         -- * DN AST
         Attribute
       , AttrType (..), attrOid
       , AttrValue (..)

       , Component (..), component

       , DN, consDN, unconsDN

       , List1

       , LdifAttrValue (..)

       , -- * Weaken constraint but popular list type
         DN', toDN', Component'

       , -- * Low-level Charset check interfaces
         Bound, exact, boundsElems, inBounds, elem', notElem', inSBounds

       , ordW8
       , quotation, specialChars

       , ldifSafeBounds
       , ldifSafeInitBounds
       ) where

import Prelude hiding (reverse)
import Data.Ord (comparing)
import Data.List (sortBy)
import Data.Char (ord)
import Data.Word (Word8)
import Data.ByteString (ByteString)
import Data.Set (fromList, member)
import Data.List.NonEmpty (NonEmpty ((:|)), reverse, toList)


-- | Not empty list type
type List1 = NonEmpty

-- | Type to express value bound
type Bound a = (a, a)

-- | Bound value to express exact value
exact :: a -> Bound a
exact a = (a, a)

{-# SPECIALIZE bexpand :: (Char, Char) -> [Char] #-}
bexpand :: Enum a => (a, a) -> [a]
bexpand (x, y) = [x .. y]

-- | Element list in value bounds
{-# SPECIALIZE boundsElems :: [(Char, Char)] -> [Char] #-}
boundsElems :: Enum a => [(a, a)] -> [a]
boundsElems =  (>>= bexpand)

{-# SPECIALIZE widerFirst :: [(Char, Char)] -> [(Char, Char)] #-}
widerFirst :: (Enum a, Ord a) => [(a, a)] -> [(a, a)]
widerFirst =  sortBy (flip $ comparing $ length . bexpand)

-- | Test element in value bounds.
{-# SPECIALIZE inBounds :: Char -> [(Char, Char)] -> Bool #-}
inBounds :: (Enum a, Ord a) => a -> [(a, a)] -> Bool
inBounds a = or . map (\(x, y) -> (x <= a && a <= y)) . widerFirst

-- | Test element using ordered set.
{-# SPECIALIZE elem' :: Char -> [Char] -> Bool #-}
elem' :: Ord a => a -> [a] -> Bool
elem' a = (a `member`) . fromList

-- | Test not element using ordered set.
{-# SPECIALIZE notElem' :: Char -> [Char] -> Bool #-}
notElem' :: Ord a => a -> [a] -> Bool
notElem' a = not . (a `elem'`)

-- | Test element in value bounds using ordered set.
{-# SPECIALIZE inSBounds :: Char -> [(Char, Char)] -> Bool #-}
inSBounds :: (Enum a, Ord a) => a -> [(a, a)] -> Bool
inSBounds a = (a `elem'`) . boundsElems

infix 4 `inBounds`, `elem'`, `notElem'`, `inSBounds`

-- | Type of dn attribute type
data AttrType
  = AttrType ByteString
  | AttrOid  (List1 ByteString)
  deriving (Eq, Ord, Show)

-- | Construct OID attribute type
attrOid :: ByteString -> [ByteString] -> AttrType
attrOid hd tl = AttrOid $ hd :| tl

-- | Type of dn attribute value
newtype AttrValue = AttrValue ByteString
                  deriving (Eq, Ord, Show)

-- | Type of dn attribute
type Attribute = (AttrType, AttrValue)

-- | Type of dn component (rdn)
data Component
  = S Attribute
  | L (List1 Attribute)
  deriving (Eq, Ord, Show)

-- | Construct dn component (rdn)
component :: Attribute -> [Attribute] -> Component
component =  d  where
  d x  []        =  S   x
  d x  xs@(_:_)  =  L $ x :| xs

-- | Type of dn
type DN = List1 Component

-- | Construct dn
consDN :: Component -> [Component] -> DN
consDN h tl = reverse $ h :| tl

-- | Deconstruct dn
unconsDN :: DN -> (Component, [Component])
unconsDN dn = (h, tl)  where (h :| tl) = reverse dn

-- | Type of dn component (rdn), simple list type
type Component' = [Attribute]

-- | Type of dn, simple list type
type DN' = [Component']

-- | From 'DN' to 'DN''
toDN' :: DN -> DN'
toDN' =  map comp' . toList  where
  comp' (S a)  = [a]
  comp' (L as) = toList as

-- | Word8 value of Char
ordW8 :: Char -> Word8
ordW8 =  fromIntegral . ord

-- | Quotation word8 code of dn
quotation :: Word8
quotation =  ordW8 '"'

-- | Secial word8 codes of dn
specialChars :: [Word8]
specialChars =  map ordW8 [',', '=', '+', '<', '>', '#', ';']


-- LDIF
-- | Type of LDIF attribute value
data LdifAttrValue
  = LAttrValRaw    ByteString
  | LAttrValBase64 ByteString
  deriving (Eq, Ord, Show)

-- | Char bounds LDIF safe string
ldifSafeBounds :: [Bound Char]
ldifSafeBounds =
  [ ('\x01', '\x09')
  , ('\x0B', '\x0C')
  , ('\x0E', '\x7F')
  ]

-- | Char bounds LDIF safe string first char
ldifSafeInitBounds :: [Bound Char]
ldifSafeInitBounds =
  [ ('\x01', '\x09')
  , ('\x0B', '\x0C')
  , ('\x0E', '\x1F')
  , ('\x21', '\x39')
  , exact '\x3B'
  , ('\x3D', '\x7F')
  ]
