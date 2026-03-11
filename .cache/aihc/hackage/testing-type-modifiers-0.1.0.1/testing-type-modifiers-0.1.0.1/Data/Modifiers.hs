{-# LANGUAGE DeriveDataTypeable #-}

-- | Type modifiers for writing properties that quantify over commonly used subsets of standard types. 
-- | 
-- | Currently there are only a few modifiers, more will be added. 
module Data.Modifiers(
  -- ** List modifiers
  NonEmpty(..),
  mkNonEmpty,

  -- ** Numeric modifiers
  Nat(..),
  NonZero(..),
  
  -- ** Character and string modifiers
  Unicode(..),
  unicodes,
  Printable(..),
  printables
  
  ) where

import Data.Typeable

-- | A type of natural numbers such that @ nat a >= 0 @.
newtype Nat a = Nat {nat :: a} 
  deriving (Typeable, Show, Eq, Ord)

-- | A type of non-zero integers such that @ nonZero a /= 0 @.
newtype NonZero a = NonZero {nonZero :: a}
  deriving (Typeable, Show, Eq, Ord)


-- | A type of non empty lists such that @ nonEmpty xs /= [] @.
newtype NonEmpty a = NonEmpty {nonEmpty :: [a]} 
  deriving (Typeable, Show)
mkNonEmpty :: a -> [a] -> NonEmpty a
mkNonEmpty x xs = NonEmpty $ x:xs

-- | Any unicode character. Should contain all values of the Char type.
newtype Unicode = Unicode {unicode :: Char} 
  deriving (Typeable, Show, Eq, Ord)

-- | Access function for unicode strings.
unicodes :: [Unicode] -> String
unicodes = map unicode

-- | Printable ASCII characters.
newtype Printable = Printable {printable :: Char}
  deriving (Typeable, Show)

-- | Access function for printable ASCII strings
printables :: [Printable] -> String
printables = map printable

