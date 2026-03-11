{-# OPTIONS_GHC  -fno-warn-orphans #-}

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE DeriveDataTypeable    #-}
-- |
-- Module      : TypeLevel.Number.Nat
-- Copyright   : Alexey Khudyakov
-- License     : BSD3-style (see LICENSE)
--
-- Maintainer  : Alexey Khudyakov <alexey.skladnoy@gmail.com>
-- Stability   : unstable
-- Portability : unportable (GHC only)
--
--
-- This is type level natural numbers. They are represented using
-- binary encoding which means that reasonable large numbers could be
-- represented. With default context stack depth (20) maximal number
-- is 2^18-1 (262143).
--
-- > Z           = 0
-- > I Z         = 1
-- > O (I Z)     = 2
-- > I (I Z)     = 3
-- > O (O (I Z)) = 4
-- > ...
--
-- It's easy to see that representation for each number is not
-- unique. One could add any numbers of leading zeroes:
--
-- > I Z = I (O Z) = I (O (O Z)) = 1
--
-- In order to enforce uniqueness of representation only numbers
-- without leading zeroes are members of Nat type class. This means
-- than types are equal if and only if numbers are equal.
--
-- Natural numbers support comparison and following operations: Next,
-- Prev, Add, Sub, Mul. All operations on numbers return normalized
-- numbers.
--
-- Interface type classes are reexported from TypeLevel.Number.Classes
module TypeLevel.Number.Nat ( -- * Natural numbers
                          I
                        , O
                        , Z
                        , Nat(..)
                          -- ** Lifting
                        , SomeNat(..)
                        , withNat
                          -- * Template haskell utilities
                          -- $TH
                        , natT
                        , nat
                        , module TypeLevel.Number.Classes
                        ) where

import Data.Word     (Word8,Word16,Word32,Word64)
import Data.Int      (Int8, Int16, Int32, Int64 )
import Data.Typeable (Typeable)

import TypeLevel.Number.Classes
import TypeLevel.Number.Nat.Types
import TypeLevel.Number.Nat.TH
import TypeLevel.Reify

-- $TH
-- Here is usage example for natT:
--
-- > n123 :: $(natT 123)
-- > n123 = undefined

----------------------------------------------------------------

-- | Type class for natural numbers. Only numbers without leading
-- zeroes are members of this type class.
class Nat n where
  -- | Convert natural number to integral value. It's not checked
  -- whether value could be represented.
  toInt :: Integral i => n -> i

-- | Type class for positive natural numbers. It's synonym for
-- Positive and Nat.
class Pos n

instance              Nat       Z   where toInt _ = 0
instance              Nat    (I Z)  where toInt _ = 1
instance Nat (O n) => Nat (O (O n)) where toInt _ = 0 + 2 * toInt (undefined :: (O n))
instance Nat (O n) => Nat (I (O n)) where toInt _ = 1 + 2 * toInt (undefined :: (O n))
instance Nat (I n) => Nat (O (I n)) where toInt _ = 0 + 2 * toInt (undefined :: (I n))
instance Nat (I n) => Nat (I (I n)) where toInt _ = 1 + 2 * toInt (undefined :: (I n))
-- Error reporting. Stop for denormalized numbers
class    Number_Is_Denormalized a
instance (Number_Is_Denormalized Z) => Nat (O Z) where
  toInt = error "quench warning"

-- Synonym for positive
instance (Nat n, Positive n) => Pos n


-- | Some natural number
data SomeNat where
  SomeNat :: Nat n => n -> SomeNat
  deriving Typeable

instance Show SomeNat where
  showsPrec d (SomeNat n) = showParen (d > 10) $
    showString "withNat SomeNat " . shows (toInt n :: Integer)



-- | Apply function which could work with any 'Nat' value only know at runtime.
withNat :: forall i a. (Integral i) => (forall n. Nat n => n -> a) -> i -> a
withNat f i0
  | i0 <  0   = error "TypeLevel.Number.Nat.withNat: negative number"
  | i0 == 0   = f (undefined :: Z)
  | otherwise = cont (fromIntegral i0) f f
  where
    cont :: Integer -> (forall n m. (Nat n, n ~ O m) => n -> a)
                    -> (forall n m. (Nat n, n ~ I m) => n -> a) -> a
    cont 1 _  k1 = k1 (undefined :: I Z)
    cont i k0 k1 = cont (i `quot` 2) k0' k1'
      where
        k0' :: forall n m. (Nat n, n ~ O m) => n -> a
        k0' _ | odd i     = k1 (undefined :: I n)
              | otherwise = k0 (undefined :: O n)
        k1' :: forall n m. (Nat n, n ~ I m) => n -> a
        k1' _ | odd i     = k1 (undefined :: I n)
              | otherwise = k0 (undefined :: O n)



----------------------------------------------------------------
-- Data conversion

-- To Integer
instance                Reify    Z  Integer where witness = Witness 0
instance (Nat (O n)) => Reify (O n) Integer where witness = Witness $ toInt (undefined :: O n)
instance (Nat (I n)) => Reify (I n) Integer where witness = Witness $ toInt (undefined :: I n)

-- To Int
instance                Reify    Z  Int where witness = Witness 0
instance (Nat (O n)) => Reify (O n) Int where witness = Witness $ toInt (undefined :: O n)
instance (Nat (I n)) => Reify (I n) Int where witness = Witness $ toInt (undefined :: I n)

-- To Word8
instance                                              Reify    Z  Word8 where witness = Witness 0
instance (Nat (O n), (O n) `Lesser` $(natT 0x100)) => Reify (O n) Word8 where witness = Witness $ toInt (undefined :: O n)
instance (Nat (I n), (I n) `Lesser` $(natT 0x100)) => Reify (I n) Word8 where witness = Witness $ toInt (undefined :: I n)

-- To Word16
instance                                                Reify    Z  Word16 where witness = Witness 0
instance (Nat (O n), (O n) `Lesser` $(natT 0x10000)) => Reify (O n) Word16 where witness = Witness $ toInt (undefined :: O n)
instance (Nat (I n), (I n) `Lesser` $(natT 0x10000)) => Reify (I n) Word16 where witness = Witness $ toInt (undefined :: I n)

-- To Word32 (No checks. Won't to default centext stack length)
instance                Reify    Z  Word32 where witness = Witness 0
instance (Nat (O n)) => Reify (O n) Word32 where witness = Witness $ toInt (undefined :: O n)
instance (Nat (I n)) => Reify (I n) Word32 where witness = Witness $ toInt (undefined :: I n)

-- To Word64 (No checks. Won't to default centext stack length)
instance                Reify    Z  Word64 where witness = Witness 0
instance (Nat (O n)) => Reify (O n) Word64 where witness = Witness $ toInt (undefined :: O n)
instance (Nat (I n)) => Reify (I n) Word64 where witness = Witness $ toInt (undefined :: I n)

-- To Int8
instance                                             Reify    Z  Int8 where witness = Witness 0
instance (Nat (O n), (O n) `Lesser` $(natT 0x80)) => Reify (O n) Int8 where witness = Witness $ toInt (undefined :: O n)
instance (Nat (I n), (I n) `Lesser` $(natT 0x80)) => Reify (I n) Int8 where witness = Witness $ toInt (undefined :: I n)

-- To Int16
instance                                               Reify    Z  Int16 where witness = Witness 0
instance (Nat (O n), (O n) `Lesser` $(natT 0x8000)) => Reify (O n) Int16 where witness = Witness $ toInt (undefined :: O n)
instance (Nat (I n), (I n) `Lesser` $(natT 0x8000)) => Reify (I n) Int16 where witness = Witness $ toInt (undefined :: I n)

-- To Int32 (No checks. Won't to default centext stack length)
instance                Reify    Z  Int32 where witness = Witness 0
instance (Nat (O n)) => Reify (O n) Int32 where witness = Witness $ toInt (undefined :: O n)
instance (Nat (I n)) => Reify (I n) Int32 where witness = Witness $ toInt (undefined :: I n)

-- To Int64 (No checks. Won't to default centext stack length)
instance                Reify    Z  Int64 where witness = Witness 0
instance (Nat (O n)) => Reify (O n) Int64 where witness = Witness $ toInt (undefined :: O n)
instance (Nat (I n)) => Reify (I n) Int64 where witness = Witness $ toInt (undefined :: I n)

----------------------------------------------------------------
-- Number normalization

-- Add trailing zero bit to number. It's added only if number is not
-- equal to zero. Actual normalization is done here.
type family   Add0Bit n :: *
type instance Add0Bit    Z  = Z
type instance Add0Bit (O n) = (O (O n))
type instance Add0Bit (I n) = (O (I n))

type instance Normalized    Z  = Z
type instance Normalized (I n) = I (Normalized n)
type instance Normalized (O n) = Add0Bit (Normalized n)

----------------------------------------------------------------
-- Show instances.
-- Nat contexts are used to ensure correctness of numbers.
instance              Show    Z  where show _ = "[0:N]"
instance Nat (O n) => Show (O n) where show n = "["++show (toInt n :: Integer)++":N]"
instance Nat (I n) => Show (I n) where show n = "["++show (toInt n :: Integer)++":N]"

----------------------------------------------------------------
-- Next number.
-- Number normalization is not required.
type instance Next    Z  = I Z
type instance Next (I n) = O (Next n)
type instance Next (O n) = I n

----------------------------------------------------------------
-- Previous number.
-- Normalization isn't requred too. It's done manually in (I Z) case.
type instance Prev    (I Z)   = Z
type instance Prev (O (O n))  = I (Prev (O n))
type instance Prev (I (O n))  = O (O n)
type instance Prev (O (I n))  = I (Prev (I n))
type instance Prev (I (I n))  = O (I n)


----------------------------------------------------------------
-- Comparison

-- Join compare results. a is result of comparison of low digits b is
-- result of comparion of higher digits.
type family Join a b :: *

type instance Join IsLesser  IsEqual   = IsLesser
type instance Join IsEqual   IsEqual   = IsEqual
type instance Join IsGreater IsEqual   = IsGreater
type instance Join a         IsLesser  = IsLesser
type instance Join a         IsGreater = IsGreater

-- Instances for comparison
type instance Compare    Z     Z  = IsEqual
type instance Compare (O n)    Z  = IsGreater
type instance Compare (I n)    Z  = IsGreater
type instance Compare    Z  (O n) = IsLesser
type instance Compare    Z  (I n) = IsLesser

type instance Compare (O n) (O m) = Compare n m
type instance Compare (O n) (I m) = Join IsLesser  (Compare n m)
type instance Compare (I n) (O m) = Join IsGreater (Compare n m)
type instance Compare (I n) (I m) = Compare n m

----------------------------------------------------------------
-- Positive and Non-zero numbers

instance Nat (I n) => Positive (I n)
instance Nat (O n) => Positive (O n)

instance Nat (I n) => NonZero (I n)
instance Nat (O n) => NonZero (O n)

----------------------------------------------------------------
-- Addition
data Carry      -- Designate carry bit
data NoCarry    -- No carry bit in addition

-- Type family which actually implement addtition of natural numbers
type family Add' n m c :: *

-- Recursion termination without carry bit. Full enumeration is
-- required to avoid overlapping instances
type instance Add'    Z     Z  NoCarry = Z
type instance Add' (O n)    Z  NoCarry = O n
type instance Add' (I n)    Z  NoCarry = I n
type instance Add'    Z  (O n) NoCarry = O n
type instance Add'    Z  (I n) NoCarry = I n
-- Recursion termination with carry bit
type instance Add'    Z   Z      Carry = I Z
type instance Add' (O n)  Z      Carry = I n
type instance Add' (I n)  Z      Carry = Add' (I n) (I Z) NoCarry
type instance Add'    Z  (O n)   Carry = I n
type instance Add'    Z  (I n)   Carry = Add' (I n) (I Z) NoCarry
-- Generic recursion (No carry)
type instance Add' (O n) (O m) NoCarry = O (Add' n m NoCarry)
type instance Add' (I n) (O m) NoCarry = I (Add' n m NoCarry)
type instance Add' (O n) (I m) NoCarry = I (Add' n m NoCarry)
type instance Add' (I n) (I m) NoCarry = O (Add' n m   Carry)
-- Generic recursion (with carry)
type instance Add' (O n) (O m)   Carry = I (Add' n m NoCarry)
type instance Add' (I n) (O m)   Carry = O (Add' n m   Carry)
type instance Add' (O n) (I m)   Carry = O (Add' n m   Carry)
type instance Add' (I n) (I m)   Carry = I (Add' n m   Carry)

-- Enumeration of all possible instances heads is required to avoid
-- overlapping.
type instance Add (O n) (O m) = Normalized (Add' (O n) (O m) NoCarry)
type instance Add (I n) (O m) = Normalized (Add' (I n) (O m) NoCarry)
type instance Add (O n) (I m) = Normalized (Add' (O n) (I m) NoCarry)
type instance Add (I n) (I m) = Normalized (Add' (I n) (I m) NoCarry)
type instance Add (O n)    Z  = Normalized (Add' (O n)    Z  NoCarry)
type instance Add (I n)    Z  = Normalized (Add' (I n)    Z  NoCarry)
type instance Add    Z  (O n) = Normalized (Add'    Z  (O n) NoCarry)
type instance Add    Z  (I n) = Normalized (Add'    Z  (I n) NoCarry)
type instance Add    Z     Z  = Normalized (Add'    Z     Z  NoCarry)

----------------------------------------------------------------
-- Subtraction
data Borrow     -- Borrow bit
data NoBorrow   -- Do not borrow bit

-- Type class which actually implement addtition of natural numbers
type family Sub' n m c :: *

-- Recursion termination without carry bit. Full enumeration is
-- required to avoid overlapping instances
type instance Sub'    Z     Z  NoBorrow = Z
type instance Sub' (O n)    Z  NoBorrow = O n
type instance Sub' (I n)    Z  NoBorrow = I n
-- Recursion termination with carry bit
type instance Sub' (O n)  Z      Borrow = I (Sub' n Z Borrow)
type instance Sub' (I n)  Z      Borrow = O n
-- Generic recursion (No carry)
type instance Sub' (O n) (O m) NoBorrow = O (Sub' n m NoBorrow)
type instance Sub' (I n) (O m) NoBorrow = I (Sub' n m NoBorrow)
type instance Sub' (O n) (I m) NoBorrow = I (Sub' n m   Borrow)
type instance Sub' (I n) (I m) NoBorrow = O (Sub' n m NoBorrow)
-- -- Generic recursion (with carry)
type instance Sub' (O n) (O m)   Borrow = I (Sub' n m   Borrow)
type instance Sub' (I n) (O m)   Borrow = O (Sub' n m NoBorrow)
type instance Sub' (O n) (I m)   Borrow = O (Sub' n m   Borrow)
type instance Sub' (I n) (I m)   Borrow = I (Sub' n m   Borrow)

-- Enumeration of all possible instances heads is required to avoid
-- overlapping.
type instance Sub (O n) (O m) = Normalized (Sub' (O n) (O m) NoBorrow)
type instance Sub (I n) (O m) = Normalized (Sub' (I n) (O m) NoBorrow)
type instance Sub (O n) (I m) = Normalized (Sub' (O n) (I m) NoBorrow)
type instance Sub (I n) (I m) = Normalized (Sub' (I n) (I m) NoBorrow)
type instance Sub (O n)    Z  = Normalized (Sub' (O n)    Z  NoBorrow)
type instance Sub (I n)    Z  = Normalized (Sub' (I n)    Z  NoBorrow)
type instance Sub    Z     Z  = Normalized (Sub'    Z     Z  NoBorrow)

----------------------------------------------------------------
-- Multiplication
----------------------------------------------------------------

type instance Mul n    Z  = Z
type instance Mul n (O m) = Normalized (O (Mul n m))
type instance Mul n (I m) = Normalized (Add' n (O (Mul n m)) NoCarry)
