{-# OPTIONS_HADDOCK show-extensions #-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ > 805
{-# LANGUAGE NoStarIsType #-}
#endif

{- |
   Copyright  : Copyright (C) 2006-2015 Bjorn Buckwalter
   License    : BSD3

   Maintainer : bjorn.buckwalter@gmail.com
   Stability  : Stable
   Portability: GHC only

= Summary

Type-level integers for GHC 7.8+.

We provide type level arithmetic operations. We also provide term-level arithmetic operations on proxys,
and conversion from the type level to the term level.

= Planned Obsolesence

We commit this package to hackage in sure and certain hope of the coming of glorious GHC integer type literals,
when the sea shall give up her dead, and this package shall be rendered unto obsolescence.

-}
module Numeric.NumType.DK.Integers
(
  -- * Type-Level Integers
  type TypeInt(..),
  -- * Type-level Arithmetic
  Pred, Succ, Negate, Abs, Signum,
  type (+), type (-), type (*), type (/), type (^),
  -- * Arithmetic on Proxies
  pred, succ, negate, abs, signum,
  (+), (-), (*), (/), (^),
  -- * Convenience Synonyms for Proxies
  zero,
  pos1, pos2, pos3, pos4, pos5, pos6, pos7, pos8, pos9,
  neg1, neg2, neg3, neg4, neg5, neg6, neg7, neg8, neg9,
  -- * Conversion from Types to Terms
  KnownTypeInt(..)
)
where

import Data.Proxy
import Prelude hiding ((+), (-), (*), (/), (^), pred, succ, negate, abs, signum)
import qualified Prelude

#if MIN_VERSION_base(4, 8, 0)
-- Use @Nat@s from @GHC.TypeLits@.
import qualified GHC.TypeLits as TN

type Z  = 0  -- GHC.TypeLits
type N1 = 1  -- GHC.TypeLits
#else
-- Use custom @Typeable@ @Nat@s.
import qualified Numeric.NumType.DK.Naturals as TN

type Z = 'TN.Z
type N1 = 'TN.S 'TN.Z  -- TypeInt.DK.Nats
#endif

-- Use the same fixity for operators as the Prelude.
infixr 8  ^
infixl 7  *, /
infixl 6  +, -


-- Natural numbers
-- ===============

type family NatPred (n::TN.Nat) :: TN.Nat where NatPred n = n TN.- N1
type family NatSucc (n::TN.Nat) :: TN.Nat where NatSucc n = n TN.+ N1


-- Integers
-- ========

data TypeInt = Neg10Minus TN.Nat  -- 10, 11, 12, 13, ...
             | Neg9
             | Neg8
             | Neg7
             | Neg6
             | Neg5
             | Neg4
             | Neg3
             | Neg2
             | Neg1
             | Zero -- 0
             | Pos1
             | Pos2
             | Pos3
             | Pos4
             | Pos5
             | Pos6
             | Pos7
             | Pos8
             | Pos9
             | Pos10Plus TN.Nat  -- -10, -11, -12, -13, ...


-- Unary operations
-- ----------------

type family Pred (i::TypeInt) :: TypeInt where
  Pred ('Neg10Minus n) = 'Neg10Minus (NatSucc n)
  Pred 'Neg9 = 'Neg10Minus Z
  Pred 'Neg8 = 'Neg9
  Pred 'Neg7 = 'Neg8
  Pred 'Neg6 = 'Neg7
  Pred 'Neg5 = 'Neg6
  Pred 'Neg4 = 'Neg5
  Pred 'Neg3 = 'Neg4
  Pred 'Neg2 = 'Neg3
  Pred 'Neg1 = 'Neg2
  Pred 'Zero = 'Neg1
  Pred 'Pos1 = 'Zero
  Pred 'Pos2 = 'Pos1
  Pred 'Pos3 = 'Pos2
  Pred 'Pos4 = 'Pos3
  Pred 'Pos5 = 'Pos4
  Pred 'Pos6 = 'Pos5
  Pred 'Pos7 = 'Pos6
  Pred 'Pos8 = 'Pos7
  Pred 'Pos9 = 'Pos8
  Pred ('Pos10Plus Z) = 'Pos9
  Pred ('Pos10Plus n) = 'Pos10Plus (NatPred n)

type family Succ (i::TypeInt) :: TypeInt where
  Succ ('Neg10Minus Z) = 'Neg9
  Succ ('Neg10Minus n) = 'Neg10Minus (NatPred n)
  Succ 'Neg9 = 'Neg8
  Succ 'Neg8 = 'Neg7
  Succ 'Neg7 = 'Neg6
  Succ 'Neg6 = 'Neg5
  Succ 'Neg5 = 'Neg4
  Succ 'Neg4 = 'Neg3
  Succ 'Neg3 = 'Neg2
  Succ 'Neg2 = 'Neg1
  Succ 'Neg1 = 'Zero
  Succ 'Zero = 'Pos1
  Succ 'Pos1 = 'Pos2
  Succ 'Pos2 = 'Pos3
  Succ 'Pos3 = 'Pos4
  Succ 'Pos4 = 'Pos5
  Succ 'Pos5 = 'Pos6
  Succ 'Pos6 = 'Pos7
  Succ 'Pos7 = 'Pos8
  Succ 'Pos8 = 'Pos9
  Succ 'Pos9 = 'Pos10Plus Z
  Succ ('Pos10Plus n) = 'Pos10Plus (NatSucc n)

-- | TypeInt negation.
type family Negate (i::TypeInt) :: TypeInt where
  Negate ('Neg10Minus n) = 'Pos10Plus  n
  Negate 'Neg9 = 'Pos9
  Negate 'Neg8 = 'Pos8
  Negate 'Neg7 = 'Pos7
  Negate 'Neg6 = 'Pos6
  Negate 'Neg5 = 'Pos5
  Negate 'Neg4 = 'Pos4
  Negate 'Neg3 = 'Pos3
  Negate 'Neg2 = 'Pos2
  Negate 'Neg1 = 'Pos1
  Negate 'Zero = 'Zero
  Negate 'Pos1 = 'Neg1
  Negate 'Pos2 = 'Neg2
  Negate 'Pos3 = 'Neg3
  Negate 'Pos4 = 'Neg4
  Negate 'Pos5 = 'Neg5
  Negate 'Pos6 = 'Neg6
  Negate 'Pos7 = 'Neg7
  Negate 'Pos8 = 'Neg8
  Negate 'Pos9 = 'Neg9
  Negate ('Pos10Plus  n) = 'Neg10Minus n

-- | Absolute value.
type family Abs (i::TypeInt) :: TypeInt where
  Abs ('Neg10Minus n) = 'Pos10Plus  n
  Abs 'Neg9 = 'Pos9
  Abs 'Neg8 = 'Pos8
  Abs 'Neg7 = 'Pos7
  Abs 'Neg6 = 'Pos6
  Abs 'Neg5 = 'Pos5
  Abs 'Neg4 = 'Pos4
  Abs 'Neg3 = 'Pos3
  Abs 'Neg2 = 'Pos2
  Abs 'Neg1 = 'Pos1
  Abs i = i

-- | Signum.
type family Signum (i::TypeInt) :: TypeInt where
  Signum ('Neg10Minus n) = 'Neg1
  Signum 'Neg9 = 'Neg1
  Signum 'Neg8 = 'Neg1
  Signum 'Neg7 = 'Neg1
  Signum 'Neg6 = 'Neg1
  Signum 'Neg5 = 'Neg1
  Signum 'Neg4 = 'Neg1
  Signum 'Neg3 = 'Neg1
  Signum 'Neg2 = 'Neg1
  Signum 'Neg1 = 'Neg1
  Signum 'Zero = 'Zero
  Signum i = 'Pos1


-- Binary operations
-- -----------------

-- | TypeInt addition.
type family (i::TypeInt) + (i'::TypeInt) :: TypeInt where
  'Zero + i = i
  i + 'Neg10Minus n = Pred i + Succ ('Neg10Minus n)
  i + 'Neg9 = Pred i + 'Neg8
  i + 'Neg8 = Pred i + 'Neg7
  i + 'Neg7 = Pred i + 'Neg6
  i + 'Neg6 = Pred i + 'Neg5
  i + 'Neg5 = Pred i + 'Neg4
  i + 'Neg4 = Pred i + 'Neg3
  i + 'Neg3 = Pred i + 'Neg2
  i + 'Neg2 = Pred i + 'Neg1
  i + 'Neg1 = Pred i
  i + 'Zero = i
  i + i' = Succ i + Pred i'  -- i + Pos

-- | TypeInt subtraction.
type family (i::TypeInt) - (i'::TypeInt) :: TypeInt where
  i - i' = i + Negate i'

-- | TypeInt multiplication.
type family (i::TypeInt) * (i'::TypeInt) :: TypeInt
  where
    'Zero * i = 'Zero
    i * 'Zero = 'Zero
    i * 'Pos1 = i
    i * 'Pos2 = i + i
    i * 'Pos3 = i + i + i
    i * 'Pos4 = i + i + i + i
    i * 'Pos5 = i + i + i + i + i
    i * 'Pos6 = i + i + i + i + i + i
    i * 'Pos7 = i + i + i + i + i + i + i
    i * 'Pos8 = i + i + i + i + i + i + i + i
    i * 'Pos9 = i + i + i + i + i + i + i + i + i
    i * 'Pos10Plus n = i + i * Pred ('Pos10Plus n)
    i * i' = Negate (i * Negate i')

-- | TypeInt exponentiation.
type family (i::TypeInt) ^ (i'::TypeInt) :: TypeInt
  where
    i ^ 'Zero = 'Pos1
    i ^ 'Pos1 = i
    i ^ 'Pos2 = i * i
    i ^ 'Pos3 = i * i * i
    i ^ 'Pos4 = i * i * i * i
    i ^ 'Pos5 = i * i * i * i * i
    i ^ 'Pos6 = i * i * i * i * i * i
    i ^ 'Pos7 = i * i * i * i * i * i * i
    i ^ 'Pos8 = i * i * i * i * i * i * i * i
    i ^ 'Pos9 = i * i * i * i * i * i * i * i * i
    i ^ 'Pos10Plus n = i * i ^ Pred ('Pos10Plus n)

-- | TypeInt division.
type family (i::TypeInt) / (i'::TypeInt) :: TypeInt
  where

    i / 'Pos1 = i
    i / 'Neg1 = Negate i
    -- @Zero / n = Zero@ would allow division by zero.
    -- @i / i = Pos1@ would allow division by zero.
    'Zero / ('Neg10Minus n) = 'Zero
    'Zero / 'Neg9 = 'Zero
    'Zero / 'Neg8 = 'Zero
    'Zero / 'Neg7 = 'Zero
    'Zero / 'Neg6 = 'Zero
    'Zero / 'Neg5 = 'Zero
    'Zero / 'Neg4 = 'Zero
    'Zero / 'Neg3 = 'Zero
    'Zero / 'Neg2 = 'Zero
    'Zero / 'Pos2 = 'Zero
    'Zero / 'Pos3 = 'Zero
    'Zero / 'Pos4 = 'Zero
    'Zero / 'Pos5 = 'Zero
    'Zero / 'Pos6 = 'Zero
    'Zero / 'Pos7 = 'Zero
    'Zero / 'Pos8 = 'Zero
    'Zero / 'Pos9 = 'Zero
    'Zero / ('Pos10Plus n) = 'Zero

    'Neg2 / 'Neg2 = 'Pos1
    'Neg3 / 'Neg3 = 'Pos1
    'Neg4 / 'Neg4 = 'Pos1
    'Neg5 / 'Neg5 = 'Pos1
    'Neg6 / 'Neg6 = 'Pos1
    'Neg7 / 'Neg7 = 'Pos1
    'Neg8 / 'Neg8 = 'Pos1
    'Neg9 / 'Neg9 = 'Pos1
    'Neg10Minus n / 'Neg10Minus n = 'Pos1

    'Neg2 / 'Pos2 = 'Neg1
    'Neg3 / 'Pos3 = 'Neg1
    'Neg4 / 'Pos4 = 'Neg1
    'Neg5 / 'Pos5 = 'Neg1
    'Neg6 / 'Pos6 = 'Neg1
    'Neg7 / 'Pos7 = 'Neg1
    'Neg8 / 'Pos8 = 'Neg1
    'Neg9 / 'Pos9 = 'Neg1
    'Neg10Minus n / 'Pos10Plus n = 'Neg1

    'Pos2 / 'Neg2 = 'Neg1
    'Pos3 / 'Neg3 = 'Neg1
    'Pos4 / 'Neg4 = 'Neg1
    'Pos5 / 'Neg5 = 'Neg1
    'Pos6 / 'Neg6 = 'Neg1
    'Pos7 / 'Neg7 = 'Neg1
    'Pos8 / 'Neg8 = 'Neg1
    'Pos9 / 'Neg9 = 'Neg1
    'Pos10Plus n / 'Neg10Minus n = 'Neg1

    'Pos2 / 'Pos2 = 'Pos1
    'Pos3 / 'Pos3 = 'Pos1
    'Pos4 / 'Pos4 = 'Pos1
    'Pos5 / 'Pos5 = 'Pos1
    'Pos6 / 'Pos6 = 'Pos1
    'Pos7 / 'Pos7 = 'Pos1
    'Pos8 / 'Pos8 = 'Pos1
    'Pos9 / 'Pos9 = 'Pos1
    'Pos10Plus n / 'Pos10Plus n = 'Pos1

    'Neg4 / 'Neg2 = 'Pos2
    'Neg6 / 'Neg2 = 'Pos3
    'Neg8 / 'Neg2 = 'Pos4
    'Neg6 / 'Neg3 = 'Pos2
    'Neg9 / 'Neg3 = 'Pos3
    'Neg8 / 'Neg4 = 'Pos2
    'Neg10Minus n / i = ('Neg10Minus n + Abs i) / i - Signum i

    'Neg4 / 'Pos2 = 'Neg2
    'Neg6 / 'Pos2 = 'Neg3
    'Neg8 / 'Pos2 = 'Neg4
    'Neg6 / 'Pos3 = 'Neg2
    'Neg9 / 'Pos3 = 'Neg3
    'Neg8 / 'Pos4 = 'Neg2

    'Pos4 / 'Neg2 = 'Neg2
    'Pos6 / 'Neg2 = 'Neg3
    'Pos8 / 'Neg2 = 'Neg4
    'Pos6 / 'Neg3 = 'Neg2
    'Pos9 / 'Neg3 = 'Neg3
    'Pos8 / 'Neg4 = 'Neg2

    'Pos4 / 'Pos2 = 'Pos2
    'Pos6 / 'Pos2 = 'Pos3
    'Pos8 / 'Pos2 = 'Pos4
    'Pos6 / 'Pos3 = 'Pos2
    'Pos9 / 'Pos3 = 'Pos3
    'Pos8 / 'Pos4 = 'Pos2
    'Pos10Plus n / i = ('Pos10Plus n - Abs i) / i + Signum i


-- Term level
-- ==========

-- Term level operators
-- --------------------

pred   :: Proxy i -> Proxy (Pred   i); pred   _ = Proxy
succ   :: Proxy i -> Proxy (Succ   i); succ   _ = Proxy
negate :: Proxy i -> Proxy (Negate i); negate _ = Proxy
abs    :: Proxy i -> Proxy (Abs    i); abs    _ = Proxy
signum :: Proxy i -> Proxy (Signum i); signum _ = Proxy

(+) :: Proxy i -> Proxy i' -> Proxy (i + i'); _ + _ = Proxy
(-) :: Proxy i -> Proxy i' -> Proxy (i - i'); _ - _ = Proxy
(*) :: Proxy i -> Proxy i' -> Proxy (i * i'); _ * _ = Proxy
(/) :: Proxy i -> Proxy i' -> Proxy (i / i'); _ / _ = Proxy
(^) :: Proxy i -> Proxy i' -> Proxy (i ^ i'); _ ^ _ = Proxy


-- Term level TypeNats for convenience
-- -----------------------------------

neg9 :: Proxy 'Neg9
neg9 = Proxy :: Proxy 'Neg9
neg8 :: Proxy 'Neg8
neg8 = Proxy :: Proxy 'Neg8
neg7 :: Proxy 'Neg7
neg7 = Proxy :: Proxy 'Neg7
neg6 :: Proxy 'Neg6
neg6 = Proxy :: Proxy 'Neg6
neg5 :: Proxy 'Neg5
neg5 = Proxy :: Proxy 'Neg5
neg4 :: Proxy 'Neg4
neg4 = Proxy :: Proxy 'Neg4
neg3 :: Proxy 'Neg3
neg3 = Proxy :: Proxy 'Neg3
neg2 :: Proxy 'Neg2
neg2 = Proxy :: Proxy 'Neg2
neg1 :: Proxy 'Neg1
neg1 = Proxy :: Proxy 'Neg1
zero :: Proxy 'Zero
zero = Proxy :: Proxy 'Zero
pos1 :: Proxy 'Pos1
pos1 = Proxy :: Proxy 'Pos1
pos2 :: Proxy 'Pos2
pos2 = Proxy :: Proxy 'Pos2
pos3 :: Proxy 'Pos3
pos3 = Proxy :: Proxy 'Pos3
pos4 :: Proxy 'Pos4
pos4 = Proxy :: Proxy 'Pos4
pos5 :: Proxy 'Pos5
pos5 = Proxy :: Proxy 'Pos5
pos6 :: Proxy 'Pos6
pos6 = Proxy :: Proxy 'Pos6
pos7 :: Proxy 'Pos7
pos7 = Proxy :: Proxy 'Pos7
pos8 :: Proxy 'Pos8
pos8 = Proxy :: Proxy 'Pos8
pos9 :: Proxy 'Pos9
pos9 = Proxy :: Proxy 'Pos9


-- Reification
-- -----------

-- | Conversion to a @Num@.
class KnownTypeInt (i::TypeInt) where toNum :: Num a => Proxy i -> a

instance KnownTypeInt (Succ ('Neg10Minus n)) => KnownTypeInt ('Neg10Minus n)
  where toNum = (Prelude.- 1) . toNum . succ

instance KnownTypeInt 'Neg9 where toNum _ = -9
instance KnownTypeInt 'Neg8 where toNum _ = -8
instance KnownTypeInt 'Neg7 where toNum _ = -7
instance KnownTypeInt 'Neg6 where toNum _ = -6
instance KnownTypeInt 'Neg5 where toNum _ = -5
instance KnownTypeInt 'Neg4 where toNum _ = -4
instance KnownTypeInt 'Neg3 where toNum _ = -3
instance KnownTypeInt 'Neg2 where toNum _ = -2
instance KnownTypeInt 'Neg1 where toNum _ = -1
instance KnownTypeInt 'Zero where toNum _ = 0
instance KnownTypeInt 'Pos1 where toNum _ = 1
instance KnownTypeInt 'Pos2 where toNum _ = 2
instance KnownTypeInt 'Pos3 where toNum _ = 3
instance KnownTypeInt 'Pos4 where toNum _ = 4
instance KnownTypeInt 'Pos5 where toNum _ = 5
instance KnownTypeInt 'Pos6 where toNum _ = 6
instance KnownTypeInt 'Pos7 where toNum _ = 7
instance KnownTypeInt 'Pos8 where toNum _ = 8
instance KnownTypeInt 'Pos9 where toNum _ = 9

instance KnownTypeInt (Pred ('Pos10Plus n)) => KnownTypeInt ('Pos10Plus n)
  where toNum = (Prelude.+ 1) . toNum . pred
