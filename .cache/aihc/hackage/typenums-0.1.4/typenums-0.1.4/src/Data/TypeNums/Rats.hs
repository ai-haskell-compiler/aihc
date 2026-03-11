{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ExplicitNamespaces        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE Trustworthy               #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

{-|
Module: Data.TypeNums.Rats
Copyright: (c) 2018-2021 Iris Ward
License: BSD3
Maintainer: aditu.venyhandottir@gmail.com
Stability: experimental

Type level rational numbers expressed as a ratio between a type-level integer
and a type-level natural.  For example @'Neg' 3 :% 2@.

See also: "Data.TypeInts"
-}
module Data.TypeNums.Rats
  ( -- * Construction
    Rat((:%))
    -- * Linking type and value level
  , KnownRat
  , ratVal
  , ratVal'
  , SomeRat(..)
  , someRatVal
  ) where

import Data.TypeNums.Equality (type (/=))
import Data.TypeNums.Ints
import Data.TypeNums.Rats.Type(Rat(..))

import Data.Bifunctor (first)
import Data.Proxy     (Proxy (..))
import Data.Ratio     ((%))
import GHC.Exts       (Proxy#, proxy#)
import GHC.TypeLits   (ErrorMessage (..), KnownNat, Nat, TypeError, natVal')
import Unsafe.Coerce  (unsafeCoerce)

newtype SRat r =
  SRat Rational

-- | This class gives the (value-level) rational associated with a type-level
--   rational.  There are instances of this class for every combination of a
--   concrete integer and concrete natural.
class KnownRat r where
  ratSing :: SRat r

instance {-# OVERLAPPING #-} (TypeError ('Text "Denominator must not equal 0")) =>
                             KnownRat (n ':% 0) where
  ratSing = error "Unreachable"

instance {-# OVERLAPS #-} forall k (n :: k) (d :: Nat). (KnownInt n, KnownNat d, d /= 0) =>
                          KnownRat (n ':% d) where
  ratSing = SRat $! intVal' (proxy# :: Proxy# n) % natVal' (proxy# :: Proxy# d)

instance {-# OVERLAPPABLE #-} forall k (n :: k). (KnownInt n) => KnownRat n where
  ratSing = SRat $! intVal' (proxy# :: Proxy# n) % 1

-- | Get the value associated with a type-level rational
ratVal ::
     forall proxy r. KnownRat r
  => proxy r
  -> Rational
ratVal _ =
  case ratSing :: SRat r of
    SRat x -> x

-- | Get the value associated with a type-level rational.  The difference
--   between this function and 'ratVal' is that it takes a 'Proxy#' parameter,
--   which has zero runtime representation and so is entirely free.
ratVal' ::
     forall r. KnownRat r
  => Proxy# r
  -> Rational
ratVal' _ =
  case ratSing :: SRat r of
    SRat x -> x

-- | This type represents unknown type-level integers.
--
-- @since 0.1.1
data SomeRat =
  forall r. KnownRat r =>
            SomeRat (Proxy r)

instance Eq SomeRat where
  SomeRat x == SomeRat y = ratVal x == ratVal y

instance Ord SomeRat where
  compare (SomeRat x) (SomeRat y) = compare (ratVal x) (ratVal y)

instance Show SomeRat where
  showsPrec p (SomeRat x) = showsPrec p (ratVal x)

instance Read SomeRat where
  readsPrec p xs = first someRatVal <$> readsPrec p xs

-- For implementation notes, see $impl in "Data.TypeNats.Ints"

data SomeRatWithDict =
  forall r. SomeRatWithDict (SRat r)
                            (Proxy r)

-- | Convert a rational into an unknown type-level rational.
--
-- @since 0.1.1
someRatVal :: Rational -> SomeRat
someRatVal r = unsafeCoerce $ SomeRatWithDict (SRat r) Proxy
