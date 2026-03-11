{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MagicHash                 #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE Trustworthy               #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

{-|
Module: Data.TypeNums.Ints
Copyright: (c) 2018-2021 Iris Ward
License: BSD3
Maintainer: aditu.venyhandottir@gmail.com
Stability: experimental

Type level integers can be used in the same way as type level naturals
from "GHC.TypeLits", for example @3@.  However, a minus sign is not
recognised in this context, so a negative type-level integer is instead
written as @'Neg' n@.
-}
module Data.TypeNums.Ints
  ( -- * Construction
    TInt(..)
    -- * Linking type and value level
  , KnownInt
  , intVal
  , intVal'
  , SomeInt(..)
  , someIntVal
  ) where

import Data.Bifunctor (first)
import Data.Proxy     (Proxy (..))
import GHC.Exts       (Proxy#, proxy#)
import GHC.TypeLits   (KnownNat, Nat, natVal')
import Unsafe.Coerce  (unsafeCoerce)

newtype SInt (n :: k) =
  SInt Integer

-- | (Kind) An integer that may be negative.
data TInt
  = Pos Nat
  | Neg Nat

-- | This class gives the (value-level) integer associated with a type-level
--   integer.  There are instances of this class for every concrete natural:
--   0, 1, 2, etc.  There are also instances of this class for every negated
--   natural, such as @'Neg' 1@.
class KnownInt (n :: k) where
  intSing :: SInt n

instance forall (n :: Nat). KnownNat n => KnownInt n where
  intSing = SInt $! natVal' (proxy# :: Proxy# n)

instance forall (n :: Nat). KnownNat n => KnownInt ('Pos n) where
  intSing = SInt $! natVal' (proxy# :: Proxy# n)

instance forall (n :: Nat). KnownNat n => KnownInt ('Neg n) where
  intSing = SInt $! negate (natVal' (proxy# :: Proxy# n))


-- | Get the value associated with a type-level integer
intVal ::
     forall n proxy. KnownInt n
  => proxy n
  -> Integer
intVal _ =
  case intSing :: SInt n of
    SInt x -> x

-- | Get the value associated with a type-level integer.  The difference
--   between this function and 'intVal' is that it takes a 'Proxy#' parameter,
--   which has zero runtime representation and so is entirely free.
intVal' ::
     forall n. KnownInt n
  => Proxy# n
  -> Integer
intVal' _ =
  case intSing :: SInt n of
    SInt x -> x

-- | This type represents unknown type-level integers.
--
-- @since 0.1.1
data SomeInt =
  forall n. KnownInt n =>
            SomeInt (Proxy n)

instance Eq SomeInt where
  SomeInt x == SomeInt y = intVal x == intVal y

instance Ord SomeInt where
  compare (SomeInt x) (SomeInt y) = compare (intVal x) (intVal y)

instance Show SomeInt where
  showsPrec p (SomeInt x) = showsPrec p (intVal x)

instance Read SomeInt where
  readsPrec p xs = first someIntVal <$> readsPrec p xs

-- $impl
-- Implementation notes:
--   * A typeclass on a data constructor is represented internally as an extra
--     field on the data constructor containing the typeclass dictionary. This
--     field occurs before the other fields.
--   * A dictionary for the KnownInt typeclass is represented as just an SInt
--     value on its own, as there is only one member of the typeclass, and that
--     member is not a function.
--
-- someIntVal therefore constructs a datatype with the same representation as
-- the existentially qualified constructor SomeInt, then uses unsafeCoerce to
-- produce the SomeInt value.

data SomeIntWithDict =
  forall n. SomeIntWithDict (SInt n)
                            (Proxy n)

-- | Convert an integer into an unknown type-level integer.
--
-- @since 0.1.1
someIntVal :: Integer -> SomeInt
someIntVal x = unsafeCoerce $ SomeIntWithDict (SInt x) Proxy
