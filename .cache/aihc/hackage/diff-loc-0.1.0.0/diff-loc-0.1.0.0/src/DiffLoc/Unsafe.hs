{-# LANGUAGE
  DataKinds,
  ScopedTypeVariables,
  TypeApplications #-}
-- | Unsafe functions that will throw errors if misused.
module DiffLoc.Unsafe
  ( -- ** Inverting monoid actions
    (.-.)

    -- ** Smart constructors for 'IndexFrom'
  , indexFrom
  , indexFrom0
  , indexFrom1

    -- ** Smart constructor for 'Offset'
  , offset
  ) where

import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy(..))
import GHC.Stack (HasCallStack)
import GHC.TypeNats (KnownNat, natVal)
import DiffLoc.Shift (Amor(Trans, (.-.?)))
import DiffLoc.Index (IndexFrom, Offset, indexFromM, offsetM)

infixl 6 .-.

-- | An unsafe variant of @('.-.?')@ which throws an exception on @Nothing@.
-- This operator may appear in class laws, imposing an implicit requirement
-- that its operands must be ordered.
(.-.) :: HasCallStack => Amor p => p -> p -> Trans p
i .-. j = fromMaybe (error "undefined vector") (i .-.? j)

-- | Constructor for 'IndexFrom'. The index must be greater than the origin,
-- otherwise an error is raised.
--
-- @
-- origin <= indexFrom i
-- @
indexFrom :: forall n a. (HasCallStack, KnownNat n, Num a, Ord a) => a -> IndexFrom n a
indexFrom i = fromMaybe err (indexFromM i)
  where err = error ("IndexFrom must not be less than origin " <> show (natVal @n Proxy))

-- | 'indexFrom' specialized to 0-indexing.
indexFrom0 :: (HasCallStack, Num a, Ord a) => a -> IndexFrom 0 a
indexFrom0 = indexFrom

-- | 'indexFrom' specialized to 1-indexing.
indexFrom1 :: (HasCallStack, Num a, Ord a) => a -> IndexFrom 1 a
indexFrom1 = indexFrom

-- | Construct an 'Offset'. The offset must be nonnegative, otherwise
-- an error is raised.
offset :: (HasCallStack, Num a, Ord a) => a -> Offset a
offset i = fromMaybe (error "Offset must not be negative") (offsetM i)
