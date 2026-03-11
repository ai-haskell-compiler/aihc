{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Type-indexed runtime-checked properties.
module Data.Checked
  ( Checked
  , trustMe
  , trustThat
  , trustMap
  , checked
  , Property(..)
  , maybeHolds
  , check
  ) where

import Data.Typeable (Typeable)
import Control.DeepSeq (NFData(..))

-- | Wrapper-evidence for property /p/.
newtype Checked p v = Checked v deriving Typeable

instance NFData v ⇒ NFData (Checked p v) where
  rnf (Checked v) = rnf v

-- | Use when the property can be deduced without a runtime check.
trustMe ∷ v → Checked p v
trustMe = Checked
{-# INLINE trustMe #-}

-- | Use when the property can be deduced without a runtime check.
trustThat ∷ p → v → Checked p v
trustThat _ = Checked
{-# INLINE trustThat #-}

-- | Apply a fuction that preserves the property to the checked value.
trustMap ∷ (v → v) → Checked p v → Checked p v
trustMap f (Checked v) = Checked (f v)
{-# INLINE trustMap #-}

-- | Unwrap the checked value.
checked ∷ Checked p v → v
checked (Checked v) = v
{-# INLINE checked #-}

class Property p v where
  -- | Test if the property holds for the given value.
  --   The first argument is supposed to be ignored.
  holds ∷ p → v → Bool

-- | Return 'Just' /v/ if /p/ holds and 'Nothing' overwise.
maybeHolds ∷ Property p v ⇒ p → v → Maybe v
maybeHolds p v | holds p v = Just v
               | otherwise = Nothing
{-# INLINABLE maybeHolds #-}

-- | Wrap the value if the property holds.
check ∷ ∀ p v . Property p v ⇒ v → Maybe (Checked p v)
check v | holds (undefined ∷ p) v = Just (Checked v)
        | otherwise               = Nothing
{-# INLINABLE check #-}
