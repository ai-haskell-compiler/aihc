{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A version of 'DC.Checked' that requires client code to provide
--   a non-bottom value of the property index type to use @trust*@
--   functions.
module Data.Checked.Strict
  ( Checked
  , trustThat
  , trustMap
  , checked
  , Property(..)
  , maybeHolds
  , check
  , relax
  ) where

import Data.Typeable (Typeable)
import Control.DeepSeq (NFData(..))
import Data.Checked (Property(..), maybeHolds)
import qualified Data.Checked as DC

-- | Wrapper-evidence for property /p/.
newtype Checked p v = Checked v deriving Typeable

instance NFData v ⇒ NFData (Checked p v) where
  rnf (Checked v) = rnf v

-- | Use when the property can be deduced without a runtime check.
--   Note that /p/ is evaluated to WHNF, so you can't use 'undefined'.
trustThat ∷ p → v → Checked p v
trustThat p v = p `seq` Checked v
{-# INLINE trustThat #-}

-- | Apply a fuction that preserves the property to the checked value.
--   Note that /p/ is evaluated to WHNF, so you can't use 'undefined'.
trustMap ∷ p → (v → v) → Checked p v → Checked p v
trustMap p f (Checked v) = p `seq` Checked (f v)
{-# INLINE trustMap #-}

-- | Unwrap the checked value.
checked ∷ Checked p v → v
checked (Checked v) = v
{-# INLINE checked #-}

-- | Wrap the value if the property holds.
check ∷ ∀ p v . Property p v ⇒ v → Maybe (Checked p v)
check v | holds (undefined ∷ p) v = Just (Checked v)
        | otherwise               = Nothing
{-# INLINABLE check #-}

-- | Rewrap a value into the less strict 'DC.Checked' type.
relax ∷ Checked p v → DC.Checked p v
relax (Checked v) = DC.trustMe v
{-# INLINE relax #-}
