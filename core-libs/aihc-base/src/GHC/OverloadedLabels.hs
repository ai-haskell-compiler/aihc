{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE KindSignatures #-}

module GHC.OverloadedLabels
  ( IsLabel (..),
  )
where

import GHC.Types (Symbol)

-- | The class of the values that an overloaded label @#x@ can give.
class IsLabel (x :: Symbol) a where
  fromLabel :: a
