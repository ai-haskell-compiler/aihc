{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

module Data.Constraint.Flip
  ( FlipC
  ) where

import Data.Constraint

-- | Flip for constraints.
class c h g => FlipC (c :: k -> k' -> Constraint) (g :: k') (h :: k)
instance c h g => FlipC c g h
