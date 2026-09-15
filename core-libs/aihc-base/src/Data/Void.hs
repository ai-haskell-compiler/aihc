{-# LANGUAGE EmptyDataDecls #-}

module Data.Void
  ( Void,
    absurd,
    vacuous,
  )
where

import Prelude (Bool (..), Eq (..), Functor (..), Ord (..), Ordering (..), Show (..), seq)

data Void

absurd :: Void -> a
absurd impossible = impossible `seq` absurd impossible

vacuous :: (Functor f) => f Void -> f a
vacuous = fmap absurd

-- | 'Void' has no values, so no two of them can differ.
instance Eq Void where
  _ == _ = True
  _ /= _ = False

instance Ord Void where
  compare _ _ = EQ
  _ < _ = False
  _ <= _ = True
  _ > _ = False
  _ >= _ = True
  max left _ = left
  min left _ = left

instance Show Void where
  showsPrec _ = absurd
