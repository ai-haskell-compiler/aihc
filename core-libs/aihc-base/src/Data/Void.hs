{-# LANGUAGE EmptyDataDecls #-}

module Data.Void
  ( Void,
    absurd,
    vacuous,
  )
where

import Prelude (Functor (..), seq)

data Void

absurd :: Void -> a
absurd impossible = impossible `seq` absurd impossible

vacuous :: (Functor f) => f Void -> f a
vacuous = fmap absurd
