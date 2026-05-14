{- ORACLE_TEST pass -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module UnicodeIdentifierContinuation where

data ((s :: αₛ -> αₛ -> *) :×: (t :: αₜ -> αₜ -> *)) :: (αₛ, αₜ) -> (αₛ, αₜ) -> * where
  (:×:) :: s aₛ bₛ -> t aₜ bₜ -> (s :×: t) '(aₛ, aₜ) '(bₛ, bₜ)
