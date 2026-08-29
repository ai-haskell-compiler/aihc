{-# LANGUAGE TypeOperators #-}

module Data.Type.Equality
  ( (:~:) (..),
    (:~~:) (..),
  )
where

infix 4 :~:, :~~:

-- | Propositional equality. A value of type a :~: b proves that a and b are the same type.
data a :~: b where
  Refl :: a :~: a

-- | Kind-heterogeneous propositional equality.
data a :~~: b where
  HRefl :: a :~~: a
