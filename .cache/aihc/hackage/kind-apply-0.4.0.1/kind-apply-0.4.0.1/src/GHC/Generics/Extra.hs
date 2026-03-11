{-# language ConstraintKinds #-}
{-# language GADTs           #-}
{-# language PolyKinds       #-}
{-# language TypeOperators   #-}
-- | Extensions to the "GHC.Generics" module.
module GHC.Generics.Extra (
  module GHC.Generics
, (:=>:)(..)
) where

import           Data.Kind
import           GHC.Generics

-- | Constraints: used to represent constraints in a constructor.
--
-- > data Showable a = Show a => a -> X a
-- >
-- > instance Generic (Showable a) where
-- >   type Rep (Showable a) = (Show a) :=>: (K1 R a)
data (:=>:) (c :: Constraint) (f :: k -> Type) (a :: k) where
  SuchThat :: c => f a -> (c :=>: f) a
