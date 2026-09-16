{-# LANGUAGE GADTs #-}

module Data.Dynamic
  ( Dynamic (..),
  )
where

import Type.Reflection (TypeRep)

-- | A value of type 'Dynamic' is an object encapsulated together with its
-- type.
--
-- Only the type itself is provided so far; the conversions ('toDyn',
-- 'fromDyn', 'fromDynamic') and the dynamic application operators
-- ('dynApply', 'dynApp', 'dynTypeRep') are still missing.
data Dynamic where
  Dynamic :: forall a. TypeRep a -> a -> Dynamic
