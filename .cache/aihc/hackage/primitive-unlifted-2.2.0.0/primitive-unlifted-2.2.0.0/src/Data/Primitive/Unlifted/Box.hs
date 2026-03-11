{-# language KindSignatures #-}
{-# language TypeFamilies #-}
{-# language MagicHash #-}
{-# language DataKinds #-}

-- | Traditionally, there were only a few basic unlifted types available in
-- GHC, all of them primitive. Now, with the @UnliftedNewtypes@ and
-- @UnliftedDatatypes@ extensions, users are free to create as many as they
-- like. However, many essential facilities, like the 'Monad' class, still work
-- only with lifted types, so users must wrap their unlifted types into lifted
-- ones to use those. If the wrapped version of a type is likely to be used
-- heavily on its own, it often makes sense to write a custom wrapper type for
-- it. This module exports a general box for situations where the focus should
-- be on the unlifted type rather than its wrapper.
module Data.Primitive.Unlifted.Box where

import Data.Primitive.Unlifted.Class
import Data.Primitive.Unlifted.Type

-- | Turn an arbitrary unlifted type into a lifted one with a 'PrimUnlifted'
-- instance. For example, given
--
-- @
-- data UnliftedMaybe a :: UnliftedType where
--   UnliftedNothing :: UnliftedMaybe a
--   UnliftedJust :: a -> UnliftedMaybe a
-- @
--
-- we have
--
-- @
-- Box (UnliftedMaybe a) :: Type
-- @
data Box (a :: UnliftedType) = Box# { unBox# :: a }

instance PrimUnlifted (Box a) where
  {-# INLINE toUnlifted# #-}
  {-# INLINE fromUnlifted# #-}
  type Unlifted (Box a) = a

  toUnlifted# (Box# a) = a
  fromUnlifted# a = Box# a

toBox :: PrimUnlifted a => a -> Box (Unlifted a)
toBox a = Box# (toUnlifted# a)

fromBox :: PrimUnlifted a => Box (Unlifted a) -> a
fromBox (Box# a) = fromUnlifted# a
