------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.Compat.Write
-- Copyright:   (c) 2013 Leon P Smith
-- License:     BSD3
-- Maintainer:  https://github.com/blaze-builder
-- Stability:   stable
--
-- Conversions from the new Prims to the old Writes.
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.Compat.Write
    ( Write
    , writePrimFixed
    , writePrimBounded
    ) where

import Data.ByteString.Builder.Prim.Internal (BoundedPrim, FixedPrim
                                             , runB, runF, size, sizeBound)
import Blaze.ByteString.Builder.Internal.Write (Poke(..), Write
                                               , boundedWrite, exactWrite)

writePrimFixed :: FixedPrim a -> a -> Write
writePrimFixed fe a = exactWrite (size fe) (runF fe a)
{-# INLINE writePrimFixed #-}

writePrimBounded :: BoundedPrim a -> a -> Write
writePrimBounded be a = boundedWrite (sizeBound be) (Poke (runB be a))
{-# INLINE writePrimBounded #-}
