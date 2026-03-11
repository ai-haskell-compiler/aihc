-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Arrow.Transformer
-- Copyright   :  (c) Ross Paterson 2003
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  portable
--
-- Arrow transformers, for making new arrow types out of old ones.

module Control.Arrow.Transformer (
        ArrowTransformer(..)
    ) where

import Control.Arrow

-- | Construct a new arrow from an existing one.
class (Arrow a, Arrow (f a)) => ArrowTransformer f a where

    -- | A transformation of arrows, preserving 'arr', '>>>' and 'first'.
    --
    -- Typical usage in arrow notation:
    --
    -- >    proc p -> ...
    -- >        (|lift cmd|)

    lift :: a b c -> f a b c
