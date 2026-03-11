{-# LANGUAGE CPP         #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe        #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
-- | Indexed non-empty Foldables.
module Data.Foldable1.WithIndex (
    -- * Indexed Foldables
    Foldable1WithIndex (..),
) where

import WithIndex
