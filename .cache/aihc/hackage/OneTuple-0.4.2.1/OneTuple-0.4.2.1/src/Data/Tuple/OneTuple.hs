{-# LANGUAGE CPP             #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE PatternSynonyms #-}
#endif
-- | This is a module to help migration from @OneTuple@ to @Solo@.
-- Migrate to use "Data.Tuple" from @base-4.16@ or "Data.Tuple.Solo" with all GHCs.
--
-- The pattern synonym is provided for GHCs supporting pattern synonyms (7.8+)
module Data.Tuple.OneTuple
{-# DEPRECATED "Use Data.Tuple.Solo" #-}
(
    OneTuple,
#if __GLASGOW_HASKELL__ >= 708
    pattern OneTuple,
#endif
    only,
) where

import Data.Tuple.Solo

type OneTuple = Solo

only :: OneTuple a -> a
only = getSolo

#if __GLASGOW_HASKELL__ >= 708
#if __GLASGOW_HASKELL__ >= 710
pattern OneTuple :: a -> Solo a
#endif
pattern OneTuple a = MkSolo a
#endif

#if __GLASGOW_HASKELL__ >= 800
{-# COMPLETE OneTuple #-}
#endif
