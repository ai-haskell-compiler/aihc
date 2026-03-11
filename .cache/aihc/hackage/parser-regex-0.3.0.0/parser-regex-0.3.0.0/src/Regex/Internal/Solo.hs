{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
#ifdef __GLASGOW_HASKELL__
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
#endif

-- | This is an internal module. You probably don't need to import this.
--
module Regex.Internal.Solo
  ( Solo
  , mkSolo
  , matchSolo
  ) where

mkSolo :: a -> Solo a
matchSolo :: Solo a -> (a -> b) -> b

#ifdef __GLASGOW_HASKELL__
newtype Solo a = Solo (# a #)
mkSolo x = Solo (# x #)
matchSolo (Solo (# x #)) f = f x
#else
data Solo a = Solo a
mkSolo = Solo
matchSolo (Solo x) f = f x
#endif
