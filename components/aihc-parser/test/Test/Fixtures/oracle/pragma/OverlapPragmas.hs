{- ORACLE_TEST pass -}
{-# LANGUAGE FlexibleInstances #-}

module OverlapPragmas where

class Render a where
  render :: a -> String

instance {-# OVERLAPPABLE #-} Show a => Render [a] where
  render = show

instance {-# OVERLAPPING #-} Render [Char] where
  render = id

instance {-# OVERLAPS #-} Render Int where
  render = show

class Pick a where
  pick :: a -> String

instance {-# INCOHERENT #-} Pick a where
  pick _ = "fallback"
