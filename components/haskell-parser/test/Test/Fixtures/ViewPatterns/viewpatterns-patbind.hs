{-# LANGUAGE ViewPatterns #-}

module ViewPatternsPatBind where

project :: a -> a
project x = x

bound :: a
(project -> bound) = project value
  where
    value = error "fixture"
