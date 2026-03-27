{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependenciesEmptyRight where

class KeepLeft a b | a -> where
  keepLeft :: a -> b -> a
