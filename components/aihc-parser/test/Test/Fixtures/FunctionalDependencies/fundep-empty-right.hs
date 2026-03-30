{- ORACLE_TEST
id: fundep-empty-right
category: declarations
expected: pass
-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependenciesEmptyRight where

class KeepLeft a b | a -> where
  keepLeft :: a -> b -> a
