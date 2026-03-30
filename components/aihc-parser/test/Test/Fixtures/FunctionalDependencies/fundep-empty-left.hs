{- ORACLE_TEST
id: fundep-empty-left
category: declarations
expected: pass
-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependenciesEmptyLeft where

class KeepRight a b | -> b where
  keepRight :: a -> b
