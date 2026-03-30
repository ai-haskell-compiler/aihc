{- ORACLE_TEST
id: fundep-single
category: declarations
expected: pass
-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependenciesSingle where

class Collects e c | c -> e where
  insert :: e -> c -> c
