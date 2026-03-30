{- ORACLE_TEST
id: fundep-with-context
category: declarations
expected: pass
-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependenciesWithContext where

class Show a => Pretty a b | a -> b where
  pretty :: a -> b
