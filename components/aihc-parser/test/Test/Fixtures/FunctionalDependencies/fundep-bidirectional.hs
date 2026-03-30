{- ORACLE_TEST
id: fundep-bidirectional
category: declarations
expected: pass
-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FunctionalDependenciesBidirectional where

class Iso a b | a -> b, b -> a where
  to :: a -> b
  from :: b -> a
