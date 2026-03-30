{- ORACLE_TEST
id: mptc-superclass
category: declarations
expected: pass
-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MultiParamTypeClassesSuperclass where

class Parent a b where
  parent :: a -> b

class Parent a b => Child a b where
  child :: a -> b
