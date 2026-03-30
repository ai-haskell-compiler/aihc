{- ORACLE_TEST
id: mptc-class-basic
category: declarations
expected: pass
-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MultiParamTypeClassesClassBasic where

class Converts a b where
  convert :: a -> b
