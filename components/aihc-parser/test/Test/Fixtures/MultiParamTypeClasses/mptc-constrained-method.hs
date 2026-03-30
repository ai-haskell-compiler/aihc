{- ORACLE_TEST
id: mptc-constrained-method
category: declarations
expected: pass
-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MultiParamTypeClassesConstrainedMethod where

class Render a b where
  render :: Show a => a -> b -> String
