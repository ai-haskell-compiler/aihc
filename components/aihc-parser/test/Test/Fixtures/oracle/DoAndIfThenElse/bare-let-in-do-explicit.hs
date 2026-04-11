{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}

module BareLetInDoExplicit where

-- Explicit empty braces in do block
test = do
  let {}
  x <- undefined
  return ()
