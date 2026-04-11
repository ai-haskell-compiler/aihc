{- ORACLE_TEST xfail reason="Bare let in do block without bindings not handled" -}
{-# LANGUAGE Haskell2010 #-}

module BareLetInDo where

test = do
  let
  x <- undefined
  return ()
