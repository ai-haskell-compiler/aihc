{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}

module BareLetInDo where

test = do
  let
  x <- undefined
  return ()
