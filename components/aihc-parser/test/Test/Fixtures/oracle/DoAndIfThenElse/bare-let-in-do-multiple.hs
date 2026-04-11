{- ORACLE_TEST pass -}
{-# LANGUAGE Haskell2010 #-}

module BareLetInDoMultiple where

-- Multiple empty lets in do block
test = do
  let
  let {}
  x <- undefined
  return ()
