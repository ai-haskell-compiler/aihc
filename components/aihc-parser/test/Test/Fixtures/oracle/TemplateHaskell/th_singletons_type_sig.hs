{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}

module THSingletonsWithTypeSig where

x =
  singletons
    [d|
      f :: Int -> Int
      f y = y
      |]
