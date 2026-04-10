{- ORACLE_TEST xfail reason="TemplateHaskell singletons function with declaration quote and type signature not handled" -}
{-# LANGUAGE TemplateHaskell #-}

module THSingletonsWithTypeSig where

x = singletons [d|
  f :: Int -> Int
  f y = y
  |]
