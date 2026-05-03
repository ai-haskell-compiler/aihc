{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
module S where

class C a

instance C $(pure Int)
