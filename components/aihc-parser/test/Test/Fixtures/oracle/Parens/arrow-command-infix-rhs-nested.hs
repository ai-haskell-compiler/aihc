{- ORACLE_TEST pass -}
{-# LANGUAGE Arrows #-}
module M where

x = proc _ -> do
            [] -<< []
           `a` (([] -<< []) + ([] -< []))
