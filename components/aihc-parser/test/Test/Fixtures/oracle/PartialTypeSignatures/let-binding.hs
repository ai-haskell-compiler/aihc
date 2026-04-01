{- ORACLE_TEST pass -}
{-# LANGUAGE PartialTypeSignatures #-}
module LetBinding where

x = let y :: _; y = False in y
