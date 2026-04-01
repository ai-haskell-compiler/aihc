{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
module TH_Splice_P where

f $pat = True
g $(pat arg) = False