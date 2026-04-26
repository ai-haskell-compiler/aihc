{- ORACLE_TEST pass -}
{-# LANGUAGE LinearTypes #-}
module LinearRecordFieldPoly where

data T a m = MkT { x %m :: a }
