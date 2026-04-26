{- ORACLE_TEST pass -}
{-# LANGUAGE LinearTypes #-}
module LinearRecordFieldMany where

data T a b = MkT { x %'Many :: a, y :: b }
