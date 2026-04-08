{- ORACLE_TEST pass -}
module PragmaDeprecatedPrimeName where

ioMemo' :: ()
{-# DEPRECATED ioMemo' "Please just call the action directly." #-}
ioMemo' = ()
