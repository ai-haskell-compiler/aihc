{- ORACLE_TEST xfail io-memoize deprecated prime identifier pragma -}
module PragmaDeprecatedPrimeName where

ioMemo' :: ()
{-# DEPRECATED ioMemo' "Please just call the action directly." #-}
ioMemo' = ()
