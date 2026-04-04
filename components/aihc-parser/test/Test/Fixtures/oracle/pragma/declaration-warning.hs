{- ORACLE_TEST xfail tasty-focus declaration warning after binding -}
module DeclarationWarning where

x = ()
{-# WARNING x "w" #-}
