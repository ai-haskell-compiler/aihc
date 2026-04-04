{- ORACLE_TEST xfail path deprecated declaration pragma -}
module PragmaDeprecatedAddFileExtension where

addFileExtension :: ()
{-# DEPRECATED addFileExtension "Please use addExtension instead." #-}
addFileExtension = ()
