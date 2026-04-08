{- ORACLE_TEST pass -}
module PragmaDeprecatedAddFileExtension where

addFileExtension :: ()
{-# DEPRECATED addFileExtension "Please use addExtension instead." #-}
addFileExtension = ()
