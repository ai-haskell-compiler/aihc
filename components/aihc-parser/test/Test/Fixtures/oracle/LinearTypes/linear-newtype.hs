{- ORACLE_TEST pass -}
{-# LANGUAGE LinearTypes #-}
module LinearNewtype where

newtype Wrap a = Wrap { unwrap :: a }
