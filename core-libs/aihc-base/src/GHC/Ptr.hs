{-# LANGUAGE MagicHash #-}

module GHC.Ptr (Ptr (..)) where

data Ptr a = Ptr Addr#
