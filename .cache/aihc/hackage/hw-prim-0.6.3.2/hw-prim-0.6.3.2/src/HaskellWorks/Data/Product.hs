{-# LANGUAGE TypeOperators #-}

module HaskellWorks.Data.Product where

infixr :*:

data (:*:) a b = (:*:) a b
  deriving (Eq, Show)
