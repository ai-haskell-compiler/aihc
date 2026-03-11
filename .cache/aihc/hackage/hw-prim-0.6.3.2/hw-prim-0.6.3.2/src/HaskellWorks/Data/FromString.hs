module HaskellWorks.Data.FromString
  ( FromString(..)
  ) where

class FromString a where
  fromString :: String -> a
