{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Demo where

class Identity a where
  identity :: a -> a

newtype Box a = Box a
  deriving (Identity)
