{-# LANGUAGE GADTs #-}

module GADTsRecord where

data Box a where
  MkIntBox :: {unIntBox :: Int} -> Box Int
  MkBoolBox :: {unBoolBox :: Bool} -> Box Bool
