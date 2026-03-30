{- ORACLE_TEST
id: gadts-record
category: declarations
expected: pass
-}
{-# LANGUAGE GADTs #-}

module GADTsRecord where

data Box a where
  MkIntBox :: {unIntBox :: Int} -> Box Int
  MkBoolBox :: {unBoolBox :: Bool} -> Box Bool
