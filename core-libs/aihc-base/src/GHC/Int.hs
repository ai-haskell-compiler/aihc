{-# LANGUAGE MagicHash #-}

module GHC.Int
  ( Int (..),
    Int8 (..),
    Int16 (..),
    Int32 (..),
    Int64 (..),
  )
where

import GHC.Prim ((*#), (+#), (-#))

data Int = I# Int#

data Int8 = I8# Int8#

data Int16 = I16# Int16#

data Int32 = I32# Int32#

data Int64 = I64# Int64#
