{-# LANGUAGE MagicHash #-}

module GHC.Int
  ( Int (..),
    Int32 (..),
  )
where

import GHC.Prim ((*#), (+#), (-#))

data Int = I# Int#

data Int32 = I32# Int32#
