{-# LANGUAGE MagicHash #-}

module GHC.Num.Integer
  ( Integer (..),
    integerLog2#,
    integerLogBase#,
  )
where

import GHC.Internal.Integer (Integer (..), integerLog2#, integerLogBase#)
