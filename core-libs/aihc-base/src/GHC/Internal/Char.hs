{-# LANGUAGE MagicHash #-}

module GHC.Internal.Char
  ( Char (..),
  )
where

import GHC.Prim (Char#)

data Char = C# Char#
