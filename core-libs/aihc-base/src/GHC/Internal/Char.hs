{-# LANGUAGE MagicHash #-}

module GHC.Internal.Char
  ( Char (..),
  )
where

data Char = C# Char#
