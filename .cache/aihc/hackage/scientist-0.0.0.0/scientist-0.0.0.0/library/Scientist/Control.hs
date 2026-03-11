module Scientist.Control
  ( Control(..)
  ) where

newtype Control a = Control
  { unControl :: a
  }
