module Termbox.Internal.Pos
  ( Pos (..),
    posUp,
    posDown,
    posLeft,
    posRight,
  )
where

import GHC.Generics (Generic)

-- | A relative terminal position.
data Pos = Pos
  { row :: {-# UNPACK #-} !Int,
    col :: {-# UNPACK #-} !Int
  }
  deriving stock (Eq, Generic, Ord, Show)

instance Monoid Pos where
  mempty = Pos 0 0

instance Semigroup Pos where
  Pos row1 col1 <> Pos row2 col2 =
    Pos (row1 + row2) (col1 + col2)

-- | Move a position up.
posUp :: Int -> Pos -> Pos
posUp n (Pos row col) =
  Pos (row - n) col

-- | Move a position down.
posDown :: Int -> Pos -> Pos
posDown n (Pos row col) =
  Pos (row + n) col

-- | Move a position left.
posLeft :: Int -> Pos -> Pos
posLeft n (Pos row col) =
  Pos row (col - n)

-- | Move a position right.
posRight :: Int -> Pos -> Pos
posRight n (Pos row col) =
  Pos row (col + n)
