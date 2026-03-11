{-# LANGUAGE DeriveGeneric #-}

module ROC.ID.Digit
  ( Digit (..)
  , parseDigit
  ) where

import GHC.Generics
    ( Generic )

import ROC.ID.Utilities

-- | Represents a single decimal digit in the range 0 to 9.
--
data Digit
  = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Bounded, Enum, Eq, Generic, Ord)

instance Show Digit where show = show . fromEnum

parseDigit :: Char -> Maybe Digit
parseDigit c = maybeRead [c] >>= maybeToEnum

