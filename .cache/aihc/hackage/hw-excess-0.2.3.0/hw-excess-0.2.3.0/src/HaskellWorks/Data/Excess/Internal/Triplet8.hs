module HaskellWorks.Data.Excess.Internal.Triplet8
  ( Triplet8(..)
  ) where

import Data.Int

data Triplet8 = Triplet8
  { triplet8Lo :: {-# UNPACK #-} !Int8
  , triplet8Ex :: {-# UNPACK #-} !Int8
  , triplet8Hi :: {-# UNPACK #-} !Int8
  } deriving (Eq, Show)
