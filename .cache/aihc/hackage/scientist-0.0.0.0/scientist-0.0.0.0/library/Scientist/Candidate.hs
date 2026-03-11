module Scientist.Candidate
  ( Candidate(..)
  ) where

newtype Candidate a = Candidate
  { unCandidate :: a
  }
