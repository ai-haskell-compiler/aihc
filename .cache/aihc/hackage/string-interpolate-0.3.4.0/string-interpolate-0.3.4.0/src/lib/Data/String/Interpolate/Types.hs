module Data.String.Interpolate.Types
  ( InterpSegment(..)
  , Line, Lines
  )
where

data InterpSegment
  = Expression String
  | Verbatim String
  | Spaces Int
  | Tabs Int
  deriving (Eq, Show)

type Line = [InterpSegment]
type Lines = [Line]
