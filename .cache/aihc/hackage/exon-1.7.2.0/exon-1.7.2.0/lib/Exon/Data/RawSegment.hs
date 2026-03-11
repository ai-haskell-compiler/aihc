-- | Description: Data Type 'RawSegment', Internal

module Exon.Data.RawSegment where

-- | An intermediate representation for internal use.
data RawSegment =
  WsSegment String
  |
  StringSegment String
  |
  ExpSegment String
  |
  AutoExpSegment String
  deriving stock (Eq, Show)
