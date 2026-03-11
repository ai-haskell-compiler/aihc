module Data.Possibly where

-- | For contexts where the @Left@ type is a 'String' diagnostic.
type Possibly a = Either String a
