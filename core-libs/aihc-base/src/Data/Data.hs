module Data.Data
  ( Data,
  )
where

import Data.Typeable (Typeable)

class (Typeable a) => Data a
