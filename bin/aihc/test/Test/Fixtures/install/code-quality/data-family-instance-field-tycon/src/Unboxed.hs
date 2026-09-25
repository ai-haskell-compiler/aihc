module Unboxed (Vector (..), ViaBoxed (..)) where

import qualified Boxed

data family Vector a

newtype ViaBoxed a = ViaBoxed a

newtype instance Vector (ViaBoxed a) = VViaBoxed (Boxed.Vector a)
