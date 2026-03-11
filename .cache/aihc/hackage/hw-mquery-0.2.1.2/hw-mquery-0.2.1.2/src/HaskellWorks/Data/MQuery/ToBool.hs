
module HaskellWorks.Data.MQuery.ToBool where

import qualified Data.DList as DL

class ToBool a where
  toBool :: a -> Bool

instance ToBool Bool where
  toBool = id

instance ToBool [a] where
  toBool (_:_) = True
  toBool    _  = False

instance ToBool (DL.DList a) where
  toBool = toBool . DL.toList
