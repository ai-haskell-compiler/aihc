module Generic where

import GHC.Generics (Generic, Generic1)

newtype Wrapper f a = Wrapper (f a) deriving (Generic, Generic1)
newtype Constant a b = Constant a deriving (Generic, Generic1)
