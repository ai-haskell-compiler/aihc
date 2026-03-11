{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Foo
  (Foo -- the constructor is not exported!
  ,mkFoo
  ) where
import Data.Vector.Unboxing (Unboxable(..))
import Control.DeepSeq (NFData)
import GHC.Generics

newtype Foo = Foo Int deriving (Eq,Show,Generic,Unboxable)

instance NFData Foo

mkFoo :: Foo
mkFoo = Foo 42
