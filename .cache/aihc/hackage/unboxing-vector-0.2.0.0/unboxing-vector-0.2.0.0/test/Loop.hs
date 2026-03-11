{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
-- {-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
module Loop where
import qualified Data.Vector.Unboxing as V
import GHC.Generics

-- Loop!
{-
newtype Bad = Bad Bad
  deriving newtype V.Unboxable
-}

{-
data Bad2 = Bad2 Bad2
  deriving Generic
  deriving V.Unboxable via V.Generics Bad2
-}

{-
class SomeClass a where
  type Foo a
--   foo :: a -> Foo a

newtype Bad = Bad Bad
  deriving newtype SomeClass

newtype BadT = BadT (Int, BadT)
  deriving newtype V.Unboxable
-}
