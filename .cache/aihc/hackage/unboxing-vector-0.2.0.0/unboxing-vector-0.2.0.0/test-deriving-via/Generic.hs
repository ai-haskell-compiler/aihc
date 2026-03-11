{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE UndecidableInstances #-}
module Generic where
import Test.HUnit
import qualified Data.Vector.Unboxing as V
import GHC.Generics
import Foo (Foo,mkFoo)
import Enum (Direction(North))

-- Deriving using Generic
data ComplexDouble = ComplexDouble { realPartD :: {-# UNPACK #-} !Double
                                   , imagPartD :: {-# UNPACK #-} !Double
                                   }
  deriving (Eq,Show,Generic)
  deriving V.Unboxable via V.Generics ComplexDouble

testComplexDouble = TestCase $ do
  let v :: V.Vector ComplexDouble
      v = V.singleton (ComplexDouble 1.0 2.0)
      x :: V.Vector Double
      x = V.map realPartD v
  assertEqual "construction" (ComplexDouble 1.0 2.0) (V.head v)
  assertEqual "map" (V.singleton 1.0) x

data Bar = Bar {-# UNPACK #-} !Foo {-# UNPACK #-} !Double !Direction
  deriving (Eq,Show,Generic)
  deriving V.Unboxable via V.Generics Bar

testBar = TestCase $ do
  let v :: V.Vector Bar
      v = V.singleton (Bar mkFoo 3.14 North)
  assertEqual "construction" (Bar mkFoo 3.14 North) (V.head v)
  assertEqual "map" (V.singleton mkFoo) (V.map (\(Bar foo _ _) -> foo) v)
