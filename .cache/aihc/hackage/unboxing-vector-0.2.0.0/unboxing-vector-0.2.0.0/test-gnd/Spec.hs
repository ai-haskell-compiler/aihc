{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
import Prelude
import Test.HUnit
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxing as V
import Data.Monoid (Sum(..))
#if defined(ENABLE_MONO_TRAVERSABLE)
import Data.MonoTraversable (ofold)
#endif
---
import Foo (Foo,mkFoo)

#if !defined(ENABLE_MONO_TRAVERSABLE)
-- Ad-hoc definition
ofold :: (Monoid a, G.Vector v a) => v a -> a
ofold = mconcat . G.toList
#endif

newtype IntMod17 = IntMod17 Int
  deriving (Eq,Show)
  deriving newtype V.Unboxable -- with DerivingStrategies

instance Num IntMod17 where
  IntMod17 x + IntMod17 y = IntMod17 ((x + y) `rem` 17)
  IntMod17 x - IntMod17 y = IntMod17 ((x - y) `mod` 17)
  IntMod17 x * IntMod17 y = IntMod17 ((x * y) `rem` 17)
  negate (IntMod17 x) = IntMod17 (negate x `mod` 17)
  fromInteger x = IntMod17 (fromIntegral (x `mod` 17))
  abs = undefined; signum = undefined

testIntMod17 = TestCase $ do
  let v = V.fromList [-3,-2,-1,0,1,2,3,4,5] :: V.Vector IntMod17
  assertEqual "construction" (V.fromList [14,15,16,0,1,2,3,4,5]) v
  assertEqual "sum" 9 (V.sum v) -- not 60
  assertEqual "coercion" (V.fromList [14,15,16,0,1,2,3,4,5] :: V.Vector Int) (V.coerceVector v) -- this is possible because the constructor of IntMod17 is visible here
  let vSum = V.coerceVector v :: V.Vector (Sum IntMod17)
  assertEqual "coercion and sum" (Sum 9) (ofold vSum)

newtype Baz = Baz Foo
  deriving (Eq,Show,V.Unboxable)

testBaz = TestCase $ do
  let foo :: V.Vector Foo
      foo = V.singleton mkFoo
      baz :: V.Vector Baz
      baz = V.singleton (Baz mkFoo)
  assertEqual "construction" (Baz mkFoo) (V.head baz)
  assertEqual "map 1" baz (V.map Baz foo)
  assertEqual "map 2" foo (V.map (\(Baz x) -> x) baz)
  assertEqual "coercion" baz (V.coerceVector foo)

-- We can make an unboxed vector of Foo, even though we don't have 'Coercible Int Foo' in scope.
testAbstractType = TestCase $ do
  let v = V.singleton mkFoo :: V.Vector Foo
  assertEqual "Foo" mkFoo (V.head v)
  assertEqual "coercion" mkFoo (getSum $ V.head (V.coerceVector v :: V.Vector (Sum Foo)))

tests = TestList [TestLabel "Basic features" testIntMod17
                 ,TestLabel "Test with abstract type" testAbstractType
                 ,TestLabel "Test with GND" testBaz
                 ]

main = runTestTT tests
