{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fdefer-type-errors -Wno-deferred-type-errors #-}
module TestTypeErrors where
import Test.HUnit
import Test.ShouldNotTypecheck
import Data.Coerce
import qualified Data.Vector.Unboxing as V
import Foo (Foo)

-- Since the module Foo does not export Foo's constructor,
-- it should be impossible to create a value of Foo in this module.

-- 'intToFoo1' should not compile because the constructor of 'Foo' is not visible.
intToFoo1 :: Int -> Foo
intToFoo1 x = coerce x

-- 'intToFoo2' should not compile because 'coerceVector' requires the constructor to be visible.
-- This one does compile if 'coerceVector' is defined as
-- > coerceVector :: ({- Coercible a b, -} Rep a ~ Rep b) => Vector a -> Vector b
intToFoo2 :: Int -> Foo
intToFoo2 x = V.head (V.coerceVector (V.singleton x))

-- 'intToFoo3' should not compile because the constructor of Foo is not visible.
-- This one does compile if 'Unboxable' is defined as
-- > class (U.Unbox (Rep a), Coercible a (Rep a)) => Unboxable a
intToFoo3 :: (V.Unboxable a, a ~ Foo) => Int -> a
intToFoo3 x = coerce x

testTypeErrors :: Test
testTypeErrors = TestList [TestLabel "Basic test for coerce" $ TestCase $ shouldNotTypecheck (intToFoo1 0xDEAD)
                          ,TestLabel "Test for coerceVector" $ TestCase $ shouldNotTypecheck (intToFoo2 0xDEAD)
                          ,TestLabel "Test for Unboxable" $ TestCase $ shouldNotTypecheck (intToFoo3 0xDEAD)
                          ]

