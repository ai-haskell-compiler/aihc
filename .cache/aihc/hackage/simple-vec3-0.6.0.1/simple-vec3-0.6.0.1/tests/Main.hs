{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Proxy

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Vec3


infix 4 <~=>
(<~=>) :: forall v1 v2. (Vec3 v1, Vec3 v2) => v1 -> v2 -> Bool
(<~=>) a b = ax ~= bx &&
             ay ~= by &&
             az ~= bz
  where
    (ax, ay, az) = toXYZ (a :: v1)
    (bx, by, bz) = toXYZ (b :: v2)


infix 4 ~=
(~=) :: Double -> Double -> Bool
(~=) a b | a == b = True
         | a == 0 || b == 0 || isDenormalized absDiff = absDiff < maxError
         | otherwise = absDiff / (abs a + abs b) < maxError
  where
    absDiff = abs $ a - b
    maxError = 1e-13


tests
  :: forall ty.
     ( Arbitrary ty, Arbitrary (Matrix ty)
     , Show ty, Show (Matrix ty)
     , Vec3 ty, Eq ty
     )
  => Proxy ty
  -> [TestTree]
tests _ =
  [ testProperty
    "Commutativity of addition: a + b = b + a"
    (\(a :: ty) b -> a <+> b <~=> b <+> a)
  , testProperty
    "Associativity of addition: (a + b) + c = a + (b + c)"
    (\(a :: ty) b c -> (a <+> b) <+> c <~=> a <+> (b <+> c))
  , testProperty
    "Identity element of addition (zero): v + 0 = v"
    (\(v :: ty) -> v <+> origin <~=> v)
  , testProperty
    "Inverse element of addition: v + (-v) = 0"
    (\(v :: ty) -> v <+> invert v <~=> (origin :: ty))
  , testProperty
    "Compatibility of scalar and field multiplication"
    (\(v :: ty) p q -> v .^ p .^ q <~=> v .^ (p * q))
  , testProperty
    "Identity of scalar multiplication"
    (\(v :: ty) -> v .^ 1 <~=> v)
  , testProperty
    "Distributivity wrt vector addition"
    (\(a :: ty) b p -> ((a <+> b) .^ p) <~=> a .^ p <+> b .^ p)
  , testProperty
    "Distributivity wrt scalar addition"
    (\(a :: ty) p q -> a .^ (p + q) <~=> a .^ p <+> a .^ q)
  , testProperty
    "Subtraction definition"
    (\(a :: ty) b -> a <+> invert b <~=> a <-> b)
  , testProperty
    "Normalization"
    (\(v :: ty) -> v <~=> (origin :: ty) || norm (normalize v) ~= 1)
  , testProperty
    "Triangle inequality"
    (\(a :: ty) b c -> distance a b + distance b c >= distance a c)
  , testProperty
    "Diagonal matrix multiplication"
    (\(v :: ty) (s :: Double) -> diag s `mxv` v == v .^ s)
  ]


main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "CVec3" (tests (Proxy :: Proxy CVec3))
  , testGroup "TVec3" (tests (Proxy :: Proxy TVec3))
  ]
