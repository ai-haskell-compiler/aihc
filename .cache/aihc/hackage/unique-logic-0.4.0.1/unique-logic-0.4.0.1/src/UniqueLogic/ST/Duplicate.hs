{- |
This module provides several ways to cope with over-determined values.
-}
module UniqueLogic.ST.Duplicate (
   C, accept,
   Ignore(Ignore),
   Forbid(Forbid),
   Verify(Verify),
   ) where


class C a where
   accept :: a -> a -> Bool

instance (C a, C b) => C (a, b) where
   accept (a0,b0) (a1,b1) =
      accept a0 a1 && accept b0 b1

instance (C a, C b, C c) => C (a, b, c) where
   accept (a0,b0,c0) (a1,b1,c1) =
      accept a0 a1 && accept b0 b1 && accept c0 c1


{- |
Ignore duplicate ways to determine a variable.
The chosen value depends on the particular algorithm.
-}
newtype Ignore a = Ignore a deriving (Eq, Ord, Show)

instance C (Ignore a) where accept _ _ = True

ignore1 :: (a -> b) -> Ignore a -> Ignore b
ignore1 f (Ignore x) = Ignore $ f x

ignore2 :: (a -> b -> c) -> Ignore a -> Ignore b -> Ignore c
ignore2 f (Ignore x) (Ignore y) = Ignore $ f x y

instance Num a => Num (Ignore a) where
   fromInteger = Ignore . fromInteger
   (+) = ignore2 (+)
   (-) = ignore2 (-)
   (*) = ignore2 (*)
   abs = ignore1 abs
   signum = ignore1 signum

instance Fractional a => Fractional (Ignore a) where
   fromRational = Ignore . fromRational
   (/) = ignore2 (/)

instance Floating a => Floating (Ignore a) where
   pi = Ignore pi
   exp = ignore1 exp
   sqrt = ignore1 sqrt
   log = ignore1 log
   (**) = ignore2 (**)
   logBase = ignore2 logBase
   sin = ignore1 sin
   tan = ignore1 tan
   cos = ignore1 cos
   asin = ignore1 asin
   atan = ignore1 atan
   acos = ignore1 acos
   sinh = ignore1 sinh
   tanh = ignore1 tanh
   cosh = ignore1 cosh
   asinh = ignore1 asinh
   atanh = ignore1 atanh
   acosh = ignore1 acosh



{- |
Duplicate ways to determine a variable value
are always considered an error.
If you use @Rule@s or @Expression@s this is not a good idea,
since every rule is over-determined.
-}
newtype Forbid a = Forbid a deriving (Eq, Ord, Show)

instance C (Forbid a) where accept _ _ = False

forbid1 :: (a -> b) -> Forbid a -> Forbid b
forbid1 f (Forbid x) = Forbid $ f x

forbid2 :: (a -> b -> c) -> Forbid a -> Forbid b -> Forbid c
forbid2 f (Forbid x) (Forbid y) = Forbid $ f x y

instance Num a => Num (Forbid a) where
   fromInteger = Forbid . fromInteger
   (+) = forbid2 (+)
   (-) = forbid2 (-)
   (*) = forbid2 (*)
   abs = forbid1 abs
   signum = forbid1 signum

instance Fractional a => Fractional (Forbid a) where
   fromRational = Forbid . fromRational
   (/) = forbid2 (/)

instance Floating a => Floating (Forbid a) where
   pi = Forbid pi
   exp = forbid1 exp
   sqrt = forbid1 sqrt
   log = forbid1 log
   (**) = forbid2 (**)
   logBase = forbid2 logBase
   sin = forbid1 sin
   tan = forbid1 tan
   cos = forbid1 cos
   asin = forbid1 asin
   atan = forbid1 atan
   acos = forbid1 acos
   sinh = forbid1 sinh
   tanh = forbid1 tanh
   cosh = forbid1 cosh
   asinh = forbid1 asinh
   atanh = forbid1 atanh
   acosh = forbid1 acosh


{- |
Duplicate ways to determine a variable value are allowed
as long as every way yields the same result.
\"Same\" is meant with respect to the 'Eq' class.
-}
newtype Verify a = Verify a deriving (Eq, Ord, Show)

instance Eq a => C (Verify a) where accept (Verify x) (Verify y) = x==y

verify1 :: (a -> b) -> Verify a -> Verify b
verify1 f (Verify x) = Verify $ f x

verify2 :: (a -> b -> c) -> Verify a -> Verify b -> Verify c
verify2 f (Verify x) (Verify y) = Verify $ f x y

instance Num a => Num (Verify a) where
   fromInteger = Verify . fromInteger
   (+) = verify2 (+)
   (-) = verify2 (-)
   (*) = verify2 (*)
   abs = verify1 abs
   signum = verify1 signum

instance Fractional a => Fractional (Verify a) where
   fromRational = Verify . fromRational
   (/) = verify2 (/)

instance Floating a => Floating (Verify a) where
   pi = Verify pi
   exp = verify1 exp
   sqrt = verify1 sqrt
   log = verify1 log
   (**) = verify2 (**)
   logBase = verify2 logBase
   sin = verify1 sin
   tan = verify1 tan
   cos = verify1 cos
   asin = verify1 asin
   atan = verify1 atan
   acos = verify1 acos
   sinh = verify1 sinh
   tanh = verify1 tanh
   cosh = verify1 cosh
   asinh = verify1 asinh
   atanh = verify1 atanh
   acosh = verify1 acosh
