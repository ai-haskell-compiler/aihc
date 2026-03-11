{- |
Define common properties that can be used e.g. for automated tests.
Cf. to "Test.QuickCheck.Utils".
-}
module Algebra.Laws where


commutative :: Eq a => (b -> b -> a) -> b -> b -> Bool
commutative op x y  =  x `op` y == y `op` x

associative :: Eq a => (a -> a -> a) -> a -> a -> a -> Bool
associative op x y z  =  (x `op` y) `op` z == x `op` (y `op` z)

leftIdentity :: Eq a => (b -> a -> a) -> b -> a -> Bool
leftIdentity op y x  =  y `op` x == x

rightIdentity :: Eq a => (a -> b -> a) -> b -> a -> Bool
rightIdentity op y x  =  x `op` y == x

identity :: Eq a => (a -> a -> a) -> a -> a -> Bool
identity op x y  =  leftIdentity op x y &&  rightIdentity op x y

leftZero :: Eq a => (a -> a -> a) -> a -> a -> Bool
leftZero  =  flip . rightIdentity

rightZero :: Eq a => (a -> a -> a) -> a -> a -> Bool
rightZero  =  flip . leftIdentity

zero :: Eq a => (a -> a -> a) -> a -> a -> Bool
zero op x y  =  leftZero op x y  &&  rightZero op x y

leftInverse :: Eq a => (b -> b -> a) -> (b -> b) -> a -> b -> Bool
leftInverse op inv y x  =  inv x `op` x == y

rightInverse :: Eq a => (b -> b -> a) -> (b -> b) -> a -> b -> Bool
rightInverse op inv y x  =  x `op` inv x == y

inverse :: Eq a => (b -> b -> a) -> (b -> b) -> a -> b -> Bool
inverse op inv y x  =  leftInverse op inv y x && rightInverse op inv y x

leftDistributive :: Eq a => (a -> b -> a) -> (a -> a -> a) -> b -> a -> a -> Bool
leftDistributive ( # ) op x y z  =  (y `op` z) # x == (y # x) `op` (z # x)

rightDistributive :: Eq a => (b -> a -> a) -> (a -> a -> a) -> b -> a -> a -> Bool
rightDistributive ( # ) op x y z  =  x # (y `op` z) == (x # y) `op` (x # z)

homomorphism :: Eq a =>
   (b -> a) -> (b -> b -> b) -> (a -> a -> a) -> b -> b -> Bool
homomorphism f op0 op1 x y  =  f (x `op0` y) == f x `op1` f y

rightCascade :: Eq a =>
   (b -> b -> b) -> (a -> b -> a) -> a -> b -> b -> Bool
rightCascade ( # ) op x i j  =  (x `op` i) `op` j == x `op` (i#j)

leftCascade :: Eq a =>
   (b -> b -> b) -> (b -> a -> a) -> a -> b -> b -> Bool
leftCascade ( # ) op x i j  =  j `op` (i `op` x) == (j#i) `op` x
