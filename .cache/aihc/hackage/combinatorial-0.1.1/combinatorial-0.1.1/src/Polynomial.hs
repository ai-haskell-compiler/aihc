module Polynomial (
   T, fromScalar, add, sub, neg, scale, mul,
   differentiate, progression,
   ) where


type T a = [a]


fromScalar :: a -> [a]
fromScalar = (:[])

-- | add two polynomials or series
add :: Num a => [a] -> [a] -> [a]
{- zipWith (+) would cut the resulting list
   to the length of the shorter operand -}
add [] ys = ys
add xs [] = xs
add (x:xs) (y:ys) = x+y : add xs ys

-- | subtract two polynomials or series
sub :: Num a => [a] -> [a] -> [a]
sub [] ys = map negate ys
sub xs [] = xs
sub (x:xs) (y:ys) = x-y : sub xs ys

neg :: Num a => [a] -> [a]
neg = map negate

-- | scale a polynomial or series by a factor
scale :: Num a => a -> [a] -> [a]
scale s = map (s*)


-- | multiply two polynomials or series
mul :: Num a => [a] -> [a] -> [a]
{- prevent from generation of many zeros
   if the first operand is the empty list -}
mul [] = const []
mul xs = foldr (\y zs -> add (scale y xs) (0:zs)) []


progression :: Num a => [a]
progression = iterate (1+) 1


differentiate :: (Num a) => [a] -> [a]
differentiate x = zipWith (*) (tail x) progression
