module Data.Function.HT.Private where

import Data.List (genericReplicate, unfoldr)
import Data.Maybe.HT (toMaybe)
import Data.Tuple.HT (swap)

-- $setup
-- >>> import Test.QuickCheck (NonNegative(NonNegative))

{- |
Compositional power of a function,
i.e. apply the function @n@ times to a value.
It is rather the same as @iter@
in Simon Thompson: \"The Craft of Functional Programming\", page 172
-}
{-# INLINE nest #-}
nest :: Int -> (a -> a) -> a -> a
nest 0 _ x = x
nest n f x = f (nest (n-1) f x)

{- |
prop> \(NonNegative n) x -> nest n succ x == nest1 n succ (x::Integer)
prop> \(NonNegative n) x -> nest n succ x == nest2 n succ (x::Integer)
-}
nest1, nest2 :: Int -> (a -> a) -> a -> a
nest1 n f = foldr (.) id (replicate n f)
nest2 n f x = iterate f x !! n


{- |
@powerAssociative@ is an auxiliary function that,
for an associative operation @op@,
computes the same value as

  @powerAssociative op a0 a n = foldr op a0 (genericReplicate n a)@

but applies "op" O(log n) times and works for large n.
-}

{-# INLINE powerAssociative #-}
powerAssociative :: (a -> a -> a) -> a -> a -> Integer -> a
powerAssociative op =
   let go acc _ 0 = acc
       go acc a n = go (if even n then acc else op acc a) (op a a) (div n 2)
   in  go

{- |
prop> \a0 a (NonNegative n) -> powerAssociative (+) a0 a n == (powerAssociativeList (+) a0 a n :: Integer)
prop> \a0 a (NonNegative n) -> powerAssociative (+) a0 a n == (powerAssociativeNaive (+) a0 a n :: Integer)
-}
powerAssociativeList, powerAssociativeNaive ::
   (a -> a -> a) -> a -> a -> Integer -> a
powerAssociativeList op a0 a n =
   foldl (\acc (bit,pow) -> if bit==0 then acc else op acc pow) a0 $
   zip
      (unfoldr (\k -> toMaybe (k>0) $ swap $ divMod k 2) n)
      (iterate (\pow -> op pow pow) a)

powerAssociativeNaive op a0 a n =
   foldr op a0 (genericReplicate n a)


infixl 0 $%

{- |
Flipped version of '($)'.

It was discussed as (&) in
http://www.haskell.org/pipermail/libraries/2012-November/018832.html

I am not sure, that we need it.
It is not exported for now.
-}
($%) :: a -> (a -> b) -> b
($%) = flip ($)
