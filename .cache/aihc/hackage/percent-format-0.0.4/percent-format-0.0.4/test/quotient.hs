-- Copyright (c) 2018 Rudy Matela.
-- Distributed under the 3-Clause BSD licence (see the file LICENSE).
import Test

import Test.LeanCheck
import Test.LeanCheck.Utils

import Text.PercentFormat.Quotient as Q
import Text.PercentFormat.Utils

quotient :: Quotient
quotient  =  error "dummy quotient value"

instance Listable Quotient where
  tiers  =  mapT (uncurry (Q.%)) . reset
         $  tiers `suchThat` canonical
    where
    --canonical (0,0)  =  True
    --canonical (1,0)  =  True
    canonical (n,d)  =  d > 0 && n `gcd` d == 1

main :: IO ()
main = reportTests (tests 360)

tests :: Int -> [Bool]
tests n =
  [ True

  , holds n $ isCommutative ((+) -:> quotient)
  , holds n $ isAssociative ((+) -:> quotient)
  , holds n $ isCommutative ((*) -:> quotient)
  , holds n $ isAssociative ((*) -:> quotient)
  , holds n $ (*) `isDistributiveOver` ((+) -:> quotient)
  , holds n $ isIdempotent (+0) -:> quotient
  , holds n $ isIdempotent (*1) -:> quotient
  , holds n $ isIdempotent abs -:> quotient
  , holds n $ isIdempotent signum -:> quotient
  , holds n $ \x -> negate (negate x) == (x -: quotient)
  , holds n $ \x -> abs x * signum x == (x -: quotient)

  , holds n $ \q r s -> none Q.isNaN [q,r,s] ==> okEqOrd q r s
--, holds n $ okNum -:> quotient  -- TODO: when new LeanCheck is released

  , readQ "Infinity" == infinity
  , readQ "-Infinity" == -infinity
  , let nan' = readQ "NaN" in nan' /= nan'
  , maybeReadQ "blah" == Nothing
  , readQ "10" == 10
  , readQ "-300" == (-300)
  , readQ "10.10" == 101 % 10
  , readQ "10.1" == 101 % 10
  , readQ "33.3e2" == 3330
  , readQ "3.33e2" == 333
  , round (1/2 :: Quotient) == 0
  , round (3/2 :: Quotient) == 2
  ]
