{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
----------------------------------------------------------------------
-- |
-- Module      :  Data.NumInstances.Tuple
-- Copyright   :  (c) Conal Elliott 2008-2013
-- License     :  BSD3
--
-- Maintainer  :  conal@conal.net
-- Stability   :  experimental
--
-- Number class instances for tuples
----------------------------------------------------------------------

module Data.NumInstances.Tuple () where

-- TODO: Replace with a Template Haskell definition and applications

lift2 :: (a->u) -> (b->v) -> (a,b) -> (u,v)
lift2 fa fb (a,b) = (fa a, fb b)

-- Equivalently, lift2 = (***)

instance (Num a, Num b) => Num (a,b) where
  fromInteger n   = (fromInteger n, fromInteger n)
  (a,b) + (a',b') = (a+a',b+b')
  (a,b) - (a',b') = (a-a',b-b')
  (a,b) * (a',b') = (a*a',b*b')
  negate = lift2 negate negate
  abs    = lift2 abs abs
  signum = lift2 signum signum

instance (Fractional a, Fractional b) => Fractional (a,b) where
  fromRational x = (fromRational x, fromRational x)
  recip = lift2 recip recip

instance (Floating a, Floating b) => Floating (a,b) where
  pi    = (pi,pi)
  exp   = lift2 exp exp
  log   = lift2 log log
  sqrt  = lift2 sqrt sqrt
  sin   = lift2 sin sin
  cos   = lift2 cos cos
  sinh  = lift2 sinh sinh
  cosh  = lift2 cosh cosh
  asin  = lift2 asin asin
  acos  = lift2 acos acos
  atan  = lift2 atan atan
  asinh = lift2 asinh asinh
  acosh = lift2 acosh acosh
  atanh = lift2 atanh atanh

instance (Num a, Num b, Num c) => Num (a,b,c) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n)
  (a,b,c) + (a',b',c') = (a+a',b+b',c+c')
  (a,b,c) - (a',b',c') = (a-a',b-b',c-c')
  (a,b,c) * (a',b',c') = (a*a',b*b',c*c')
  negate = lift3 negate negate negate
  abs    = lift3 abs abs abs
  signum = lift3 signum signum signum

instance (Fractional a, Fractional b, Fractional c)
    => Fractional (a,b,c) where
  fromRational x = (fromRational x, fromRational x, fromRational x)
  recip = lift3 recip recip recip

lift3 :: (a->u) -> (b->v) -> (c->w)
      -> (a,b,c) -> (u,v,w)
lift3 fa fb fc (a,b,c) = (fa a, fb b, fc c)

instance (Floating a, Floating b, Floating c)
    => Floating (a,b,c) where
  pi    = (pi,pi,pi)
  exp   = lift3 exp exp exp
  log   = lift3 log log log
  sqrt  = lift3 sqrt sqrt sqrt
  sin   = lift3 sin sin sin
  cos   = lift3 cos cos cos
  sinh  = lift3 sinh sinh sinh
  cosh  = lift3 cosh cosh cosh
  asin  = lift3 asin asin asin
  acos  = lift3 acos acos acos
  atan  = lift3 atan atan atan
  asinh = lift3 asinh asinh asinh
  acosh = lift3 acosh acosh acosh
  atanh = lift3 atanh atanh atanh

lift4 :: (a->u) -> (b->v) -> (c->w) -> (d->x)
      -> (a,b,c,d) -> (u,v,w,x)
lift4 fa fb fc fd (a,b,c,d) = (fa a, fb b, fc c, fd d)

instance (Num a, Num b, Num c, Num d) => Num (a,b,c,d) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n)
  (a,b,c,d) + (a',b',c',d') = (a+a',b+b',c+c',d+d')
  (a,b,c,d) - (a',b',c',d') = (a-a',b-b',c-c',d-d')
  (a,b,c,d) * (a',b',c',d') = (a*a',b*b',c*c',d*d')
  negate = lift4 negate negate negate negate
  abs    = lift4 abs abs abs abs
  signum = lift4 signum signum signum signum

instance (Fractional a, Fractional b, Fractional c, Fractional d)
    => Fractional (a,b,c,d) where
  fromRational x = (fromRational x, fromRational x, fromRational x, fromRational x)
  recip = lift4 recip recip recip recip

instance (Floating a, Floating b, Floating c, Floating d)
    => Floating (a,b,c,d) where
  pi    = (pi,pi,pi,pi)
  exp   = lift4 exp exp exp exp
  log   = lift4 log log log log
  sqrt  = lift4 sqrt sqrt sqrt sqrt
  sin   = lift4 sin sin sin sin
  cos   = lift4 cos cos cos cos
  sinh  = lift4 sinh sinh sinh sinh
  cosh  = lift4 cosh cosh cosh cosh
  asin  = lift4 asin asin asin asin
  acos  = lift4 acos acos acos acos
  atan  = lift4 atan atan atan atan
  asinh = lift4 asinh asinh asinh asinh
  acosh = lift4 acosh acosh acosh acosh
  atanh = lift4 atanh atanh atanh atanh

lift5 :: (a->u) -> (b->v) -> (c->w) -> (d->x) -> (e->y)
      -> (a,b,c,d,e) -> (u,v,w,x,y)
lift5 fa fb fc fd fe (a,b,c,d,e) =
  (fa a, fb b, fc c, fd d, fe e)

instance (Num a, Num b, Num c, Num d, Num e) => Num (a,b,c,d,e) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
  (a,b,c,d,e) + (a',b',c',d',e') = (a+a',b+b',c+c',d+d',e+e')
  (a,b,c,d,e) - (a',b',c',d',e') = (a-a',b-b',c-c',d-d',e-e')
  (a,b,c,d,e) * (a',b',c',d',e') = (a*a',b*b',c*c',d*d',e*e')
  negate = lift5 negate negate negate negate negate
  abs    = lift5 abs abs abs abs abs
  signum = lift5 signum signum signum signum signum

instance (Fractional a, Fractional b, Fractional c, Fractional d, Fractional e)
    => Fractional (a,b,c,d,e) where
  fromRational x = (fromRational x, fromRational x, fromRational x, fromRational x, fromRational x)
  recip = lift5 recip recip recip recip recip

instance (Floating a, Floating b, Floating c, Floating d, Floating e)
    => Floating (a,b,c,d,e) where
  pi    = (pi,pi,pi,pi,pi)
  exp   = lift5 exp exp exp exp exp
  log   = lift5 log log log log log
  sqrt  = lift5 sqrt sqrt sqrt sqrt sqrt
  sin   = lift5 sin sin sin sin sin
  cos   = lift5 cos cos cos cos cos
  sinh  = lift5 sinh sinh sinh sinh sinh
  cosh  = lift5 cosh cosh cosh cosh cosh
  asin  = lift5 asin asin asin asin asin
  acos  = lift5 acos acos acos acos acos
  atan  = lift5 atan atan atan atan atan
  asinh = lift5 asinh asinh asinh asinh asinh
  acosh = lift5 acosh acosh acosh acosh acosh
  atanh = lift5 atanh atanh atanh atanh atanh

lift6 :: (a->u) -> (b->v) -> (c->w) -> (d->x) -> (e->y) -> (f->t)
      -> (a,b,c,d,e,f) -> (u,v,w,x,y,t)
lift6 fa fb fc fd fe ff (a,b,c,d,e,f) =
  (fa a, fb b, fc c, fd d, fe e, ff f)

instance (Num a, Num b, Num c, Num d, Num e, Num f) => Num (a,b,c,d,e,f) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
  (a,b,c,d,e,f) + (a',b',c',d',e',f') = (a+a',b+b',c+c',d+d',e+e',f+f')
  (a,b,c,d,e,f) - (a',b',c',d',e',f') = (a-a',b-b',c-c',d-d',e-e',f-f')
  (a,b,c,d,e,f) * (a',b',c',d',e',f') = (a*a',b*b',c*c',d*d',e*e',f*f')
  negate = lift6 negate negate negate negate negate negate
  abs    = lift6 abs abs abs abs abs abs
  signum = lift6 signum signum signum signum signum signum

instance (Fractional a, Fractional b, Fractional c, Fractional d, Fractional e, Fractional f)
    => Fractional (a,b,c,d,e,f) where
  fromRational x = (fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x)
  recip = lift6 recip recip recip recip recip recip

instance (Floating a, Floating b, Floating c, Floating d, Floating e, Floating f)
    => Floating (a,b,c,d,e,f) where
  pi    = (pi,pi,pi,pi,pi,pi)
  exp   = lift6 exp exp exp exp exp exp
  log   = lift6 log log log log log log
  sqrt  = lift6 sqrt sqrt sqrt sqrt sqrt sqrt
  sin   = lift6 sin sin sin sin sin sin
  cos   = lift6 cos cos cos cos cos cos
  sinh  = lift6 sinh sinh sinh sinh sinh sinh
  cosh  = lift6 cosh cosh cosh cosh cosh cosh
  asin  = lift6 asin asin asin asin asin asin
  acos  = lift6 acos acos acos acos acos acos
  atan  = lift6 atan atan atan atan atan atan
  asinh = lift6 asinh asinh asinh asinh asinh asinh
  acosh = lift6 acosh acosh acosh acosh acosh acosh
  atanh = lift6 atanh atanh atanh atanh atanh atanh

lift7 :: (a->u) -> (b->v) -> (c->w) -> (d->x) -> (e->y) -> (f->t) -> (g->z)
      -> (a,b,c,d,e,f,g) -> (u,v,w,x,y,t,z)
lift7 fa fb fc fd fe ff fg (a,b,c,d,e,f,g) =
  (fa a, fb b, fc c, fd d, fe e, ff f, fg g)

instance (Num a, Num b, Num c, Num d, Num e, Num f, Num g) => Num (a,b,c,d,e,f,g) where
  fromInteger n = (fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n, fromInteger n)
  (a,b,c,d,e,f,g) + (a',b',c',d',e',f',g') = (a+a',b+b',c+c',d+d',e+e',f+f',g+g')
  (a,b,c,d,e,f,g) - (a',b',c',d',e',f',g') = (a-a',b-b',c-c',d-d',e-e',f-f',g-g')
  (a,b,c,d,e,f,g) * (a',b',c',d',e',f',g') = (a*a',b*b',c*c',d*d',e*e',f*f',g*g')
  negate = lift7 negate negate negate negate negate negate negate
  abs    = lift7 abs abs abs abs abs abs abs
  signum = lift7 signum signum signum signum signum signum signum

instance (Fractional a, Fractional b, Fractional c, Fractional d, Fractional e, Fractional f, Fractional g)
    => Fractional (a,b,c,d,e,f,g) where
  fromRational x = (fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x, fromRational x)
  recip = lift7 recip recip recip recip recip recip recip

instance (Floating a, Floating b, Floating c, Floating d, Floating e, Floating f, Floating g)
    => Floating (a,b,c,d,e,f,g) where
  pi    = (pi,pi,pi,pi,pi,pi,pi)
  exp   = lift7 exp exp exp exp exp exp exp
  log   = lift7 log log log log log log log
  sqrt  = lift7 sqrt sqrt sqrt sqrt sqrt sqrt sqrt
  sin   = lift7 sin sin sin sin sin sin sin
  cos   = lift7 cos cos cos cos cos cos cos
  sinh  = lift7 sinh sinh sinh sinh sinh sinh sinh
  cosh  = lift7 cosh cosh cosh cosh cosh cosh cosh
  asin  = lift7 asin asin asin asin asin asin asin
  acos  = lift7 acos acos acos acos acos acos acos
  atan  = lift7 atan atan atan atan atan atan atan
  asinh = lift7 asinh asinh asinh asinh asinh asinh asinh
  acosh = lift7 acosh acosh acosh acosh acosh acosh acosh
  atanh = lift7 atanh atanh atanh atanh atanh atanh atanh

{--------------------------------------------------------------------
    Some experiments in Enum and Integral instances for tuples
--------------------------------------------------------------------}

{-

-- Integral needs Enum

instance (Enum a, Enum b, Bounded b) => Enum (a,b) where
  toEnum = noPair "toEnum"
  fromEnum = noPair "fromEnum"
  -- enumerate consistently with Ord
  enumFromTo (alo,blo) (ahi,bhi) =
       [ (alo,b) | b <- [blo .. maxBound] ]
    ++ [ (a  ,b) | a <- [succ alo .. pred ahi], b <- [minBound .. maxBound] ]
    ++ [ (ahi,b) | b <- [minBound .. bhi] ]
  -- To do: replace enumFromTo def with an enumFromThenTo def

-- deriving instance (Enum a, Enum b) => Enum (a,b)
--
--     The data constructors of `(,)' are not all in scope
--       so you cannot derive an instance for it

-- To do: toEnum and fromEnum

{-
-- Test:

data Foo = A | B | C | D deriving (Enum,Bounded,Show)

t1 :: [(Foo,Foo)]
t1 = [(B,C) .. (C,C)]
-}

instance (Real a, Real b) => Real (a,b) where
  toRational = noPair "toRational"


instance (Integral a, Integral b, Bounded b) => Integral (a,b) where
--   (a,b) `quotRem` (a',b') = ((qa,qb),(ra,rb))
--     where
--       (qa,ra) = a `quotRem` a'
--       (qb,rb) = b `quotRem` b'
  (a,b) `quotRem` (a',b') = transpose (a `quotRem` a' , b `quotRem` b')
    where
      transpose ((w,x),(y,z)) = ((w,y),(x,z)) -- to-do: pretty point-free
  toInteger (a , b::b) = toInteger a * widthB + toInteger b
   where
     widthB = toInteger (maxBound :: b) - toInteger (minBound :: b)

{-
t2 :: ((Int,Int),(Int,Int))
t2 = (7,8) `quotRem` (2,3)

t3 :: (Int,Int)
t3 = (7,8) `quot` (2,3)

t4 :: (Int,Int)
t4 = (7,8) `rem` (2,3)
-}

{-
  (a,b) `quot` (a',b') = (a `quot` a', b `quot` b')


  rem           = liftA2 rem
  div           = liftA2 div
  mod           = liftA2 mod
  toInteger     = noOv "toInteger"
  x `quotRem` y = (x `quot` y, x `rem` y)
  x `divMod`  y = (x `div`  y, x `mod` y)
-}

-}
