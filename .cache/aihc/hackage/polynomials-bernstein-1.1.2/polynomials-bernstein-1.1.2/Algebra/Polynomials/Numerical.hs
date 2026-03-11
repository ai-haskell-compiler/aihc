{-# CFILES cnumerical.c #-}
{-# OPTIONS -XUnboxedTuples -XMagicHash -XScopedTypeVariables -XBangPatterns -cpp -XTypeFamilies -XMultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}
-- | This module contains the definition of the main arithmetic tools
-- used in Metafont'.
module Algebra.Polynomials.Numerical(
  -- * Raw operations
  fromIntegral#,plus,minus,over,times,
  sqrt#,cos#,sin#,acos#,asin#,
  -- * The 'Interval' type
  Interval(..),Intervalize(..),
  interval,intersectsd, union,
  fpred,fsucc
  ) where


import Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Generic as GV
import Foreign.C.Types
foreign import ccall unsafe "c_succ" c_fsucc::CDouble->CDouble
foreign import ccall unsafe "c_pred" c_fpred::CDouble->CDouble

fsucc,fpred::Double->Double
fpred=realToFrac.c_fpred.realToFrac
fsucc=realToFrac.c_fsucc.realToFrac

{-# INLINE plus #-}
-- | Interval addition
plus::Double->Double->Double->Double->(# Double, Double #)
plus !a !b !c !d=
    let !x=a+c
        !y=b+d
    in
     (# fpred x,fsucc y #)
   
-- | Interval substraction
{-# INLINE minus #-}
minus::Double->Double->Double->Double->(# Double, Double #)
minus !a !b !c !d=
    let !x=a-d
        !y=b-c
    in
     (# fpred x, fsucc y #)
-- | Interval multiplication
{-# INLINE times #-}
times::Double->Double->Double->Double->(# Double, Double #)
times !a !b !c !d=
    let !w=a*c
        !x=a*d
        !y=b*c
        !z=b*d
      
        (# !aa,!bb #)=if w<x then (# w,x #) else (# x,w #)
        (# !cc,!dd #)=if y<z then (# y,z #) else (# z,y #)
        !m=min aa cc
        !m'=max bb dd
    in
     (# fpred m, fsucc m' #)

-- | Interval division
{-# INLINE over #-}
over::Double->Double->Double->Double->(# Double, Double #)
over !a !b !c !d=
    if c*d<=0 then 
      if a>0 then (# 1/0,1/0 #) else
        if b<0 then (# (-1/0), (-1/0) #) else
          (# 0/0, 0/0 #)
                         
    else
      let !w=a/c
          !x=a/d
          !y=b/c
          !z=b/d
      
          !(aa,bb)=if w<x then (w,x) else (x,w)
          !(cc,dd)=if y<z then (y,z) else (z,y)
          !m=min aa cc
          !m'=max bb dd
      in
       (# fpred m, fsucc m' #)

-- | Converts an 'Integral' value into an interval.
fromIntegral#::Integral x=>x->(# Double,Double #)
fromIntegral# n=
    let !n_=fromIntegral n in
    (# fpred n_,fsucc n_ #)

-- | Interval cosine
cos#::Double->Double->(# Double,Double #)
cos# !a !b=
  let (# !_m0,!_m0' #)=if cos a<=cos b then (# cos a, cos b #) else (# cos b, cos a #)
      !m0=fpred _m0
      !m0'=fsucc _m0'
      checkUp !(k::Int) !m !m'=
        let (# !ka,!kb #)=fromIntegral# k
            (# !ka0,!kb0 #)=times ka kb (fpred pi) (fsucc pi)
        in
         if ka0>b then (# m,m' #) else
           if kb0<a then
             checkUp (k+1) m m'
           else
             if k`mod`2==0 then
               checkUp (k+1) m 1
             else
               checkUp (k+1) (-1) m'
  in
   checkUp (floor $ fpred (a/pi)) m0 m0'
-- | Interval sine
sin# ::Double->Double->(# Double,Double #)
sin# !a !b=
  let (# _m0,_m0' #)
        | sin a<sin b = (# sin a, sin b #)
        | otherwise = (# sin b, sin a #)
      m0=max (-1) $ fpred _m0
      m0'=min 1 $ fsucc _m0'
      (# pa,pb #)=(# fpred pi, fsucc pi #)
      (# ka1,kb1 #)=over pa pb 2 2
      
      up (k::Int) !m !m'=
        let (# ka,kb #)=fromIntegral# k
            (# ka0,kb0 #)=times ka kb pa pb
            (# ka2,kb2 #)=plus ka0 kb0 ka1 kb1 -- kpi+pi/2
        in
         if ka2>b then
           (# m,m' #)
         else
           if kb2<a then
             up (k+1) m m'
           else
             if k`mod`2 == 0 then
               up (k+1) m 1
             else
               up (k+1) (-1) m'
  in
   up (floor $ a/pi) m0 m0'
  
      
sqrt#::Double->Double->(# Double,Double #)
sqrt# !a !b=
  let sa=sqrt a
      sb=sqrt b
      sa_=max 0 (fpred sa)
      sb_=fsucc sb
  in
   (# sa_, sb_ #)

acos#::Double->Double->(# Double,Double #)
acos# !a !b=
  let aca=acos $ max (-1) a
      acb=acos $ min 1 b
  in
   (# fpred (min aca acb), fsucc (max aca acb) #)

asin#::Double->Double->(# Double,Double #)
asin# !a !b=
  let aca=asin $ max (-1) a
      acb=asin $ min 1 b
  in
   (# fpred (min aca acb), fsucc (max aca acb) #)

-- | The interval type (most of its operations are calls to the raw functions)
data Interval=Interval {ilow::Double,iup::Double} deriving (Eq, Show)

instance Floating Interval where
  cos (Interval a b)=
    let (# c,d #)=cos# a b in
    Interval c d
  sin (Interval a b)=
    let (# c,d #)=sin# a b in
    Interval c d
  sqrt (Interval a b)=
    let (# a#,b# #)=sqrt# a b in
    Interval a# b#
  acos (Interval a b)=
    let (# a#,b# #)=acos# a b in
    Interval a# b#
  asin (Interval a b)=
    let (# a#,b# #)=asin# a b in
    Interval a# b#
  pi=Interval (fpred pi) (fsucc pi)
  
  
-- | Intersection of two 'Interval's.
{-# INLINE intersectsd #-}
intersectsd::Interval->Interval->Bool
intersectsd (Interval a b) (Interval c d) = b>=c && a<=d

-- | Union of two intersecting intervals (undefined behaviour if they do not intersect).
{-# INLINE union #-}
union::Interval->Interval->Interval
union (Interval a b) (Interval c d) = Interval (min a c) (max b d)

-- | Two common operations on types defined with intervals.
class Intervalize a where
  intervalize::a Double->a Interval
  intersects::a Interval->a Interval->Bool
 
-- | Converts an optimal IEEE-754 representation of a number into an
-- optimal interval containing this number.
interval::Double->Interval
interval x=Interval (fpred x) (fsucc x)

instance Num Interval where
  (+) (Interval a b) (Interval c d)=
    let (# a',b' #)=plus a b c d in
    Interval a' b'
  (-) (Interval a b) (Interval c d)=
    let (# a',b' #)=minus a b c d in
    Interval a' b'
  (*) (Interval a b) (Interval c d)=
    let (# a',b' #)=times a b c d in
    Interval a' b'
  abs x@(Interval a b)=
    if b<=0 then Interval (negate b) (negate a) else
      if a<=0 then
        Interval 0 (max b $ negate a)
      else
        x
        
  signum _=undefined
  
  fromInteger=interval.fromInteger
      
instance Fractional Interval where

  (/) (Interval a b) (Interval c d)=
    let (# a',b' #)=over a b c d in
    Interval a' b'

  fromRational r=
    let r'=fromRational r in
    Interval (fpred r') (fsucc r')


newtype instance UV.MVector s Interval = MV_Interval (UV.MVector s (Double,Double))
newtype instance UV.Vector Interval = V_Interval  (UV.Vector (Double,Double))
instance Unbox Interval

instance GMV.MVector UV.MVector Interval where
  basicInitialize (MV_Interval a)=GMV.basicInitialize a
  basicLength (MV_Interval a)=GMV.basicLength a
  basicUnsafeSlice a b (MV_Interval c)=MV_Interval $ GMV.basicUnsafeSlice a b c
  basicOverlaps (MV_Interval a) (MV_Interval b)=GMV.basicOverlaps a b
  basicUnsafeNew a=GMV.basicUnsafeNew a >>= return.MV_Interval
  basicUnsafeReplicate a (Interval b c)=GMV.basicUnsafeReplicate a (b,c)>>=return.MV_Interval
  basicUnsafeRead (MV_Interval a) b=GMV.basicUnsafeRead a b >>= (\(u,v)->return $ Interval u v)
  basicUnsafeWrite (MV_Interval a) b (Interval c d)=GMV.basicUnsafeWrite a b (c,d)
  basicClear (MV_Interval a)=GMV.basicClear a
  basicSet (MV_Interval a) (Interval b c)=GMV.basicSet a (b,c)
  basicUnsafeCopy (MV_Interval a) (MV_Interval b)=GMV.basicUnsafeCopy a b
  basicUnsafeGrow (MV_Interval a) b=GMV.basicUnsafeGrow a b >>= return.MV_Interval

instance GV.Vector UV.Vector Interval where
  basicUnsafeFreeze (MV_Interval a)=GV.basicUnsafeFreeze a >>= return.V_Interval
  basicUnsafeThaw (V_Interval a)=GV.basicUnsafeThaw a >>= return.MV_Interval
  basicLength (V_Interval a)=GV.basicLength a
  basicUnsafeSlice a b (V_Interval c)=V_Interval (GV.basicUnsafeSlice a b c)
  basicUnsafeIndexM (V_Interval a) b=GV.basicUnsafeIndexM a b >>= (\(u,v)->return $ Interval u v)

(!#)::UV.Vector Interval->Int->(# Double,Double #)
(!#) a b=
  let Interval u v=a!b in (# u,v #)

