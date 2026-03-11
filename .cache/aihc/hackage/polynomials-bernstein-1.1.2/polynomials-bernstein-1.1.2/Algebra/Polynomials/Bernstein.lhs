\begin{code}
{-# OPTIONS -XUnboxedTuples -XScopedTypeVariables -XFlexibleContexts -XBangPatterns -XUndecidableInstances -XMultiParamTypeClasses -XFunctionalDependencies -XFlexibleInstances -XMagicHash -XTypeFamilies #-}
-- | Various functions for manipulating polynomials, essentially when
-- represented in the Bernstein basis, in one or two variables.
module Algebra.Polynomials.Bernstein (Bernsteinp(..),solve,Bernstein(..),
                                      derivate,reorient) where


import Control.Monad.ST
import Algebra.Polynomials.Numerical

import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV
import qualified Data.Vector as V
import Data.Vector.Generic as GV hiding ((++))
  
-- | The type for Bernstein polynomials with an arbitrary number of variables
data Bernsteinp a b=Bernsteinp { bounds::a, coefs::UV.Vector b } deriving (Eq,Show)

type family Param a b
type instance Param Int a=a
type instance Param (Int,Int) a=(a,a)
type instance Param (Int,Int,Int) a=(a,a,a)
type instance Param (Int,Int,Int,Int) a=(a,a,a,a)

type family Inter a b
type instance Inter Int a=(a,a)
type instance Inter (Int,Int) a=(a,a,a,a)
type instance Inter (Int,Int,Int) a=(a,a,a,a,a,a)
type instance Inter (Int,Int,Int,Int) a=(a,a,a,a,a,a,a,a)


class Bernstein a where
  -- Constructeurs
  (?)::UV.Unbox b=>Bernsteinp a b->a->b
  constant::(UV.Unbox b,Num b, Fractional b)=>b->Bernsteinp a b
  scale::(Num b, Fractional b,UV.Unbox b)=>b->Bernsteinp a b->Bernsteinp a b
  scale a (Bernsteinp i b)=Bernsteinp i $ UV.map (*a) b
  promote::(UV.Unbox b,Num b, Fractional b)=>Int->Bernsteinp Int b->Bernsteinp a b
  elevate::(UV.Unbox b,Num b, Fractional b)=>a->Bernsteinp a b->Bernsteinp a b
  eval::(UV.Unbox b,Num b,Fractional b)=>Bernsteinp a b->Param a b->b
  restriction::(UV.Unbox b,Fractional b,Num b)=>Bernsteinp a b->Param a b->Param a b->Bernsteinp a b


instance Num (Bernsteinp a Interval)=>Intervalize (Bernsteinp a) where
  intervalize (Bernsteinp i x)=Bernsteinp i $! UV.map interval x
  intersects bxf bxg=
    let (Bernsteinp _ xx)=bxf-bxg in
    UV.all (\(Interval a b)->a<=0 && b>=0) xx

binomials::(Num a, MUV.Unbox a)=>Int->UV.Vector a
binomials m=
  UV.create $ do
    v<-MUV.replicate ((m+1)*(m+1)) 0
    MUV.write v 0 1
    let fill i
          | i>=(m+1)*(m+1) = return v
          | i`mod`(m+1) == 0 = do
            MUV.write v i 1
            fill (i+1)
          | otherwise = do
            a<-MUV.read v (i-m-1)
            b<-MUV.read v (i-m-2)
            MUV.write v i (a+b)
            fill (i+1)
    fill (m+1)

  
instance Bernstein Int where
  (?) (Bernsteinp _ a) b=a!b
  constant x=Bernsteinp 1 $ GV.singleton x
  promote _=id
  elevate r (Bernsteinp n f)=
    if r<=0 then Bernsteinp n f else
      let coef j=
            let sumAll i result
                  | (i>j) || (i>=n) = result
                  | otherwise =
                    sumAll (i+1) $ result+(f!i)*((bin i (n-1))*((bin (j-i) r))/(bin j (n+r-1)))
            in
             (sumAll 0 0)
          binomial=binomials $ n+r-1
          bin a b=binomial!(b*(n+r)+a)
      in
       Bernsteinp (n+r) $ UV.generate (n+r) coef
  eval (Bernsteinp n points) t=
    if n==0 then 0 else runST $ do
      arr<-thaw points
      let fill i s
            | s>=n = MUV.read arr 0
            | i>=n-s = fill 0 (s+1)
            | otherwise = do
              a<-MUV.read arr i
              b<-MUV.read arr (i+1)
              MUV.write arr i $ a*(1-t)+b*t
              fill (i+1) s
      fill 0 1
      
  restriction (Bernsteinp n0 points) a b=
    runST $ do
      pf<-thaw points
      let casteljau bef aft nv u v i k s j
            | i>=bef = return ()
            | k>=aft= casteljau bef aft nv u v (i+1) 0 1 0
            | s>=nv = casteljau bef aft nv u v i (k+1) 1 0
            | j>=nv = casteljau bef aft nv u v i k (s+1) 0
            | otherwise=
              let idx i_ j_ k_=(i_*nv+j_)*aft + k_
                  v'=(v-u)/(1-u)
              in
               if s+j<nv then do
                 pfi0<-MUV.read pf (idx i j k)
                 pfi1<-MUV.read pf (idx i (j+1) k)
                 MUV.write pf (idx i j k) $ (1-u)*pfi0+u*pfi1
                 casteljau bef aft nv u v i k s (j+1)
               else do
                 pfi0<-MUV.read pf (idx i (j-1) k)
                 pfi1<-MUV.read pf (idx i j k)
                 MUV.write pf (idx i j k) $ (1-v')*pfi0+v'*pfi1
                 casteljau bef aft nv u v i k s (j+1)
      casteljau 1 1 n0 a b 0 0 1 0
      pff<-unsafeFreeze pf
      return $ Bernsteinp n0 pff
    

instance Bernstein (Int,Int) where
  (?) (Bernsteinp (_,b) c) (i,j)=c!(i*b+j)
  constant x=Bernsteinp (1,1) $ UV.singleton x
  
  promote 1 (Bernsteinp i x)=Bernsteinp (i,1) x
  promote _ (Bernsteinp i x)=Bernsteinp (1,i) x

  elevate (ra_,rb_) (Bernsteinp (a,b) f)=
    let ra
          | ra_>0 = ra_
          | otherwise = 0
        rb
          | rb_>0 = rb_
          | otherwise = 0
    in
     if a<=0 || b<=0 then Bernsteinp (a+ra,b+rb) $ GV.replicate ((a+ra)*(b+rb)) 0 else
       let idx i j=(i*b)+j
           idx' i j=(i*(b+rb))+j
           vect=create $ do
             v<-MUV.new ((a+ra)*(b+rb))
             let coef i j
                   | i>=(a+ra) = return v
                   | j>=(b+rb) = coef (i+1) 0
                   | otherwise=do
                     let sumAll i' j' !result
                           | i'>=a || i'>i = result
                           | j'>=b || j'>j = sumAll (i'+1) 0 result
                           | otherwise =
                             let x0=(bin i' (a-1))*((bin (i-i') ra)/(bin i (a+ra-1)))
                                 x1=(bin j' (b-1))*((bin (j-j') rb)/(bin j (b+rb-1)))
                             in
                              sumAll i' (j'+1) $! result+x0*x1*(f!(idx i' j'))
                     MUV.write v (idx' i j) $ sumAll 0 0 0
                     coef i (j+1)
             coef 0 0

           m=max (a+ra-1) (b+rb-1)
           bin i j=binomial!(j*(m+1)+i)
           binomial=binomials m
       in
        Bernsteinp (a+ra,b+rb) vect
  eval (Bernsteinp (n0,n1) points) (a,b)=
    if n0<=0 || n1<=0 then 0 else
    runST $ do
      pf<-thaw points
      let casteljau p0 p1 u i j s
            | i>=p0 = return ()
            | s>=p1 = casteljau p0 p1 u (i+1) 0 1
            | j>=(p1-s) = casteljau p0 p1 u i 0 (s+1)
            | otherwise = do
              x0<-MUV.read pf $ i+p0*j
              x1<-MUV.read pf $ i+p0*(j+1)
              MUV.write pf (i+p0*j) $ (1-u)*x0+u*x1
              casteljau p0 p1 u i (j+1) s
      casteljau n1 n0 a 0 0 1
      casteljau 1 n1 b 0 0 1
      MUV.read pf 0

  restriction (Bernsteinp (n0,n1) points) (a,c) (b,d)=
    runST $ do
      pf<-thaw points
      let casteljau bef aft nv u v i k s j
            | i>=bef = return ()
            | k>=aft= casteljau bef aft nv u v (i+1) 0 1 0
            | s>=nv = casteljau bef aft nv u v i (k+1) 1 0
            | j>=nv = casteljau bef aft nv u v i k (s+1) 0
            | otherwise=
              let idx i_ j_ k_=(i_*nv+j_)*aft + k_
                  v'=(v-u)/(1-u)
              in
               if s+j<nv then do
                 pfi0<-MUV.read pf (idx i j k)
                 pfi1<-MUV.read pf (idx i (j+1) k)
                 MUV.write pf (idx i j k) $ (1-u)*pfi0+u*pfi1
                 casteljau bef aft nv u v i k s (j+1)
               else do
                 pfi0<-MUV.read pf (idx i (j-1) k)
                 pfi1<-MUV.read pf (idx i j k)
                 MUV.write pf (idx i j k) $ (1-v')*pfi0+v'*pfi1
                 casteljau bef aft nv u v i k s (j+1)
      casteljau 1 n1 n0 a b 0 0 1 0
      casteljau n0 1 n1 c d 0 0 1 0
      pff<-unsafeFreeze pf
      return $ Bernsteinp (n0,n1) pff

instance Bernstein (Int,Int,Int) where
  
  (?) (Bernsteinp (_,b,c) d) (i,j,k)=d!(((i*b+j)*c)+k)
  constant x=Bernsteinp (1,1,1) $ UV.singleton x
  
  promote 1 (Bernsteinp i x)=Bernsteinp (i,1,1) x
  promote 2 (Bernsteinp i x)=Bernsteinp (1,i,1) x
  promote _ (Bernsteinp i x)=Bernsteinp (1,1,i) x

  elevate (ra_,rb_,rc_) (Bernsteinp (a,b,c) f)=
    let ra
          | ra_>0 = ra_
          | otherwise = 0
        rb
          | rb_>0 = rb_
          | otherwise = 0
        rc
          | rc_>0 = rc_
          | otherwise = 0
    in
     if a<=0 || b<=0 || c<=0 then 
       Bernsteinp (a+ra,b+rb,c+rc) $ GV.replicate ((a+ra)*(b+rb)*(c+rc)) 0
     else
       let idx i j k=((i*b)+j)*c+k
           idx' i j k=((i*(b+rb))+j)*(c+rc)+k
           vect=create $ do
             v<-MUV.new ((a+ra)*(b+rb)*(c+rc))
             let coef i j k
                   | i>=a+ra = return v
                   | j>=b+rb = coef (i+1) 0 0
                   | k>=c+rc = coef i (j+1) 0
                   | otherwise=do
                     let sumAll i' j' k' !result
                           | i'>=a || i'>i = result
                           | j'>=b || j'>j = sumAll (i'+1) 0 0 result
                           | k'>=c || k'>k = sumAll i' (j'+1) 0 result
                           | otherwise =
                             let x0=(bin i' (a-1))*((bin (i-i') ra)/(bin i (a+ra-1)))
                                 x1=(bin j' (b-1))*((bin (j-j') rb)/(bin j (b+rb-1)))
                                 x2=(bin k' (c-1))*((bin (k-k') rc)/(bin k (c+rc-1)))
                             in
                              sumAll i' j' (k'+1) $! result+x0*x1*x2*(f!(idx i' j' k'))
                     MUV.write v (idx' i j k) $ sumAll 0 0 0 0
                     coef i j (k+1)
             coef 0 0 0

           m=max (max (a+ra-1) (b+rb-1)) (c+rc-1)
           bin i j=binomial!(j*(m+1)+i)
           binomial=binomials m
       in
        Bernsteinp (a+ra,b+rb,c+rc) vect
  eval (Bernsteinp (n0,n1,n2) points) (a,b,c)=
    if n0<=0 || n1<=0 || n2<=0 then 0 else
    runST $ do
      pf<-thaw points
      let casteljau p0 p1 u i j s
            | i>=p0 = return ()
            | s>=p1 = casteljau p0 p1 u (i+1) 0 1
            | j>=(p1-s) = casteljau p0 p1 u i 0 (s+1)
            | otherwise = do
              x0<-MUV.read pf $ i+p0*j
              x1<-MUV.read pf $ i+p0*(j+1)
              MUV.write pf (i+p0*j) $ (1-u)*x0+u*x1
              casteljau p0 p1 u i (j+1) s
      casteljau (n1*n2) n0 a 0 0 1
      casteljau n2 n1 b 0 0 1
      casteljau 1 n2 c 0 0 1
      MUV.read pf 0
  restriction (Bernsteinp (n0,n1,n2) points) (a,c,e) (b,d,f)=
    runST $ do
      pf<-thaw points
      let casteljau bef aft nv u v i k s j
            | i>=bef = return ()
            | k>=aft= casteljau bef aft nv u v (i+1) 0 1 0
            | s>=nv = casteljau bef aft nv u v i (k+1) 1 0
            | j>=nv = casteljau bef aft nv u v i k (s+1) 0
            | otherwise=
              let idx i_ j_ k_=(i_*nv+j_)*aft + k_
                  v'=(v-u)/(1-u)
              in
               if s+j<nv then do
                 pfi0<-MUV.read pf (idx i j k)
                 pfi1<-MUV.read pf (idx i (j+1) k)
                 MUV.write pf (idx i j k) $ (1-u)*pfi0+u*pfi1
                 casteljau bef aft nv u v i k s (j+1)
               else do
                 pfi0<-MUV.read pf (idx i (j-1) k)
                 pfi1<-MUV.read pf (idx i j k)
                 MUV.write pf (idx i j k) $ (1-v')*pfi0+v'*pfi1
                 casteljau bef aft nv u v i k s (j+1)
      casteljau 1 (n1*n2) n0 a b 0 0 1 0
      casteljau n0 n2 n1 c d 0 0 1 0
      casteljau (n0*n1) 1 n2 e f 0 0 1 0
      pff<-unsafeFreeze pf
      return $ Bernsteinp (n0,n1,n2) pff

instance Bernstein (Int,Int,Int,Int) where
  
  (?) (Bernsteinp (_,b,c,d) e) (i,j,k,l)=e!((((i*b+j)*c)+k)*d+l)
  constant x=Bernsteinp (1,1,1,1) $ UV.singleton x
  
  promote 1 (Bernsteinp i x)=Bernsteinp (i,1,1,1) x
  promote 2 (Bernsteinp i x)=Bernsteinp (1,i,1,1) x
  promote 3 (Bernsteinp i x)=Bernsteinp (1,1,i,1) x
  promote _ (Bernsteinp i x)=Bernsteinp (1,1,1,i) x

  elevate (ra_,rb_,rc_,rd_) (Bernsteinp (a,b,c,d) f)=
  
    let ra
          | ra_>0 = ra_
          | otherwise = 0
        rb
          | rb_>0 = rb_
          | otherwise = 0
        rc
          | rc_>0 = rc_
          | otherwise = 0
        rd
          | rd_>0 = rd_
          | otherwise = 0
    in        
     if a<=0 || b<=0 || c<=0 || d<=0 then 
       Bernsteinp (a+ra,b+rb,c+rc,d+rd) $ GV.replicate ((a+ra)*(b+rb)*(c+rc)*(d+rd)) 0
     else
       let idx i j k l=(((i*b)+j)*c+k)*d+l
           idx' i j k l=(((i*(b+rb))+j)*(c+rc)+k)*(d+rd)+l
           vect=create $ do
             v<-MUV.new ((a+ra)*(b+rb)*(c+rc)*(d+rd))
             let coef i j k l
                   | i>=a+ra = return v
                   | j>=b+rb = coef (i+1) 0 0 0
                   | k>=c+rc = coef i (j+1) 0 0
                   | l>=d+rd = coef i j (k+1) 0
                   | otherwise=do
                     let sumAll i' j' k' l' !result
                           | i'>=a || i'>i = result
                           | j'>=b || j'>j = sumAll (i'+1) 0 0 0 result
                           | k'>=c || k'>k = sumAll i' (j'+1) 0 0 result
                           | l'>=d || l'>l = sumAll i' j' (k'+1) 0 result
                           | otherwise =
                             let x0=(bin i' (a-1))*((bin (i-i') ra)/(bin i (a+ra-1)))
                                 x1=(bin j' (b-1))*((bin (j-j') rb)/(bin j (b+rb-1)))
                                 x2=(bin k' (c-1))*((bin (k-k') rc)/(bin k (c+rc-1)))
                                 x3=(bin l' (d-1))*((bin (l-l') rd)/(bin l (d+rd-1)))
                             in
                              sumAll i' j' k' (l'+1) $! result+x0*x1*x2*x3*(f!(idx i' j' k' l'))
                     MUV.write v (idx' i j k l) $ sumAll 0 0 0 0 0
                     coef i j k (l+1)
             coef 0 0 0 0

           m=max (max (a+ra) (b+rb)) (max (c+rc) (d+rd))
           bin i j=binomial!(j*(m+1)+i)
           binomial=binomials m
       in
        Bernsteinp (a+ra,b+rb,c+rc,d+rd) vect
     
  eval (Bernsteinp (n0,n1,n2,n3) points) (a,b,c,d)=
    if n0<=0 || n1<=0 || n2<=0 || n3<=0 then 0 else
    runST $ do
      pf<-thaw points
      let casteljau p0 p1 u i j s
            | i>=p0 = return ()
            | s>=p1 = casteljau p0 p1 u (i+1) 0 1
            | j>=(p1-s) = casteljau p0 p1 u i 0 (s+1)
            | otherwise = do
              x0<-MUV.read pf $ i+p0*j
              x1<-MUV.read pf $ i+p0*(j+1)
              MUV.write pf (i+p0*j) $ (1-u)*x0+u*x1
              casteljau p0 p1 u i (j+1) s
      casteljau (n1*n2*n3) n0 a 0 0 1
      casteljau (n2*n3) n1 b 0 0 1
      casteljau n3 n2 c 0 0 1
      casteljau 1 n3 d 0 0 1
      MUV.read pf 0
      
  restriction (Bernsteinp (n0,n1,n2,n3) points) (a,c,e,g) (b,d,f,h)=
    runST $ do
      pf<-thaw points
      let casteljau bef aft nv u v i k s j
            | i>=bef = return ()
            | k>=aft= casteljau bef aft nv u v (i+1) 0 1 0
            | s>=nv = casteljau bef aft nv u v i (k+1) 1 0
            | j>=nv = casteljau bef aft nv u v i k (s+1) 0
            | otherwise=
              let idx i_ j_ k_=(i_*nv+j_)*aft + k_
                  v'=(v-u)/(1-u)
              in
               if s+j<nv then do
                 pfi0<-MUV.read pf (idx i j k)
                 pfi1<-MUV.read pf (idx i (j+1) k)
                 MUV.write pf (idx i j k) $ (1-u)*pfi0+u*pfi1
                 casteljau bef aft nv u v i k s (j+1)
               else do
                 pfi0<-MUV.read pf (idx i (j-1) k)
                 pfi1<-MUV.read pf (idx i j k)
                 MUV.write pf (idx i j k) $ (1-v')*pfi0+v'*pfi1
                 casteljau bef aft nv u v i k s (j+1)
      casteljau 1 (n1*n2*n3) n0 a b 0 0 1 0
      casteljau n0 (n2*n3) n1 c d 0 0 1 0
      casteljau (n0*n1) n3 n2 e f 0 0 1 0
      casteljau (n0*n1*n2) 1 n3 g h 0 0 1 0
      pff<-unsafeFreeze pf
      return $ Bernsteinp (n0,n1,n2,n3) pff
        

instance (Num a,Fractional a,MUV.Unbox a)=>Num (Bernsteinp Int a) where

  (+) bf@(Bernsteinp m _) bg@(Bernsteinp n _)=
    let (Bernsteinp m' f')=elevate (n-m) bf
        (Bernsteinp _ g')=elevate (m-n) bg
    in
     Bernsteinp m' $ UV.generate m' $ \i->f'!i+g'!i



  (*) ff@(Bernsteinp (af) _) gg@(Bernsteinp (ag) _)=
    if af<=0 || ag<=0 then
      Bernsteinp 0 UV.empty
    else
    let mm=(af+ag)-2
        binomial=binomials mm
        bin a b=binomial!(b*(mm+1)+a)
    in
     Bernsteinp (af+ag-1) $ create $ do
       v<-MUV.new $ af+ag-1
       let fill i
             | i>=af+ag-1 = return v
             | otherwise = do
               let mCoef' i' result
                     | i'>i || i'>=af = 
                       result
                     | otherwise =
                       let a=((bin i' (af-1))*(bin (i-i') (ag-1)))/(bin i (af+ag-2)) in
                       mCoef' (i'+1) $
                       result+a*(ff?i')*(gg?(i-i'))
               
               MUV.write v i $! mCoef' (max 0 (i-ag+1)) 0
               fill (i+1)
       fill 0

  (-) bf (Bernsteinp i g)= bf+(Bernsteinp i $ UV.map negate g)

  signum _=error "No signum operation for Bernstein1"
  abs _=error "No abs operation for Bernstein1"

  fromInteger x=Bernsteinp 1 $ UV.singleton $ fromIntegral x
      
instance (Fractional a, Num a,UV.Unbox a)=>Num (Bernsteinp (Int,Int) a) where

  (+) bff@(Bernsteinp (af,bf) _) bgg@(Bernsteinp (ag,bg) _)=
    let (Bernsteinp (a,b) f')=elevate (ag-af,bg-bf) bff
        (Bernsteinp _ g')=elevate (af-ag,bf-bg) bgg
    in
     Bernsteinp (a,b) $ UV.generate (a*b) $ \i->f'!i+g'!i


  (*) ff@(Bernsteinp (af,bf) _) gg@(Bernsteinp (ag,bg) _)=
    if af<=0 || bf<=0 || ag<=0 || bg<=0 then
      Bernsteinp (0,0) UV.empty
    else
    let mm=max (af+ag) (bf+bg)-2
        binomial=binomials mm
        bin a b=binomial!(b*(mm+1)+a)
    in
     Bernsteinp (af+ag-1,bf+bg-1) $ create $ do
       v<-MUV.new $ (af+ag-1)*(bf+bg-1)
       let idx i j=i*(bf+bg-1)+j
           fill i j
             | i>=af+ag-1 = return v
             | j>=bf+bg-1 = fill (i+1) 0
             | otherwise =
               do
               let mCoef' i' j' result
                     | i'>i || i'>=af = 
                       let b=(bin i (af+ag-2))*(bin j (bf+bg-2)) in
                       result/b
                     | j'>j || j'>=bf = mCoef' (i'+1) (max 0 (j-bg+1)) result
                     | otherwise =
                       let a=(bin i' (af-1))*(bin (i-i') (ag-1))*
                             (bin j' (bf-1))*(bin (j-j') (bg-1))
                       in
                        mCoef' i' (j'+1) $
                        result+a*(ff?(i',j'))*(gg?(i-i',j-j'))
               
               MUV.write v (idx i j) $!
                 mCoef' (max 0 (i-ag+1)) (max 0 (j-bg+1)) 0
               fill i (j+1)
       fill 0 0
       
  (-) bf bg=bf+(bg { coefs=UV.map negate $ coefs bg })

  signum _=error "No signum operation for Bernstein1"
  abs _=error "No abs operation for Bernstein1"

  fromInteger x=Bernsteinp (1,1) $ UV.singleton $ fromIntegral x
instance (Fractional a, Num a, UV.Unbox a)=>Num (Bernsteinp (Int,Int,Int) a) where

  (+) bff@(Bernsteinp (af,bf,cf) _) bgg@(Bernsteinp (ag,bg,cg) _)=
    let (Bernsteinp (a,b,c) f')=elevate (ag-af,bg-bf,cg-cf) bff
        (Bernsteinp _ g')=elevate (af-ag,bf-bg,cf-cg) bgg
    in
     Bernsteinp (a,b,c) $ UV.generate (a*b*c) $ \i->f'!i+g'!i

  (*) ff@(Bernsteinp (af,bf,cf) _) gg@(Bernsteinp (ag,bg,cg) _)=
    if af<=0 || bf<=0 || cf<=0 || ag<=0 || bg<=0 || cg<=0 then
      Bernsteinp (0,0,0) UV.empty
    else
    let mm=(max (max (af+ag) (bf+bg)) (cf+cg))-2
        binomial=binomials mm
        bin a b=binomial!(b*(mm+1)+a)
    in
     Bernsteinp (af+ag-1,bf+bg-1,cf+cg-1) $ create $ do
       v<-MUV.new $ (af+ag-1)*(bf+bg-1)*(cf+cg-1)
       let idx i j k=(i*(bf+bg-1)+j)*(cf+cg-1)+k
           fill i j k
             | i>=af+ag-1 = return v
             | j>=bf+bg-1 = fill (i+1) 0 0
             | k>=cf+cg-1 = fill i (j+1) 0
             | otherwise =
               do
               let mCoef' i' j' k' result
                     | i'>i || i'>=af = 
                       let b=(bin i (af+ag-2))*(bin j (bf+bg-2))*
                             (bin k (cf+cg-2))
                       in
                        result/b
                     | j'>j || j'>=bf = mCoef' (i'+1) (max 0 (j-bg+1)) (max 0 (k-cg+1)) result
                     | k'>k || k'>=cf = mCoef' i' (j'+1) (max 0 (k-cg+1)) result
                     | otherwise =
                       let a=(bin i' (af-1))*(bin (i-i') (ag-1))*
                             (bin j' (bf-1))*(bin (j-j') (bg-1))*
                             (bin k' (cf-1))*(bin (k-k') (cg-1))
                       in
                        mCoef' i' j' (k'+1) $
                        result+a*(ff?(i',j',k'))*(gg?(i-i',j-j',k-k'))
               
               MUV.write v (idx i j k) $!
                 mCoef' (max 0 (i-ag+1)) (max 0 (j-bg+1)) (max 0 (k-cg+1)) 0
               fill i j (k+1)
       fill 0 0 0

  (-) bf bg=bf+(bg { coefs=UV.map negate $ coefs bg })

  signum _=error "No signum operation for Bernstein1"
  abs _=error "No abs operation for Bernstein1"

  fromInteger x=Bernsteinp (1,1,1) $ UV.singleton $ fromIntegral x
instance (Fractional a, Num a,UV.Unbox a)=>Num (Bernsteinp (Int,Int,Int,Int) a) where

  (+) bff@(Bernsteinp (af,bf,cf,df) _) bgg@(Bernsteinp (ag,bg,cg,dg) _)=
    let (Bernsteinp (a,b,c,d) f')=elevate (ag-af,bg-bf,cg-cf,dg-df) bff
        (Bernsteinp _ g')=elevate (af-ag,bf-bg,cf-cg,df-dg) bgg
    in
     Bernsteinp (a,b,c,d) $ UV.generate (a*b*c*d) $ \i->f'!i+g'!i

  (*) ff@(Bernsteinp (af,bf,cf,df) _) gg@(Bernsteinp (ag,bg,cg,dg) _)=
    if af<=0 || bf<=0 || cf<=0 || df<=0 || ag<=0 || bg<=0 || cg<=0 || dg<=0 then
      Bernsteinp (0,0,0,0) UV.empty
    else
    let mm=(max (max (af+ag) (bf+bg)) (max (cf+cg) (df+dg)))-2
        binomial=binomials mm
        bin a b=binomial!(b*(mm+1)+a)
    in
     Bernsteinp (af+ag-1,bf+bg-1,cf+cg-1,df+dg-1) $ create $ do
       v<-MUV.new $ (af+ag-1)*(bf+bg-1)*(cf+cg-1)*(df+dg-1)
       let idx i j k l=((i*(bf+bg-1)+j)*(cf+cg-1)+k)*(df+dg-1)+l
           fill i j k l
             | i>=af+ag-1 = return v
             | j>=bf+bg-1 = fill (i+1) 0 0 0
             | k>=cf+cg-1 = fill i (j+1) 0 0
             | l>=df+dg-1 = fill i j (k+1) 0
             | otherwise =
               do
               let mCoef' i' j' k' l' result
                     | i'>i || i'>=af = 
                       let b=(bin i (af+ag-2))*(bin j (bf+bg-2))*
                             (bin k (cf+cg-2))*(bin l (df+dg-2))
                       in
                        result/b
                     | j'>j || j'>=bf = mCoef' (i'+1) (max 0 (j-bg+1)) (max 0 (k-cg+1)) (max 0 (l-dg+1)) result
                     | k'>k || k'>=cf = mCoef' i' (j'+1) (max 0 (k-cg+1)) (max 0 (l-dg+1)) result
                     | l'>l || l'>=df = mCoef' i' j' (k'+1) (max 0 (l-dg+1)) result
                     | otherwise =
                       let a=(bin i' (af-1))*(bin (i-i') (ag-1))*
                             (bin j' (bf-1))*(bin (j-j') (bg-1))*
                             (bin k' (cf-1))*(bin (k-k') (cg-1))*
                             (bin l' (df-1))*(bin (l-l') (dg-1))
                       in
                        mCoef' i' j' k' (l'+1) $
                        result+a*(ff?(i',j',k',l'))*(gg?(i-i',j-j',k-k',l-l'))
               
               MUV.write v (idx i j k l) $!
                 mCoef' (max 0 (i-ag+1)) (max 0 (j-bg+1)) (max 0 (k-cg+1)) (max 0 (l-dg+1)) 0
               fill i j k (l+1)
       fill 0 0 0 0
  (-) bf bg=bf+(bg { coefs=UV.map negate $ coefs bg })

  signum _=error "No signum operation for Bernstein1"
  abs _=error "No abs operation for Bernstein1"

  fromInteger x=Bernsteinp (1,1,1,1) $ UV.singleton $ fromIntegral x

-- | Computes the derivative of a univariate Bernstein polynomial.
derivate::(UV.Unbox a,Num a)=>Bernsteinp Int a->Bernsteinp Int a
derivate (Bernsteinp n f)
  | n<=1 = Bernsteinp 0 $ UV.empty
  | otherwise=Bernsteinp (n-1) $ UV.generate (n-1) (\i->(f!(i+1)-f!i)*(fromIntegral $ n-1))

-- | Computes @f(1-x)@ (useful when used with Bezier curves).
reorient::(UV.Unbox a)=>Bernsteinp Int a->Bernsteinp Int a
reorient (Bernsteinp n f)=Bernsteinp n (UV.reverse f)

\end{code}

\begin{code}
{-
restrict::Int->Int->Int->Bernsteinp i Interval->Double->Double->Bernsteinp i Interval
restrict bef aft nv (Bernsteinp ix poly) a b=
  --traceShow "Restrict" $   
  runST $ do
    poly'<-thaw poly :: ST s (MUV.STVector s Interval)
    casteljau poly' 0 0 1 0
    unsafeFreeze poly' >>= return.(Bernsteinp ix)
  
  where
    
    
    (# bl,bu #)=
      let (# b0,b1 #)=minus b b a a
          (# b2,b3 #)=minus 1 1 a a
      in
       over b0 b1 b2 b3

    idx i j k= --traceShow (i,j,k,d) $
      (i*nv+j)*aft + k
      
    casteljau::MUV.STVector s Interval->
               Int->Int->Int->Int->ST s ()
                                  
    casteljau pf i k s j
      | i>=bef = return ()
      | k>=aft= casteljau pf (i+1) 0 1 0
      | s>=nv = casteljau pf i (k+1) 1 0
      | j>=nv = casteljau pf i k (s+1) 0
        
    -- Au boulot
      | s+j<nv = do
        (Interval l1 u1)<-MUV.read pf (idx i j k)
        (Interval l3 u3)<-MUV.read pf (idx i (j+1) k)
        let (# l0,u0 #)=minus 1 1 a a
            (# l2,u2 #)=times l0 u0 l1 u1
            (# l4,u4 #)=times a a l3 u3
            (# l5,u5 #)=plus l2 u2 l4 u4
        MUV.write pf (idx i j k) (Interval l5 u5)
        casteljau pf i k s (j+1)
                  
      | otherwise = do
        (Interval l1 u1)<-MUV.read pf (idx i (j-1) k)
        (Interval l3 u3)<-MUV.read pf (idx i j k)
        let (# l0,u0 #)=minus 1 1 bl bu
            (# l2,u2 #)=times l0 u0 l1 u1
            (# l4,u4 #)=times bl bu l3 u3
            (# l5,u5 #)=plus l2 u2 l4 u4
        MUV.write pf (idx i j k) (Interval l5 u5)
        casteljau pf i k s (j+1)
-}
{-
restrict::Int->Int->Int->Bernsteinp i a->a->a->Bernsteinp i a
restrict bef aft nv (Bernsteinp ix poly) a b=
  --traceShow "Restrict" $   
  runST $ do
    poly'<-thaw poly
    casteljau poly' 0 0 1 0
    unsafeFreeze poly' >>= return.(Bernsteinp ix)
  
  where
    
    
    casteljau pf bef aft nv a b i k s j
      | i>=bef = return ()
      | k>=aft= casteljau pf bef aft nv a b (i+1) 0 1 0
      | s>=nv = casteljau pf bef aft nv a b i (k+1) 1 0
      | j>=nv = casteljau pf bef aft nv a b i k (s+1) 0
      | otherwise=
        let idx i j k=(i*nv+j)*aft + k 
            b'=(b-a)/(1-a)
        in
         if s+j<nv then do
           pfi0<-MUV.read pf (idx i j k)
           pfi1<-MUV.read pf (idx i (j+1) k)
           MUV.write pf (idx i j k) $ (1-a)*pfi0+a*pfi1
           casteljau pf bef aft nv a b i k s (j+1)
         else do
           pfi0<-MUV.read pf (idx i (j-1) k)
           pfi1<-MUV.read pf (idx i j k)
           MUV.write pf (idx i j k) $ (1-b')*pfi0+b*pfi1
           casteljau pf bef aft nv a b i k s (j+1)
-}

-- Le booleen veut dire "tous les coefs sont nuls"
convexHull::Int->Int->Int->Bernsteinp i Interval->Double->Double->(Bool,Double,Double)
convexHull bef aft nv (Bernsteinp _ points) a b=
  let (allzero,pointsl,pointsu)=runST $ do
        let idx i j k=(i*nv+j)*aft + k
        pl<-MUV.replicate nv (1/0)
        pu<-MUV.replicate nv (-1/0)
        let fill i j k allzero_ -- a avant, b courant, c apres
              | i>=bef = return allzero_
              | j>=nv = fill (i+1) 0 0 allzero_
              | k>=aft = fill i (j+1) 0 allzero_
              | otherwise = do
                pl0<-MUV.read pl j
                pu0<-MUV.read pu j
                MUV.write pl j (min pl0 $ ilow $ points!(idx i j k))
                MUV.write pu j (max pu0 $ iup $ points!(idx i j k))
                fill i j (k+1) (allzero_ && pl0<=0 && pu0>=0)
        allzero_<-fill 0 0 0 True
        pl'<-UV.unsafeFreeze pl
        pu'<-UV.unsafeFreeze pu
        return (allzero_,pl',pu')
      inter::Int->Int->(Double,Double)
      inter i j
        | i>j = inter j i
        | otherwise =
          let yli=pointsl!i
              yui=pointsu!i
              ylj=pointsl!j
              yuj=pointsu!j
              fi=fromIntegral i
              fj=fromIntegral j
              inter0 yi yj=
                let k=fromIntegral $ i-j in
                Interval fi fi + 
                (Interval yi yi)*(Interval k k)/
                (Interval yj yj-Interval yi yi)
          in
           if yli<=0 then
             if yui>=0 then
               if ylj<=0 then
                 if yuj>=0 then
                   -- 1 les deux sont sur la ligne
                   --traceShow "1" $
                   (fi,fj)
                 else
                   -- 2 M est sur la ligne, M' est en-dessous
                   --traceShow "2" $
                   (fi, iup $ inter0 yui yuj)
               else
                 -- 3 M est sur la ligne, M' est au-dessus
                 --traceShow "3" $
                 (fi,iup $ inter0 yli ylj)
             else
               -- M est en-dessous de la ligne 
               if ylj<=0 then
                 if yuj>=0 then
                   -- 4 M' est sur la ligne
                   --traceShow "4" $
                   (ilow $ inter0 yui yuj, fj)
                 else
                   -- 5 M' est en-dessous (comme M)
                   --traceShow "5" $
                   (1/0,-1/0)
               else
                 -- 6 M' est au-dessus
                 --traceShow "6" $
                 (ilow $ inter0 yui yuj, iup $ inter0 yli ylj)
           else
               -- M est au-dessus de la ligne
               if ylj<=0 then
                 if yuj>=0 then
                   -- 7 M' est sur la ligne
                   --traceShow "7" $
                   (ilow $ inter0 yli ylj,fj)
                 else
                   -- 8 M' est en-dessous (comme M)
                   --traceShow "8" $
                   (ilow $ inter0 yli ylj, iup $ inter0 yui yuj)
               else
                 -- 9 M' est au-dessus
                 --traceShow "9" $
                 (1/0,-1/0)
                 
      testAll i j m0 m1
        | i>=nv = 
          let fn=fromIntegral (nv-1)
              (# a0,b0 #)=over m0 m0 fn fn
              (# a1,b1 #)=minus b b a a
              (# a2,b2 #)=times a0 b0 a1 b1
              (# a3,_ #)=plus a a a2 b2
              
              (# c0,d0 #)=over m1 m1 fn fn
              (# c2,d2 #)=times c0 d0 a1 b1
              (# _,d3 #)=plus a a c2 d2
          in
           (False,a3,d3)
        | j>=nv = testAll (i+1) (i+1) m0 m1
        | otherwise = 
          let (m0',m1')=inter i j in
          testAll i (j+1)
          (min m0 m0') (max m1 m1')
  in
   --traceShow allzero $
   if allzero then (True,a,b) else
     testAll 0 0 (1/0) (-1/0)
--traceShow ("convexHull",pointsl,pointsu,m) $ m

class Box a i | a->i where
  cut::Int->a->[a]
  size::Int->a->Double
  restriction#::a->Bernsteinp i Interval->a
  variables::a->Int

instance Box (Double,Double) Int where
  cut _ (a,b)=
    let m=(a+b)/2 in
    if a<m && m<b then
      [(a,m),(m,b)]
    else
      [(a,b)]
  size _ (a,b)=b-a
  restriction# (a,b) points@(Bernsteinp n0 _)=
    let restr=restriction points (Interval a a) (Interval b b)
        (allz,a',b')=convexHull 1 1 n0 restr a b
    in
     (max a a', min b b')
  variables _ = 1
instance Box (Double,Double,Double,Double) (Int,Int) where
  cut 0 x@(a,b,c,d)=
    let m=(a+b)/2 in
    if a<m && m<b then
      [(a,m,c,d),(m,b,c,d)]
    else
      [x]
  cut _ x@(a,b,c,d)=
    let m=(c+d)/2 in
    if c<m && m<d then
      [(a,b,c,m),(a,b,m,d)]
    else
      [x]
  size 0 (a,b,_,_)=b-a
  size _ (_,_,a,b)=b-a
                   
  restriction# (a,b,c,d) points@(Bernsteinp (n0,n1) _)=
    let restr=restriction points (Interval a a,Interval c c) (Interval b b,Interval d d)
        (allz0,a',b')
          | n0>1 = convexHull 1 n1 n0 restr a b
          | otherwise = (False,a,b)
        (allz1,c',d')
          | n1>1 = convexHull n0 1 n1 restr c d
          | otherwise = (False,c,d)
    in
     (max a a', min b b', max c c', min d d')
  variables _=2


instance Box (Double,Double,Double,Double,Double,Double) (Int,Int,Int) where
  cut 0 x@(a,b,c,d,e,f)=
    let m=(a+b)/2 in
    if a<m && m<b then
      [(a,m,c,d,e,f),(m,b,c,d,e,f)]
    else
      [x]
  cut 1 x@(a,b,c,d,e,f)=
    let m=(c+d)/2 in
    if c<m && m<d then
      [(a,b,c,m,e,f),(a,b,m,d,e,f)]
    else
      [x]
  cut _ x@(a,b,c,d,e,f)=
    let m=(e+f)/2 in
    if e<m && m<f then
      [(a,b,c,d,e,m),(a,b,c,d,m,f)]
    else
      [x]
  size 0 (a,b,_,_,_,_)=b-a
  size 1 (_,_,a,b,_,_)=b-a
  size _ (_,_,_,_,a,b)=b-a
    
  restriction# (a,b,c,d,e,f) points@(Bernsteinp (n0,n1,n2) _)=
    let restr=restriction points (Interval a a,Interval c c,Interval e e)
              (Interval b b,Interval d d,Interval f f)
        (allz0,a',b')
          | n0>1 = convexHull 1 (n1*n2) n0 restr a b
          | otherwise = (False,a,b)
        (allz1,c',d')
          | n1>1 = convexHull n0 n2 n1 restr c d
          | otherwise = (False,c,d)
        (allz2,e',f')
          | n2>1 = convexHull (n0*n1) 1 n2 restr e f
          | otherwise = (False,e,f)
    in
     (max a a', min b b', max c c', min d d', max e e', min f f')
     
  variables _=3
  
instance Box (Double,Double,Double,Double,Double,Double,Double,Double) (Int,Int,Int,Int) where
  cut 0 x@(a,b,c,d,e,f,g,h)=
    let m=(a+b)/2 in
    if a<m && m<b then
      [(a,m,c,d,e,f,g,h),(m,b,c,d,e,f,g,h)]
    else
      [x]
  cut 1 x@(a,b,c,d,e,f,g,h)=
    let m=(c+d)/2 in
    if c<m && m<d then
      [(a,b,c,m,e,f,g,h),(a,b,m,d,e,f,g,h)]
    else
      [x]
  cut 2 x@(a,b,c,d,e,f,g,h)=
    let m=(e+f)/2 in
    if e<m && m<f then
      [(a,b,c,d,e,m,g,h),(a,b,c,d,m,f,g,h)]
    else
      [x]
  cut _ x@(a,b,c,d,e,f,g,h)=
    let m=(g+h)/2 in
    if g<m && m<h then
      [(a,b,c,d,e,f,g,m),(a,b,c,d,e,f,m,h)]
    else
      [x]
    
  size 0 (a,b,_,_,_,_,_,_)=b-a
  size 1 (_,_,a,b,_,_,_,_)=b-a
  size 2 (_,_,_,_,a,b,_,_)=b-a
  size _ (_,_,_,_,_,_,a,b)=b-a
    
  restriction# (a,b,c,d,e,f,g,h) points@(Bernsteinp (n0,n1,n2,n3) _)=
    let restr=restriction points (Interval a a,Interval c c,Interval e e,Interval g g)
              (Interval b b,Interval d d,Interval f f,Interval h h)
        (allz0,a',b')
          | n0>1 = convexHull 1 (n1*n2*n3) n0 restr a b
          | otherwise = (False,a,b)
        (allz1,c',d')
          | n1>1 = convexHull n0 (n2*n3) n1 restr c d
          | otherwise = (False,c,d)
        (allz2,e',f')
          | n2>1 = convexHull (n0*n1) n3 n2 restr e f
          | otherwise = (False,e,f)
        (allz3,g',h')
          | n3>1 = convexHull (n0*n1*n2) 1 n3 restr g h
          | otherwise = (False,g,h)
    in
     --traceShow restr $
    (max a a', min b b', max c c', min d d', 
      max e e', min f f', max g g', min h h')
     
  variables _=4

-- | Computes the intersection of a given Bezier hypersurface, given
-- by its graph, with plane @z=0@.
solve::(Show a,Show i,Eq a,Box a i)=>Double->V.Vector (Bernsteinp i Interval)->a->[a]
solve sizeThreshold equations h= -- traceShow h $
  let splitThreshold=0.95
      restrictAll neq box
        | neq>=V.length equations = box
        | not (check 0 box) = box
        | otherwise =
          let next=restriction# box (equations!neq) in
          restrictAll (neq+1) next
      check v box=
        (v>=(variables box)) ||
        (let s=size v box in
          0<=s && s<(1/0) && check (v+1) box)
           
      h'=restrictAll 0 h
      
      isSmall v box=
        (v>=variables box) ||
        ((size v box <= sizeThreshold) && (isSmall (v+1) box))
      
  in
   if isSmall 0 h' then
     if check 0 (restrictAll 0 h') then
       [h']
     else
       []
   else
     if check 0 h' then
       let cutAll v boxes
             | v>=(variables h) = boxes
             | otherwise =
               cutAll (v+1) $
               Prelude.concatMap (\b->if (size v b)>=splitThreshold*(size v h) 
                                         && (size v b)>sizeThreshold
                                      then
                                        cut v b
                                      else [b]) boxes
           cc=cutAll 0 [h']
       in
        case cc of
          [h'']
            | h''==h -> 
              [h]
            | otherwise -> Prelude.concatMap (solve sizeThreshold equations) cc
          _->
            Prelude.concatMap (solve sizeThreshold equations) cc
      else
       []
\end{code}