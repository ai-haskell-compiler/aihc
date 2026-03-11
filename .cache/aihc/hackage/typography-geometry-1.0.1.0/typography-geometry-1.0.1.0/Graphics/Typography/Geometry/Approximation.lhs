\begin{code}
{-# OPTIONS -XRecordWildCards -XNamedFieldPuns #-}
-- | This module contains the function to approximate a list of curves with
-- degree 3 Bezier curves, using a least squares method.

module Graphics.Typography.Geometry.Approximation(approximate) where

import qualified Data.Vector.Unboxed as UV
import Graphics.Typography.Geometry.Bezier
import Graphics.Typography.Geometry
import Algebra.Polynomials.Bernstein

import Algebra.Polynomials.Numerical
-- import Debug.Trace
rnd::Interval->Double
rnd (Interval a b)=(a+b)/2


-- | Approximates a list of 'Curves' with a list of degree 3 Bernstein curves.
approximate::[Curve]->[(Bernsteinp Int Double,Bernsteinp Int Double)]
approximate []=[]
approximate l0@(h0:_)= -- traceShow "starting" $
  let approx::Double->Double->[Curve]->[(Bernsteinp Int Double,Bernsteinp Int Double)]
      approx _ _ []=[]
      approx x0 y0 (cc@(Circle {..}):s)= -- traceShow "circle" $
        let theta=abs $ t1-t0 in
        if theta <= pi/2 then
          let x0_=cos $ theta/2
              y0_=sin $ theta/2
              x1_=(4-x0_)/3
              y1_=(1-x0_)*(3-x0_)/(3*y0_)
        
              c0=cos $! theta/2+t0
              s0=sin $! theta/2+t0
        
        
              px0=c0*x0_ - s0*y0_
              py0=s0*x0_ + c0*y0_
        
              px1=c0*x1_ - s0*y1_
              py1=s0*x1_ + c0*y1_
        
              px2=c0*x1_ + s0*y1_
              py2=s0*x1_ - c0*y1_
        
              -- px3=c0*x0_ + s0*y0_
              -- py3=s0*x0_ - c0*y0_
        
              x1=cx0+(a*px0+b*py0)
              y1=cy0+(c*px0+d*py0)
              
              (Matrix2 a b c d)=matrix
              x=Bernsteinp 4 $ UV.fromList
                [ x0, -- cx0+(a*px3+b*py3),
                  cx0+(a*px2+b*py2),
                  cx0+(a*px1+b*py1),
                  x1]
              
              y=Bernsteinp 4 $ UV.fromList
                [ y0, -- cy0+(c*px3+d*py3),
                  cy0+(c*px2+d*py2),
                  cy0+(c*px1+d*py1),
                  y1 ]
          in
           (x,y):(approx x1 y1 s)
     
        else
          let t1'=(t1+t0)/2 in
          approx x0 y0 $ (cc { t1=t1' }):(cc { t0=t1' }):s
{-
      approx x0 y0 (h@(Bezier{}):s)=
        -- incorrect !
        (restriction (cx h) (t0 h) (t1 h),
         restriction (cy h) (t0 h) (t1 h)):
        (approx (UV.last $ coefs $ cx h)
         (UV.last $ coefs $ cy h) s)
-}
      -- Ce qui suit est une methode de moindres carres
      approx x0 y0 (off_:s)= -- traceShow ("offset") $
        -- On commence par chercher les points ou la derivee de la norme
        -- de la tangente est maximale. C'est la qu'on va couper s'il y
        -- a un probleme.
        let bx=restriction (cx off_) (t0 off_) (t1 off_)
            by=restriction (cy off_) (t0 off_) (t1 off_)
            off=off_ { cx=bx,cy=by,t0=0,t1=1 }
            ibx=elevate (bounds by-bounds bx) $ intervalize bx
            iby=elevate (bounds bx-bounds by) $ intervalize by
            points=
              let np=10 in
              map (\x->(x/np,x/np)) [0..np]
            -- Ensuite, moindres carres standard, comme dans Hoschek 88.
      
            vx0=ibx?1-ibx?0
            vy0=iby?1-iby?0
            vx1=ibx?(bounds ibx-2)-ibx?(bounds ibx-1)
            vy1=iby?(bounds iby-2)-iby?(bounds iby-1)
    
            (wx0,wy0)=evalCurve off 0
            (wx1,wy1)=evalCurve off 1

            wx=Bernsteinp 4 $ UV.fromList [wx0,wx0,wx1,wx1] :: Bernsteinp Int Interval
            wy=Bernsteinp 4 $ UV.fromList [wy0,wy0,wy1,wy1] :: Bernsteinp Int Interval

            bern1=Bernsteinp 4 $ UV.fromList [0,1,0,0] :: Bernsteinp Int Interval
            bern2=Bernsteinp 4 $ UV.fromList [0,0,1,0] :: Bernsteinp Int Interval

            sumAll a b c d x1 y1 ((h1,h2):ss)=
        
              let h=Interval h1 h2
                  (xi,yi)=evalCurve off h
            
                  b1=eval bern1 h
                  b2=eval bern2 h
            
                  a'=a + (vx0*vx0+vy0*vy0)*b1*b1
                  b'=b + (vx0*vx1 + vy0*vy1)*b1*b2
                  c'=c + (vx0*vx1 + vy0*vy1)*b1*b2
                  d'=d + (vx1*vx1+vy1*vy1)*b2*b2
            
                  dx=xi-(eval wx h)
                  dy=yi-(eval wy h)
            
                  x1'=x1 + (vx0*dx + vy0*dy)*b1
                  y1'=y1 + (vx1*dx + vy1*dy)*b2
              in
               sumAll a' b' c' d' x1' y1' ss
         
            sumAll a b c d x1 y1 []=(a,b,c,d,x1,y1)
                                    
            (ra,rb,rc,rd,rx1,ry1)=sumAll 0 0 0 0 0 0 points
            
            (Matrix2 a_ b_ c_ d_)=inverse $ Matrix2 ra rb rc rd
            lambda1=a_*rx1+b_*ry1
            lambda2=c_*rx1+d_*ry1
      
            -- On a la courbe optimale. Il faut chercher ou on va couper, maintenant
            xap=Bernsteinp 4 $ UV.fromList [wx0,
                                            wx0+lambda1*vx0,
                                            wx1+lambda2*vx1,
                                            wx1]
            yap=Bernsteinp 4 $ UV.fromList [wy0,
                                            wy0+lambda1*vy0,
                                            wy1+lambda2*vy1,
                                            wy1]
            (err,arg)=foldl (\m (h1,h2)->
                              let (xi,yi)=evalCurve off (Interval h1 h2)
                                  xj=eval xap (Interval h1 h2)
                                  yj=eval yap (Interval h1 h2)
                              in
                               max m (iup $ abs $ (xi-xj)*(xi-xj)+(yi-yj)*(yi-yj), (h1+h2)/2))
                      (0,0) points
        in
         if err<=0.01 then
           (desintervalize xap,desintervalize yap):(approx (rnd wx1) (rnd wy1) s)
         else
           approx x0 y0 $
            (off { cx=restriction (cx off) 0 arg,
                   cy=restriction (cy off) 0 arg }):
            (off { cx=restriction (cx off) arg 1,
                   cy=restriction (cy off) arg 1 }):s
           
      (x0h,y0h)=evalCurve h0 $ Interval (t0 h0) (t0 h0)
      
  in
   approx (rnd x0h) (rnd y0h) l0
           
desintervalize::(Bernsteinp a Interval)->(Bernsteinp a Double)
desintervalize b=b { coefs=UV.map rnd $ coefs b}
  
\end{code}
