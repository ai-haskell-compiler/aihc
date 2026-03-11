
\documentclass{article}
%include lhs2TeX.fmt
\begin{document}
\begin{code}
{-# LANGUAGE FlexibleContexts, UnboxedTuples, BangPatterns, NamedFieldPuns, RecordWildCards, MagicHash, CPP #-}
-- | This module contains the basic functions for manipulating Bezier curves. It is heavily
-- based on the book by N. M. Patrikalakis and T. Maekawa, Shape Interrogation for Computer
-- Aided Design and Manufacturing.

module Graphics.Typography.Geometry.Bezier (
  Curve(..),line,bezier3,
  offset,
  inter,
  evalCurve,distance,
  left,bottom,right,top)  where

import Algebra.Polynomials.Bernstein
import Algebra.Polynomials.Numerical
import Graphics.Typography.Geometry
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Data.List (partition,sort)

-- | The type for representing all types of curves.
data Curve=
  Bezier { cx::Bernsteinp Int Double,
           cy::Bernsteinp Int Double, 
           t0::Double,
           t1::Double }
  
  | Offset { cx::Bernsteinp Int Double,
             cy::Bernsteinp Int Double,
             t0::Double,
             t1::Double,
             matrix::Matrix2 Double
           }
  | Circle { cx0::Double,
             cy0::Double,
             t0::Double,
             t1::Double,
             matrix::Matrix2 Double
           }
  deriving (Show)


-- | The basic constructor for lines : a line is a degree 1 Bezier curve
line::Double->Double->Double->Double->Curve
line px py px' py'=Bezier { cx=Bernsteinp 2 $ UV.fromList [px,px'], 
                            cy=Bernsteinp 2 $ UV.fromList [py,py'],
                            t0=0,t1=1 }

-- | A shortcut to define degree 3 Bezier curves from points. If the control
-- points are @a,b,c,d@, the function should be called with
-- @'bezier3' xa ya xb yb xc yc xd yd@.
bezier3::Double->Double->Double->Double->Double->Double->Double->Double->Curve
bezier3 px0 py0 px1 py1 px2 py2 px3 py3=
  Bezier { cx=Bernsteinp 4 $ UV.fromList [px0,px1,px2,px3],
           cy=Bernsteinp 4 $ UV.fromList [py0,py1,py2,py3],
           t0=0,t1=1 }

instance Geometric Curve where
  translate x y cur@(Circle{cx0,cy0})=        
    cur { cx0=cx0+x,cy0=cy0+y }
  translate x y cur=
    cur { cx=(cx cur) { coefs=UV.map (+x) $ coefs $ cx cur},
          cy=(cy cur) { coefs=UV.map (+y) $ coefs $ cy cur} }

  apply m0@(Matrix2 a b c d) cir@(Circle{cx0,cy0,matrix})=
    cir { cx0=a*cx0+b*cy0, cy0=c*cx0+d*cy0, matrix=m0*matrix }
  apply (Matrix2 a b c d) cur=
    cur { cx=(scale a $ cx cur)+(scale b $ cy cur),
          cy=(scale c $ cx cur)+(scale d $ cy cur) }



-- | Gives the point corresponding to the given value of the parameter
evalCurve::Curve->Interval->(Interval,Interval)
evalCurve (Offset{..}) t=
  let ix=intervalize cx
      iy=intervalize cy
      xt=eval ix t
      yt=eval iy t
      m@(Matrix2 a b c d)=intervalize matrix
      (Matrix2 a_ b_ c_ d_)=inverse m
      xt0'=eval (derivate ix) t
      yt0'=eval (derivate iy) t
      xt'=a_*xt0' + b_*yt0'
      yt'=c_*xt0' + d_*yt0'
      dd=sqrt $ xt'*xt' + yt'*yt'
  in
   (xt+(a*yt'-b*xt')/dd, yt+(c*yt'-d*xt')/dd)

evalCurve (Circle{..}) alpha=
  let xx=cos alpha
      yy=sin alpha
      (Matrix2 a b c d)=intervalize matrix
  in
   (interval cx0+a*xx+b*yy, interval cy0+c*xx+d*yy)
   
evalCurve (Bezier{..}) t=
  let ix=intervalize cx
      iy=intervalize cy
      xx=eval ix t
      yy=eval iy t
  in
   (xx,yy)
   
data Topo=Dehors | SurLaLigne | Dedans deriving Eq

-- | @'inter' c0 c1@ is a list of all possible points of intersection
-- between curves @c0@ and @c1@ : if @(u,v,w,x)@ is returned by 'inter',
-- then curve @c0@ may intersect with @c1@ between parameter values @u@
-- and @v@, which corresponds to parameter values between @w@ and @x@ for
-- @c1@. The implementation guarantees that all actual solutions are found,
-- but possibly false solutions may also be returned.

inter::Curve->Curve->[((Double,Double,Double,Double))]
inter op@(Offset { cx=bxp_,cy=byp_,matrix=mp,t0=t0a,t1=t1a })
  (Offset { cx=bxq_,cy=byq_,matrix=mq,t0=t0b,t1=t1b })=
  
  -- Attention : verifier si c'est la meme generatrice
  let thrx=1e-5
      solutions _ []=[]
      solutions thr boxes@(_:_)=
        let sol0=concatMap (solve thr (V.fromList [eq0,eq1,eq2,eq3])) boxes 
        
            (correct,toRefine)=partition (\(u,v,_,_,_,_,_,_)->
                              let (xu,yu)=evalCurve op (Interval u u)
                                  (xv,yv)=evalCurve op (Interval v v)
                              in
                               (iup $ (xu-xv)^(2::Int)+(yu-yv)^(2::Int))<=thrx) sol0
        in
         correct++(solutions (thr/2) toRefine)
  in
   map (\(u,v,w,x,_,_,_,_)->(u,v,w,x)) $ solutions 1e-2 $
   [(t0a,t1a,t0b,t1b,0,1,0,1)::
       (Double,Double,Double,Double,Double,Double,Double,Double)]

     where
       
       
       imp@(Matrix2 ap bp cp dp)=intervalize mp
       imq@(Matrix2 aq bq cq dq)=intervalize mq
       (Matrix2 ap_ bp_ cp_ dp_)=inverse imp
       (Matrix2 aq_ bq_ cq_ dq_)=inverse imq

       bxp=intervalize bxp_
       byp=intervalize byp_
       bxq=intervalize bxq_
       byq=intervalize byq_
    
       bxp4=promote 1 bxp
       byp4=promote 1 byp
       bxq4=promote 2 bxq
       byq4=promote 2 byq
    

       bxp'=derivate bxp
       byp'=derivate byp
       bxq'=derivate bxq
       byq'=derivate byq

       bXp'=promote 1 $ (scale ap_ bxp')+(scale bp_ byp')
       bYp'=promote 1 $ (scale cp_ bxp')+(scale dp_ byp')

       bXq'=promote 2 $ (scale aq_ bxq')+(scale bq_ byq')
       bYq'=promote 2 $ (scale cq_ bxq')+(scale dq_ byq')

       bomp@(Bernsteinp _ omegap)=(bXp'*bXp')+(bYp'*bYp') :: Bernsteinp (Int,Int,Int,Int) Interval
       bomq@(Bernsteinp _ omegaq)=(bXq'*bXq')+(bYq'*bYq') :: Bernsteinp (Int,Int,Int,Int) Interval
       
       au=
         let au_=sqrt $ UV.minimum $ UV.map ilow omegap in
         max 0 $ fpred au_
       bu=
         let bu_=sqrt $ UV.maximum $ UV.map iup omegap in
         fsucc bu_
       av=
         let av_=sqrt $ UV.minimum $ UV.map ilow omegaq in
         max 0 $ fpred av_
       bv=
         let bv_=sqrt $ UV.maximum $ UV.map iup omegaq in
         fsucc bv_
    
       alphau=Bernsteinp (1,1,2,1) $ UV.fromList [Interval au au,Interval bu bu]
       alphav=Bernsteinp (1,1,1,2) $ UV.fromList [Interval av av,Interval bv bv]

       eq0=
         ((bxp4*alphau*alphav) + (scale ap $ bYp'*alphav) - (scale bp $ bXp'*alphav)
          -(bxq4*alphau*alphav) - (scale aq $ bYq'*alphau) + (scale bq $ bXq'*alphau))
       eq1=
         ((byp4*alphau*alphav) + (scale cp $ bYp'*alphav) - (scale dp $ bXp'*alphav)
          -(byq4*alphau*alphav) - (scale cq $ bYq'*alphau) + (scale dq $ bXq'*alphau))    
       eq2=bomp-(alphau*alphau)
       eq3=bomq-(alphav*alphav)
      
    


inter b@(Circle{}) a@(Offset{})=
  map (\(i,j,k,l)->(k,l,i,j)) $ inter a b

inter o@(Offset { cx=bxp, cy=byp, matrix=mp })
  cir@(Circle{cx0,cy0,matrix=mq})=

  let ix=intervalize bxp
      iy=intervalize byp
      m@(Matrix2 a b c d)=intervalize mp
      (Matrix2 a_ b_ c_ d_)=inverse m
      x'=derivate ix
      y'=derivate iy
      xx'=(scale a_ x')+(scale b_ y')
      yy'=(scale c_ x')+(scale d_ y')
      omega@(Bernsteinp _ omegap)=xx'*xx'+yy'*yy'
      au=
        let au_=sqrt $ UV.minimum $ UV.map ilow omegap in
        max 0 $ fpred au_
      bu=
        let bu_=sqrt $ UV.maximum $ UV.map iup omegap in
        fsucc bu_
      
      alphau=Bernsteinp (1,2) $ UV.fromList [Interval au au,Interval bu bu]
      
      lambda=(promote 1 omega) - alphau*alphau
      -- Avant multiplication par M_C^-1
      xx0=(promote 1 $ ix-(intervalize $ constant cx0))*alphau
          +(promote 1 $ scale a yy'-scale b xx')
      yy0=(promote 1 $ iy-(intervalize $ constant cy0))*alphau
          +(promote 1 $ scale c yy'-scale d xx') :: Bernsteinp (Int,Int) Interval
    
      (Matrix2 ac_ bc_ cc_ dc_)=inverse $ intervalize mq
      xx1=(scale ac_ xx0)+(scale bc_ yy0)
      yy1=(scale cc_ xx0)+(scale dc_ yy0)
         
      eqc=xx1*xx1+yy1*yy1-alphau*alphau
      
      thrx=1e-5
      
      solutions _ []=[]
      solutions thr boxes@(_:_)=
        let sol0=concatMap (solve thr (V.fromList [eqc,lambda])) boxes 
        
            (correct,toRefine)=partition (\(u,v,_,_)->
                              let (xu,yu)=evalCurve o (Interval u u)
                                  (xv,yv)=evalCurve o (Interval v v)
                              in
                               (iup $ (xu-xv)^(2::Int)+(yu-yv)^(2::Int))<=thrx) sol0
        in
         correct++(solutions (thr/2) toRefine)
         

      -- Removing false positives by computing the distance to the center of
      -- the circle (this is quite fast).
      
      removeFalse cl0 (h@(_,v,_,_):h'@(u',_,_,_):s)=
        let u''=(v+u')/2
            (xu,yu)=evalCurve o (Interval u'' u'')
            Interval dl du=distance xu yu cir
            cl1
              | du<1 = Dedans
              | dl>1 = Dehors
              | otherwise = SurLaLigne
        in
         if cl0/=cl1 then h:(removeFalse cl1 (h':s)) else
           removeFalse cl1 (h':s)
      removeFalse _ l=l
      
      initCl=
        let (x0,y0)=evalCurve o (Interval (t0 o) (t0 o)) 
            Interval dl du=distance x0 y0 cir
        in
        if dl>1 then Dehors else if du<1 then Dedans else SurLaLigne
  in
   foldl (\l (u,v,_,_)->
              let (Interval xl xu,Interval yl yu)=evalCurve o (Interval u v) in
              case angle (Interval xl xu) (Interval yl yu) cir of
                Just (Interval a0 a1)->
                  (u,v,a0,a1):l
                Nothing->l
            ) [] $ removeFalse initCl $ sort $ solutions 1e-2 [(t0 o,t1 o,0::Double,1::Double)]
   
  
inter a@(Circle{cx0=x0a,cy0=y0a,matrix=ma})
  b@(Circle{cx0=x0b,cy0=y0b,matrix=mb})=
  
  if (intervalize ma)`intersects`(intervalize mb) && x0a==x0b && y0a==y0b then
    let up ix@(Interval _ x_) tt0 tt1
          | x_<tt0 =
            up (ix+(2*interval pi)) tt0 tt1
          | otherwise = down ix tt0 tt1
        down ix@(Interval x_ x__) tt0 tt1
          | x_>tt1 =
            down (ix-(2*interval pi)) tt0 tt1
          | x__<tt0 =
              Nothing
          | otherwise =
            Just ix
            
        alpha=up (interval $ t0 a) (t0 b) (t1 b)
        beta=up (interval $ t0 b) (t0 b) (t1 b)
    in
     
     case (alpha,beta) of
       (Just aa,Just ab)->
         case (up aa (t0 a) (t1 a),
               up ab (t0 a) (t1 a)) of
           
           (Just ba,Just bb)
             | ilow aa<=iup ab -> [(ilow ba, iup bb,
                                    ilow aa, iup ab)]
             | otherwise->
               case (up (interval $ t0 b) (t0 a) (t1 a),
                     up (interval $ t1 b) (t0 a) (t1 a)) of
                 (Just b0,Just b1)->
                   [(ilow b0,iup bb,
                     t0 b, iup ab),
                    (ilow ba,iup b1,
                     ilow aa, t1 b)]
                 _->[]
           _->[]
       _->[]

  else
    let thr=1e-5
        solutions=solve thr (V.fromList [eq0,eq1]) (fpred u0,fsucc v0,
                                                    fpred w0,fsucc x0)
    in
     foldl (\l (u,v,w,x)->
             let alpha=angle (Interval u v) (Interval w x) a
                 beta=angle (Interval u v) (Interval w x) b
             in
              case alpha of
                Just (Interval a0l a0u)->
                  case beta of
                    Just (Interval b0l b0u)->(a0l,a0u,b0l,b0u):l
                    _->l
                _->l
           ) [] solutions 
  where
    
    ima@(Matrix2 am bm cm dm)=intervalize ma
    
    maxa=max (iup $ abs am+abs bm) (iup $ abs cm+abs dm)
    (u0,v0,w0,x0)=(x0a-maxa,x0a+maxa,y0a-maxa,y0a+maxa)
    
    -- x-x0
    xxa0=intervalize $ Bernsteinp (2,1) $ UV.fromList [-x0a,1-x0a] :: Bernsteinp (Int,Int) Interval
    yya0=intervalize $ Bernsteinp (1,2) $ UV.fromList [-y0a,1-y0a] :: Bernsteinp (Int,Int) Interval
    (Matrix2 aa_ ba_ ca_ da_)=inverse ima
    xxa=(scale aa_ xxa0)+(scale ba_ yya0)::Bernsteinp (Int,Int) Interval
    yya=(scale ca_ xxa0)+(scale da_ yya0)
    
    xxb0=intervalize $ Bernsteinp (2,1) $ UV.fromList [-x0b,1-x0b]
    yyb0=intervalize $ Bernsteinp (1,2) $ UV.fromList [-y0b,1-y0b]
    (Matrix2 ab_ bb_ cb_ db_)=inverse $ intervalize mb
    xxb=(scale ab_ xxb0)+(scale bb_ yyb0)
    yyb=(scale cb_ xxb0)+(scale db_ yyb0)
    
    c1=Bernsteinp (1,1) $ UV.singleton 1
    
    eq0=xxa*xxa+yya*yya-c1
    eq1=xxb*xxb+yyb*yyb-c1

inter op@(Bezier{cx=bxa,cy=bya,t0=t0a,t1=t1a}) (Bezier{cx=xb,cy=yb,t0=t0b,t1=t1b})=
  
  let p0=(promote 1 $ intervalize bxa)-(promote 2 $ intervalize xb) :: Bernsteinp (Int,Int) Interval
      p1=(promote 1 $ intervalize bya)-(promote 2 $ intervalize yb) :: Bernsteinp (Int,Int) Interval
      thrx=1e-2
      solutions _ []=[]
      solutions thr boxes@(_:_)=
        let sol0=concatMap (solve thr (V.fromList [p0,p1])) boxes 
        
            (correct,toRefine)=partition (\(u,v,_,_)->
                              let (xu,yu)=evalCurve op (Interval u u)
                                  (xv,yv)=evalCurve op (Interval v v)
                              in
                               (iup $ (xu-xv)^(2::Int)+(yu-yv)^(2::Int))<=thrx) sol0
        in
         correct++(solutions (thr/2) toRefine)
  in
   solutions 1e-2 [(t0a,t1a,t0b,t1b)]


inter cir@(Circle{}) bez@(Bezier{})=map (\(u,v,w,x)->(w,x,u,v)) $ inter bez cir

inter bez@(Bezier{}) cir@(Circle{})=
  let xx=(intervalize $ cx bez)-(intervalize $ constant $ cx0 cir)
      yy=(intervalize $ cy bez)-(intervalize $ constant $ cy0 cir)
      (Matrix2 a b c d)=inverse $ intervalize $ matrix cir
      xx0=scale a xx+scale b yy
      yy0=scale c xx+scale d yy
      
      thrx=1e-5
      
      solutions _ []=[]
      solutions thr boxes@(_:_)=
        let sol0=concatMap (solve thr (V.singleton (xx0*xx0+yy0*yy0-(constant 1)))) boxes
        
            (correct,toRefine)=partition (\(u,v)->
                              let (xu,yu)=evalCurve bez (Interval u u)
                                  (xv,yv)=evalCurve bez (Interval v v)
                              in
                               (iup $ (xu-xv)^(2::Int)+(yu-yv)^(2::Int))<=thrx) sol0
        in
         correct++(solutions (thr/2) toRefine)
  in
   foldl (\l (u,v)->
              let (Interval xl xu,Interval yl yu)=evalCurve bez (Interval u v) in
              case angle (Interval xl xu) (Interval yl yu) cir of
                Just (Interval a0 a1)->
                  (u,v,a0,a1):l
                Nothing->l
            ) [] $!
   solutions (1e-2) [(t0 bez,t1 bez)]

inter bez@(Bezier{}) off@(Offset{})=map (\(u,v,w,x)->(w,x,u,v)) $ inter off bez


inter off@(Offset{}) bez@(Bezier{})=
  
  let thr=1e-2
      thrx=1e-5
      solutions _ []=[]
      solutions thr boxes@(_:_)=
        let sol0=concatMap (solve thr (V.fromList [eq0,eq1,eq2])) boxes
        
            (correct,toRefine)=partition (\(u,v,_,_,_,_)->
                              let (xu,yu)=evalCurve off (Interval u u)
                                  (xv,yv)=evalCurve off (Interval v v)
                              in
                               (iup $ (xu-xv)^(2::Int)+(yu-yv)^(2::Int))<=thrx) sol0
        in
         correct++(solutions (thr/2) toRefine)
  in
   map (\(a,b,c,d,_,_)->(a,b,c,d)) $ solutions 1e-2 $
   [(0,1,0,1,0,1)::(Double,Double,Double,Double,Double,Double)]
    where
      
      bxp=intervalize $ cx off
      byp=intervalize $ cy off
      bxq=intervalize $ cx bez
      byq=intervalize $ cy bez
      
      bxp'=derivate bxp
      byp'=derivate byp
      bxp3=promote 1 bxp
      byp3=promote 1 byp
      bxq3=promote 2 bxq
      byq3=promote 2 byq
      
      mp@(Matrix2 ap bp cp dp)=intervalize $ matrix $ off
      (Matrix2 ap_ bp_ cp_ dp_)=inverse mp
      bXp'=promote 1 $ (scale ap_ bxp')+(scale bp_ byp')
      bYp'=promote 1 $ (scale cp_ bxp')+(scale dp_ byp')

      omp@(Bernsteinp _ omegap)=(bXp'*bXp')+(bYp'*bYp')
      au=
        let au_=sqrt $ UV.minimum $ UV.map ilow omegap in
        max 0 $ fpred au_
      bu=
        let bu_=sqrt $ UV.maximum $ UV.map iup omegap in
        fsucc bu_
    
      alphau=Bernsteinp (1,1,2) $ UV.fromList [Interval au au,Interval bu bu]
      eq0=bxp3*alphau + (scale ap bYp') - (scale bp bXp') - bxq3
      eq1=byp3*alphau + (scale cp bYp') - (scale dp bXp') - byq3
      eq2=alphau*alphau-omp
      


angle::Interval->Interval->Curve->Maybe Interval
angle x y (Circle { cx0,cy0,matrix,t0,t1 })=
  let vx=x-interval cx0
      vy=y-interval cy0
      
      Matrix2 a b c d=inverse $ intervalize matrix
      -- L'arithmetique d'intervalles fait un peu n'importe quoi
      -- quand le vecteur est trop long. On le raccourcit.
      alpha=
        let co@(Interval col cou)=a*vx+b*vy
            Interval sil siu=c*vx+d*vy
            co2=
              let (col2,cou2)=if col*col<cou*cou then (col*col,cou*cou) else
                                (cou*cou,col*col)
              in
               Interval (fpred col2) (fsucc cou2)
            si2=
              let (sil2,siu2)=if sil*sil<siu*siu then (sil*sil,siu*siu) else
                                (siu*siu,sil*sil)
              in
               Interval (fpred sil2) (fsucc siu2)
            coco=co/(sqrt (co2+si2))
            ac@(Interval acl acu)=acos $ Interval (max (-1) $ ilow coco) (min 1 $ iup coco)
        in
         if siu<0 then negate ac else
           if sil>=0 then ac else
             Interval (negate $ min (abs acl) (abs acu))
             (max (abs acl) (abs acu))
      up ix
        | iup ix<t0 =
          up $ ix+(2*interval pi)
        | otherwise =
            down ix
      down ix
        | ilow ix>t1 =
          down $ ix-(2*interval pi)
        | iup ix<t0 =
          Nothing
        | otherwise =
          Just ix
  in
   up alpha


angle _ _ _=error "angle"

-- | Pseudo-distance from a point to a curve. Is the result is
-- smaller than 1, the point is inside the curve. If it is greater
-- than 1, the point is outside. Else we don't know (as usual with
-- interval arithmetic).

distance::Interval->Interval->Curve->Interval
distance x0 y0 (Bezier{..})=
  distance x0 y0 (Offset{cx,cy,t0,t1,matrix=Matrix2 1 0 0 1})
  
distance x0 y0 (Offset{..})=
  let (Matrix2 a b c d)=inverse $ intervalize matrix
      vx_=intervalize cx-(constant x0)
      vy_=intervalize cy-(constant y0)
      vx=scale a vx_+scale b vy_
      vy=scale c vx_+scale d vy_
      
      dist=vx*vx+vy*vy
  in
   foldl (\di (u,v)->let di'=eval dist (Interval u v) in
           if iup di<iup di' then di else di') (Interval (1/0) (1/0)) $
   (t0,t0):(t1,t1):(solve 1e-5 (V.singleton (derivate dist)) (t0,t1))
  
  
distance x1 y1 (Circle{..})=
  let (Matrix2 a b c d)=inverse $ intervalize matrix
      vx_=x1-Interval cx0 cx0
      vy_=y1-Interval cy0 cy0
      vx=a*vx_+b*vy_
      vy=c*vx_+d*vy_
  in
   vx*vx+vy*vy
   
-- | Offsets a given Bezier curve with the given pen matrix. The original
-- pen is a circle of radius one, the matrix, if inversible, is applied to it.

offset::Matrix2 Double->Curve->[Curve]
offset m (Bezier{cx=x@(Bernsteinp nx bx),cy=y@(Bernsteinp ny by)})=
  if nx <=1 && ny <=1 then
    [Circle { cx0=UV.head bx,cy0=UV.head by,t0=ilow 0,t1=iup $ 2*pi,matrix=m }]
  else
    [ c0,c1,c2,c3 ]
    
  where
    im=intervalize m
    (Matrix2 a_ b_ c_ d_)=inverse im
    
    ibx=intervalize x
    iby=intervalize y
    
    lastCoef (Bernsteinp n c)
      | n>=1 = UV.last c
      | otherwise = 0
    firstCoef (Bernsteinp n c)
      | n>=1 = UV.head c
      | otherwise = 0
    
    -- Premiere courbe offset
    c0=Offset { cx=x, cy=y, t0=0,t1=1,matrix=m }
    
    -- Demi-cercle 1
    
    ibx'=derivate ibx
    iby'=derivate iby
    
    -- Calcul du vecteur tangent au bout du premier

    alpha0=
      let xx0=lastCoef ibx'
          yy0=lastCoef iby'

          xx0'=a_*xx0+b_*yy0
          yy0'=c_*xx0+d_*yy0
          norm0=sqrt $ xx0'*xx0'+yy0'*yy0'
          
          xx'=xx0'/norm0
          yy'=yy0'/norm0
      in
       if ilow xx'>=0 then
         -(acos yy')
       else
         if iup xx'<=0 then
           acos yy'
         else
           let Interval u v=acos yy' in
           Interval (negate $ max (abs u) (abs v))
           (max (abs u) (abs v))
          
                    
    alpha0'=alpha0+interval pi
    c1=Circle { cx0=lastCoef x,
                cy0=lastCoef y,
                t0=ilow alpha0,
                t1=iup alpha0',
                matrix=m }
    
    
    -- Deuxieme courbe offset
    c2=Offset { cx=reorient x,
                cy=reorient y,
                t0=0,t1=1,
                matrix=m }
       
    -- Deuxieme demi-cercle
    alpha1=
      let xx0=firstCoef ibx'
          yy0=firstCoef iby'

          xx0'=a_*xx0+b_*yy0
          yy0'=c_*xx0+d_*yy0
          norm0=sqrt $ xx0'*xx0'+yy0'*yy0'
          
          xx'=xx0'/norm0
          yy'=yy0'/norm0
      in
       if ilow xx'>=0 then
         -(acos yy')
       else
         if iup xx'<=0 then
           acos yy'
         else
           let Interval u v=acos yy' in
           Interval (negate $ max (abs u) (abs v))
           (max (abs u) (abs v))
           
    alpha1'=alpha1-pi
    c3=Circle { cx0=firstCoef x, 
                cy0=firstCoef y,
                t0=ilow alpha1',
                t1=iup alpha1,
                matrix=m }
    

offset _ _=error "offset : undefined yet for other than Bezier"


rnd::Interval->Double
rnd (Interval a b)=(a+b)/2

derivRoots::Double->Curve->([(Double,Double)],[(Double,Double)])
derivRoots thr (Bezier{..})=
  (solve thr (V.singleton $ derivate $ intervalize cx) (t0,t1),
   solve thr (V.singleton $ derivate $ intervalize cy) (t0,t1))
derivRoots thr (Offset{..})=
  let ix=intervalize cx
      iy=intervalize cy
      x'=derivate ix
      y'=derivate iy
      m@(Matrix2 a b c d)=intervalize matrix
      (Matrix2 a_ b_ c_ d_)=inverse m
      
      xx'=(scale a_ x')+(scale b_ y')
      yy'=(scale c_ x')+(scale d_ y')
      xx''=derivate xx'
      yy''=derivate yy'
      
      omega=xx'*xx'+yy'*yy'
      au=
        let au_=sqrt $ UV.minimum $ UV.map ilow $ coefs omega in
        max 0 $ fpred au_
      bu=
        let bu_=sqrt $ UV.maximum $ UV.map iup $ coefs omega in
        fsucc bu_
      alphau=Bernsteinp (1,2) $ UV.fromList [Interval au au,Interval bu bu]
      
      eqx0=(promote 1 yy'')*(alphau^(2::Int))-(promote 1 $ yy'*yy''+xx'*xx'')
      eqy0=(promote 1 $ yy'*yy''+xx'*xx'')-(promote 1 xx'')*(alphau^(2::Int))
      
      eqx=(promote 1 x')*(alphau^(3::Int))+(scale a eqx0)+(scale b eqy0)
          :: Bernsteinp (Int,Int) Interval
      eqy=(promote 1 y')*(alphau^(3::Int))+(scale c eqx0)+(scale d eqy0)
      
      eq=(promote 1 omega)-alphau^(2::Int) :: Bernsteinp (Int,Int) Interval
  in
   (map (\(u,v,_,_)->(u,v)) $ solve thr (V.fromList [eqx,eq])
    ((t0,t1,0,1)::(Double,Double,Double,Double)),
    map (\(u,v,_,_)->(u,v)) $ solve thr (V.fromList [eqy,eq])
    ((t0,t1,0,1)::(Double,Double,Double,Double)))
derivRoots _ (Circle{..})=
  let (Matrix2 a b c d)=intervalize matrix
      aa=sqrt $ a*a+b*b
      cc=sqrt $ c*c+d*d
      sx
        | ilow a>=0 = acos $ b/aa
        | otherwise = negate $ acos $ b/aa
      sy
        | ilow c>=0 = acos $ d/cc
        | otherwise = negate $ acos $ d/cc
      Interval ux vx=(-sx)-pi/2
      Interval uy vy=(-sy)-pi/2
  in
   ([(ux,vx)],[(uy,vy)])

-- | The leftmost point on a curve
left::Curve->(Double,Double)
left cur=
  let (x,y)=
        foldl (\m@(xx,_) (s,t)->
                let m'@(xx',_)=evalCurve cur $ interval $ (s+t)/2 in
                if ilow xx<ilow xx' then m else m') (1/0,1/0) $
        (t0 cur,t0 cur):(t1 cur,t1 cur):(fst $ derivRoots 1e-5 cur)
  in
   (rnd x,rnd y)
-- | The bottommost point on a curve
bottom::Curve->(Double,Double)
bottom cur=
  let (x,y)=
        foldl (\m@(_,yy) (s,t)->
                let m'@(_,yy')=evalCurve cur $ interval $ (s+t)/2 in
                if ilow yy<ilow yy' then m else m') (1/0,1/0) $
        (t0 cur,t0 cur):(t1 cur,t1 cur):(snd $ derivRoots 1e-5 cur)
  in
   (rnd x,rnd y)
-- | The rightmost point on a curve
right::Curve->(Double,Double)
right cur=
  let (x,y)=
        foldl (\m@(xx,_) (s,t)->
                let m'@(xx',_)=evalCurve cur $ interval $ (s+t)/2 in
                if iup xx>iup xx' then m else m') (-1/0,-1/0) $
        (t0 cur,t0 cur):(t1 cur,t1 cur):(fst $ derivRoots 1e-5 cur)
  in
   (rnd x,rnd y)
-- | The topmost point on a curve
top::Curve->(Double,Double)
top cur=
  let (x,y)=
        foldl (\m@(_,yy) (s,t)->
                let m'@(_,yy')=evalCurve cur $ interval $ (s+t)/2 in
                if iup yy>iup yy' then m else m') (-1/0,-1/0) $
        (t0 cur,t0 cur):(t1 cur,t1 cur):(snd $ derivRoots 1e-5 cur)
  in
   (rnd x,rnd y)
\end{code}
