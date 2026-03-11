\begin{code}
{-# OPTIONS -XUnboxedTuples -cpp -XRecordWildCards -XNamedFieldPuns -XBangPatterns -XMagicHash -XScopedTypeVariables #-}
-- | This module contains the necessary calls to the other modules of Metafont'
-- to compute the outlines of a given number of pen strokes. The normal way of
-- using it is by calling 'outlines'. One other possible way would be :
--
-- @
-- let curves=cutAll curvesList in
-- remerge $ contour curves $ intersections curves
-- @

module Graphics.Typography.Geometry.Outlines (cutAll, intersections, contour, remerge, outlines) where

import Algebra.Polynomials.Bernstein
import Algebra.Polynomials.Numerical 
import Graphics.Typography.Geometry.Bezier
import Graphics.Typography.Geometry
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

import Control.Parallel

(!)::V.Vector a->Int->a
(!)=(V.!)

-- | Cuts a curve into a list of consecutive non-selfintersecting curves.
cutNoSelf::Curve->[Curve]
cutNoSelf c@(Circle{})=[c]
cutNoSelf bez@(Bezier{..})=
  let ix=intervalize cx
      dx=derivate ix
      solutions=
        sort $ filter (\(s,t)->(ilow $ eval ix (Interval s s))*
                               (iup $ eval ix (Interval t t)) <= 0) $
        solve 1e-10 (V.singleton dx) (t0,t1)
      roots lastU []=
        if lastU>=t1 then
          []
        else
          [bez { t0=lastU }]
      roots lastU (u:s)
        | u<=lastU = roots lastU s -- on ne coupe pas au debut
        | otherwise =
          (bez { t0=lastU, t1=u }):
          (roots u s)
  in
   roots t0 $ map (\(s,t)->(s+t)/2) solutions
  
cutNoSelf off@(Offset{..})= -- offset
  let thr=1e-2
      ix=intervalize cx
      iy=intervalize cy
      x'=derivate ix
      y'=derivate iy
      (Matrix2 a b c d)=intervalize matrix
      (Matrix2 a_ b_ c_ d_)=inverse $ intervalize matrix
                            
      xx'=(scale a_ x')+(scale b_ y')
      yy'=(scale c_ x')+(scale d_ y')
          
      xx''=derivate xx'
      yy''=derivate yy'
      
      evalC (t::Interval)=
        let norm=sqrt $ (eval xx' t)*(eval xx' t)+(eval yy' t)*(eval yy' t)
            derx=(eval yy'' t)/norm - 
                 ((eval yy' t)*((eval xx' t)*(eval xx'' t)+
                                (eval yy' t)*(eval yy'' t)))/(norm*norm*norm)
            dery=(eval xx'' t)/norm - 
                 ((eval xx' t)*((eval xx' t)*(eval xx'' t)+
                                (eval yy' t)*(eval yy'' t)))/(norm*norm*norm)
        in
         ((eval x' t)+(a*derx-b*dery), (eval y' t)+(c*derx-d*dery))
      
      zerosx=
        let verif t lastxx
              | t>=t1 = []
              | otherwise =
                let (xx,_)=evalC (Interval t t) in
                if (iup $ xx*lastxx)<=0 then
                  t:verif (t+thr) xx
                else
                  verif (t+thr) xx
                  
                  
                  
        in
         verif t0 $ fst $ evalC (Interval t0 t0)
             
      roots lastU []=
        if lastU>=t1 then
          []
        else
          [off { t0=lastU }]
      roots lastU (u:s)
        | u<=lastU = roots lastU s -- on ne coupe pas au debut
        | otherwise =
          (off { t0=lastU, t1=u }):
          (roots u s)
  in
   roots t0 zerosx

-- | @'cutAll' curves@ is the array of all the curves, cut such that
-- each part does not intersect itself.
cutAll::[[Curve]]->V.Vector (V.Vector Curve)
cutAll l=V.fromList $ map (\c->V.fromList $ concatMap cutNoSelf c) l


data Topology=Dedans | SurLaLigne | Dehors deriving (Eq, Ord, Show)

minsert::Ord a=>a->b->M.Map a [b]->M.Map a [b]
minsert x y m=M.insertWith (++) x [y] m

munion::Ord a=>M.Map a [b]->M.Map a [b]->M.Map a [b]
munion=M.unionWith (++)
  
  
mdeleteFindMin::Ord a=>M.Map a [b]->(Maybe (a,b),M.Map a [b])
mdeleteFindMin m=
  if M.null m then
    (Nothing, m)
  else
    let ((a,b),m')=M.deleteFindMin m in
    case b of
      []->mdeleteFindMin m'
      (h:s)->(Just (a,h), if null s then m' else M.insert a s m')


-- | Computes the intersections between any pair of curves given
-- as input, in parallel in GHC using @+RTS -N@.
intersections::V.Vector (V.Vector Curve)->
               M.Map (Int,Int,Double) [(Int,Int,Double,Double)]
intersections curves=
  let interAll ci cj
        | ci>=V.length curves = M.empty
        | cj>=V.length curves = interAll (ci+1) (ci+1)
        | otherwise = 
            -- traceShow (ci,i,cj,j) $
          let next=interAll ci (cj+1)
              inters
                | ci==cj =
                  V.ifoldl'
                  (\s0 i curvei->
                    V.ifoldl' 
                    (\s1 j curvej->
                      foldl (\s2 (ti,ti',tj,tj')->
                              minsert (ci,i,ti) (cj,j+i+1,tj,tj') $
                              minsert (cj,j+i+1,tj) (ci,i,ti,ti') $ s2) s1 $
                      inter curvei curvej
                    )
                    s0 $ V.drop (i+1) (curves!cj)
                  ) M.empty $ V.take (V.length (curves!ci)-1) (curves!ci)
                | otherwise = 
                  V.ifoldl'
                  (\s0 i curvei->
                    V.ifoldl'
                    (\s1 j curvej->
                      foldl (\s2 (ti,ti',tj,tj')->
                              minsert (ci,i,ti) (cj,j,tj,tj') $
                              minsert (ci,i,ti') (cj,j,tj,tj') $
                              minsert (cj,j,tj) (ci,i,ti,ti') $
                              minsert (cj,j,tj') (ci,i,ti,ti') $ s2) s1 $
                      inter curvei curvej
                    )
                    s0 (curves!cj)
                  ) M.empty $ V.take (V.length (curves!ci)-1) (curves!ci)
          in
           (next`par`inters)`seq`
           (next`munion`inters)
  in
   interAll 0 0
   
-- | 'contour' takes the curves and the intersections computed as in 'intersections',
-- and outputs a list of all simple closed paths defined by the curves in the input.
contour::V.Vector (V.Vector Curve)->
         M.Map (Int,Int,Double) [(Int,Int,Double,Double)]->
         [[(Int,Int,Double,Double)]]
contour curves inters0=
  
  let allPaths inters1 passages1=
        let (first,inters2)=mdeleteFindMin inters1 in
        case first of
          Nothing->[]
          Just ((ci0,i0,ti0),(cj0,j0,tj0a,tj0b))->
            --traceShow ("new path",pi0,pj0) $
            let walk ci i tia tib inters passages=
                  --traceShow ("point",ci,i,tia,tib) $ traceShow (inters) $
                  let (a,b)=M.split (ci,i,tib) inters
                      (next,b')=mdeleteFindMin b
                  in
                   case next of
                     Nothing-> -- traceShow ("echec 1") $
                              ([],a,passages)
                     Just ((ci',i',ti'),(cj,j,tja,tjb))
                       | ci==ci0 && i==i0 && (ci',i',ti')>=(ci,i,ti0)->
                         -- fin du chemin
                         ([(ci,i,tia,ti0)],a`munion`b',passages)
                     
                       | ci==ci' && i==i' ->
                         let isVisible=
                               let tt=(tia+ti')/2
                                   (xi,yi)=evalCurve (curves!ci!i) (Interval tt tt) 
                               in
                                V.foldl (\vis cur->
                                          vis && 
                                          iup (distance xi yi $ (cur!0) {t0=0,t1=1})>=1)
                                True curves
                         in
                          if (not isVisible) then
                            --traceShow ("invisible",pi') $
                            ([],a`munion`b',passages)
                          else
                           let alreadyPassed=
                                 let (_,p1)=M.split (ci,i,ti') passages in
                                 (not $ M.null p1) &&
                                 (let ((ci_,i_,_),ti'_)=M.findMin p1 in
                                   ci_==ci && i_==i && ti'_<=ti')
                           in
                            if alreadyPassed then
                              --traceShow ("already passed",pi') $
                              ([],a`munion`b',passages)
                            else
                              --traceShow ("trying",pi') $
                              let (nextPath,nextInters,nextPassages)=
                                    walk cj j tja tjb (a`munion`b') $
                                    M.insert (ci,i,ti') tia passages
                              in
                               if null nextPath then
                                 walk ci i tia tib (a`munion`b') passages
                               else
                                 ((ci,i,tia,ti'):nextPath,
                                  nextInters,
                                  M.insert (ci,i,ti') tia nextPassages)
                       | otherwise -> --traceShow ("echec 2",ci',i',ti') $
                           ([],inters,passages)
                                      
                (path,inters3,passages1')=walk cj0 j0 tj0a tj0b inters2 passages1
            in                       
             if null path then
               --traceShow ("abandon") $
               allPaths inters3 passages1'
             else
               --traceShow ("reussi") $
               path:(allPaths inters3 passages1')
  in
   allPaths inters0 M.empty
   
-- | 'remerge' takes the curves, the output of 'contour', and outputs
-- the list of "remerged" curves, i.e. where the parts free of self-intersections
-- are glued back to each other.
remerge::V.Vector (V.Vector Curve)->[(Int,Int,Double,Double)]->[Curve]
remerge _ []=[]
remerge curves [(ci,i,ti0,ti1)]=[(curves!ci!i) { t0=ti0,t1=ti1 }]
remerge curves (l@((ci,i,ti0,_):s))=
  
  let (cj,j,_,tj1)=last s in
  if ci==cj && j+1==i && tj1==ti0 then
    -- dans ce cas, le dernier est colle au premier
    let takeFirsts []=(# [],[] #)
        takeFirsts ((h@(ci',_,_,_)):ss)
          | ci'==ci = 
            let (# u,v #)=takeFirsts ss in
            (# h:u, v #)
          | otherwise = (# [],h:ss #)
        (# uu,vv #)=takeFirsts l
    in
     remerge_ $ vv++uu
  else
    remerge_ l
  
  where
    remerge_ []=[]
    remerge_ [(cj,j,tj0,tj1)]=[(curves!cj!j) { t0=tj0,t1=tj1 }]
    remerge_ ((cj,j,tj0,tj1):(cck@(ck,k,tk0,_)):ss)
      | cj==ck && k==j+1 && tj1==tk0 =
        let (h':s')=remerge_ $ cck:ss in
        (h' { t0=tj0 }) : s'
        
      | otherwise = 
          ((curves!cj!j) { t0=tj0,t1=tj1 }) : (remerge_ $ cck:ss)
    

-- | Takes a list of curves, potentially offset, and outputs the relevants part
-- of the outlines.
outlines::[[Curve]]->[[Curve]]
outlines curves=
  let curves'=cutAll curves in
  map (remerge curves') $ contour curves' $ intersections curves'

\end{code}
