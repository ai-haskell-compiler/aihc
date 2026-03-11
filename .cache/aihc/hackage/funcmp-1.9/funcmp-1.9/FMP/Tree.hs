{- |
   Module      :  FMP.Tree
   Copyright   :  (c) 2003-2010 Peter Simons
                  (c) 2002-2003 Ferenc Wágner
                  (c) 2002-2003 Meik Hellmund
                  (c) 1998-2002 Ralf Hinze
                  (c) 1998-2002 Joachim Korittky
                  (c) 1998-2002 Marco Kuhlmann
   License     :  GPLv3
   Maintainer  :  simons@cryp.to
   Stability   :  provisional
   Portability :  portable
 -}
{-
  This program is free software: you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free Software
  Foundation, either version 3 of the License, or (at your option) any later
  version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <http://www.gnu.org/licenses/>.
 -}

module FMP.Tree (
      number, Tree'(..),
      Tree(..), Edge(..), AlignSons(..), Distance(..),
      edge, edge', cross, cross', enode, node, stair,
      forEachNode, forEachLevelNode, forEachPic, forEachEdge,
      defaultAlign, alignLeft, alignRight, alignLeftSon, alignRightSon,
      alignOverN, alignAngles, alignConst, alignFunction,
      setAlign, getAlign, setDistH, getDistH, setDistV, getDistV,
      fit, fitLeft, fitRight,
      distCenter, distBorder,
      NodeName(..)
      ) where

import Prelude ( Read(..), Show(..), Eq(..), Num(..), Fractional(..), Int, Maybe(..)
               , otherwise, map, length, zip, unzip, init, take, (++), showString, tail
               , foldl, foldr, fst, snd, Bool(..), drop, fromIntegral, (.), concat, reverse
               , Ord(..)
               )
import FMP.Types
import FMP.Color
import FMP.Picture

instance (Read a,Read b) => Read (a -> b) where
      readsPrec _ _           =  []

instance (Eq a,Eq b) => Eq (a -> b) where
      _ == _                  =  False

instance (Num a, Num b) => Num (a, b) where
      (a1, b1) + (a2, b2)     =  (a1 + a2, b1 + b2)
      (a1, b1) - (a2, b2)     =  (a1 - a2, b1 - b2)
      (a1, b1) * (a2, b2)     =  (a1 * a2, b1 * b2)
      negate (a, b)           =  (negate a, negate b)
      abs a                   =  a
      signum _                =  (1, 1)
      fromInteger i           =  (fromInteger i, fromInteger i)


----------------------------------------------------------

stair                         :: Point -> Point -> Path
stair p1 p2                   =  p1 .-. p1 + vec (0, 0.5*ydist p2 p1)
                                 .-. p2 - vec (0, 0.5*ydist p2 p1) .-. p2

edge                          :: Tree -> Edge
edge t                        =  edge' (ref (This <+ C) ... ref (Parent <+ C)) t

edge'                         :: Path -> Tree -> Edge
edge'                         =  Edge

cross                         :: Point -> Edge
cross p                       =  cross' (ref (This <+ C) ... p)

cross'                        :: Path -> Edge
cross'                        =  Cross

enode                         :: IsPicture a => a -> [Edge] -> Edge
enode p ts                    =  edge (node p ts)

node                          :: IsPicture a => a -> [Edge] -> Tree
node p ts                     =  Node (toPicture p) stdNodeDescr ts


defaultAlign, alignLeft, alignRight, alignLeftSon, alignRightSon :: AlignSons
defaultAlign                  =  DefaultAlign
alignLeft                     =  AlignLeft
alignRight                    =  AlignRight
alignLeftSon                  =  AlignLeftSon
alignRightSon                 =  AlignRightSon

alignOverN                    :: Int -> AlignSons
alignOverN                    =  AlignOverN

alignAngles                   :: [Numeric] -> AlignSons
alignAngles                   =  AlignAngles

alignConst                    :: Numeric -> AlignSons
alignConst                    =  AlignConst

alignFunction                 :: (NodeDescr -> [Extent] -> Int -> [Numeric]) -> AlignSons
alignFunction                 =  AlignFunction

setAlign                      :: AlignSons -> Tree -> Tree
setAlign align (Node a nd ts) =  Node a nd{nAlignSons = align} ts

getAlign                      :: Tree -> AlignSons
getAlign (Node _ nd _)        =  nAlignSons nd

setDistH                      :: Distance -> Tree -> Tree
setDistH sh (Node a nd ts)    =  Node a nd{nDistH = sh } ts

getDistH                      :: Tree -> Distance
getDistH (Node _ nd _)        =  nDistH nd

setDistV                      :: Distance -> Tree -> Tree
setDistV sh (Node a nd ts)    =  Node a nd{nDistV = sh } ts

getDistV                      :: Tree -> Distance
getDistV (Node _ nd _)        =  nDistV nd

distCenter                    :: Numeric -> Distance
distCenter                    =  DistCenter

distBorder                    :: Numeric -> Distance
distBorder                    =  DistBorder

----------------------------------------------------------

instance HasColor Edge where
      setColor c (Edge e ts)  =  Edge (setColor c e) ts
      setColor c (Cross e)    =  Cross (setColor c e)
      setDefaultColor (Edge e ts)
                              =  Edge (setDefaultColor e) ts
      setDefaultColor  (Cross e)
                              =  Cross (setDefaultColor e)
      getColor (Edge e _)     =  getColor e
      getColor (Cross e)      =  getColor e

instance HasLabel Edge where
      setLabel l i o (Edge e ts)
                              =  Edge (setLabel l i o e) ts
      setLabel l i o (Cross e)
                              =  Cross (setLabel l i o e)
      removeLabel (Edge e ts) =  Edge (removeLabel e) ts
      removeLabel (Cross e)   =  Cross (removeLabel e)

instance HasPattern Edge where
      setPattern pat (Edge e ts)
                              =  Edge (setPattern pat e) ts
      setPattern pat (Cross e)
                              =  Cross (setPattern pat e)
      setDefaultPattern (Edge e ts)
                              =  Edge (setDefaultPattern e) ts
      setDefaultPattern (Cross e)
                              =  Cross (setDefaultPattern e)
      getPattern (Edge e _)   =  getPattern e
      getPattern (Cross e)    =  getPattern e

instance HasArrowHead Edge where
      setArrowHead ar (Edge e ts)
                              =  Edge (setArrowHead ar e) ts
      setArrowHead ar  (Cross e)
                              =  Cross (setArrowHead ar e)
      removeArrowHead (Edge e ts)
                              =  Edge (removeArrowHead e) ts
      removeArrowHead  (Cross e)
                              =  Cross (removeArrowHead e)
      getArrowHead (Edge e _) =  getArrowHead e
      getArrowHead (Cross e)  =  getArrowHead e
      setStartArrowHead ar (Edge e ts)
                              =  Edge (setStartArrowHead ar e) ts
      setStartArrowHead ar (Cross e)
                              =  Cross (setStartArrowHead ar e)
      removeStartArrowHead (Edge e ts)
                              =  Edge (removeStartArrowHead e) ts
      removeStartArrowHead (Cross e)
                              =  Cross (removeStartArrowHead e)
      getStartArrowHead (Edge e _)
                              =  getStartArrowHead e
      getStartArrowHead (Cross e)
                              =  getStartArrowHead e

instance HasPen Edge where
      setPen pen (Edge e ts)  =  Edge (setPen pen e) ts
      setPen pen (Cross e)    =  Cross (setPen pen e)
      setDefaultPen  (Edge e ts)
                              =  Edge (setDefaultPen e) ts
      setDefaultPen (Cross e)
                              =  Cross (setDefaultPen e)
      getPen (Edge e _)       =  getPen e
      getPen (Cross e)        =  getPen e

instance HasStartEndDir Edge where
      setStartAngle a (Edge e ts)
                              =  Edge (setStartAngle a e) ts
      setStartAngle a  (Cross e)
                              =  Cross (setStartAngle a e)
      setEndAngle a  (Edge e ts)
                              =  Edge (setEndAngle a e) ts
      setEndAngle a (Cross e) =  Cross (setEndAngle a e)
      setStartCurl a (Edge e ts)
                              =  Edge (setStartCurl a e) ts
      setStartCurl a (Cross e)=  Cross (setStartCurl a e)
      setEndCurl a (Edge e ts)=  Edge (setEndCurl a e) ts
      setEndCurl a (Cross e)  =  Cross (setEndCurl a e)
      setStartVector a (Edge e ts)
                              =  Edge (setStartVector a e) ts
      setStartVector a (Cross e)
                              =  Cross (setStartVector a e)
      setEndVector a (Edge e ts)
                              =  Edge (setEndVector a e) ts
      setEndVector a (Cross e)=  Cross (setEndVector a e)
      removeStartDir (Edge e ts)
                              =  Edge (removeStartDir e) ts
      removeStartDir (Cross e)=  Cross (removeStartDir e)
      removeEndDir (Edge e ts)=  Edge (removeEndDir e) ts
      removeEndDir (Cross e)  =  Cross (removeEndDir e)

instance IsHideable Edge where
      hide (Edge e ts)        =  Edge (hide e) ts
      hide (Cross e)          =  Cross (hide e)

instance HasName Tree where
      setName n (Node p nd es)=  Node (setName n p) nd es
      getNames (Node p _ _)   =  getNames p

instance HasColor Tree where
      setColor c (Node p nd es )
                              =  Node (setColor c p) nd es
      setDefaultColor t       =  setColor default' t
      getColor (Node p _ _)   =  getColor p

----------------------------------------------------------

data Tree                     =  Node Picture NodeDescr [Edge]
                                 deriving Show

data Edge                     =  Edge Path Tree
                              |  Cross Path
                                 deriving Show

data AlignSons                =  DefaultAlign
                              |  AlignLeft
                              |  AlignRight
                              |  AlignLeftSon
                              |  AlignRightSon
                              |  AlignOverN Int
                              |  AlignAngles [Numeric]
                              |  AlignConst Numeric
                              |  AlignFunction (NodeDescr -> [Extent] -> Int -> [Numeric])
                                 deriving Show

instance Show (a -> b) where  -- RH
    showsPrec _ _             =  showString "<function>"

data Distance                 =  DistCenter Numeric
                              |  DistBorder Numeric
                                 deriving (Eq, Show)

instance Num Distance where
      (DistBorder a)  + (DistBorder b)
                              =  DistBorder (a + b)
      (DistCenter a)  + (DistCenter b)
                              =  DistCenter (a + b)
      a               + _     =  a
      (DistBorder a)  - (DistBorder b)
                              =  DistBorder (a - b)
      (DistCenter a)  - (DistCenter b)
                              =  DistCenter (a - b)
      a               - _     =  a
      (DistBorder a)  * (DistBorder b)
                              =  DistBorder (a * b)
      (DistCenter a)  * (DistCenter b)
                              =  DistCenter (a * b)
      a               * _     =  a
      negate (DistBorder a)   =  DistBorder (-a)
      negate (DistCenter a)   =  DistCenter (-a)
      abs (DistBorder a)      =  DistBorder (abs a)
      abs (DistCenter a)      =  DistCenter (abs a)
      signum a                =  a
      fromInteger             =  DistBorder . fromInteger

instance Fractional Distance where
      (DistBorder a)  / (DistBorder b)
                              =  DistBorder (a / b)
      (DistCenter a)  / (DistCenter b)
                              =  DistCenter (a / b)
      a               / _     =  a
      recip (DistBorder a)    =  DistBorder (1 / a)
      recip (DistCenter a)    =  DistCenter (1 / a)
      fromRational            =  DistBorder . fromRational


data NodeDescr                =  NodeDescr {  nEdges          :: [Path],
                                              nAlignSons      :: AlignSons,
                                              nDistH, nDistV  :: Distance }
                                 deriving Show

stdNodeDescr                  :: NodeDescr
stdNodeDescr                  =  NodeDescr {  nEdges          = [],
                                              nAlignSons      = DefaultAlign,
                                              nDistH          = 8,
                                              nDistV          = 10 }


-- Interne Baumstruktur.

data Tree' a                  =  Node' a NodeDescr [Tree' a]

-- Baum inorder durchnumerieren. (Vater, This, Tiefe)

number                        :: Tree -> (Tree' (Int, Int, Int))
number t                      =  snd (traverse (-1) 0 0 t [])
    where
    traverse j k l (Node _ nd ts) pe
                              = (k', Node' (j, k, l) nd{nEdges = edges ts} nts)
       where
       edges []               =  pe
       edges  (Edge _ _:es)   =  edges es
       edges  (Cross e:es)    =  e:edges es
       sons []                =  []
       sons (Edge e s:es)     =  (e,s):sons es
       sons (_:es)            =  sons es
       (k', nts)              = traverses k (k+1) (l+1) (sons ts)
       traverses _ k _ []     = (k, [])
       traverses j k l ((e,t): ts)
                              = (k'', nt : nts)
           where
           (k',  nt)          =  traverse j k l t [e]
           (k'', nts)         =  traverses j k' l ts

-- Liste der Knotenbilder
-- toDo: effizienter

extractPics                   :: Tree -> [Picture]
extractPics (Node p _ ts)     =  p:pics ts
     where
     pics []                  =  []
     pics (Edge _ t:es)       =  extractPics t ++ pics es
     pics (_:es)              =  pics es

-- Relative Plazierung der Punkte

relPlacements                 :: Tree' (Int, Int, Int) -> [Equation]
relPlacements (Node' (a, b, l) nd ts)
                              =  [case nDistV nd of
                                      DistBorder v -> ref (b <* C) .= ref (a <* C)
                                                              + vec(hoff b, voff l-v)
                                      DistCenter v -> ref (b <* C) .= ref (a <* C)
                                                              + vec(hoff b, -v)]
                                 & map (equations.relPlacements) ts

-- Berechne Pfade aller Baumkanten

data NodeName                 =  Parent | This | Root | Up Int | Son Int
                                 deriving Show

instance IsName NodeName where
      toName a                =  toName (show a)

edges                         :: [Int] -> Tree' (Int, Int, Int) -> [Path]
edges path (Node' (a,b,_) nd ts)
                              =  [replacePath edge aliases | edge <- nEdges nd]
                              ++ concat (map (edges (b:path)) ts)
              where
              aliases         =  [(toName Parent,     toName a),
                                  (toName This,               toName b),
                                  (toName Root,               toName (0::Int))]
                              ++ [(toName (Up n),     toName u)| (u,n) <- zip (b:path) [0..]]
                              ++ [(toName (Son n),    toName s)
                                 |(Node' (_,s,_) _ _,n)  <- zip ts [0..]]


instance IsPicture Tree where
    toPicture t               =  draw edgePaths
                                    (overlay (widthsL & widthsR
                                              & heightsTop & heightsBot
                                              & voffs & hoffs
                                              & placements)
                                             (enumPics nodePics))
              where
              widthsL         =  [ widthL i .= xpart (ref (i <+ W)- ref (i <+ C))
                                 | i <- [0..length nodePics-1]]
              widthsR         =  [ widthR i .= xpart (ref (i <+ E)- ref (i <+ C))
                                 | i <- [0..length nodePics-1]]
              heightsTop      =  [ heightT l .= maximum' (map heightTop ns)
                                 | (ns,l) <- zip (levels nt) [1..]]
              heightsBot      =  [ heightB l .= maximum' (map heightBot ns)
                                 | (ns,l) <- zip (levels nt) [1..]]
              voffs           =  [ voff l .= - heightT (l+1) - heightB l
                                 | l <- (tail [0..length (levels nt)-1])]
              heightTop n     =  ypart (ref (n <+ N) - ref (n <+ C ))
              heightBot n     =  ypart (ref (n <+ C) - ref (n <+ S ))

              nt              =  number t
              hoffs           =  design nt
              placements      =  tail (relPlacements nt)
              nodePics        =  extractPics t
              edgePaths       =  edges [] nt

levels                        :: Tree' (a,b,c) -> [[b]]
levels (Node' (_,a,_) _ [])   =  [[a]]
levels (Node' (_,a,_) _ ts)   =  [a] : foldl zipLists [] (map levels ts)
      where
      zipLists [] l           =  l
      zipLists l []           =  l
      zipLists (l:ls) (l':ls')=  (l ++ l'):zipLists ls ls'

-- Wie zip, nur da"s in zwei Listen von Listen jeweils die ersten, die
-- zweiten, usw. Listen konkateniert werden.
-- Nicht gleich zipWith (++)


getHEqs                 :: Tree' [Equation] -> [Equation]
getHEqs (Node' eqs _ ts)        =  map (equations.getHEqs) ts & eqs

hoff, voff,widthL,widthR,heightT,heightB      :: Int -> Numeric
hoff i                        =  var (6*i)
voff i                        =  var (6*i+1)
widthL i                      =  var (6*i+2)
widthR i                      =  var (6*i+3)
heightT i                     =  var (6*i+4)
heightB i                     =  var (6*i+5)

-- Design eines Baums

design                        :: Tree' (Int, Int, Int) -> [Equation]
design t                      =  fst (design' t)

design'                       :: Tree' (Int, Int, Int) -> ([Equation], Extent)
design' (Node' (_,m,l) nd ts) =  (foldl (&) [] designedTrees & eqs,
                                  topExtent (nDistH nd) : mergedExtent)
      where
      (designedTrees, es)     =  unzip [ design' t| t <- ts ]
      relPositions            =  calcPos nd es l
      mergedExtent            =  mergeMany [ moveExtent h e | (h, e) <- zip hoffVars es ]
      eqs                     =  [ h .= rp    | (h, rp) <- zip hoffVars relPositions]
      hoffVars                =  [ hoff m     | Node' (_, m, _) _ _ <- ts]
      topExtent (DistBorder _)
                              =  (widthL m, widthR m)
      topExtent _             =  (0, 0)


-- Transliteriert von Andrew J. Kennedy's "Drawing Tree's".

type Position                 =  Numeric
type Extent                   =  [(Position, Position)]

-- Position relativ zum Elternknoten.
--
-- moveTree                        :: Position -> PositionedTree a -> PositionedTree a
-- moveTree d (Node' (a,p) as ts)=  Node' (a, p ) as ts

-- Absolute Positionen.

moveExtent                    :: Position -> Extent -> Extent
moveExtent x                  =  map (+ (x, x))

merge                         ::  Extent -> Extent -> Extent
merge [] qs                   =  qs
merge ps []                   =  ps
merge ((l, _):ps) ((_, r):qs) =  (l, r) : merge ps qs

mergeMany                     :: [Extent] -> Extent
mergeMany                     =  foldr merge []


-- Zusammenpassen von Extents


fit                           :: Numeric -> Extent -> Extent -> Position
fit hDist ps qs               =  maximum' dists
     where dists              =  [ r - l + hDist | ((_, r), (l, _)) <- zip ps qs]

fitLeft                       :: Numeric -> [Extent] -> [Position]
fitLeft hDist es              =  traverse hDist [] es
    where
    traverse                  :: Numeric -> Extent -> [Extent] -> [Position]
    traverse _ _ []           =  []
    traverse hDist acc (e:es) =  x:traverse hDist (merge acc (moveExtent x e)) es
        where x               =  fit hDist acc e

fitRight                      :: Numeric -> [Extent] -> [Position]
fitRight hDist es             =  reverse (traverse hDist [] (reverse es))
    where
    traverse                  :: Numeric -> Extent -> [Extent] -> [Position]
    traverse _ _ []           =  []
    traverse hDist acc (e:es) =  x:traverse hDist (merge (moveExtent x e) acc) es
        where x               =  -fit hDist e acc

fitMany                       :: Numeric -> [Extent] -> [Position]
fitMany hDist es              =  [ (x + y) / 2
                                 | (x, y) <- zip (fitLeft hDist es) (fitRight hDist es) ]

getHDist                      :: NodeDescr -> Numeric
getHDist nd                   =  case nDistH nd of
                                      DistCenter h -> h
                                      DistBorder h -> h


getVDist                      :: NodeDescr -> Numeric
getVDist nd                   =  case nDistV nd of
                                      DistCenter v -> v
                                      DistBorder v -> v

calcPos                       :: NodeDescr -> [Extent] -> Int -> [Position]
calcPos nd es l               =  calcPos' (nAlignSons nd) nd es l

calcPos'                      :: AlignSons -> NodeDescr -> [Extent] -> Int -> [Position]
calcPos' DefaultAlign nd es _ =  fitMany (getHDist nd) es
calcPos' AlignLeft    nd es _ =  fitLeft (getHDist nd) es
calcPos' AlignRight   nd es _ =  fitRight (getHDist nd) es
calcPos' AlignLeftSon nd [((l,_ ):_)] _
                              =  [l-0.5*getHDist nd]
calcPos' AlignLeftSon nd es _ =  fitMany (getHDist nd) es
calcPos' AlignRightSon        nd [((_,r):_)] _
                              =  [r+0.5*getHDist nd]
calcPos' AlignRightSon nd es _
                              =  fitMany (getHDist nd) es
calcPos'(AlignOverN n) nd es _
                              =  init (fitRight (getHDist nd) (take n es))
                                 ++ 0:tail (fitLeft (getHDist nd) (drop (n-1) es))
calcPos' (AlignAngles ds) nd es h
                              =  take (length es) (calcOffsets ds
                                                   ++ fitLeft (getHDist nd)
                                                              (drop (length ds) es))
      where
      calcOffsets []          =  []
      calcOffsets (d:ds)      =  offset d:calcOffsets ds
      offset d                =  (voff (h+1)-getVDist nd)*cos d / sin d
calcPos' (AlignConst x) _ es _
                              =  fitConst (fromIntegral (length es-1)) x
              where
              fitConst n x    =  [-n/2*x, -n/2*x+x .. n/2*x]
calcPos' (AlignFunction f) nd es h
                              =  f nd es h

forEachNode                   :: (Tree -> Tree) -> Tree -> Tree
forEachNode f (Node a nd ts)  =  f (Node a nd (map (edge f) ts))
      where
      edge f (Edge e t)       =  Edge e (forEachNode f t)
      edge _ e                =  e

forEachLevelNode              :: Int -> (Tree -> Tree) -> Tree -> Tree
forEachLevelNode l f (Node a nd es)
              | l <  0        =  Node a nd es
              | l == 0        =  f (Node a nd es)
              | otherwise     =  Node a nd (map (edge (l-1) f) es)
      where
      edge f l (Edge e t)     =  Edge e (forEachLevelNode f l t)
      edge _ _ e              =  e

forEachPic                    :: (Picture -> Picture) -> Tree -> Tree
forEachPic f (Node a nd es)   =  Node (f a) nd (map (edge f) es)
      where
      edge f (Edge e t)       =  Edge e (forEachPic f t)
      edge _ e                =  e

forEachEdge                   :: (Path -> Path) -> Tree -> Tree
forEachEdge f (Node a nd ts)  =  Node a nd (map (edge f) ts)
      where
      edge f (Edge e t)       =  Edge (f e) (forEachEdge f t)
      edge f (Cross e)        =  Cross (f e)


-----------------------------------------------------------------
-----------------------------------------------------------------

replacePath                   :: Path -> [(Name, Name)] -> Path
replacePath (PathPoint p) al  =  PathPoint (replacePoint p al)
replacePath PathCycle _       =  PathCycle
replacePath (PathJoin p1 pj p2 ) al
                              =  PathJoin     (replacePath p1 al)
                                              (replacePathElemDescr pj al)
                                              (replacePath p2 al)
replacePath (PathEndDir p d) al
                              =  PathEndDir   (replacePoint p al) (replaceDir' d al)
replacePath (PathBuildCycle p1 p2) al
                              =  PathBuildCycle       (replacePath p1 al) (replacePath p2 al)
replacePath (PathTransform t p ) al
                              =  PathTransform t (replacePath p al)
replacePath (PathDefine eqs p) al
                              =  PathDefine   (replaceEquations eqs al)
                                              (replacePath p al)

replacePathElemDescr          :: PathElemDescr -> [(Name, Name)] -> PathElemDescr
replacePathElemDescr ped al   =  ped {peStartCut = case peStartCut ped of
                                              Just a  -> Just (replaceCutPic a al)
                                              a       -> a,
                                      peEndCut = case peEndCut ped of
                                              Just a  -> Just (replaceCutPic a al)
                                              a       -> a,
                                      peStartDir = replaceDir' (peStartDir ped) al,
                                      peEndDir = replaceDir' (peEndDir ped) al}

replaceDir'                   :: Dir' -> [(Name, Name)] -> Dir'
replaceDir' (DirCurl a) al    =  DirCurl      (replaceNumeric a al)
replaceDir' (DirDir a) al     =  DirDir       (replaceNumeric a al)
replaceDir' (DirVector a) al  =  DirVector    (replacePoint a al)
replaceDir' a _               =  a

replaceCutPic                 :: CutPic -> [(Name, Name)] -> CutPic
replaceCutPic (CutPic name) al=  CutPic (replaceName name al)
replaceCutPic c _             =  c

replacePoint                  :: Point -> [(Name, Name)] -> Point
replacePoint (PointVar name) al
                              =  PointVar (replaceName name al)
replacePoint (PointPPP c a b) al
                              =  PointPPP c   (replacePoint a al) (replacePoint b al)
replacePoint (PointVec (a, b)) al
                              =  PointVec     (replaceNumeric a al, replaceNumeric b al)
replacePoint (PointMediate a b c) al
                              =  PointMediate (replaceNumeric a al)
                                              (replacePoint b al)
                                              (replacePoint c al)
replacePoint (PointDirection a ) al
                              =  PointDirection (replaceNumeric a al)
replacePoint (PointNeg a) al  =  PointNeg (replacePoint a al)
replacePoint a _              =  a

replaceName                   :: Name -> [(Name, Name)] -> Name
replaceName (Hier n n') al    =  Hier (replaceName n al) (replaceName n' al)
replaceName n al              =  replaceName' n al
      where
      replaceName' n []       =  n
      replaceName' n ((n',r):al)
              | n == n'       =  r
              | otherwise     =  replaceName' n al


replaceNumeric                :: Numeric -> [(Name, Name)] -> Numeric
replaceNumeric (NumericVar a) al
                              =  NumericVar (replaceName a al)
replaceNumeric (NumericDist a b) al
                              =  NumericDist  (replacePoint a al) (replacePoint b al)
replaceNumeric (NumericMediate a b c) al
                              =  NumericMediate       (replaceNumeric a al)
                                                      (replaceNumeric b al)
                                                      (replaceNumeric c al)
replaceNumeric (NumericPN c a) al
                              =  NumericPN c (replacePoint a al)
replaceNumeric (NumericNN c a) al
                              =  NumericNN c (replaceNumeric a al)
replaceNumeric (NumericNNN c a b) al
                              =  NumericNNN c (replaceNumeric a al) (replaceNumeric b al)
replaceNumeric (NumericNsN c as) al
                              =  NumericNsN c (map (\a -> replaceNumeric a al) as)
replaceNumeric a _            =  a

replaceEquations              :: [Equation] -> [(Name,Name)] -> [Equation]
replaceEquations eqs al       =  map (\a -> replaceEquation a al) eqs

replaceEquation               :: Equation -> [(Name,Name)] -> Equation
replaceEquation (PEquations ps) al
                              =  PEquations (map (\a -> replacePoint a al) ps)
replaceEquation (NEquations ns) al
                              =  NEquations (map (\a -> replaceNumeric a al) ns)
replaceEquation (EquationCond b e1 e2) al
                              =  EquationCond (replaceBoolean b al) (replaceEquation e1 al)
                                              (replaceEquation e2 al)
replaceEquation (Equations eqs) al
                              =  Equations (replaceEquations eqs al)

replaceBoolean                :: Boolean -> [(Name,Name)] -> Boolean
replaceBoolean (BoolNum a c b) al
                              =  BoolNum (replaceNumeric a al) c (replaceNumeric b al)
replaceBoolean (BoolPnt a c b) al
                              =  BoolPnt (replacePoint a al) c (replacePoint b al)
replaceBoolean (BoolOr a b) al=  BoolOr (replaceBoolean a al) (replaceBoolean b al)
replaceBoolean (BoolAnd a b) al
                              =  BoolAnd (replaceBoolean a al) (replaceBoolean b al)
replaceBoolean (BoolNot a) al =  BoolNot (replaceBoolean a al)
replaceBoolean a _            =  a
