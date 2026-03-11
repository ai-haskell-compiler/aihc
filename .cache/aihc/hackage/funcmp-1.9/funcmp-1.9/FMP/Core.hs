{- |
   Module      :  FMP.Core
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

module FMP.Core (
      mp
      ) where

import FMP.Types
import FMP.Syntax
import FMP.Symbols
import FMP.Term
import FMP.Color
import FMP.Syntax
import FMP.Picture
import FMP.Resolve

-- Hilfsfunktionen
--
-- ( wferi ?
--
-- It would be interesting to know what these types contain.  The |Int|
-- and the |Symbols| probably represent some kind of state.  It seems
-- that every invocation of |mp| must have different |n| parameter,
-- probably for suffixing its local variables.
--
-- wferi )

type MPArg                    =  (Int, Symbols)
type MPResult                 =  (Int, Symbols, MetaPost, MetaPost)

drawBC                        :: Attrib -> Int -> MetaPost
drawBC a n                    =  case aBGColor a of
                                      DefaultColor -> relax
                                      Graduate c1 c2 a n'
                                        -> MPGraduate (mpColor c1) (mpColor c2)
                                                      (MPBPath (Id (suff n))) n' a
                                      a -> MPFill (MPBPath (Id (suff n)))
                                                  (mpColor a)
                                                  MPDefaultPen

drawFrameBC                   :: Symbols -> FrameAttrib -> Int -> MetaPost
drawFrameBC d fa n            =  case bgColor of
                                      DefaultColor -> mpShadow
                                      Graduate c1 c2 a n'
                                              -> MPGraduate   (mpColor c1)
                                                              (mpColor c2)
                                                              (MPBPath (Id (suff n))) n' a
                                      _       -> mpShadow
                                                 & MPFill (MPBPath (Id (suff n)))
                                                          (mpColor bgColor)
                                                          MPDefaultPen
              where
              bgColor         =  faBGColor fa
              (sx,sy)         =  getDefault (faShadow fa) (0, 0)
              mpShadow        =  if sx==0 && sy == 0
                                      then relax
                                      else MPFill (MPShiftedP
                                                      (pair sx' sy')
                                                      (MPBPath (Id (suff n))))
                                              (mpColor black)
                                              MPDefaultPen
              sx'             =  maybe 0 mpNumeric (resolveNumeric (n, d) sx)
              sy'             =  maybe 0 mpNumeric (resolveNumeric (n, d) sy)

drawBorder                    :: FrameAttrib -> Int -> MetaPost
drawBorder fa n               =  if faVisible fa
                                      then case fgColor of
                                      Graduate c1 c2 a n'
                                              -> MPGraduatePath MPNormal
                                                      (mpColor c1) (mpColor c2)
                                                      (MPBPath (Id (suff n)))
                                                      (mpPattern patt) (mpPen pen)
                                                      n' a
                                      _       -> MPDraw MPNormal (MPBPath (Id (suff n)))
                                                      (mpPattern patt) (mpColor fgColor)
                                                      (mpPen pen)
                                      else relax
              where
              fgColor         =  faColor fa
              pen             =  faPen fa
              patt            =  faPattern fa

-- Erzeugen von MetaPost
--
-- ( wferi
--
-- Here we do the real work.  |mp| gives the |MetaPost| equivalents of
-- all constructors of |Picture|.
--
-- The meanings of the arguments and result values are not clear.  From
-- |generate| we get |(1,relax)::MPArg|, the first two members of the
-- result are ignored, and the last two make up the MetaPost code.
--
-- wferi )

mp                            :: Picture -> MPArg -> MPResult
mp (Attributes as p) (n, symDown)
                              =  (n', symNames (aNames as) n symUp', l',
                                  case aColor as of
                                  DefaultColor -> drawBC as n & z'
                                  Graduate c1 c2 a num
                                      -> MPImage rememberPic z' & drawBC as n
                                       & MPGraduatePic (mpColor c1) (mpColor c2)
                                                       (Id rememberPic) num a
                                  c   -> MPImage rememberPic z' & drawBC as n
                                       & MPDrawPic (mpColor c) (Id rememberPic))
      where
      (n', symUp', l', z')    =  mp p (n, symDown)
      rememberPic             =  "r"++show n

mp (Overlay _ _ []) ns        =  mp (Empty 0 0) ns
mp (Overlay eqs bbox ps) (n, symDown)
                              =  (n', newSym & symUp',
                                  l' & mpEquations (maybes2List eqs') & newBBox bbox,
                                  z')
   where
   (n':ns',symSons',l',z')    =  foldr (\j i-> mergeResult (mp j (params i)) i)
                                      ([n+1], [], relax, relax) ps
   mergeResult (n, sym, l, z)  (ns', syms', l', z')
                              =  (n:ns', sym:syms', l & l', z & z')
   params (n:_,_,_,_)         =  (n, symUp' & symDown)
   params ([],_,_,_)          =  (n, symUp' & symDown)
   mapping                    =  zip3 [0..] ns' symSons'
   symUp'                     =  symUnions symSons'
   symLocalNames              =  symUnion3
                                      (symUnions [ addPDef (SymPName (toName n) m 0)
                                                           relax
                                                 | (n, m, _) <- mapping ])
                                      (symUnions [ symHier n m sym
                                                 | (n, m, sym) <- mapping ])
                                      symUp'
   newSym                     =  snd (symEquations (n, 0, relax) eqs)
   eqs'                       =  map (resolveEquation (n, sym)) eqs
              where sym       =  newSym & symLocalNames & symDown
   newBBox Nothing            =  MPBoxit (suff n) relax
                                & MPEquals [tdot (suff n) SW,
                                            pair (Min [XPart (tdot (suff n) W)| n <- ns'])
                                                 (Min [YPart (tdot (suff n) S)| n <- ns'])]
                                & MPEquals [tdot (suff n) NE,
                                            pair (Max [XPart (tdot (suff n) E)| n <- ns'])
                                                 (Max [YPart(tdot (suff n) N)| n <- ns'])]
                                & MPFixSize [suff n]
   newBBox (Just b)           =  let nBBox = head ([ n | (m,n,_) <- mapping, m==b]++[0])
                                 in MPCloneit (suff n) (suff nBBox)

mp (Define eqs p) (n, symDown)
                              =  (n', symUp',
                                  mpEquations (maybes2List eqs') & l'
                                 & MPCloneit (suff n) (suff (n+1)), z')
      where
      (n', symUp', l', z')    =  mp p (n+1, newSym & symDown)
      newSym                  =  snd (symEquations (n, 0, relax) eqs)
      eqs'                    =  map (resolveEquation (n, sym)) eqs
              where sym       =  newSym & symUp' & symDown

mp (Frame fa eqs path p) (n, symDown)
                              =  (n', symNames (faNames fa) n symUp',
                                  l' & MPShapeit (suff n) & mpEquations eqs' & assignPath,
                                  drawFrameBC sym fa n & z' & drawBorder fa n)
      where
      (n', symUp', l', z')    =  mp p (n+1, relax)
      sym                     =  newSym & symUp' & symDown
      newSym                  =  snd (symEquations (n, 0, relax) eqs)
      eqs'                    =  maybes2List (map (resolveEquation (n, sym)) eqs)
      assignPath              =  case resolvePath (n, 0, sym) path of
                                      Nothing -> relax
                                      Just (_, p')-> MPAssign (Id (suff n++".p"))
                                              (Id $ show $ show $ emit $ mpPath p')

mp (Draw ls p) (n, symDown)   =  (n'', symUp'' & symUp',
                                  l' & l'' & MPCloneit (suff n) (suff (n+1)),
                                  z' & z'')
  where
  nThis                       =  n
  (n', symUp', l', z')        =  mp p (n+1, symDown)
  (n'', _, symUp'', l'', z'') =  paths ls n' 0
  sym                         =  symUp' &  symUnion3 relax relax symDown
  paths [] n m                =  (n, m, relax, relax, relax)
  paths (l:ls) n m            =  (n'',m'', symUp2' & symUp2'', l' & l'', z' & z'')
    where
    (n', m',symUp2',l',z')    =  constructPath sym l n nThis m
    (n'',m'',symUp2'',l'',z'')=  paths ls n' m'

mp (Fill ars p) (n, symDown)  =  if preFill == relax
                                      then (n',
                                            symUp',
                                            l' & l'' & MPCloneit (suff n) (suff (n+1)),
                                            z' & postFill)
                                      else (n',
                                            symTrans symUp' n,
                                            defShift & l' & l'' & MPSubBox n z',
                                            preFill & MPDrawUnBoxed [suff n] & postFill)
      where
      (n', symUp', l', z')    =  mp p (n+1, symDown)
      defShift                =  MPDef (tr n) (Shifted Identity
                                       (LLCorner (Pic (suff n)) - shiftRefPoint n))
      sym                     =  if and [getLayer a == Front | a <- ars]
                                      then symUp' & symUnion3 relax relax symDown
                                      else symTrans symUp' n & symUnion3 relax relax symDown
      (_,l'',preFill,postFill)=  fillCommands 0 ars
      fillCommands            :: Int -> [Area] -> (Int, MetaPost, MetaPost, MetaPost)
      fillCommands m []       =  (m, relax, relax, relax)
      fillCommands m (Area ad ps:ars)
                              =  if arLayer ad == Back
                                      then ( m'', l' & l'', fill & pre, post )
                                      else ( m'', l' & l'', pre, fill & post )
         where
         (m', l',fill)        =  fillCommand m ad ps
         (m'', l'', pre, post)=  fillCommands m' ars
      fillCommand             :: Int -> AreaDescr -> Path -> (Int, MetaPost, MetaPost)
      fillCommand m ad ps     =  case resolvePath (n, m, sym)  ps of
                                      Just (m', PathJoin (PathPoint p) _ PathCycle)
                                              -> ( m', relax,
                                                   MPDraw MPNormal
                                                          (MPPathTerm (mpPoint p))
                                                          MPDefaultPattern
                                                          (mpColor (arColor ad))
                                                          (mpPen (arPen ad)))
                                      Just (m', path')
                                              -> case (arColor ad) of
                                              Graduate c1 c2 a n'
                                               -> ( m', mpEquations (getEqs path'),
                                                    MPGraduate (mpColor c1) (mpColor c2)
                                                               (mpPath path') n' a)
                                              c-> ( m', mpEquations (getEqs path'),
                                                    MPFill (mpPath path') (mpColor c)
                                                           (mpPen (arPen ad)))
                                      Nothing -> ( m, relax, relax)

mp (Clip path p) (n, symDown)
                              =  (n',
                                  symTrans symUp' n,
                                  defShift & eqs & l' & MPSubBox n z',
                                  MPAssign (Id ("p"++show n)) CurrentPicture
                                 & MPClearIt
                                 & MPDrawUnBoxed [suff n]
                                 & clipCommand
                                 & MPAssign (Id ("q"++show n)) CurrentPicture
                                 & MPClearIt
                                 & MPAssign CurrentPicture (Id ("p"++show n))
                                 & MPDrawPic MPDefaultColor (Id ("q"++show n)))
      where
      (n', symUp', l', z')    =  mp p (n+1, relax)
      sym                     =  symTrans symUp' n & symUnion3 relax relax symDown
      defShift                =  MPDef (tr n) (Shifted Identity (LLCorner (Pic (suff n))
                                                              - shiftRefPoint n))
      path'                   =  resolvePath (n, 0, sym) path
      eqs                     =  case path' of
                                      Just (_, path'')-> mpEquations (getEqs path'')
                                      Nothing         -> relax
      clipCommand             =  case path' of
                                      Just (_, path'') -> MPClip (mpPath path'')
                                      Nothing         -> relax

mp (Empty w h) (n, symDown)   =  (n+1, relax,
                                  MPBoxit (suff n) relax
                                 & width (getDefault (resolveNumeric (n, symDown) w) 0)
                                 & hight (getDefault (resolveNumeric (n, symDown) h) 0),
                                  MPDrawUnBoxed [suff n])
      where   width w         =  MPEquals [tdot (suff n) E,
                                           tdot (suff n) W + pair (mpNumeric w) 0]
              hight h         =  MPEquals [tdot (suff n) N,
                                           tdot (suff n) S + pair 0 (mpNumeric h)]

mp (Tex s) (n, _)             =  (n+1, relax,
                                  MPBoxit (suff n) (MPTex s)
                                 & MPEquals [Id (suff n++".dx"), txtDX]
                                 & MPEquals [Id (suff n++".dy"), txtDY],
                                  MPDrawUnBoxed [suff n])

mp (Text s) (n, _)            =  (n+1,
                                  relax,
                                  MPBoxit (suff n) (MPText s)
                                 & MPEquals [Id (suff n++".dx"), txtDX]
                                 & MPEquals [Id (suff n++".dy"), txtDY],
                                  MPDrawUnBoxed [suff n])

mp (PTransform (Transformation xx xy yx yy dx dy) p) (n, symDown)
                              =  (n', symTrans symUp' n,
                                  defShift & l' & MPSubBox n (
                                      z' & MPDefineTrans trn
                                          (MPTransform (mpNumeric xx) (mpNumeric xy)
                                                       (mpNumeric yx) (mpNumeric yy)
                                                       (mpNumeric dx) (mpNumeric dy))
                                      & MPAssign CurrentPicture
                                                 (Transformed CurrentPicture (Id trn))),
                                   MPDrawUnBoxed [suff n])
      where
      (n', symUp', l', z')    =  mp p (n+1, symDown)
      trn                     =  "tr"++show n
      defShift                =  MPDef (tr n)
                                       (Shifted (Transformed Identity (Id trn))
                                                (LLCorner (Pic (suff n)) - shiftRefPoint n))

mp (TrueBox p) (n, symDown)   =  (n', symTrans symUp' n,
                                  defShift & l' & MPSubBox n z',
                                  MPDrawUnBoxed [suff n])
      where
      (n', symUp', l', z')    =  mp p (n+1, symDown)
      defShift                =  MPDef (tr n) (Shifted Identity (LLCorner (Pic (suff n))
                                                              - shiftRefPoint n))

mp (BitLine o d bs) (n, symDown)
                              =  (n,
                                  relax,
                                  relax,
                                  MPBitLine ( mpNumeric (xpart o'),
                                              mpNumeric (ypart o') ) (depth d) bs)
      where
      o'                      =  getDefault (resolvePoint (n, symDown) o) 0
      depth Depth1            =  1
      depth Depth8            =  8
      depth Depth24           =  24


constructPath                 :: Symbols -> Path -> Int -> Int -> Int
                              -> (Int,Int,Symbols,MetaPost,MetaPost)
constructPath d p n nThis m   =  (n',
                                  maybe m fst (resolvePath (nThis, m, d) p),
                                  d',
                                  maybe relax (\(_,p)->mpEquations (getEqs p) & l')
                                      (resolvePath (nThis, m, d) p),
                                  maybe relax (\(_,p)->MPAssignPath "tempPath"
                                                              (mpPath p)
                                                      & snd (drawPath d p 0 relax)
                                                      & z')
                                      (resolvePath (nThis, m, d) p))
      where
      (n', d', l', z')        =  mpLabels' p n d (MPPathTerm (Id "tempPath"))


getEqs                        :: Path -> [Equation]
getEqs (PathJoin p1 _ p2)     =  equations (getEqs p1):getEqs p2
getEqs (PathBuildCycle p1 p2) =  equations (getEqs p1):getEqs p2
getEqs (PathDefine eqs p)     =  equations (eqs):getEqs p
getEqs _                      =  []

mpPath                        :: Path -> MPPath
mpPath p                      =  MPPathNorm (mpPath' p Nothing)

mpPath'                       :: Path -> Maybe (PathElemDescr,MPPathSub) -> MPPathSub
mpPath' PathCycle _   =  MPCycle
mpPath' (PathPoint p) (Just (j, c))
                              =  MPPathSub (mpPoint p) (MPPathJoin
                                                      (mpDir (peStartDir j))
                                                      (mpJoin (peJoin j))
                                                      (mpDir (peEndDir j))) c
mpPath' (PathPoint p) Nothing
                              =  MPPathEnd (mpPoint p)
mpPath' (PathEndDir p d) _    =  MPPathEndDir (mpPoint p) (mpDir d)
mpPath' (PathJoin p1 ped p2) end
                              =  mpPath' p1 (Just (ped, (mpPath' p2 end)))
mpPath' (PathBuildCycle p1 p2) _
                              =  MPPathBuildCycle [   mpPath p1, mpPath p2 ]
mpPath' (PathTransform (Transformation a b c d e f) p) end
                              =  (MPPathTransform (MPTransform (mpNumeric a)
                                                      (mpNumeric b) (mpNumeric c)
                                                      (mpNumeric d) (mpNumeric e)
                                                      (mpNumeric f))
                                                  (mpPath' p end) )
mpPath' (PathDefine _ p) end  =  mpPath' p end

drawPath                      :: Symbols -> Path -> Double -> MetaPost
                              -> (Double,MetaPost)
drawPath def (PathJoin p1 ped p2) n pr
                              =  (n2', p1')
              where
              drawSegment     =  case color of
                                      Graduate c1 c2 a q
                                              -> MPGraduatePath
                                                      (mpPathArrow arrow)
                                                      (mpColor c1) (mpColor c2)
                                                      (path p)
                                                      (mpPattern pat) (mpPen pen) q a
                                      _       -> MPDraw
                                                      (mpPathArrow arrow)
                                                      (path p)
                                                      (mpPattern pat)
                                                      (mpColor color)
                                                      (mpPen pen)
              (n1', p1')      =  if peVisible ped
                                      then drawPath def p1 n (  drawSegment
                                                              & p2'
                                                              & backArrrowHead )
                                      else drawPath def p1 n p2'
              (n2', p2')      =  if (peJoin ped == BJCat)
                                      then drawPath def p2 n1' pr
                                      else drawPath def p2 (n1'+1) pr
              arrow           =  getDefault (peArrowHead ped) DefaultArrowHead
              arrowS          =  getDefault (peSArrowHead ped) DefaultArrowHead
              backArrrowHead  =  if arrowS == DefaultArrowHead
                                      then relax
                                      else MPDrawAHead
                                              (mpPathRArrow arrowS)
                                              (path p)
                                              (mpColor color)
                                              (mpPen pen)
              color           =  peColor ped
              pat             =  pePattern ped
              pen             =  pePen ped
              p               =  MPSubPath (Const n1') (Const(n1'+1))
                                      (MPPathTerm (Id "tempPath"))
              path p          =  case peEndCut ped of
                                  Nothing -> path' p
                                  Just cb -> MPCutafter (path' p)
                                                      (MPBPath (mpCutPic cb))
              path' p         =  case peStartCut ped of
                                  Nothing -> p
                                  Just ca -> MPCutbefore p
                                                      (MPBPath (mpCutPic ca))
drawPath def (PathTransform _ p) n pr
                              =  drawPath def p n pr
drawPath _ (PathBuildCycle _ _) _ _
                              =  (0, MPDraw   MPNormal
                                              (MPPathTerm (Id "tempPath"))
                                              MPDefaultPattern
                                              MPDefaultColor
                                              MPDefaultPen)
drawPath def (PathDefine _ p) n pr
                              =  drawPath def p n pr
drawPath _ _ n mp             =  (n, mp)


mpDir                         :: Dir' -> MPPathDir
mpDir DirEmpty                =  MPDefaultPathDir
mpDir (DirCurl a)             =  MPPathDirCurl (mpNumeric a)
mpDir (DirDir a)              =  MPPathDir (mpNumeric a)
mpDir (DirVector a)           =  MPPathDirPair (mpNumeric (xpart a))
                                       (mpNumeric (ypart a))

mpJoin                        :: BasicJoin -> MPPathBasicJoin
mpJoin BJStraight             =  MPPathBasicJoinStraight
mpJoin BJTense                =  MPPathBasicJoinTense
mpJoin BJFree                 =  MPPathBasicJoin2
mpJoin BJBounded              =  MPPathBasicJoin3
mpJoin BJCat                  =  MPPathBasicJoinCat
mpJoin (BJTension a)          =  MPPathBasicJoinTension1 (mpTension a)
mpJoin (BJTension2 a b)       =  MPPathBasicJoinTension2 (mpTension a) (mpTension b)
mpJoin (BJControls a)         =  MPPathBasicJoinControls1 (mpPoint a)
mpJoin (BJControls2 a b)      =  MPPathBasicJoinControls2 (mpPoint a) (mpPoint b)

mpTension                     :: Tension -> MPPathBasicJoinTension
mpTension (Tension a)         =  MPPathBasicJoinTension (mpNumeric a)
mpTension (TensionAtLeast a)  =  MPPathBasicJoinAtLeast (mpNumeric a)


mpLabels                      :: Int -> [PathLabel] -> Symbols -> MPPath -> MPResult
mpLabels n [] _ _             =  (n, relax, relax, relax)
mpLabels n (PathLabel p i o:lls) sym p'
                              = (n'',
                                 d & d',
                                 l & l',
                                 z' & MPEquals [Id (suff n ++ (emitDir o)),
                                                Id ("point " ++ showFF i ""
                                                ++ "*length(" ++ show (emit p')
                                                ++ ") of (" ++ show (emit p') ++ ")")]
                                 & z)
      where
      (n'  ,d  ,l  ,z)        =  mp p (n, sym)
      (n'', d', l', z')       =  mpLabels n' lls sym p'

mpLabels'                     :: Path -> Int -> Symbols -> MPPath -> MPResult
mpLabels' (PathJoin p1 ped p2) n sym p'
                              =  (n''',
                                  symUnions [d', d, d''],
                                  l & l' & l'',
                                  z' & z & z'')
      where
      (n'  , d,   l,   z)     =  mpLabels n (peLabels ped) sym p'
      (n'' , d',  l',  z')    =  mpLabels' p1 n' sym p'
      (n''', d'', l'', z'')   =  mpLabels' p2 n'' sym p'
mpLabels' (PathDefine _ p) n sym p'
                              =  mpLabels' p n sym p'
mpLabels' _ n _ _             =  (n, relax, relax, relax)

-- > emitDir                    :: Dir -> String
-- > emitDir C                  =  ".c"
-- > emitDir N                  =  ".n"
-- > emitDir NE                 =  ".ne"
-- > emitDir E                  =  ".e"
-- > emitDir SE                 =  ".se"
-- > emitDir S                  =  ".s"
-- > emitDir SW                 =  ".sw"
-- > emitDir W                  =  ".w"
-- > emitDir NW                 =  ".nw"
--
