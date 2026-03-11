{-# LANGUAGE CPP #-}

{- |
   Module      :  FMP.Syntax
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

module FMP.Syntax where

import FMP.Color
import FMP.Picture hiding ( empty, text )
import FMP.Term
import FMP.Types

-- Avoid name clash with Prelude.<> exported by post-SMP versions of base.
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ( (<>) )
#endif
import Numeric
import Text.PrettyPrint

class HasEmit a where
      emit                    :: a -> Doc

-- This data type contains the compiled graphics, ready to |emit|.

data MetaPost                 =  MPAssign     Term    Term
                              |  MPAssignPath String  MPPath
                              |  MPBoxit      String  MetaPost
                              |  MPBitLine    (Term, Term)    Term    String
                              |  MPCloneit    String  String
                              |  MPClearIt
                              |  MPClip       MPPath
                              |  MPComment    String
                              |  MPConc       MetaPost MetaPost
                              |  MPDef        String  Term
                              |  MPDefineTrans String MPTransform
                              |  MPDraw       MPArrow MPPath  MPPattern
                                              MPColor MPPen
                              |  MPDrawAHead  MPArrow MPPath
                                              MPColor MPPen
                              |  MPDrawPic    MPColor Term
                              |  MPDrawUnBoxed [String]
                              |  MPEquals     [Term]
                              |  MPFigure     Int     MetaPost
                              |  MPFill       MPPath  MPColor MPPen
                              |  MPFixPos     [String]
                              |  MPFixSize    [String]
                              |  MPGraduate   MPColor MPColor MPPath
                                              Int     Double
                              |  MPGraduatePic MPColor MPColor Term
                                               Int    Double
                              |  MPGraduatePath MPArrow MPColor MPColor
                                                        MPPath        MPPattern
                                                        MPPen Int     Double
                              |  MPGroup      MetaPost
                              |  MPIfElse     Term    MetaPost        MetaPost
                              |  MPImage      String  MetaPost
                              |  MPRelax
                              |  MPShapeit    String
                              |  MPSubBox     Int     MetaPost
                              |  MPTex        String
                              |  MPText       String
                              |  MPVerbatim   String
                                 deriving Eq

---------------------

instance HasConcat MetaPost where
      MPRelax & a             =  a
      a & MPRelax             =  a
      a & b                   =  MPConc a b

instance HasRelax MetaPost where
      relax                   =  MPRelax

mpConcs                       :: [MetaPost] -> MetaPost
mpConcs as                    =  foldl (&) MPRelax as

data MPArrow                  =  MPNormal
                              |  MPArrow      (Maybe Double)  (Maybe Double)  MPArrowStyle
                              |  MPReverse    (Maybe Double)  (Maybe Double)  MPArrowStyle
                                 deriving Eq

data MPArrowStyle             =  MPArrowStyleFilled
                              |  MPArrowStyleLine
                                 deriving Eq

mpArrowStyle                  :: ArrowHeadStyle -> MPArrowStyle
mpArrowStyle AHFilled         =  MPArrowStyleFilled
mpArrowStyle AHLine           =  MPArrowStyleLine

mpPathArrow                   :: ArrowHead -> MPArrow
mpPathArrow DefaultArrowHead
                              =  MPNormal
mpPathArrow (ArrowHead a l style)
                              =  MPArrow a l (mpArrowStyle style)

mpPathRArrow                  :: ArrowHead -> MPArrow
mpPathRArrow DefaultArrowHead =  MPNormal
mpPathRArrow (ArrowHead a l style)
                              =  MPReverse a l (mpArrowStyle style)

data MPTransform              =  MPTransform Term Term Term Term Term Term
                                 deriving Eq

data MPPath                   =  MPPathNorm   MPPathSub
                              |  MPCutbefore  MPPath  MPPath
                              |  MPCutafter   MPPath  MPPath
                              |  MPBPath      Term
                              |  MPTransformP [Int]   MPPath
                              |  MPShiftedP   Term    MPPath
                              |  MPSubPath    Term    Term    MPPath
                              |  MPPathTerm   Term
                                 deriving Eq

data MPPathSub                =  MPPathSub    Term    MPPathJoin      MPPathSub
                              |  MPPathEndDir Term    MPPathDir
                              |  MPPathEnd    Term
                              |  MPCycle
                              |  MPPathBuildCycle     [MPPath]
                              |  MPPathTransform      MPTransform     MPPathSub
                                 deriving Eq

data MPPathJoin               =  MPPathJoin   MPPathDir MPPathBasicJoin MPPathDir
                                 deriving Eq

data MPPathBasicJoin          =  MPPathBasicJoinCat
                              |  MPPathBasicJoin2
                              |  MPPathBasicJoin3
                              |  MPPathBasicJoinTense
                              |  MPPathBasicJoinStraight
                              |  MPPathBasicJoinTension1 MPPathBasicJoinTension
                              |  MPPathBasicJoinTension2 MPPathBasicJoinTension
                                                         MPPathBasicJoinTension
                              |  MPPathBasicJoinControls1     Term
                              |  MPPathBasicJoinControls2     Term    Term
                                 deriving Eq

data MPPathDir                =  MPDefaultPathDir
                              |  MPPathDirCurl        Term
                              |  MPPathDirPair        Term Term
                              |  MPPathDir            Term
                                 deriving Eq

data MPPathBasicJoinTension   =  MPPathBasicJoinTension Term
                              |  MPPathBasicJoinAtLeast Term
                                 deriving Eq


data MPColor                  =  MPDefaultColor
                              |  MPColor Double Double Double
                                 deriving Eq

mpColor                       :: Color -> MPColor
mpColor (Color r g b)         =  MPColor r g b
mpColor (Graduate c1 _ _ _)   =  mpColor c1
mpColor DefaultColor          =  MPDefaultColor

data MPPen                    =  MPDefaultPen
                              |  MPPenCircle (Term, Term) Term
                              |  MPPenSquare (Term, Term) Term
                                 deriving Eq

mpPen                         :: Pen -> MPPen
mpPen DefaultPen              =  MPDefaultPen
mpPen (PenCircle (a,b) c)     =  MPPenCircle (mpNumeric a,mpNumeric b) (mpNumeric c)
mpPen (PenSquare (a,b) c)     =  MPPenSquare (mpNumeric a,mpNumeric b) (mpNumeric c)

data MPPattern                =  MPDefaultPattern
                              |  MPDashPattern [Double]
                                 deriving Eq

mpPattern                     :: Pattern -> MPPattern
mpPattern DefaultPattern      =  MPDefaultPattern
mpPattern (DashPattern pat)   =  MPDashPattern pat

mpEquations                   :: [Equation] -> MetaPost
mpEquations []                =  MPRelax
mpEquations (PEquations ps:eqs)
                              =  MPEquals (mpPEquations ps) &  mpEquations eqs
mpEquations (NEquations ns:eqs)
                              =  MPEquals (mpNEquations ns) &  mpEquations eqs
mpEquations (EquationCond b t e:eqs)
                              =  MPIfElse (mpBoolean b) (mpEquations [t]) (mpEquations [e])
                              &  mpEquations eqs
mpEquations (Equations e:eqs) =  mpEquations e &  mpEquations eqs


mpPEquations                  :: [Point] -> [Term]
mpPEquations []               =  []
mpPEquations (p:ps)           =  mpPoint p:mpPEquations ps

mpNEquations                  :: [Numeric] -> [Term]
mpNEquations []               =  []
mpNEquations (n:ns)           =  mpNumeric n:mpNEquations ns

mpBoolean                     :: Boolean -> Term
mpBoolean (Boolean True)      =  Id "true"
mpBoolean (Boolean False)     =  Id "false"
mpBoolean (BoolNum a c b)     =  Infix (mpNumeric a) (mpBoolRelat c) (mpNumeric b)
mpBoolean (BoolPnt a c b)     =  Infix (mpPoint a) (mpBoolRelat c) (mpPoint b)
mpBoolean (BoolOr a b)        =  Infix (Parens (mpBoolean a))
                                      "or"
                                      (Parens (mpBoolean b))
mpBoolean (BoolAnd a b)       =  Infix (Parens (mpBoolean a))
                                      "and"
                                      (Parens (mpBoolean b))
mpBoolean (BoolNot a)         =  VerbFunction "not" (Parens (mpBoolean a))

mpBoolRelat                   :: BoolRelat -> String
mpBoolRelat BoolEQ            =  "="
mpBoolRelat BoolL             =  "<"
mpBoolRelat BoolLE            =  "<="
mpBoolRelat BoolNE            =  "<>"


mpPoint                       :: Point -> Term
mpPoint (PointVarArray' n m)  =  Id ("pvi"++show n++" "++show m)
mpPoint (PointVar' n m)       =  Id ("pv"++show n++" "++show m)
mpPoint (PointPic' n d)       =  tdot (suff n) d
mpPoint (PointTrans' p [])    =  mpPoint p
mpPoint (PointTrans' p ts)    =  Transform ts (mpPoint p)
mpPoint (PointPPP PPPAdd p1 p2 )
                              =  mpPoint p1 + mpPoint p2
mpPoint (PointPPP PPPSub p1 p2 )
                              =  mpPoint p1 - mpPoint p2
mpPoint (PointPPP PPPDiv p1 p2 )
                              =  mpPoint p1 / mpPoint p2
mpPoint (PointDirection a)    =  Dirop (mpNumeric a)
mpPoint (PointVec (ox,oy))    =  Pair (mpNumeric ox) (mpNumeric oy)
mpPoint (PointMediate o p1 p2)
                              =  Mediate (mpNumeric o) (mpPoint p1) (mpPoint p2)
mpPoint (PointNMul n p)       =  Mul (mpNumeric n) (mpPoint p)
mpPoint (PointNeg p)          =  -mpPoint p
mpPoint PointWhatever         =  Id "whatever"
mpPoint (PointCond b t e)     =  IfElse (mpBoolean b) (mpPoint t) (mpPoint e)
mpPoint _                     =  Id ""

mpCutPic                      :: CutPic -> Term
mpCutPic (CutPic' name)       =  Id name
mpCutPic (CutPicTrans c [])   =  mpCutPic c
mpCutPic (CutPicTrans c ts)   =  Transform ts (mpCutPic c)
mpCutPic _                    =  Id ""

mpNumeric                     :: Numeric -> Term
mpNumeric (Numeric a)         =  Const a
mpNumeric (NumericArray' n m)
                              =  Id ("nvi"++show n++" "++show m)
mpNumeric (NumericVar' n m)
                              =  Id ("nv"++show n++" "++show m)
mpNumeric (NumericDist p1 p2)
                              =  Pythagoras (XPart (mpPoint p1-mpPoint p2))
                                            (YPart (mpPoint p1-mpPoint p2))
mpNumeric (NumericMediate a b c)
                              =  Mediate (mpNumeric a) (mpNumeric b) (mpNumeric c)
mpNumeric (NumericNNN NNNAdd a1 a2)
                              =  mpNumeric a1 + mpNumeric a2
mpNumeric (NumericNNN NNNSub a1 a2)
                              =  mpNumeric a1 - mpNumeric a2
mpNumeric (NumericNNN NNNMul a1 a2)
                              =  mpNumeric a1 * mpNumeric a2
mpNumeric (NumericNNN NNNDiv a1 a2)
                              =  mpNumeric a1 / mpNumeric a2
mpNumeric (NumericNNN NNNPyth a b)
                              =  Pythagoras (mpNumeric a) (mpNumeric b)
mpNumeric (NumericNNN NNNPower a b)
                              =  Power (mpNumeric a) (mpNumeric b)
mpNumeric (NumericNsN NsNMin as)
                              =  Min (map mpNumeric as)
mpNumeric (NumericNsN NsNMax as)
                              =  max' (map mpNumeric as)
mpNumeric (NumericPN PNXPart p )
                              =  XPart (mpPoint p)
mpNumeric (NumericPN PNYPart p )
                              =  YPart (mpPoint p)
mpNumeric (NumericPN PNAngle a )
                              =  Angle (mpPoint a)
mpNumeric (NumericNN NNSinD a)
                              =  SinD (mpNumeric a)
mpNumeric (NumericNN NNCosD a)
                              =  CosD (mpNumeric a)
mpNumeric (NumericNN NNSqrt a)
                              =  Sqrt (mpNumeric a)
mpNumeric (NumericNN NNNeg a)
                              =  -mpNumeric a
mpNumeric (NumericNN NNExp a)
                              =  Exp (mpNumeric a)
mpNumeric (NumericNN NNLog a)
                              =  Ln (mpNumeric a)
mpNumeric (NumericNN NNRound a )
                              =  Round (mpNumeric a)
mpNumeric (NumericNN NNCeil a)
                              =  Ceil (mpNumeric a)
mpNumeric (NumericNN NNFloor a )
                              =  Floor (mpNumeric a)
mpNumeric NumericWhatever     =  Id "whatever"
mpNumeric (NumericCond b t e)
                              =  IfElse (mpBoolean b) (mpNumeric t) (mpNumeric e)
mpNumeric _                   =  Id ""

emitL                         :: [String] -> Doc
emitL []                      =  empty
emitL [d]                     =  text d
emitL (d:ds)                  =  hcat (text d:[comma <+> text d | d <- ds])

instance HasEmit MetaPost where
      emit (MPAssign l r)     =  emit l <+> text ":=" <+> emit r <> semi
      emit (MPAssignPath l p)
                              =  text l <+> text ":=" <+> emit p <> semi
      emit (MPBoxit s pic)    =  text "boxit." <> text s <> parens (emit pic) <> semi
      emit (MPBitLine (x,y) d bs)
                              =  text "bitline(" <> emit x <> comma
                              <+>emit y <> comma <+> emit d <> comma
                              <+>text (show bs) <> char ')' <> semi
      emit (MPCloneit s s2)   =  text "cloneit." <> text s <> parens (text s2) <> semi
      emit (MPShapeit s)      =  text "shapeit." <> text s <> semi
      emit MPClearIt          =  text "clearit" <> semi
      emit (MPClip path)      =  text "clip currentpicture to "<+> emit path <> semi
      emit (MPComment s)      =  char '%' <+> text s $+$ empty
      emit (MPDef s t)        =  text ("def "++s++" = ") <> emit t
                              <+>text "enddef" <> semi
      emit (MPDefineTrans s tr )
                              =  emitDefTrans s tr
      emit (MPDrawUnBoxed [])
                              =  empty
      emit (MPConc (MPDrawUnBoxed s1) (MPConc (MPDrawUnBoxed s2) mp))
                              =  emit (MPDrawUnBoxed (s1++s2) & mp)
      emit (MPDrawUnBoxed s)
                              =  text "drawunboxed" <> parens (emitL s) <> semi
      emit (MPConc left@((MPDraw a (MPSubPath s  e  path ) d  c  p))
              (MPConc right@(MPDraw a' (MPSubPath s' e' path') d' c' p') mp))
                              =  if path == path' && a == a'  && d == d' && c == c' && p == p'
                                  then if s == e'
                                      then emit (MPDraw a (MPSubPath s' e path) d c p & mp)
                                      else if e == s'
                                              then emit (MPDraw a (MPSubPath s e' path)
                                                                      d c p & mp)                             ---
                                              else (emit left $+$ emit (right & mp))
                                  else (emit left $+$ emit (right & mp))
      emit (MPConc left@((MPDraw a  (MPSubPath s  e  path ) d  c  p))
              right@(MPDraw a' (MPSubPath s' e' path') d' c' p'))
                              =  if path == path' && a == a' && d == d' && c == c' && p == p'
                                  then if s == e'
                                      then emit (MPDraw a (MPSubPath s' e path) d c p)
                                      else if e == s'
                                              then emit (MPDraw a (MPSubPath s e' path) d c p)
                                              else (emit left $+$ emit right)
                                  else (emit left $+$ emit right)
      emit (MPConc a b)       =  emit a $+$ emit b
      emit (MPDraw ar p d c pen)
                              =  text "draw" <+> parens (emit p)
                              <> emit d <> emit c <> emit pen <> semi
                              <+>emit (MPDrawAHead ar p c pen)
      emit (MPDrawAHead MPNormal _  _ _)
                              =  empty
      emit (MPDrawAHead ar p c pen)
                              =  empty
                              $$ style ar
                              <> parens (revOrNot ar  <>  parens (emit p) <>  comma
                                         <+> double (fst (al ar)) <> comma
                                         <+> double (snd (al ar)))
                              <+> emit c <+> emit pen <> semi
       where
       al (MPArrow a l _)     =  (getDefault a 4, getDefault l 45)
       al (MPReverse a l _)   =  (getDefault a 4, getDefault l 45)
       al MPNormal            =  (0, 0)
       style (MPArrow _ _ MPArrowStyleFilled)
                              =  text "fill varrowheadFull"
       style (MPArrow _ _ MPArrowStyleLine)
                              =  text "draw varrowhead"
       style (MPReverse _ _ MPArrowStyleFilled)
                              =  text "fill varrowheadFull"
       style (MPReverse _ _ MPArrowStyleLine)
                              =  text "draw varrowhead"
       revOrNot (MPReverse _ _  _)
                              =  text " reverse "
       revOrNot _             =  text ""
      emit (MPDrawPic c p)    =  text "draw" <+> emit p <+> emit c <> semi
      emit (MPEquals [])      =  empty
      emit (MPEquals (eq:eqs))
                              =  emit eq <> doc eqs <> semi
          where
          doc []              =  empty
          doc (eq:eqs)        =  empty <+> equals <+> emit eq <> doc eqs
      emit (MPFigure n mp)    =  text "beginfig" <> parens (int n) <> semi
                              $+$ emit mp $+$ text "endfig" <> semi
      emit (MPFill path c p)  =  text "fill" <+> emit path <+> emit c <+> emit p <> semi
      emit (MPFixSize [])     =  empty
      emit (MPFixSize s)      =  text "fixsize" <> parens (emitL s) <> semi
      emit (MPFixPos [])      =  empty
      emit (MPFixPos s)       =  text "fixpos" <> parens (emitL s) <> semi
      emit (MPGraduate c1 c2 path q a)
                              =  text "graduate"
                              <> parens (emitColor' c1 <> comma
                                      <+> emitColor' c2 <> comma
                                      <+> emit path <> comma
                                      <+> int q <> comma
                                      <+> double a) <> semi
      emit (MPGraduatePic c1 c2 t q a)
                              =  text "graduatePic"
                              <> parens (emitColor' c1 <> comma
                                      <+> emitColor' c2 <> comma
                                      <+> emit t <> comma
                                      <+> int q <> comma
                                      <+> double a) <> semi
      emit (MPGraduatePath ar c1 c2 path pat pen q a)
                              =  text "graduatePath"
                              <+>parens (emitColor' c1 <> comma
                                      <+> emitColor' c2 <> comma
                                      <+> emit path <> comma
                                      <+> emitPattern' pat <> comma
                                      <+> emitPen' pen <> comma
                                      <+> int q <> comma
                                      <+> double a) <> semi
                              $+$emit (MPDrawAHead ar path c2 pen)
      emit (MPGroup mp)       =  text "begingroup" $+$ emit mp$+$text "endgroup" <> semi
      emit (MPIfElse b t e)   =  text "if" <+> emit b <> colon
                              $+$ emit t $+$ text "else" <> colon
                              $+$ emit e $+$ text "fi" <> semi
      emit (MPRelax)          =  empty
      emit (MPSubBox n mp)
                              =  text "p" <> int n <+> text " := currentpicture" <> semi
                              <+>text "clearit" <> semi
                              $+$emit mp
                              $+$emit (shiftRefPoint n)
                                      <> text " := llcorner currentpicture" <> semi
                              $+$text ("boxit."++suff n
                                      ++ "(currentpicture)") <> semi
                              $+$text (suff n++".dx = 0") <> semi
                              <+>text (suff n++".dy = 0") <> semi
                              $+$text "currentpicture := p" <> int n <> semi
      emit (MPTex s)          =  text "btex" <+> text s <+> text "etex"
      emit (MPText s)         =  text $ show s
      emit (MPVerbatim a)     =  text a
      emit (MPImage s mp)     =  text (s++":=image(") $+$emit mp $+$char ')' <> semi


-- Konvertierung f"ur den Typ Pfad

instance HasEmit MPPath where
      emit (MPPathNorm p)     =  emit p
      emit (MPCutbefore p1 p2)=  emit p1 <+> text "cutbefore" <+> emit p2
      emit (MPCutafter p1 p2) =  emit p1 <+> text "cutafter" <+> emit p2
      emit (MPBPath a)        =  text "bpath" <+> emit a
      emit (MPTransformP ts a)=  emit a <> hsep [ text " transformed" <> text (tr t)
                                                | t<-ts]
      emit (MPShiftedP a p)   =  emit p <+> text "shifted" <+> emit a
      emit (MPSubPath beg end p)
                              =  text "subpath (" <> emit beg
                              <> comma <> emit end <> text ") of " <> emit p
      emit (MPPathTerm s)     =  emit s


instance HasEmit MPPathSub where
 emit (MPPathSub a j s)       =  emit a <> emit j <> emit s
 emit (MPPathEndDir a d)      =  emit a <> emit d
 emit (MPPathEnd a)           =  emit a
 emit MPCycle                 =  text "cycle"
 emit (MPPathBuildCycle  (p1:p2:_))
                              =  text "buildcycle(" <> emit p1 <> comma
                              <> emit p2 <> char ')'
 emit (MPPathTransform (MPTransform a b c d e f) p)
                              =  char '(' <> emit p <> text ") transformed"
                              <+> emit (TransformedM a b c d e f)

instance HasEmit MPPathJoin where
      emit (MPPathJoin _ MPPathBasicJoinTense _)
                              =  text "---"
      emit (MPPathJoin _ MPPathBasicJoinStraight _)
                              =  text "--"
      emit (MPPathJoin dir1 bj dir2)
                              =  emit dir1 <> emit bj <> emit dir2

instance HasEmit MPPathDir where
      emit MPDefaultPathDir   =  empty
      emit (MPPathDirCurl a)  =  text "{curl (" <> emit a <> text ")}"
      emit (MPPathDirPair a b)=  char '{' <> emit a <> comma <> emit b <> char '}'
      emit (MPPathDir a)      =  text "{dir (" <> emit a <> text ")}"

instance HasEmit MPPathBasicJoin where
      emit MPPathBasicJoinCat =  text " & "
      emit MPPathBasicJoin2   =  text ".."
      emit MPPathBasicJoin3   =  text "..."
      emit MPPathBasicJoinTense
                              =  text "---"
      emit MPPathBasicJoinStraight
                              =  text "--"
      emit (MPPathBasicJoinTension1 a)
                              =  text "..tension " <> emit a <> text ".."
      emit (MPPathBasicJoinTension2 a b)
                              =  text "..tension " <> emit a
                              <> text " and " <> emit b <> text ".."
      emit (MPPathBasicJoinControls1 a)
                              =  text "..controls" <+> emit a <> text ".."
      emit (MPPathBasicJoinControls2 a b)
                              =  text "..controls" <+> emit a
                              <+> text "and" <+> emit b <> text".."

instance HasEmit MPPathBasicJoinTension where
      emit (MPPathBasicJoinTension a)
                              =  emit a
      emit (MPPathBasicJoinAtLeast a)
                              =  text "atleast" <+> emit a

showFF                        :: Double -> ShowS
showFF                        =  showFFloat (Just 3)

instance HasEmit Term where
      emit (Const 0)          =  text "0"
      emit (Const n)          =  if n < 0
                                      then text ('(':(showFFloat (Just 4) n ")"))
                                      else text (showFFloat (Just 4) n "")
      emit (Pos n m)          =  text (pos n m)
      emit (Max [])           =  char '0'
      emit (Max [a])          =  emit a
      emit (Max (t:ts))       =  text "max(" <> emit t
                              <> hcat [comma <> emit t'|t'<-ts] <> char ')'
      emit (Min [])           =  char '0'
      emit (Min [a])          =  emit a
      emit (Min (t:ts))       =  text "min(" <> emit t
                              <> hcat [comma <> emit t'|t'<-ts] <> char ')'
      emit (Neg a)            =  text "(-(" <> emit a <> text "))"
      emit (Add a b)          =  emit a <> char '+' <> emit b
      emit (Sub a b)          =  emit a <> text "-(" <> emit b <> char ')'
      emit (Mul a b)          =  char '(' <> emit a <> text ")*(" <> emit b <> char ')'
      emit (Pair a b)         =  char '(' <> emit a <> char ','
                              <> emit b <> char ')'
      emit (XPart a)          =  text "(xpart (" <> emit a <> text "))"
      emit (YPart a)          =  text "(ypart (" <> emit a <> text "))"
      emit (Id a)             =  text a
      emit (Pythagoras a b)   =  char '(' <> emit a <> text "++"
                              <> emit b <> char ')'
      emit Identity           =  text "identity"
      emit CurrentPicture     =  text "currentpicture"
      emit (Infix a b c)      =  emit a <+> text b <+> emit c
      emit (LLCorner a)       =  text "llcorner (" <> emit a <> char ')'
      emit (URCorner a)       =  text "urcorner (" <> emit a <> char ')'
      emit (Pic a)            =  text "pic" <+> text a
      emit (Shifted a b)      =  char '(' <> emit a <> text ") shifted ("
                              <> emit b <> char ')'
      emit (Transformed a b)
                              =  char '(' <> emit a <> text ") transformed ("
                              <> emit b <> char ')'
      emit (TransformedM a b c d e f)
                              =  text "trans(" <> emit a <> comma <> emit b
                              <> comma <> emit c <> comma <> emit d
                              <> comma <> emit e <> comma <> emit f <> char ')'
      emit (SinD a)           =  text "sind(" <> emit a <> char ')'
      emit (Power a b)        =  text "((" <> emit a <> text ")**("
                              <> emit b <> text "))"
      emit (CosD a)           =  text "cosd(" <> emit a <> char ')'
      emit (Sqrt a)           =  text "sqrt(" <> emit a <> char ')'
      emit (Exp a)            =  text "mexp(256*(" <> emit a <> text "))"
      emit (Ln a)             =  text "(mlog(" <> emit a <> text ")/256)"
      emit (Round a)          =  text "round(" <> emit a <> char ')'
      emit (Angle a)          =  text "angle(" <> emit a <> char ')'
      emit (Dirop a)          =  text "(dir (" <> emit a <> text "))"
      emit (Ceil a)           =  text "ceil(" <> emit a <> char ')'
      emit (Floor a)          =  text "floor(" <> emit a <> char ')'
      emit (Div a b)          =  char '(' <> emit a <> text ")/(" <> emit b <> text ")"
      emit (Mediate a b c)    =  emit a <> brackets (emit b <> comma <> emit c)
      emit (Transform ts a)   =  emit a <> hsep [ text (" transformed "++tr t)
                                                | t <- reverse ts]
      emit (TDot a d)         =  text (a ++ emitDir d)
      emit (Parens a)         =  parens (emit a)
      emit (VerbFunction a b)
                              =  text a <> parens (emit b)
      emit (IfElse b t e)     =  text "if" <+> emit b <> char ':' <> emit t
                              <+> text "else:" <> emit e <+> text "fi "

instance HasEmit MPArrow where
      emit MPNormal           =  text "draw"
      emit (MPArrow _ _ _)    =  text "drawarrow"
      emit (MPReverse _ _ _)  =  text "drawarrow reverse"


instance HasEmit MPPattern where
      emit MPDefaultPattern   =  text ""
      emit (MPDashPattern pat)
                              =  text "dashed dashpattern" <+> parens (on pat)
              where
              on []           =  text ""
              on ((-1):as)    =  off as
              on (a:as)       =  text "on" <+> double a <+> off as
              off []          =  text ""
              off (a:as)      =  text "off" <+> double a <+> on as

emitPattern'                  :: MPPattern -> Doc
emitPattern' MPDefaultPattern =  text "0"
emitPattern' (MPDashPattern pat)
                              =  text "dashpattern" <+> parens (on pat)
              where
              on []           =  text ""
              on ((-1):as)    =  off as
              on (a:as)       =  text "on" <+> double a <+> off as
              off []          =  text ""
              off (a:as)      =  text "off" <+> double a <+> on as

emitDefTrans                  :: String -> MPTransform -> Doc
emitDefTrans s (MPTransform xx xy yx yy _ _)
                              =  text ("xpart "++s++"=0; ypart "++s++"=0;")
                              $+$ text ("xxpart "++s++"=") <> emit xx <> semi
                              $+$ text ("xypart "++s++"=") <> emit xy <> semi
                              $+$ text ("yxpart "++s++"=") <> emit yx <> semi
                              $+$ text ("yypart "++s++"=") <> emit yy <> semi

instance HasEmit MPColor where
      emit MPDefaultColor     =  empty
      emit (MPColor r g b)    =  text (" withcolor "++show (r,g,b)++" ")

emitColor'                    :: MPColor -> Doc
emitColor' MPDefaultColor     =  text "(0,0,0)"
emitColor' (MPColor r g b)    =  text (show (r,g,b))

instance HasEmit MPPen where
      emit MPDefaultPen       =  empty
      emit (MPPenCircle (x,y) a)
                              =  text " withpen pencircle" <+> emitPen2 (x,y) a
      emit (MPPenSquare (x,y) a)
                              =  text " withpen pensquare" <+> emitPen2 (x,y) a

emitPen2                      :: (Eq b, Num b, HasEmit b, Eq a, HasEmit a) => (a,a)
                              -> b -> Doc
emitPen2 (x,y) a              =  (if x == y
                                      then text "scaled" <+> parens (emit x)
                                      else text "xscaled" <+> parens (emit x)
                                       <+> text "yscaled" <+> parens (emit y))
                              <+>if a == 0
                                      then empty
                                      else text "rotated" <+> parens (emit a)

emitPen'                      :: MPPen -> Doc
emitPen' MPDefaultPen         =  text "pencircle"
emitPen' (MPPenCircle (x,y) a)=  text "pencircle" <+> emitPen2 (x,y) a
emitPen' (MPPenSquare (x,y) a)=  text "pensquare" <+> emitPen2 (x,y) a

emitDir                  :: Dir -> String
emitDir C                =  ".c"
emitDir N                =  ".n"
emitDir NE               =  ".ne"
emitDir E                =  ".e"
emitDir SE               =  ".se"
emitDir S                =  ".s"
emitDir SW               =  ".sw"
emitDir W                =  ".w"
emitDir NW               =  ".nw"

tdot                          :: String -> Dir -> Term
tdot s C                      = TDot s C
tdot s N                      = TDot s N
tdot s NE                     = TDot s NE
tdot s E                      = TDot s E
tdot s SE                     = TDot s SE
tdot s S                      = TDot s S
tdot s SW                     = TDot s SW
tdot s W                      = TDot s W
tdot s NW                     = TDot s NW

tr                            :: Int -> String
tr n                          =  "_t_" ++ savestring (show n)

pos                           :: Int -> Int -> String
pos a b                       =  "p" ++ show a ++ " " ++ show b

savestring                    :: String -> String
savestring []                 =  []
savestring (a:as)             =  toEnum (b+hi):toEnum (b+lo):savestring as
              where
              b               =  fromEnum 'A'
              lo              =  mod (fromEnum a) 16
              hi              =  div (fromEnum a) 16

defDX, defDY, txtDX, txtDY    :: Term
defDX                         =  Id "defDX"
defDY                         =  Id "defDY"
txtDX                         =  Id "txtDX"
txtDY                         =  Id "txtDY"

shiftRefPoint                 :: Int -> Term
shiftRefPoint n               =  Id ("s" ++ show n)


suff                          :: Int -> String
suff n                        =  "b" ++ show n
