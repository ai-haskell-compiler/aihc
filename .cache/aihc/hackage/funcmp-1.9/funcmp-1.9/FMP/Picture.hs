{- |
   Module      :  FMP.Picture
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

module FMP.Picture (
      (|-|), (|||), (|=|),     (||||), ( # ),
      (.&.), (...), (.-.),     (....), (.--.),
      IsPicture(..),     IsPath(..), IsArea(..),
      HasPicture(..),   HasName(..), HasDXY(..), HasExtent(..),
      HasLabel(..), HasPattern(..),
      HasPen(..),  HasArrowHead(..), HasStartEndCut(..),
      HasStartEndDir(..), HasJoin(..),
      HasShadow(..),    HasLayer(..), HasConcat(..), IsHideable(..),
      joinCat, joinFree, joinBounded, joinStraight, joinTense,
      joinTension, joinTensions, joinControl, joinControls,
      tension, tensionAtLeast,
      buildCycle, transformPath,
      mm, pt, dd, bp, cm, pc, cc, inch,
      up, down, left, right,
      fullcircle, halfcircle, quartercircle, unitsquare,
      enumPics, image,
      Picture(..), Layer(..), Transformation(..), BitDepth(..),
      row, column, rowSepBy, columnSepBy, vspace, hspace, space,
      fill, clip, draw, at, label, overlay, overlay', ooalign, HasDefine(..),
      text, tex, math, empty, setTrueBoundingBox,
      scale, rotate, transform , affine, shifted,
      rotated, reflectedX, reflectedY, reflectX, reflectY,
      scaledX, scaledY, scaled, skewedX, skewX, skewedY, skewY,
      line, curve, arrow, cycle', lastNameIsDir,
      defaultArrowHead, arrowHeadBig, arrowHeadSize,
      setArrowHeadStyle, getArrowHeadStyle, ahFilled, ahLine,
      Attrib(..), stdAttrib,
      FrameAttrib(..), stdFrameAttrib,
      PathElemDescr(..), stdPathElemDescr,
      Path(..), PathLabel(..), ArrowHead(..), ArrowHeadStyle(..), CutPic(..),
      pathLength, forEachPath,
      Dir'(..), BasicJoin(..), Tension(..),
      Area(..), AreaDescr(..), stdAreaDescr,
      getDefault,
      Frame(..), AbsOrRel(..), ExtentAttrib(..), stdExtentAttrib,
      dot, bullet,
      box, triangle, triAngle, rbox, oval, circle
      ) where

import Data.List
import FMP.Types
import FMP.Color

infixl 0 #
infixr 1 .&.,...,.-.,....,.--.
infixr 1 &
infixl 5 |-|, |=|
infixl 6 |||, ||||

( # )                         :: a -> (a -> b) -> b
a # f                         =  f a

class HasConcat a where
      (&)                     :: a -> a -> a

class FromList a where
      fromList                :: [a] -> a

instance FromList Equation where
      fromList                =  equations

instance FromList a => HasConcat [a] where
      a & b                   =  fromList a:b

class (Show a) => IsPicture a where
      toPicture               :: a -> Picture
      toPictureList           :: [a] -> Picture
      toPicture a             =  text (show a)
      toPictureList ps        =  row (map toPicture ps)

class HasPicture a where
      fromPicture             :: (IsPicture b) => b -> a

class HasName a where
      setName                 :: IsName b => b -> a -> a
      getNames                :: a -> [Name]

class HasDXY a where
      setDX                   :: Numeric -> a -> a
      getDX                   :: a -> Maybe Numeric
      setDY                   :: Numeric -> a -> a
      getDY                   :: a -> Maybe Numeric

class HasExtent a where
      setWidth                :: Numeric -> a -> a
      removeWidth             :: a -> a
      getWidth                :: a -> Maybe Numeric
      setHeight               :: Numeric -> a -> a
      removeHeight            :: a -> a
      getHeight               :: a -> Maybe Numeric

class HasLabel a where
      setLabel                :: IsPicture b => Double -> Dir -> b -> a -> a
      removeLabel             :: a -> a

class HasPattern a where
      setPattern              :: Pattern -> a -> a
      setDefaultPattern       :: a -> a
      getPattern              :: a -> Pattern

class HasPen a where
      setPen                  :: Pen -> a -> a
      setDefaultPen           :: a -> a
      getPen                  :: a -> Pen

class HasArrowHead a where
      setArrowHead            :: ArrowHead -> a -> a
      removeArrowHead         :: a -> a
      getArrowHead            :: a -> Maybe ArrowHead
      setStartArrowHead       :: ArrowHead -> a -> a
      removeStartArrowHead    :: a -> a
      getStartArrowHead       :: a -> Maybe ArrowHead

class HasStartEndCut a where
      setStartCut             :: IsName b => b -> a -> a
      removeStartCut          :: a -> a
      setEndCut               :: IsName b => b -> a -> a
      removeEndCut            :: a -> a

class HasStartEndDir a where
      setStartAngle           :: Numeric -> a -> a
      setEndAngle             :: Numeric -> a -> a
      setStartCurl            :: Numeric -> a -> a
      setEndCurl              :: Numeric -> a -> a
      setStartVector          :: Point -> a -> a
      setEndVector            :: Point -> a -> a
      removeStartDir          :: a -> a
      removeEndDir            :: a -> a

class HasJoin a where
      setJoin                 :: BasicJoin -> a -> a
      getJoin                 :: a -> BasicJoin

class HasDefine a where
      define                  :: [Equation] -> a -> a

class IsHideable a where
      hide                    :: a -> a

class HasShadow a where
      setShadow               :: (Numeric, Numeric) -> a -> a
      clearShadow             :: a -> a
      getShadow               :: a -> Maybe (Numeric, Numeric)

data Layer                    =  Front | Back
                                 deriving (Eq, Show, Read)

class HasLayer a where
      setBack                 :: a -> a
      setFront                :: a -> a
      getLayer                :: a -> Layer

------------------- Picture ---------------------

data Transformation           =  Transformation Numeric Numeric Numeric Numeric
                                               Numeric Numeric
                                 deriving (Eq, Show, Read)

data Picture                  =  Attributes   Attrib  Picture

                              |  Overlay      [Equation] (Maybe Int) [Picture]
                              |  Define       [Equation] Picture

                              |  Frame        FrameAttrib [Equation]  Path    Picture

                              |  Draw         [Path]  Picture
                              |  Fill         [Area]  Picture
                              |  Clip         Path    Picture

                              |  Empty        Numeric Numeric
                              |  Tex          String
                              |  Text         String

                              |  BitLine      Point   BitDepth        String

                              |  PTransform   Transformation  Picture

                              |  TrueBox      Picture
                                 deriving (Eq, Show, Read)

data BitDepth                 =  Depth1 | Depth8 | Depth24
                                 deriving (Eq, Show, Read)

instance IsPicture Picture where
      toPicture               =  id

instance IsPicture Char where
      toPicture c             =  text [c]
      toPictureList           =  tex

instance IsPicture Int where
      toPicture               =  tex . show

instance IsPicture Integer where
      toPicture               =  tex . show

instance IsPicture Numeric where
      toPicture (Numeric n)   =  tex (show n)
      toPicture a             =  tex (show a)

instance IsPicture a => IsPicture [a]  where
      toPicture               =  toPictureList

instance IsPicture () where
      toPicture ()            =  empty

instance (IsPicture a, IsPicture b) => IsPicture (a, b)  where
      toPicture (a, b)        =  row [toPicture a, toPicture b]

instance (IsPicture a, IsPicture b, IsPicture c) => IsPicture (a, b, c)  where
      toPicture (a, b, c)     =  row [toPicture a, toPicture b, toPicture c]

instance IsPicture Path where
      toPicture p             =  setTrueBoundingBox (draw [p] empty)
      toPictureList ps        =  setTrueBoundingBox (draw ps empty)

instance IsPicture Area where
      toPicture a             =  setTrueBoundingBox (fill [a] empty)
      toPictureList as        =  setTrueBoundingBox (fill as empty)

instance HasRelax Picture where
      relax                   =  empty

instance HasColor Picture where
      setColor c (Attributes as p)
                              =  Attributes as{ aColor = c } p
      setColor c p            =  Attributes stdAttrib{ aColor = c } p
      setDefaultColor         =  setColor DefaultColor
      getColor (Attributes as _)
                              =  aColor as
      getColor _              =  DefaultColor

instance HasBGColor Picture where
      setBGColor c (Attributes as p)
                              =  Attributes as{ aBGColor = c } p
      setBGColor c p          =  Attributes stdAttrib{ aBGColor = c } p
      setDefaultBGColor       =  setBGColor DefaultColor
      getBGColor (Attributes as _)
                              =  aBGColor as
      getBGColor _            =  DefaultColor


instance HasName Picture where
      setName s (Attributes as p)
                              =  Attributes as{ aNames = toName s:aNames as } p
      setName s p             =  Attributes stdAttrib{ aNames = [toName s] } p
      getNames (Attributes as _)
                              =  aNames as
      getNames _              =  []

instance HasDefault Color where
      default'                =  DefaultColor

-----------------------------------------------------------------


mm, pt, dd, bp, cm, pc, cc, inch:: Numeric
mm                            =  2.83464
pt                            =  0.99626
dd                            =  1.06601
bp                            =  1
cm                            =  28.34645
pc                            =  11.95517
cc                            =  12.79213
inch                          =  72

up , down, left, right        :: Point
up                            =  vec( 0, 1)
down                          =  vec( 0,-1)
left                          =  vec(-1, 0)
right                         =  vec( 1, 0)


fullcircle, halfcircle        :: Path
fullcircle                    =  right ... down ... left ... up ... cycle'
halfcircle                    =  right ... down ... left
                              #  setStartVector down
                              #  setEndVector up

quartercircle, unitsquare     :: Path
quartercircle                 =  right ... down
                              #  setStartVector down
                              #  setEndVector left
unitsquare                    =  vec(0,0).-.vec(1,0).-.vec(1,1).-.vec(0,1).-.cycle'

(|-|)                         :: (IsPicture a, IsPicture b) => a -> b -> Picture
p1 |-| p2                     =  column [toPicture p1, toPicture p2]

(|||)                         :: (IsPicture a, IsPicture b) => a -> b -> Picture
p1 ||| p2                     =  row [toPicture p1, toPicture p2]

(|=|)                         :: (IsPicture a, IsPicture b) => a -> b -> Picture
p1 |=| p2                     =  columnSepBy 8 [toPicture p1, toPicture p2]

(||||)                        :: (IsPicture a, IsPicture b) => a -> b -> Picture
p1 |||| p2                    =  rowSepBy 8 [toPicture p1, toPicture p2]

row                           :: IsPicture a => [a] -> Picture
row                           =  rowSepBy 0

column                        :: IsPicture a => [a] -> Picture
column                        =  columnSepBy 0

rowSepBy                      :: IsPicture a => Numeric -> [a] -> Picture
rowSepBy hSep ps              =  overlay [ ref (i <+ E) + vec(hSep,0) .= ref (i+1 <+ W)
                                         | i <- [0..length ps - 2] ]
                                         ps

columnSepBy                   :: IsPicture a => Numeric -> [a] -> Picture
columnSepBy vSep ps           =  overlay [ ref (i <+ S) - vec(0, vSep) .= ref (i+1 <+ N)
                                         | i <- [0..length ps - 2] ]
                                         ps

enumPics                      :: (HasName a) => [a] -> [a]
enumPics ps                   =  [ setName (n::Int) p| (p, n) <- zip ps [0..] ]

fill                          :: (IsPicture a,IsArea b) => [b] -> a -> Picture
fill as p                     =  Fill (map toArea as) (toPicture p)

clip                          :: IsPicture a => Path -> a -> Picture
clip path p                   =  Clip path (toPicture p)

draw                          :: IsPicture a => [Path] -> a -> Picture
draw ps p                     =  Draw  (map toPath ps) (toPicture p)

at                            :: (IsPicture a, IsPicture b)
                              => Dir -> a -> b -> Picture
at d l p                      =  overlay' [ ref ((0::Int) <+ C)
                                              .= ref ((1::Int) <+ d)]
                                      (Just 1) [toPicture l, toPicture p]

label                         :: (IsPicture a, IsPicture b)
                              => Dir -> a -> b -> Picture
label d l p                   =  overlay' [ ref ((0::Int) <+ (-d))
                                              .= ref ((1::Int) <+ d)]
                                      (Just 1) [toPicture l, toPicture p]

instance HasDefine Picture where
      define eqs p            = Define eqs (toPicture p)

overlay                       :: IsPicture a => [Equation] -> [a] -> Picture
overlay eqs ps                =  overlay' eqs Nothing ps

overlay'                      :: IsPicture a => [Equation] -> Maybe Int -> [a]
                              -> Picture
overlay' eqs ibb ps           =  Overlay eqs ibb (map toPicture ps)


ooalign                       :: IsPicture a => [a] -> Picture
ooalign ps                    =  overlay [ ref (i <+ C) .= ref (i+1 <+ C)
                                         | i <- [0..length ps - 2] ]
                                         ps

text                          :: String -> Picture
text p                        =  Text p

tex                           :: String -> Picture
tex p                         =  Tex p

math                          :: String -> Picture
math p                        =  tex ("$" ++ p ++ "$")

empty                         :: Picture
empty                         =  space 0 0

vspace                        :: Numeric -> Picture
vspace n                      =  space 0 n

hspace                        :: Numeric -> Picture
hspace n                      =  space n 0

space                         :: Numeric -> Numeric -> Picture
space x y                     =  Empty x y

setTrueBoundingBox            :: IsPicture a => a -> Picture
setTrueBoundingBox            =  TrueBox . toPicture


------ Transformations ---------

scale                         :: IsPicture a => Numeric -> a -> Picture
scale n                       =  transform (scaled n)

rotate                        :: IsPicture a => Numeric -> a -> Picture
rotate a                      =  transform (rotated a)

skewX                         :: IsPicture a => Numeric -> a -> Picture
skewX a                       =  transform (skewedX a)

skewY                         :: IsPicture a => Numeric -> a -> Picture
skewY a                       =  transform (skewedY a)

reflectX                      :: IsPicture a => a -> Picture
reflectX                      =  transform reflectedX

reflectY                      :: IsPicture a => a -> Picture
reflectY                      =  transform reflectedY

transform                     :: IsPicture a => Transformation -> a -> Picture
transform m p                 =  PTransform m (toPicture p)

affine                        :: (Numeric,Numeric,Numeric,Numeric,Numeric,Numeric)
                              -> Transformation
affine (a,b,c,d,e,f)          =  Transformation a b c d e f

rotated                       :: Numeric -> Transformation
rotated a                     =  affine (cos a, -sin a, sin a, cos a, 0, 0)

reflectedX                    :: Transformation
reflectedX                    =  affine (1, 0, 0, -1, 0, 0)

reflectedY                    :: Transformation
reflectedY                    =  affine (-1, 0, 0, 1, 0, 0)

shifted                       :: (Numeric,Numeric) -> Transformation
shifted (x, y)                =  affine (1, 0, 0, 1, x, y)

scaledX                       :: Numeric -> Transformation
scaledX a                     =  affine (a, 0, 0, 1, 0, 0)

scaledY                       :: Numeric -> Transformation
scaledY a                     =  affine (1, 0, 0, a, 0, 0)

scaled                        :: Numeric -> Transformation
scaled a                      =  affine (a, 0, 0, a, 0, 0)

skewedX                       :: Numeric -> Transformation
skewedX a                     =  affine (1, a, 0, 1, 0, 0)

skewedY                       :: Numeric -> Transformation
skewedY a                     =  affine (1, 0, a, 1, 0, 0)

instance HasConcat Transformation where
      (Transformation a1 b1 c1 d1 e1 f1) & (Transformation a2 b2 c2 d2 e2 f2)
                              =  Transformation       (a1*a2 + c1*b2)
                                                      (b1*a2 + d1*b2)
                                                      (a1*c2 + c1*d2)
                                                      (b1*c2 + d1*d2)
                                                      (e1+e2) (f1+f2)

----- Paths -----

line                          :: (IsPath a, IsPath b) => a -> b -> Path
line p1 p2                    =  p1 .-. p2

curve                         :: (IsPath a, IsPath b) => a -> b -> Path
curve p1 p2                   =  p1 ... p2

arrow                         :: (IsPath b, IsPath a) => a -> b -> Path
arrow a b                     =  curve a b
                                 # setArrowHead default'

cycle'                        :: Path
cycle'                        =  PathCycle

------Arrows -----------------

instance HasDefault ArrowHead where
      default'                = defaultArrowHead

defaultArrowHead              :: ArrowHead
defaultArrowHead              =  ArrowHead Nothing Nothing AHFilled

arrowHeadBig                  :: ArrowHead
arrowHeadBig                  =  ArrowHead (Just 8) Nothing AHFilled

arrowHeadSize                 :: Double -> Double -> ArrowHead
arrowHeadSize a b             =  ArrowHead (Just a) (Just b) AHFilled

-----------------------

data Attrib                   =  Attrib{      aNames          :: [Name],
                                              aColor          :: Color,
                                              aBGColor        :: Color}
                                 deriving (Eq, Read)

instance Show Attrib where            -- Bug workaround fuer Hugs' Show
      showsPrec p (Attrib aNames aColor aBGColor)
                              =  showParen ( p >= 10 )
                                      (showString "Attrib{".
                                      showString "aNames=".
                                      showsPrec 10 aNames.
                                      showString ", ".
                                      showString "aColor=".
                                      showsPrec 10 aColor.
                                      showString ", ".
                                      showString "aBGColor=".
                                      showsPrec 10 aBGColor.
                                      showString "}")
      showList []             =  showString "[]"
      showList (x:xs)         =  showChar '[' . shows x . showl xs
              where
              showl []        =  showChar ']'
              showl (x:xs)    =  showString ", "
                                 . shows x
                                 . shows xs

stdAttrib                      :: Attrib
stdAttrib                     =  Attrib{      aNames          = [],
                                              aColor          = DefaultColor,
                                              aBGColor        = DefaultColor}

data FrameAttrib              =  FrameAttrib{ faNames         :: [Name],
                                              faColor,
                                              faBGColor       :: Color,
                                              faPen           :: Pen,
                                              faPattern       :: Pattern,
                                              faShadow        :: Maybe (Numeric, Numeric),
                                              faVisible       :: Bool}
                                 deriving (Eq, Read)

instance Show FrameAttrib where       -- Bug workaround fuer Hugs' Show
      showsPrec p (FrameAttrib faNames faColor faBGColor faPen faPattern faShadow
                      faVisible)
                              =  showParen ( p >= 10 )
                                      (showString "FrameAttrib{".
                                      showString "faNames=".
                                      showsPrec 10 faNames.
                                      showString ", ".
                                      showString "faColor=".
                                      showsPrec 10 faColor.
                                      showString ", ".
                                      showString "faBGColor=".
                                      showsPrec 10 faBGColor.
                                      showString ", ".
                                      showString "faPen=".
                                      showsPrec 10 faPen.
                                      showString ", ".
                                      showString "faPattern=".
                                      showsPrec 10 faPattern.
                                      showString ", ".
                                      showString "faShadow=".
                                      showsPrec 10 faShadow.
                                      showString ", ".
                                      showString "faVisible=".
                                      showsPrec 10 faVisible.
                                      showString "}")
      showList []             =  showString "[]"
      showList (x:xs)         =  showChar '[' . shows x . showl xs
              where
              showl []        =  showChar ']'
              showl (x:xs)    =  showString ", "
                                 . shows x
                                 . shows xs

stdFrameAttrib                :: FrameAttrib
stdFrameAttrib                =  FrameAttrib{ faNames         = [],
                                              faColor         = DefaultColor,
                                              faBGColor       = DefaultColor,
                                              faPen           = DefaultPen,
                                              faPattern       = DefaultPattern,
                                              faShadow        = Just (0,0),
                                              faVisible       = True}

instance HasPen FrameAttrib where
      setPen pen fa           =  fa{ faPen = pen }
      setDefaultPen           =  setPen DefaultPen
      getPen                  =  faPen

instance HasPattern FrameAttrib where
      setPattern pat fa       =  fa{ faPattern = pat }
      setDefaultPattern       =  setPattern DefaultPattern
      getPattern              =  faPattern

instance HasShadow FrameAttrib where
      setShadow a fa          =  fa{ faShadow = Just a }
      clearShadow fa          =  fa{ faShadow = Just (0,0) }
      getShadow               =  faShadow

instance HasColor FrameAttrib where
      setColor a fa           =  fa{ faColor = a }
      setDefaultColor         =  setColor DefaultColor
      getColor fa             =  faColor fa

instance HasBGColor FrameAttrib where
      setBGColor a fa         =  fa{ faBGColor = a }
      setDefaultBGColor       =  setBGColor DefaultColor
      getBGColor fa           =  faBGColor fa

instance HasName FrameAttrib where
      setName n fa            =  fa{ faNames = toName n:faNames fa }
      getNames fa             =  faNames fa

instance IsHideable FrameAttrib where
      hide fa                 =  fa{ faVisible = False }


-------------------- Pfade --------------


data PathElemDescr            =  PathElemDescr {
                                              peColor         :: Color,
                                              pePen           :: Pen,
                                              peArrowHead     :: Maybe ArrowHead,
                                              peSArrowHead    :: Maybe ArrowHead,
                                              pePattern       :: Pattern,
                                              peVisible       :: Bool,
                                              peStartCut,
                                              peEndCut        :: Maybe CutPic,
                                              peStartDir,
                                              peEndDir        :: Dir',
                                              peJoin          :: BasicJoin,
                                              peLabels        :: [PathLabel] }
                                 deriving (Eq, Read)

instance Show PathElemDescr where     -- Bug workaround fuer Hugs' Show
      showsPrec p (PathElemDescr peColor pePen peArrowHead peSArrowHead pePattern
                      peVisible peStartCut peEndCut peStartDir peEndDir
                      peJoin peLabels)
                              =  showParen ( p >= 10 )
                                      (showString "PathElemDescr{".
                                      showString "peColor=".
                                      showsPrec 10 peColor.
                                      showString ", ".
                                      showString "pePen=".
                                      showsPrec 10 pePen.
                                      showString ", ".
                                      showString "peArrowHead=".
                                      showsPrec 10 peArrowHead.
                                      showString ", ".
                                      showString "peSArrowHead=".
                                      showsPrec 10 peSArrowHead.
                                      showString ", ".
                                      showString "pePattern=".
                                      showsPrec 10 pePattern.
                                      showString ", ".
                                      showString "peVisible=".
                                      showsPrec 10 peVisible.
                                      showString ", ".
                                      showString "peStartCut=".
                                      showsPrec 10 peStartCut.
                                      showString ", ".
                                      showString "peEndCut=".
                                      showsPrec 10 peEndCut.
                                      showString ", ".
                                      showString "peStartDir=".
                                      showsPrec 10 peStartDir.
                                      showString ", ".
                                      showString "peEndDir=".
                                      showsPrec 10 peEndDir.
                                      showString ", ".
                                      showString "peJoin=".
                                      showsPrec 10 peJoin.
                                      showString ", ".
                                      showString "peLabels=".
                                      showsPrec 10 peLabels.
                                      showString "}")
      showList []             =  showString "[]"
      showList (x:xs)         =  showChar '[' . shows x . showl xs
              where
              showl []        =  showChar ']'
              showl (x:xs)    =  showString ", "
                                 . shows x
                                 . shows xs

stdPathElemDescr              :: PathElemDescr
stdPathElemDescr              =  PathElemDescr {
                                              peColor         = DefaultColor,
                                              pePen           = DefaultPen,
                                              peArrowHead     = Nothing,
                                              peSArrowHead    = Nothing,
                                              pePattern       = DefaultPattern,
                                              peVisible       = True,
                                              peStartCut      = Nothing,
                                              peEndCut        = Nothing,
                                              peStartDir      = DirEmpty,
                                              peEndDir        = DirEmpty,
                                              peJoin          = BJStraight,
                                              peLabels        = []
                                              }

data CutPic                   =  CutPicTrans  CutPic  [Int]
                              |  CutPic       Name
                              |  CutPic'      String
                                 deriving (Eq, Show, Read)

cutPic                        :: Name -> CutPic
cutPic                        =  CutPic

data PathLabel                =  PathLabel    Picture Double  Dir
                                 deriving (Eq, Show, Read)

--------------------------

data ArrowHead                =  DefaultArrowHead
                              |  ArrowHead    (Maybe Double)  (Maybe Double)
                                              ArrowHeadStyle
                                 deriving (Eq, Show, Read)

data ArrowHeadStyle           =  AHFilled
                              |  AHLine
                                 deriving (Eq, Show, Read)

ahFilled, ahLine              :: ArrowHeadStyle
ahFilled                      =  AHFilled
ahLine                        =  AHLine

setArrowHeadStyle             :: ArrowHeadStyle -> ArrowHead -> ArrowHead
setArrowHeadStyle s (ArrowHead a b _)
                              =  ArrowHead a b s
setArrowHeadStyle s DefaultArrowHead
                              =  ArrowHead Nothing Nothing s

getArrowHeadStyle             :: ArrowHead -> ArrowHeadStyle
getArrowHeadStyle (ArrowHead _ _ s)
                              =  s
getArrowHeadStyle DefaultArrowHead
                              =  AHFilled

--------------------------

image                         :: BitDepth -> [String] -> Picture
image Depth1 bls              = setTrueBoundingBox(overlay' [] (Just 0)
                                      [BitLine (vec(0,y)) Depth1 bl
                                      | (bl,y) <- zip bls [0,-0.12..]])
image Depth8 bls              = setTrueBoundingBox(overlay' [] (Just 0)
                                      [BitLine (vec(0,y)) Depth8 bl
                                      | (bl,y) <- zip bls [0,-0.12..]])
image Depth24 bls             = setTrueBoundingBox(overlay' [] (Just 0)
                                      [BitLine (vec(0,y)) Depth24 bl
                                      | (bl,y) <- zip bls [0,-0.36..]])


--------------------------

instance HasLabel PathElemDescr where
      setLabel i d l ped      =  ped{peLabels         = PathLabel (toPicture l) i d
                                                      : peLabels ped}
      removeLabel ped         =  ped{peLabels         = []}

instance HasColor PathElemDescr where
      setColor c ped          =  ped{ peColor         = c }
      setDefaultColor         =  setColor DefaultColor
      getColor ped            =  peColor ped

instance HasPattern PathElemDescr where
      setPattern pat ped      =  ped{pePattern        = pat}
      setDefaultPattern       =  setPattern DefaultPattern
      getPattern ped          =  pePattern ped

instance HasPen PathElemDescr where
      setPen pen ped          =  ped{pePen            = pen}
      setDefaultPen           =  setPen DefaultPen
      getPen ped              =  pePen ped

instance IsHideable PathElemDescr where
      hide ped                =  ped{peVisible        = False}

instance HasArrowHead PathElemDescr where
      setArrowHead ar ped     =  ped{peArrowHead      = Just ar}
      removeArrowHead ped     =  ped{peArrowHead      = Nothing}
      getArrowHead ped        =  peArrowHead ped
      setStartArrowHead ar ped
                              =  ped{peSArrowHead     = Just ar}
      removeStartArrowHead ped
                              =  ped{peSArrowHead     = Nothing}
      getStartArrowHead ped   =  peSArrowHead ped

instance HasStartEndCut PathElemDescr where
      setStartCut s ped       =  ped{peStartCut       = Just (cutPic $ toName s)}
      removeStartCut ped      =  ped{peStartCut       = Nothing}
      setEndCut s ped         =  ped{peEndCut         = Just (cutPic $ toName s)}
      removeEndCut ped        =  ped{peEndCut         = Nothing}

instance HasJoin PathElemDescr where
      setJoin bj ped          =  ped{ peJoin = bj }
      getJoin ped             =  peJoin ped

instance HasStartEndDir PathElemDescr where
      setStartAngle    a ped  =  ped{ peStartDir      = DirDir a}
      setEndAngle      a ped  =  ped{ peEndDir        = DirDir a}
      setStartCurl     a ped  =  ped{ peStartDir      = DirCurl a}
      setEndCurl       a ped  =  ped{ peEndDir        = DirCurl a}
      setStartVector   a ped  =  ped{ peStartDir      = DirVector a}
      setEndVector     a ped  =  ped{ peEndDir        = DirVector a}
      removeStartDir     ped  =  ped{ peStartDir      = DirEmpty}
      removeEndDir       ped  =  ped{ peEndDir        = DirEmpty}

----


pathLength                    :: Num a => Path -> a
pathLength (PathJoin p1 _ p2) =  pathLength p1 + 1 + pathLength p2
pathLength _                  =  0


forEachPath                   :: (PathElemDescr -> PathElemDescr)
                              -> Path -> Path
forEachPath f (PathJoin p1 ped p2)
                              =  PathJoin     (forEachPath f p1)
                                              (f ped)
                                              (forEachPath f p2)
forEachPath f (PathDefine eqs p)
                              =  PathDefine eqs (forEachPath f p)
forEachPath f (PathTransform t p)
                              =  PathTransform t (forEachPath f p)
forEachPath _ a               =  a


pathSetStart                  :: (PathElemDescr -> PathElemDescr) -> Path -> Path
pathSetStart f (PathJoin PathCycle ped p)
                              =  PathJoin PathCycle (f ped) p
pathSetStart f (PathJoin (PathPoint p) ped p2)
                              =  PathJoin (PathPoint p) (f ped) p2
pathSetStart f (PathJoin (PathEndDir d p) ped p2)
                              =  PathJoin (PathEndDir d p) (f ped) p2
pathSetStart f (PathJoin p1 ped p2)
                              =  PathJoin (pathSetStart f p1) ped p2
pathSetStart f (PathDefine eqs p)
                              =  PathDefine eqs (pathSetStart f p)
pathSetStart f (PathTransform t p)
                              =  PathTransform t (pathSetStart f p)
pathSetStart _ p              =  p

pathSetEnd                    :: (PathElemDescr -> PathElemDescr) -> Path -> Path
pathSetEnd f (PathJoin p ped PathCycle)
                              =  PathJoin p (f ped) PathCycle
pathSetEnd f (PathJoin p1 ped  (PathPoint p))
                              =  PathJoin p1 (f ped) (PathPoint p)
pathSetEnd f (PathJoin p1 ped  (PathEndDir d p))
                              =  PathJoin p1 (f ped) (PathEndDir d p)
pathSetEnd f (PathJoin p1 ped p2)
                              =  PathJoin p1 ped (pathSetEnd f p2)
pathSetEnd f (PathDefine eqs p )
                              =  PathDefine eqs (pathSetEnd f p)
pathSetEnd f (PathTransform t p)
                              =  PathTransform t (pathSetEnd f p)
pathSetEnd _ p                =  p

pathGetStart                  :: (PathElemDescr -> a) -> Path -> a
pathGetStart f (PathJoin PathCycle ped _)
                              =  f ped
pathGetStart f (PathJoin (PathPoint _) ped _)
                              =  f ped
pathGetStart f (PathJoin (PathEndDir _ _) ped _)
                              =  f ped
pathGetStart f (PathJoin p1 _ _)
                              =  pathGetStart f p1
pathGetStart f (PathDefine _ p)
                              =  pathGetStart f p
pathGetStart f (PathTransform _ p)
                              =  pathGetStart f p
pathGetStart f _              =  f stdPathElemDescr

pathGetEnd                    :: (PathElemDescr -> a) -> Path -> a
pathGetEnd f (PathJoin _ ped PathCycle)
                              =  f ped
pathGetEnd f (PathJoin _ ped (PathPoint _))
                              =  f ped
pathGetEnd f (PathJoin _ ped (PathEndDir _ _))
                              =  f ped
pathGetEnd f (PathJoin _ _ p2)=  pathGetEnd f p2
pathGetEnd f (PathDefine _ p )
                              =  pathGetEnd f p
pathGetEnd f (PathTransform _ p)
                              =  pathGetEnd f p
pathGetEnd f _                =  f stdPathElemDescr


instance HasLabel Path where
      setLabel i d l (PathJoin p1 ped p2)
              | i <=1         =  PathJoin p1 (setLabel i d l ped) p2
              | i > len1      =  setLabel (i-len1) d l p2
              | otherwise     =  setLabel i d l p1
                      where
                      len1    =  pathLength p1
      setLabel i d l (PathDefine eqs p)
                              =  PathDefine eqs (setLabel i d l p)
      setLabel i d l (PathTransform t p)
                              =  PathTransform t (setLabel i d l p)
      setLabel _ _ _ p        =  p
      removeLabel             =  forEachPath removeLabel


instance HasColor Path where
      setColor c p            =  forEachPath (setColor c) p
      setDefaultColor         =  setColor DefaultColor
      getColor (PathJoin _ ped _)
                              =  getColor ped
      getColor _              =  DefaultColor

instance HasPattern Path where
      setPattern pat          =  forEachPath (setPattern pat)
      setDefaultPattern       =  setPattern DefaultPattern
      getPattern (PathJoin _ ped _)
                              =  getPattern ped
      getPattern _            =  DefaultPattern

instance HasPen Path where
      setPen pen              =  forEachPath (setPen pen)
      setDefaultPen           =  setPen DefaultPen
      getPen (PathJoin _ ped _ )
                              =  getPen ped
      getPen _                =  DefaultPen

instance IsHideable Path where
      hide                    =  forEachPath hide

instance HasArrowHead Path where
      setArrowHead ar         =  pathSetEnd (setArrowHead ar)
      removeArrowHead         =  pathSetEnd removeArrowHead
      getArrowHead            =  pathGetEnd peArrowHead
      setStartArrowHead ar    =  pathSetStart (setStartArrowHead ar)
      removeStartArrowHead    =  pathSetStart removeStartArrowHead
      getStartArrowHead       =  pathGetStart peSArrowHead

instance HasStartEndDir Path where
      setStartAngle d (PathEndDir p _)
                              =  PathEndDir p (DirDir d)
      setStartAngle d (PathPoint p)
                              =  PathEndDir p (DirDir d)
      setStartAngle d p       =  pathSetStart (setStartAngle d) p

      setEndAngle d (PathEndDir p _)
                              =  PathEndDir p (DirDir d)
      setEndAngle d (PathPoint p)
                              =  PathEndDir p (DirDir d)
      setEndAngle a p         =  pathSetEnd (setEndAngle a) p


      setStartCurl a (PathEndDir p _)
                              =  PathEndDir p (DirCurl a)
      setStartCurl a (PathPoint p)
                              =  PathEndDir p (DirCurl a)
      setStartCurl a p        =  pathSetStart (setStartCurl a) p

      setEndCurl a (PathEndDir p _)
                              =  PathEndDir p (DirCurl a)
      setEndCurl a (PathPoint p)
                              =  PathEndDir p (DirCurl a)
      setEndCurl a p          =  pathSetEnd (setEndCurl a) p


      setStartVector a (PathEndDir p _)
                              =  PathEndDir p (DirVector a)
      setStartVector a (PathPoint p)
                              =  PathEndDir p (DirVector a)
      setStartVector a p      =  pathSetStart (setStartVector a) p

      setEndVector a (PathEndDir p _)
                              =  PathEndDir p (DirVector a)
      setEndVector a (PathPoint p)
                              =  PathEndDir p (DirVector a)
      setEndVector a p        =  pathSetEnd (setEndVector a) p

      removeStartDir (PathEndDir p _)
                              =  PathPoint p
      removeStartDir p        =  pathSetStart removeStartDir p
      removeEndDir (PathEndDir p _)
                              =  PathPoint p
      removeEndDir p          =  pathSetEnd (removeEndDir) p

instance HasJoin Path where
      setJoin bj p            =  forEachPath (setJoin bj) p
      getJoin (PathJoin _ ped _)
                              =  peJoin ped
      getJoin _               =  BJStraight

instance HasStartEndCut Path where
      setStartCut s           =  pathSetStart (setStartCut $ toName s)
      setEndCut s             =  pathSetEnd (setEndCut $ toName s)
      removeStartCut          =  pathSetStart removeStartCut
      removeEndCut            =  pathSetEnd removeEndCut

-------------------- Pfade ------------------

data Path                     =  PathBuildCycle       Path    Path
                              |  PathTransform        Transformation  Path
                              |  PathPoint            Point
                              |  PathCycle
                              |  PathJoin             Path    PathElemDescr   Path
                              |  PathEndDir           Point   Dir'
                              |  PathDefine           [Equation]      Path
                                 deriving (Eq, Show, Read)

data Dir'                     =  DirEmpty
                              |  DirCurl      Numeric
                              |  DirDir       Numeric
                              |  DirVector    Point
                                 deriving (Eq, Show, Read)

data BasicJoin                =  BJCat
                              |  BJFree
                              |  BJBounded
                              |  BJStraight
                              |  BJTense
                              |  BJTension    Tension
                              |  BJTension2   Tension Tension
                              |  BJControls   Point
                              |  BJControls2  Point   Point
                                 deriving (Eq, Show, Read)

data Tension                  =  Tension              Numeric
                              |  TensionAtLeast       Numeric
                                 deriving (Eq, Show, Read)

joinCat                       :: BasicJoin
joinCat                       =  BJCat

joinFree                      :: BasicJoin
joinFree                      =  BJFree

joinBounded                   :: BasicJoin
joinBounded                   =  BJBounded

joinStraight                  :: BasicJoin
joinStraight                  =  BJStraight

joinTense                     :: BasicJoin
joinTense                     =  BJTense

joinTension                   :: Tension -> BasicJoin
joinTension                   =  BJTension

joinTensions                  :: Tension -> Tension -> BasicJoin
joinTensions                  =  BJTension2

joinControl                   :: Point -> BasicJoin
joinControl                   =  BJControls

joinControls                  :: Point -> Point -> BasicJoin
joinControls                  =  BJControls2

tension                       :: Numeric -> Tension
tension                       =  Tension

tensionAtLeast                :: Numeric -> Tension
tensionAtLeast                =  TensionAtLeast

instance HasDefine Path where
      define eqs p            =  PathDefine eqs p

defaultCut                    :: (Name -> a -> a) -> Point -> (a -> a)
defaultCut f (PointVar name)
      | lastNameIsDir name    =  f  $ withoutDir name
      | otherwise             =  id
defaultCut f (PointTrans' p _)=  defaultCut f p                       ---
defaultCut _ _                =  id

defaultStartCut               :: HasStartEndCut a => Path -> (a -> a)
defaultStartCut (PathPoint p) =  defaultCut setStartCut p
defaultStartCut (PathEndDir p  _)
                              =  defaultCut setStartCut p
defaultStartCut (PathJoin _ _ p)
                              =  defaultStartCut p
defaultStartCut _             =  id

defaultEndCut                 :: HasStartEndCut a => Path -> (a -> a)
defaultEndCut (PathPoint p)   =  defaultCut setEndCut p
defaultEndCut (PathEndDir p _)=  defaultCut setEndCut p
defaultEndCut (PathJoin p _ _)=  defaultEndCut p
defaultEndCut _               =  id

lastNameIsDir                 :: Name -> Bool
lastNameIsDir (Hier _ name)   =  lastNameIsDir name
lastNameIsDir (NameDir _)     =  True
lastNameIsDir _               =  False

withoutDir                    :: Name -> Name
withoutDir (Hier name (NameDir _))
                              =  name
withoutDir (Hier n1 n2)       =  Hier n1 (withoutDir n2)
withoutDir name               =  name

class IsPath a where
      toPath                  :: a -> Path
      toPathList              :: [a] -> Path
      toPathList ps           =  foldl1 (.-.) (map toPath ps)

instance IsPath Path where
      toPath                  =  id

instance IsPath Point where
      toPath                  =  PathPoint

instance IsPath Name where
      toPath                  =  toPath . ref

instance IsPath a => IsPath [a] where
      toPath                  =  toPathList

instance IsPath Char where
      toPath                  =  toPath . ref
      toPathList              =  toPath . ref

instance (Real a, Real b) => IsPath (a, b) where
      toPath (a, b)           =  toPath (vec (fromRational $ toRational a,
                                              fromRational $ toRational b))

buildCycle                    :: Path -> Path -> Path
buildCycle                    =  PathBuildCycle

transformPath                 :: Transformation -> Path -> Path
transformPath                 =  PathTransform

instance HasConcat Path where
      (&)                     =  (.&.)

(.&.)                         :: (IsPath a, IsPath b) => a -> b -> Path
p1 .&. p2                     =  PathJoin (toPath p1)
                                          (stdPathElemDescr # setJoin BJCat)
                                          (toPath p2)

(...)                         :: (IsPath a, IsPath b) => a -> b -> Path
p1 ... p2                     =  PathJoin p1' (stdPathElemDescr
                                               # setJoin BJFree
                                               # defaultStartCut p1'
                                               # defaultEndCut p2') p2'
              where   p1'     =  toPath p1
                      p2'     =  toPath p2

(.-.)                         :: (IsPath a, IsPath b) => a -> b -> Path
p1 .-. p2                     =  PathJoin p1' (stdPathElemDescr
                                               # setJoin BJStraight
                                               # defaultStartCut p1'
                                               # defaultEndCut p2') p2'
              where   p1'     =  toPath p1
                      p2'     =  toPath p2

(.--.)                        :: (IsPath a, IsPath b) => a -> b -> Path
p1 .--. p2                    =  PathJoin p1' (stdPathElemDescr
                                               # setJoin  BJTense
                                               # defaultStartCut p1'
                                               # defaultEndCut p2') p2'
              where   p1'     =  toPath p1
                      p2'     =  toPath p2

(....)                        :: (IsPath a, IsPath b) => a -> b -> Path
p1 .... p2                    =  PathJoin p1' (stdPathElemDescr
                                               # setJoin  BJBounded
                                               # defaultStartCut p1'
                                               # defaultEndCut p2') p2'
              where   p1'     =  toPath p1
                      p2'     =  toPath p2



--------------------- Flaechen ------------------

data Area                     =  Area AreaDescr Path
                                 deriving (Eq, Show, Read)

data AreaDescr                =  AreaDescr {  arColor         :: Color,
                                              arLayer         :: Layer,
                                              arPen           :: Pen}
                                 deriving (Eq, Read)

instance Show AreaDescr where         -- Bug workaround fuer Hugs' Show
      showsPrec p (AreaDescr arColor arLayer arPen)
                              =  showParen ( p >= 10 )
                                      (showString "AreaDescr{".
                                      showString "arColor=".
                                      showsPrec 10 arColor.
                                      showString ", ".
                                      showString "arLayer=".
                                      showsPrec 10 arLayer.
                                      showString ", ".
                                      showString "arPen=".
                                      showsPrec 10 arPen.
                                      showString "}")

stdAreaDescr                  :: AreaDescr
stdAreaDescr                  =  AreaDescr {  arColor         = black,
                                              arLayer         = Front,
                                              arPen           = DefaultPen}

class IsArea a where
      toArea                  :: a -> Area

instance IsArea Area where
      toArea                  =  id

instance IsArea Path where
      toArea a                =  Area stdAreaDescr (ensureCycle a)

instance (IsPath a) => IsArea  [a] where
      toArea ps               =  Area stdAreaDescr (foldr (.-.) PathCycle ps)

instance HasDefine Area where
      define eqs (Area ad p)  =  Area ad (PathDefine eqs p)

instance HasColor Area where
      setColor c (Area ad ps) =  Area ad{ arColor = c } ps
      setDefaultColor a       =  setColor DefaultColor a
      getColor (Area ad _)    =  arColor ad

instance HasPen Area where
      setPen pen (Area ad ps) =  Area ad{ arPen = pen } ps
      setDefaultPen a         =  setPen DefaultPen a
      getPen (Area ad _)      =  arPen ad

instance HasLayer Area where
      setBack (Area ad ps)    =  Area ad{ arLayer = Back } ps
      setFront (Area ad ps)   =  Area ad{ arLayer = Front } ps
      getLayer (Area ad _)    =  arLayer ad


ensureCycle                   :: Path -> Path
ensureCycle(PathTransform t p)=  PathTransform t (ensureCycle p)
ensureCycle (PathPoint  p)    =  PathJoin (PathPoint p) stdPathElemDescr PathCycle
ensureCycle (PathJoin p1 ped p2)
                              =  PathJoin p1 ped (ensureCycle p2)
ensureCycle (PathEndDir p d)  =  PathJoin (PathEndDir p d) stdPathElemDescr PathCycle
ensureCycle (PathDefine eqs p)=  PathDefine eqs (ensureCycle p)
ensureCycle p                 =  p

getDefault                    :: Maybe a -> a -> a
getDefault (Just a) _         =  a
getDefault Nothing  b         =  b


------------------------------


data Frame                    =  Frame' FrameAttrib ExtentAttrib Path Picture
                                 deriving Show

data AbsOrRel                 =  AORAbs Numeric
                              |  AORRel Numeric
                              |  AORDefault
                                 deriving Show

data ExtentAttrib             =  ExtentAttrib{eaX, eaY        :: AbsOrRel,
                                              eaEqsDX         :: [Equation],
                                              eaEqsDY         :: [Equation],
                                              eaEqsWidth      :: [Equation],
                                              eaEqsHeight     :: [Equation],
                                              eaEqs           :: [Equation]}
                                 deriving Show

stdExtentAttrib               :: ExtentAttrib
stdExtentAttrib               =  ExtentAttrib{eaX             = AORDefault,
                                              eaY             = AORDefault,
                                              eaEqsDX         = [],
                                              eaEqsDY         = [],
                                              eaEqsWidth      = [],
                                              eaEqsHeight     = [],
                                              eaEqs           = [] }

instance HasRelax Frame where
      relax                   =  box empty # setWidth 0 # setHeight 0 # hide

instance HasColor Frame where
      setColor c (Frame' fa ea path p)
                              =  Frame' (setColor c fa) ea path p
      setDefaultColor f       =  setColor DefaultColor f
      getColor (Frame' fa _ _  _)
                              =  getColor fa

instance HasBGColor Frame where
      setBGColor c (Frame' fa ea path p)
                              =  Frame' (setBGColor c fa) ea path p
      setDefaultBGColor f     =  setBGColor DefaultColor f
      getBGColor (Frame' fa _  _ _)
                              =  getBGColor fa

instance HasPen Frame where
      setPen c (Frame' fa ea path p)
                              =  Frame' (setPen c fa) ea path p
      setDefaultPen f         =  setPen DefaultPen f
      getPen (Frame' fa _ _ _)=  getPen fa

instance HasShadow Frame where
      setShadow c (Frame' fa ea path p)
                              =  Frame' (setShadow c fa) ea path p
      clearShadow (Frame' fa ea path p)
                              =  Frame' (clearShadow fa) ea path p
      getShadow (Frame' fa _ _ _)
                              =  getShadow fa

instance HasPattern Frame where
      setPattern c (Frame' fa ea path p)
                              =  Frame' (setPattern c fa) ea path p
      setDefaultPattern f     =  setPattern DefaultPattern f
      getPattern (Frame' fa _  _ _)
                              =  getPattern fa

instance HasName Frame where
      setName n (Frame' fa ea path p)
                              =  Frame' (setName n fa) ea path p
      getNames (Frame' fa _ _  _)
                              =  getNames fa

instance HasDXY Frame where
      setDX dx (Frame' fa ea path p)
                              =  Frame' fa ea{ eaX = AORRel dx} path p
      setDY dy (Frame' fa ea path p)
                              =  Frame' fa ea{ eaY = AORRel dy} path p
      getDX (Frame' _ ea _ _)=  case eaX ea of
                                      AORRel dx       -> Just dx
                                      _               -> Nothing
      getDY (Frame' _ ea _ _)=  case eaY ea of
                                      AORRel dy       -> Just dy
                                      _               -> Nothing

instance HasExtent Frame where
      setWidth w (Frame' fa ea path p)
                              =  Frame' fa ea{ eaX = AORAbs w} path p
      setHeight h (Frame' fa ea path p)
                              =  Frame' fa ea{ eaY = AORAbs h} path p
      getWidth (Frame' _ ea _ _ )
                              =  case eaX ea of
                                      AORAbs w        -> Just w
                                      _               -> Nothing
      getHeight (Frame' _ ea  _ _)
                              =  case eaY ea of
                                      AORAbs h        -> Just h
                                      _               -> Nothing
      removeWidth (Frame' fa ea path p)
                              = Frame' fa ea{ eaX = AORDefault } path p
      removeHeight (Frame' fa ea path p)
                              = Frame' fa ea{ eaY = AORDefault } path p

instance IsHideable Frame where
      hide (Frame' fa ea path p)
                              =  Frame' (hide fa) ea path p

instance IsPicture Frame where
      toPicture (Frame' fa ea path p)
                              =  Frame fa eqs path p
              where eqs       =  equations (case eaX ea of
                                   AORRel dx  -> (var "dx" .= dx):eaEqsDX ea
                                   AORDefault -> (var "dx" .= 2):eaEqsDX ea
                                   AORAbs w   -> (var "width" .= w):eaEqsWidth ea)
                                 :equations (case eaY ea of
                                   AORRel dy  -> (var "dy" .= dy):eaEqsDY ea
                                   AORDefault -> (var "dy" .= 2):eaEqsDY ea
                                   AORAbs h   -> (var "height" .= h):(var "dy" .= 2):eaEqsHeight ea)
                                 :eaEqs ea

dot                           :: Frame
dot                           =  circle empty
                                 # setBGColor black
                                 # setDX 0.75

bullet                        :: Frame
bullet                        =  circle empty
                                 # setBGColor black
                                 # setDX 1.5

box                           :: IsPicture a => a -> Frame
box p                         =  Frame' stdFrameAttrib
                                        stdExtentAttrib{eaEqs         = eqs,
                                                        eaEqsDX       = eqsDX,
                                                        eaEqsDY       = eqsDY,
                                                        eaEqsWidth    = eqsWidth,
                                                        eaEqsHeight   = eqsHeight}
                                        path
                                        (toPicture p)
      where eqsDX             =  [ref E .= ref ("last" <+ E) + vec(var "dx",0),
                                  ref W .= ref ("last" <+ W) - vec(var "dx",0) ]
            eqsDY             =  [ref N .= ref ("last" <+ N) + vec(0,var "dy"),
                                  ref S .= ref ("last" <+ S) - vec(0,var "dy") ]
            eqsWidth          =  [ref E .= ref W + vec(var "width",0),
                                  ref C - ref W .= ref E - ref C ]
            eqsHeight         =  [ref N .= ref S + vec(0,var "height"),
                                  ref C - ref S .= ref N - ref C]
            eqs               =  [ref C .= ref ("last" <+ C),
                                  xpart (ref NE) .= xpart (ref SE),
                                  ypart (ref NW) .= ypart (ref NE),
                                  ref W .= med 0.5 (ref NW) (ref SW),
                                  ref S .= med 0.5 (ref SW) (ref SE),
                                  ref E .= med 0.5 (ref NE) (ref SE),
                                  ref N .= med 0.5 (ref NE) (ref NW) ]
            path              =  ref NE .-. ref SE .-. ref SW .-. ref NW .-. cycle'

triangle                      :: IsPicture a => a -> Frame
triangle                      =  triAngle 28

triAngle                      :: IsPicture a => Numeric -> a -> Frame
triAngle a p                  =  Frame' stdFrameAttrib
                                        stdExtentAttrib{eaEqs         = eqs,
                                                        eaEqsDX       = eqsDX,
                                                        eaEqsDY       = eqsDY,
                                                        eaEqsWidth    = eqsWidth,
                                                        eaEqsHeight   = eqsHeight}
                                        path
                                        (toPicture p)
      where
      eqsDX                   =  [ref SE .= ref ("last" <+ SE) + vec(var "dx",-var "dy"),
                                  ref S .= ref ("last" <+ S) - vec(0,var "dy") ]
      eqsDY                   =  [ypart (ref N-ref S) .= 0.5*tan (90-a/2)*xpart (ref SE - ref SW) ]
      eqsWidth                =  [xpart(ref SE) .= xpart(ref SW) + var "width",
                                  ref S .= ref ("last" <+ S) - vec(0,var "dy") ]
      eqsHeight               =  [ypart (ref N-ref S) .= var "height"]
      eqs                     =  [ref C .= ref N,
                                  ypart (ref SW) .= ypart (ref SE),
                                  ref S - ref SW .= ref SE - ref S,
                                  equal [ref NW, ref N, ref NE],
                                  xpart (ref S) .= xpart (ref N),
                                  ref W .= ref SW,
                                  ref E .= ref SE]
      path                    =  ref SW .-. ref SE .-. ref N .-. cycle'

rbox                          :: IsPicture a => Numeric -> a -> Frame
rbox r p                      =  Frame' stdFrameAttrib
                                        stdExtentAttrib{eaEqs         = eqs,
                                                        eaEqsDX       = eqsDX,
                                                        eaEqsDY       = eqsDY,
                                                        eaEqsWidth    = eqsWidth,
                                                        eaEqsHeight   = eqsHeight}
                                        path
                                        (toPicture p)
      where
      eqsDX                   =  [ref E .= ref ("last" <+ E) + vec(var "dx",0),
                                  ref W .= ref ("last" <+ W) - vec(var "dx",0) ]
      eqsDY                   =  [ref N .= ref ("last" <+ N) + vec(0,var "dy"),
                                  ref S .= ref ("last" <+ S) - vec(0,var "dy") ]
      eqsWidth                =  [ref E .= ref W + vec(var "width",0),
                                  ref C - ref W .= ref E - ref C ]
      eqsHeight               =  [ref N .= ref S + vec(0,var "height"),
                                  ref C - ref S .= ref N - ref C]
      eqs                     =  [ref C .= ref ("last" <+ C),
                                  xpart (ref NE) .= xpart (ref SE),
                                  ypart (ref NW) .= ypart (ref NE),
                                  ref W .= med 0.5 (ref NW) (ref SW),
                                  ref S .= med 0.5 (ref SW) (ref SE),
                                  ref E .= med 0.5 (ref NE) (ref SE),
                                  ref N .= med 0.5 (ref NE) (ref NW),
                                  var "r" .= minimum' [r, 0.5*ypart (ref N-ref S),
                                                       0.5*xpart (ref E-ref W)]]
      path                    =    ref SW + vec(var "r", 0) .--. ref SE - vec(var "r", 0)
                              ...  ref SE + vec(0,       var "r") .--. ref NE - vec(0,           var "r")
                              ...  ref NE - vec(var "r", 0).--. ref NW + vec(var "r", 0)
                              ...  ref NW - vec(0,       var "r").--. ref SW + vec(0,    var "r")
                              ...  cycle'

oval                          :: IsPicture a => a -> Frame
oval p                        =  Frame' stdFrameAttrib
                                        stdExtentAttrib{eaEqs         = eqs,
                                                        eaEqsDX       = eqsDX,
                                                        eaEqsDY       = eqsDY,
                                                        eaEqsWidth    = eqsWidth,
                                                        eaEqsHeight   = eqsHeight}
                                        path
                                        (toPicture p)
      where
      eqsDX                   =  [ref E .= ref ("last" <+ E) + vec(var "dx",0),
                                  ref W .= ref ("last" <+ W) - vec(var "dx",0) ]
      eqsDY                   =  [ref N .= ref ("last" <+ N) + vec(0,var "dy"),
                                  ref S .= ref ("last" <+ S) - vec(0,var "dy") ]
      eqsWidth                =  [ref E .= ref W + vec(var "width",0),
                                  ref C - ref W .= ref E - ref C ]
      eqsHeight               =  [ref N .= ref S + vec(0,var "height"),
                                  ref C - ref S .= ref N - ref C ]
      eqs                     =  [ref C .= ref ("last" <+ C),
                                  xpart (ref NE)  .= xpart (ref SE),
                                  ypart (ref NW)  .= ypart (ref NE),
                                  ref W .= med 0.5 (ref NW) (ref SW),
                                  ref S .= med 0.5 (ref SW) (ref SE),
                                  ref E .= med 0.5 (ref NE) (ref SE),
                                  ref N .= med 0.5 (ref NE) (ref NW)]
      path                    = ref N .... ref E ....  ref S .... ref W ....  cycle'

circle                        :: IsPicture a => a -> Frame
circle p                      =  Frame' stdFrameAttrib
                                        stdExtentAttrib{eaEqs         = eqs,
                                                        eaEqsDX       = eqsDX,
                                                        eaEqsDY       = eqsDY,
                                                        eaEqsWidth    = eqsWidth,
                                                        eaEqsHeight   = eqsHeight}
                                        path
                                        (toPicture p)
      where
      eqsDX                   =  [
              var "r"         .= 0.5*maximum'[xpart(ref ("last" <+ E)-ref ("last" <+ W)),
                                              ypart(ref ("last" <+ N)-ref ("last" <+ S))]
                                              +var "dx"]
      eqsDY                   =  []
      eqsWidth                =  [
              var "r"         .= 0.5*var "width"]
      eqsHeight               =  []
      eqs                     =  [ref C .= ref ("last" <+ C),
                                  ref N .= ref C + vec(0,var "r"),
                                  ref E .= ref C + vec(var "r",0),
                                  ref S .= ref C - vec(0,var "r"),
                                  ref W .= ref C - vec(var "r",0),
                                  xpart (ref NE)  .= xpart (ref SE),
                                  ypart (ref NW)  .= ypart (ref NE),
                                  ref W + vec(0.293*var"r",0) .= med 0.5 (ref NW) (ref SW),
                                  ref S + vec(0,0.293*var"r") .= med 0.5 (ref SW) (ref SE),
                                  ref E - vec(0.293*var"r",0) .= med 0.5 (ref NE) (ref SE),
                                  ref N - vec(0,0.293*var"r") .= med 0.5 (ref NW) (ref NE)]
      path                    =  ref N ... ref E ... ref S ... ref W ... cycle'
