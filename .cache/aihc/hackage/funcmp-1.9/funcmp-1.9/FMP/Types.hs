{- |
   Module      :  FMP.Types
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

module FMP.Types (
      HasMed(..), HasDefault(..),
      Dir(..), Pen(..), Pattern(..),
      Equation(..),     Boolean(..), BoolRelat(..),
      Point(..), Numeric(..),
      FunPPP(..), FunPN(..), FunNN(..), FunNNN(..), FunNsN(..),
      HasRelax(..),     HasCond(..),
      penSquare, penCircle,
      dashed, dotted, dashPattern, dashPattern',
      boolean, equations,
      vec, dir, xy, (.*),
      pi, exp, log, sqrt, (**) ,
      sin, cos, tan,
      asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh,
      pythAdd, xpart, ypart, angle, minimum', maximum',
      width, height, xdist, ydist, dist,
      (.=), (.==), (./=), (.<) , (.<=), equal,
      whatever,
      Name(..), global ,ref, var, (<+), (<*),
      IsName(..)
      ) where

import Prelude ( Real(..), Enum(..), Floating(..), Fractional(..), Num(..), Ord(..)
               , takeWhile, map, iterate, fromIntegral, (.), Read(..), Show(..)
               , Eq(..), Double, Int, id, Char, error, otherwise, Bool(..), (||)
               , String, Integral(..)
               )

infixr 0 <+, <*
infixr 5 .=
infixr 7 .*

class HasWhatever a where
      whatever                :: a

class HasRelax a where
      relax                   :: a

class HasMed a where
      med                     :: Numeric -> a -> a-> a

class HasCond a where
      cond                    :: Boolean -> a -> a -> a

class HasDefault a where
      default'                :: a

data Dir                      =  C | N | NE | E | SE | S | SW | W | NW
                                 deriving (Eq, Read, Show, Enum, Ord)

instance Num Dir where
      a + b                   =  toEnum (fromEnum a + fromEnum b)
      a - b                   =  toEnum (fromEnum a - fromEnum b)
      a * b                   =  toEnum (fromEnum a * fromEnum b)
      negate C                =  C
      negate d                =  toEnum (if i >= 5
                                              then mod (i+5) 9
                                              else mod (i+4) 9)
              where i         =  fromEnum d
      abs                     =  id
      signum C                =  C
      signum _                =  N
      fromInteger a           =  toEnum (mod (fromInteger a) 9)


-------------------- Stifte ------------------

data Pen                      =  DefaultPen
                              |  PenSquare (Numeric, Numeric) Numeric
                              |  PenCircle (Numeric, Numeric) Numeric
                                 deriving (Eq, Show, Read)

instance Num Pen where
      PenCircle (a1, b1) c1    +  PenCircle (a2, b2) c2
                              =  PenCircle (a1 + a2, b1 + b2) ((c1+c2)/2)
      a               + _     =  a
      PenCircle (a1, b1) c1      -  PenCircle (a2, b2) c2
                              =  PenCircle (a1 - a2, b1 - b2) ((c1+c2)/2)
      a               - _     =  a
      PenCircle (a1, b1) c1    *  PenCircle (a2, b2) c2
                              =  PenCircle (a1 * a2, b1 * b2) ((c1+c2)/2)
      a               * _     =  a
      negate a                =  a
      abs a                   =  a
      signum _                =  1
      fromInteger a           =  PenCircle (fromInteger a, fromInteger a) 0

instance Fractional Pen where
      PenCircle (a1, b1) c1    /  PenCircle (a2, b2) c2
                              =  PenCircle (a1 / a2, b1 / b2) ((c1+c2)/2)
      a               / _     =  a
      recip (PenCircle (a, b) c)
                              =  PenCircle (1 / a, 1 / b) c
      recip a                 =  1 / a
      fromRational a          =  PenCircle (fromRational a, fromRational a) 0

instance HasDefault Pen where
      default'                =  DefaultPen

penSquare                     :: (Numeric, Numeric) -> Numeric -> Pen
penSquare                     =  PenSquare

penCircle                     :: (Numeric, Numeric) -> Numeric -> Pen
penCircle                     =  PenCircle

-------------------- Strichmuster --------------

data Pattern                  =  DefaultPattern
                              |  DashPattern [Double]
                                 deriving (Eq, Show, Read)

instance HasDefault Pattern where
      default'                =  DefaultPattern

dashPattern                   :: [Double] -> Pattern
dashPattern                   =  DashPattern

dashPattern'                  :: [Double] -> Pattern
dashPattern' p                =  dashPattern (-1:p)

dashed                        :: Pattern
dashed                        =  DashPattern [3, 3]

dotted                        :: Pattern
dotted                        =  DashPattern [-1, 1, 0, 1]

-------------------- Gleichungen ------------------

class IsEquation a where
      (.=)                    :: a -> a -> Equation
      equal                   :: [a] -> Equation

class IsBoolean a where
      (.==)                   :: a -> a -> Boolean
      (./=)                   :: a -> a -> Boolean
      (.<)                    :: a -> a -> Boolean
      (.<=)                   :: a -> a -> Boolean

data Equation                 =  NEquations   [Numeric]
                              |  PEquations   [Point]
                              |  Equations    [Equation]
                              |  EquationCond Boolean Equation        Equation
                                 deriving (Eq, Show, Read)

data Boolean                  =  Boolean      Bool
                              |  BoolNum      Numeric BoolRelat       Numeric
                              |  BoolPnt      Point   BoolRelat       Point
                              |  BoolOr       Boolean Boolean
                              |  BoolAnd      Boolean Boolean
                              |  BoolNot      Boolean
                                 deriving (Eq, Show, Read, Ord)

data BoolRelat                =  BoolEQ | BoolL | BoolLE | BoolNE
                                 deriving (Eq, Show, Read, Ord)

instance IsEquation Numeric where
      n1 .= n2                =  NEquations   [n1, n2]
      equal                   =  NEquations

instance IsEquation Point where
      p1 .= p2                =  PEquations   [p1, p2]
      equal                   =  PEquations

instance IsBoolean Numeric where
      n1 .== n2               =  BoolNum      n1      BoolEQ  n2
      n1 ./= n2               =  BoolNum      n1      BoolNE  n2
      n1 .<  n2               =  BoolNum      n1      BoolL   n2
      n1 .<= n2               =  BoolNum      n1      BoolLE  n2

instance IsBoolean Point where
      p1 .== p2               =  BoolPnt      p1      BoolEQ  p2
      p1 ./= p2               =  BoolPnt      p1      BoolNE  p2
      p1 .<  p2               =  BoolPnt      p1      BoolL   p2
      p1 .<= p2               =  BoolPnt      p1      BoolLE  p2

instance Num Boolean where
      a + b                   =  BoolOr a b
      a - b                   =  a * (-b)
      a * b                   =  BoolAnd a b
      negate a                =  BoolNot a
      abs _                   =  Boolean True
      signum a                =  a
      fromInteger a           =  Boolean (a>0)

instance HasCond Equation where
      cond b t e              =  EquationCond b t e

boolean                       :: Bool -> Boolean
boolean                       =  Boolean

equations                     :: [Equation] -> Equation
equations                     =  Equations

data  Name                    =  NameInt      Int
                              |  NameStr      String
                              |  NameDir      Dir
                              |  Hier         Name Name
                              |  Global       Name
                                 deriving (Show, Read, Eq, Ord)

global                        :: (IsName a) => a -> Name
global                        =  Global . removeGlobals . toName

hasGlobal                     :: Name -> Bool
hasGlobal (Global _)          =  True
hasGlobal (Hier a b)          =  hasGlobal a || hasGlobal b
hasGlobal _                   =  False

globalToFront                 :: Name -> Name
globalToFront a
      | hasGlobal a           =  Global (removeGlobals a)
      | otherwise             =  a

removeGlobals                 :: Name -> Name
removeGlobals (Global a)      =  removeGlobals a
removeGlobals (Hier a b)      =  Hier (removeGlobals a) (removeGlobals b)
removeGlobals a               =  a

class IsName a where
      toName                  :: a -> Name
      toNameList              :: [a] -> Name

      toNameList []           =  error "toNameList undefined for []"
      toNameList [l]          =  toName l
      toNameList (l:ls)       =  Hier (toName l) (toNameList ls)

(<+)                          :: (IsName a, IsName b) => a -> b -> Name
a <+ b                        =  globalToFront (Hier (toName a) (toName b))

(<*)                          :: (IsName a) => Int -> a -> Name
a <* b                        =  a <+ b

instance IsName Int where
      toName n                =  NameInt (fromIntegral n)

instance IsName Char where
      toName n                =  NameStr [n]
      toNameList              =  NameStr

instance IsName Dir where
      toName                  =  NameDir

instance (IsName a) => IsName  [a] where
      toName                  =  toNameList

instance IsName Name where
      toName                  =  id

ref                           :: IsName a => a -> Point
ref                           =  PointVar . toName

var                           :: IsName a => a -> Numeric
var                           =  NumericVar . toName




-------------------- Punkte ------------------

data Point                    =  PointPic'            Int     Dir
                              |  PointVar'            Int     Int
                              |  PointVarArray'       Int     Int
                              |  PointTrans'          Point   [Int]
                              |  PointVar             Name
                              |  PointVec             (Numeric, Numeric)
                              |  PointMediate         Numeric Point   Point
                              |  PointDirection       Numeric
                              |  PointWhatever
                              |  PointPPP             FunPPP  Point   Point
                              |  PointNMul            Numeric Point
                              |  PointNeg             Point
                              |  PointCond            Boolean Point   Point
                                 deriving (Eq, Show, Read, Ord)

data FunPPP                   =  PPPAdd
                              |  PPPSub
                              |  PPPDiv
                                 deriving (Eq, Show, Read, Ord)

instance HasCond Point where
      cond b t e              =  PointCond    b       t       e

instance Num Point where
      a + b                   =  addPoint a b
      a - b                   =  subPoint a b
      a * b                   =  mulPoint a b
      negate a                =  negPoint a
      abs a                   =  absPoint a
      signum 0                =  0
      signum _                =  1
      fromInteger a           =  PointVec (fromInteger a, fromInteger a)

(.*)                          :: Numeric -> Point -> Point
n .* PointVec (a1, a2)        =  PointVec (n*a1, n*a2)
n .* p                        =  PointNMul n p

addPoint :: Point -> Point -> Point
addPoint (PointVec (a1,a2)) (PointVec (b1,b2))
                              =  PointVec (a1+b1, a2+b2)
addPoint a b                  =  PointPPP PPPAdd a b

subPoint :: Point -> Point -> Point
subPoint (PointVec (a1,a2)) (PointVec (b1,b2))
                              =  PointVec (a1-b1, a2-b2)
subPoint a b                  =  PointPPP PPPSub a b

mulPoint :: Point -> Point -> Point
mulPoint (PointVec (a1,a2)) (PointVec (b1,b2))
                              =  PointVec (a1*b1, a2*b2)
mulPoint p PointWhatever      =  PointNMul NumericWhatever p
mulPoint PointWhatever p      =  PointNMul NumericWhatever p
mulPoint a b                  =  PointVec (xpart a*xpart b, ypart a*ypart b)

negPoint :: Point -> Point
negPoint (PointVec (a1,a2))   =  PointVec (-a1, -a2)
negPoint a                    =  PointNeg a

absPoint :: Point -> Point
absPoint (PointVec (a1,a2))   =  PointVec (abs a1, abs a2)
absPoint a                    =  a

vec                           :: (Numeric, Numeric) -> Point
vec                           =  PointVec

dir                           :: Numeric -> Point
dir a                         =  vec (cos a, sin a)

xy                            :: Point -> Point -> Point
xy p1 p2                      =  vec (xpart p1, ypart p2)

instance HasMed Point where
      med                     =  PointMediate

instance HasWhatever Point where
      whatever                =  PointWhatever

-----------------------------------------------------------

data Numeric                  =  NumericVar'  Int     Int
                              |  NumericArray' Int    Int
                              |  NumericVar   Name
                              |  Numeric      Double
                              |  NumericWhatever
                              |  NumericDist  Point   Point
                              |  NumericMediate Numeric Numeric Numeric
                              |  NumericPN    FunPN   Point
                              |  NumericNN    FunNN   Numeric
                              |  NumericNNN   FunNNN  Numeric Numeric
                              |  NumericNsN   FunNsN  [Numeric]
                              |  NumericCond  Boolean Numeric Numeric
                                 deriving (Eq, Show, Read, Ord)


data FunPN                    =  PNXPart
                              |  PNYPart
                              |  PNAngle
                                 deriving (Eq, Show, Read, Ord)

data FunNN                    =  NNSinD
                              |  NNCosD
                              |  NNSqrt
                              |  NNExp
                              |  NNLog
                              |  NNRound
                              |  NNCeil
                              |  NNFloor
                              |  NNNeg
                                 deriving (Eq, Show, Read, Ord)

data FunNNN                   =  NNNAdd
                              |  NNNSub
                              |  NNNMul
                              |  NNNDiv
                              |  NNNPyth
                              |  NNNPower
                                 deriving (Eq, Show, Read, Ord)

data FunNsN                   =  NsNMin
                              |  NsNMax
                                 deriving (Eq, Show, Read, Ord)

instance HasCond Numeric where
      cond b t e              =  NumericCond  b       t       e

instance Num Numeric where
      a + b                   =  addNumeric a b
      a - b                   =  subNumeric a b
      a * b                   =  mulNumeric a b
      negate a                =  negNumeric a
      abs a                   =  absNumeric a
      signum 0                =  0
      signum _                =  1
      fromInteger a           =  Numeric (fromInteger a)

addNumeric                    :: Numeric -> Numeric -> Numeric
addNumeric (Numeric a) (Numeric b)
                              =  Numeric (a+b)
addNumeric a b                =  NumericNNN NNNAdd a b

subNumeric :: Numeric -> Numeric -> Numeric
subNumeric (Numeric a) (Numeric b)
                              =  Numeric (a-b)
subNumeric a b                =  NumericNNN NNNSub a b

mulNumeric :: Numeric -> Numeric -> Numeric
mulNumeric (Numeric a) (Numeric b)
                              =  Numeric (a*b)
mulNumeric a b                =  NumericNNN NNNMul a b

negNumeric :: Numeric -> Numeric
negNumeric (Numeric a)        =  Numeric (-a)
negNumeric a                  =  NumericNN NNNeg a

absNumeric :: Numeric -> Numeric
absNumeric (Numeric a)        =  Numeric (abs a)
absNumeric a                  =  cond (a .<0) (-1*a) (a)

radAngle :: (Numeric, Numeric) -> Numeric
radAngle p = angle (PointVec p) * pi/180

instance Fractional Numeric where
      Numeric n1 / Numeric n2 =  Numeric (n1 / n2)
      n1 / n2                 =  NumericNNN NNNDiv n1 n2
      recip (Numeric a)       =  Numeric (recip a)
      recip n                 =  1 / n
      fromRational            =  Numeric . fromRational

instance Floating Numeric where
      pi                      =  Numeric pi
      exp (Numeric a)         =  Numeric (exp a)
      exp a                   =  NumericNN NNExp a
      log (Numeric a)         =  Numeric (log a)
      log a                   =  NumericNN NNLog a
      sqrt (Numeric a)        =  Numeric (sqrt a)
      sqrt a                  =  NumericNN NNSqrt a
      (**) a b                =  NumericNNN NNNPower a b
      sin (Numeric a)         =  Numeric (sin a)
      sin a                   =  NumericNN NNSinD (a*180/pi)
      cos (Numeric a)         =  Numeric (cos a)
      cos a                   =  NumericNN NNCosD (a*180/pi)
      tan (Numeric a)         =  Numeric (tan a)
      tan a                   =  sin a / cos a
      asin a                  =  radAngle (sqrt (1-a*a), a)
      acos a                  =  radAngle (a, sqrt (1-a*a))
      atan a                  =  radAngle (1, a)
      sinh a                  =  (exp a - exp (-a)) / 2
      cosh a                  =  (exp a + exp (-a)) / 2
      tanh a                  =  (exp a - exp (-a)) / (exp a + exp (-a))
      asinh a                 =  log (sqrt (a*a+1) + a)
      acosh a                 =  log (sqrt (a*a-1) + a)
      atanh a                 =  (log (1+a) - log (1-a)) / 2

instance Enum Numeric where
      toEnum                  =  Numeric . fromIntegral
      fromEnum (Numeric a)    =  fromEnum a
      fromEnum _              =  0
      enumFrom a              =  enumFromThen a (a+1)
      enumFromThen a b        =  iterate (+(b-a)) a
      enumFromTo a@(Numeric _) c@(Numeric _)
                              =  takeWhile (<=c) (enumFrom a)
      enumFromThenTo (Numeric a) (Numeric b) (Numeric c)
                              =  map Numeric
                                      (takeWhile (if b >= a then (<=c) else (>=c))
                                              (enumFromThen a b))

instance Real Numeric where
      toRational (Numeric a)  =  toRational a
      toRational _            =  0

pythAdd                       :: Numeric -> Numeric -> Numeric
pythAdd                       =  NumericNNN NNNPyth

xpart                         :: Point -> Numeric
xpart (PointVec (a, _))       =  a
xpart a                       =  NumericPN PNXPart a

ypart                         :: Point -> Numeric
ypart (PointVec (_, a))       =  a
ypart a                       =  NumericPN PNYPart a

angle                         :: Point -> Numeric
angle                         =  NumericPN PNAngle

width                         :: IsName a => a -> Numeric
width s                       =  xpart (ref (s <+ E))
                              -  xpart (ref (s <+ W))

height                        :: IsName a => a -> Numeric
height s                      =  ypart (ref (s <+ N))
                              -  ypart (ref (s <+ S))

xdist                         :: Point -> Point -> Numeric
xdist p1 p2                   =  xpart p1 - xpart p2

ydist                         :: Point -> Point -> Numeric
ydist p1 p2                   =  ypart p1 - ypart p2

dist                          :: Point -> Point -> Numeric
dist p1 p2                    =  NumericDist p1 p2

maximum'                      :: [Numeric] -> Numeric
maximum' [a]                  =  a
maximum' as                   =  NumericNsN NsNMax as

minimum'                      :: [Numeric] -> Numeric
minimum' [a]                  =  a
minimum' as                   =  NumericNsN NsNMin as

instance HasMed Numeric where
      med                     =  NumericMediate

instance HasWhatever Numeric where
      whatever                =  NumericWhatever
