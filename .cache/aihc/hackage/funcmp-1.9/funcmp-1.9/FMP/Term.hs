{- |
   Module      :  FMP.Term
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

module FMP.Term (
      Term(..),
      max', pair, add, sub, mul
      ) where

import FMP.Types ( Dir(..) )

-- Terme zur symbolischen Rechnung

data Term                     =  Const        Double
                              |  Id           String
                              |  Add          Term    Term
                              |  Sub          Term    Term
                              |  Mul          Term    Term
                              |  Div          Term    Term
                              |  Neg          Term
                              |  Parens       Term
                              |  Pythagoras   Term    Term
                              |  Power        Term    Term
                              |  SinD         Term
                              |  CosD         Term
                              |  Sqrt         Term
                              |  Ln           Term
                              |  Exp          Term
                              |  Round        Term
                              |  Ceil         Term
                              |  Floor        Term
                              |  Angle        Term
                              |  XPart        Term
                              |  YPart        Term
                              |  Pair         Term    Term
                              |  Max          [Term]
                              |  Min          [Term]
                              |  Identity
                              |  CurrentPicture
                              |  Pic          String
                              |  Infix        Term    String  Term
                              |  LLCorner     Term
                              |  URCorner     Term
                              |  Shifted      Term    Term
                              |  Transform    [Int]   Term
                              |  Transformed  Term    Term
                              |  TransformedM Term    Term    Term    Term    Term    Term
                              |  Mediate      Term    Term    Term
                              |  Pos          Int     Int
                              |  TDot         String  Dir
                              |  IfElse       Term    Term    Term
                              |  VerbFunction String  Term
                              |  Dirop        Term
                                 deriving (Eq, Show, Read)

-- > data Dir                   =  C | N | NE | E | SE | S | SW | W | NW
-- >                               deriving (Eq, Read, Show, Enum)


instance Num Term where
      a + b                   =  add a b
      a - b                   =  sub a b
      a * b                   =  mul a b
      negate a                =  neg a
      abs (Neg a)             =  abs a
      abs a                   =  a
      signum 0                =  0
      signum _                =  1
      fromInteger a           =  Const (fromInteger a)

instance Fractional Term where
      (Const a) / (Const b)   =  Const (a / b)
      a         / b           =  Div a b
      recip (Const a)         =  Const (recip a)
      recip a                 =  1 / a
      fromRational            =  Const . fromRational


-- "`Smarte"` Konstruktoren

ident                         :: String -> Term
ident a                       =  Id a

const'                        :: Double -> Term
const' a                      =  Const a

neg                           :: Term -> Term
neg (Neg a)                   =  a
neg (Const a)                 =  Const (-a)
neg a                         =  Neg a


add                           :: Term -> Term -> Term
add (Const 0) b               =  b
add a (Const 0)               =  a
add (Const a) (Const b)       =  Const (a + b)
add a (Neg b)                 =  sub a b
add a b                       =  Add a b

sub                           :: Term -> Term -> Term
sub (Const 0) b               =  neg b
sub a (Const 0)               =  a
sub (Const a) (Const b)       =  Const (a - b)
sub a (Neg b)                 =  add a b
sub a b                       =  Sub a b

mul                           :: Term -> Term -> Term
mul (Const 0) _               =  0
mul _ (Const 0)               =  0
mul (Const a) (Const b)       =  Const (a * b)
mul a b                       =  Mul a b

max'                          :: [Term] -> Term
max' []                       =  0
max' [a]                      =  a
max' as
      |null c && null v       =  0
      |null c                 =  Max v
      |null v                 =  Const (maximum c)
      |otherwise              =  Max (Const (maximum c):v)
      where
      (c, v)                  =  max2 as
      max2 []                 =  ([], [])
      max2 ((Const a):as)     =  (a:fst (max2 as), snd (max2 as))
      max2 (a:as)             =  (fst (max2 as), a:snd (max2 as))

pair                          :: Term -> Term -> Term
pair a b                      =  Pair a b
