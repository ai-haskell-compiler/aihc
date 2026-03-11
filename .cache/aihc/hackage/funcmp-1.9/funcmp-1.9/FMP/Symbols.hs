{- |
   Module      :  FMP.Symbols
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

module FMP.Symbols where

import FMP.Types
import FMP.Picture

data Symbols                  =  Symbols {    symPnts         :: SymPoint,
                                              symNums         :: SymNum}
                                 deriving (Eq, Show)

data SymPoint                 =  SymPName     Name     Int      Int
                              |  SymPHier     Name     Int      SymPoint
                              |  SymPUnion    SymPoint SymPoint
                              |  SymPUnion3   SymPoint SymPoint SymPoint
                              |  SymPTrans    SymPoint Int
                              |  SymPRelax
                                 deriving (Eq,Show)

data SymNum                   =  SymNName     Name     Int      Int
                              |  SymNHier     Name     Int      SymNum
                              |  SymNUnion    SymNum   SymNum
                              |  SymNUnion3   SymNum   SymNum   SymNum
                              |  SymNRelax
                                 deriving (Eq,Show)


instance HasConcat Symbols where
      a & b                   =  a {  symPnts = symPnts a & symPnts b,
                                      symNums = symNums a & symNums b}

instance HasConcat SymPoint where
      SymPRelax & b           =  b
      a & SymPRelax           =  a
      SymPUnion3 a1 a2 a3  & SymPUnion3 b1 b2 b3
                              =  SymPUnion3 (a1 & b1) (a2 & b2) (a3 & b3)
      a & b                   =  SymPUnion a b


instance HasConcat SymNum where
      SymNRelax & b           =  b
      a & SymNRelax           =  a
      SymNUnion3 a1 a2 a3  & SymNUnion3 b1 b2 b3
                              =  SymNUnion3 (a1 & b1) (a2 & b2) (a3 & b3)
      a & b                   =  SymNUnion a b


instance HasRelax Symbols where
      relax                   =  Symbols {    symPnts         = SymPRelax,
                                              symNums         = SymNRelax}


symUnion3                     :: Symbols -> Symbols -> Symbols -> Symbols
symUnion3 d1 d2 d3            =  d1 { symPnts = SymPUnion3    (symPnts d1)
                                                              (symPnts d2)
                                                              (symPnts d3),
                                      symNums = SymNUnion3    (symNums d1)
                                                              (symNums d2)
                                                              (symNums d3)}

symUnions                     :: [Symbols] -> Symbols
symUnions []                  =  relax
symUnions (d:ds)              =  d & symUnions ds

symName                       :: IsName a => a -> Int -> Symbols -> Symbols
symName name n d              =  d {  symPnts = symPName (toName name) n (symPnts d),
                                      symNums = symNName (toName name) n (symNums d) }

symNames                      :: IsName a => [a] -> Int -> Symbols -> Symbols
symNames names n d            =  d {  symPnts = symPNames     (map toName names) n
                                                              (symPnts d),
                                      symNums = symNNames     (map toName names) n
                                                              (symNums d) }

symHier                       :: IsName a => a -> Int -> Symbols -> Symbols
symHier name n d              =  d {  symPnts = symPHier (toName name) n (symPnts d),
                                      symNums = symNHier (toName name) n (symNums d) }

symTrans                      :: Symbols -> Int -> Symbols
symTrans d i                  =  d {  symPnts = SymPTrans (symPnts d) i}

addPDef                       :: SymPoint -> Symbols -> Symbols
addPDef d s                   =  s {  symPnts = d & symPnts s}

addNDef                       :: SymNum -> Symbols -> Symbols
addNDef d s                   =  s {  symNums = d & symNums s}

---------------------

symPUnions                    :: [SymPoint] -> SymPoint
symPUnions []                 =  SymPRelax
symPUnions (d:ds)             =  d & symPUnions ds

symPName                      :: Name -> Int -> SymPoint -> SymPoint
symPName name n(SymPUnion3 d1 d2 d3)
                              =  SymPUnion3   (SymPName name n 0 & d1)
                                              (SymPHier name n d3 & d2)
                                              d3
symPName name n d             =  SymPUnion3   (SymPName name n 0)
                                              (SymPHier name n d)
                                              d

symPNames                     :: [Name] -> Int -> SymPoint -> SymPoint
symPNames [] _ d@(SymPUnion3 _ _ _)
                              =  d
symPNames names n d@(SymPUnion3 d1 d2 d3)
                              =  SymPUnion3
                                      (symPUnions [ SymPName name n 0 | name <- names])
                                      (symPUnions [ SymPHier name n d | name <- names] & d2)
                                      (d1 & d3)
symPNames names n SymPRelax   =  SymPUnion3
                                      (symPUnions [ SymPName name n 0 | name <- names])
                                      (symPUnions [ SymPHier name n SymPRelax| name <- names])
                                      SymPRelax
symPNames names n d           =  SymPUnion3
                                      (symPUnions [ SymPName name n 0 | name <- names])
                                      (symPUnions [ SymPHier name n d | name <- names])
                                      d

symPHier                      :: Name -> Int -> SymPoint -> SymPoint
symPHier name n d             =  SymPHier name n d

symPTrans                     :: SymPoint -> Int -> SymPoint
symPTrans d i                 =  SymPTrans d i

---------------------

symNUnions                    :: [SymNum] -> SymNum
symNUnions []                 =  SymNRelax
symNUnions (d:ds)             =  d & symNUnions ds

symNName                      :: Name -> Int -> SymNum -> SymNum
symNName name n (SymNUnion3 d1 d2 d3)
                              =  SymNUnion3   (SymNName name n 0 & d1)
                                              (SymNHier name n d3 & d2)
                                              d3
symNName name n d             =  SymNUnion3   (SymNName name n 0)
                                              (SymNHier name n d)
                                              d

symNNames                     :: [Name] -> Int -> SymNum -> SymNum
symNNames [] _ d@(SymNUnion3 _ _ _)
                              =  d
symNNames names n d@(SymNUnion3 d1 d2 d3)
                              =  SymNUnion3
                                      (symNUnions     [ SymNName name n 0
                                                      | name <- names])
                                      (symNUnions     [ SymNHier name n d
                                                      | name <- names] & d2)
                                      (d1 & d3)
symNNames names n SymNRelax   =  SymNUnion3
                                      (symNUnions     [ SymNName name n 0
                                                      | name <- names])
                                      (symNUnions     [ SymNHier name n SymNRelax
                                                      | name <- names])
                                      SymNRelax
symNNames names n d           =  SymNUnion3
                                      (symNUnions     [ SymNName name n 0
                                                      | name <- names])
                                      (symNUnions     [ SymNHier name n d
                                                      | name <- names])
                                      d


symNHier                      :: Name -> Int -> SymNum -> SymNum
symNHier name n d             =  SymNHier name n d


symNHiers                     :: [Name] -> Int -> SymNum -> SymNum
symNHiers names n d           =  symNUnions [SymNHier name n d | name <- names]

