{- |
   Module      :  FMP.Matrix
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

module FMP.Matrix (
      matrix, matrixAlign, matrixSepBy, matrixAlignSepBy,
      rowAlign, columnAlign, rowAlignSepBy, columnAlignSepBy,
      cell, cell'
      ) where

import FMP.Types
import FMP.Picture

data Cell                     =  Cell Dir Picture
                                 deriving Show

cell                          :: IsPicture a => a -> Cell
cell                          =  cell' C

cell'                         :: IsPicture a => Dir -> a -> Cell
cell' d p                     =  Cell d (toPicture p)

matrix                        :: IsPicture a => [[a]] -> Picture
matrix m                      =  matrixSepBy 16 16 m

matrixSepBy                   :: IsPicture a => Numeric -> Numeric -> [[a]]
                              -> Picture
matrixSepBy sepH sepV m       =  matrixAlignSepBy sepH sepV (map (map cell) m)

matrixAlign                   :: [[Cell]] -> Picture
matrixAlign                   =  matrixAlignSepBy 16 16

matrixAlignSepBy              :: Numeric -> Numeric -> [[Cell]] -> Picture
matrixAlignSepBy _ _ []       =  empty
matrixAlignSepBy sepH sepV cs =  overlay (equations colWidths
                                         :equations rowHeights
                                         :equations ofsHs
                                         :equations ofsVs
                                         :[equations placements])
                                      (map ((\(Cell _ p) -> p).snd) nps)
      where
      origin                  =  ref "X"
      maxH, maxV, ofsH ,ofsV  :: Int -> Numeric
      maxH i                  =  var (i*4)
      maxV i                  =  var (i*4+1)
      ofsH i                  =  var (i*4+2)
      ofsV i                  =  var (i*4+3)
      nps                     =  number (0, 0, 0::Int) cs
      nH                      =  maximum [length c | c <- cs]
      nV                      =  length cs
      rowHeights              =  [ maxV i .= maximum' [height n
                                                      | ((_, x, n), _) <- nps, i == x]
                                 | i <- [0..nV-1]]
      colWidths               =  [ maxH i .= maximum' [width n
                                      | ((y, _, n), _) <- nps, i == y]
                                 | i <- [0..nH-1]]
      ofsHs                   =  [ ofsH 0 .= xpart origin]
                              ++ [ ofsH i .= ofsH (i-1) + maxH (i-1) + sepH
                                 | i <- [1..nH]]
      ofsVs                   =  [ ofsV 0 .= ypart origin]
                              ++ [ ofsV i .= ofsV (i-1) - maxV (i-1) - sepV
                                 | i <- [1..nV]]
      placements              =  [
              ref (n <+ d)    .= case d of
                                      C -> vec (ofsH x + maxH x / 2,
                                                      ofsV (y+1) + maxV y / 2)
                                      N -> vec (ofsH x + maxH x / 2, ofsV y - sepV)
                                      NE-> vec (ofsH (x+1) - sepH, ofsV y - sepV)
                                      E -> vec (ofsH (x+1) - sepH,
                                                      ofsV (y+1) + maxV y / 2)
                                      SE-> vec (ofsH (x+1) - sepH, ofsV (y+1))
                                      S -> vec (ofsH x + maxH x / 2, ofsV (y+1))
                                      SW-> vec (ofsH x, ofsV (y+1))
                                      W -> vec (ofsH x, ofsV (y+1) + maxV y / 2)
                                      NW-> vec (ofsH x, ofsV y - sepV)
                                  | ((x, y, n), Cell d _) <- nps]
      number (x, y, n) ((Cell d p:cs):rs)
                              =  ((x, y, n),Cell d p):number (x+1, y, n+1) (cs:rs)
      number (_, y, n) ([]:rs)=  number (0, y+1, n) rs
      number _ _              =  []

rowAlign                      :: [Cell] -> Picture
rowAlign                      =  rowAlignSepBy 0

columnAlign                   :: [Cell] -> Picture
columnAlign                   =  columnAlignSepBy 0

rowAlignSepBy                 :: Numeric -> [Cell] -> Picture
rowAlignSepBy hSep ps         =  matrixAlignSepBy hSep 0 [ps]

columnAlignSepBy              :: Numeric -> [Cell] -> Picture
columnAlignSepBy vSep ps      =  matrixAlignSepBy 0 vSep (map (\x->[x]) ps)
