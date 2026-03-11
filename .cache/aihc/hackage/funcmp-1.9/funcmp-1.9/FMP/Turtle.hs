{- |
   Module      :  FMP.Turtle
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

module FMP.Turtle (
      Turtle(..),                             TurtleAttrib(..),spreadAttrib,figure,
      turtle,
      home, toleft, toright, turn, turnl, turnr, forward, backward,
      penUp, penDown, plot, fork
      ) where

import FMP.Types
import FMP.Picture
import FMP.Canvas
import FMP.Color

type PicPos                   =  ((Numeric,Numeric),Picture)

data Turtle                   =  TConc Turtle Turtle
                              |  TDropPic Picture
                              |  TColor Color Turtle
                              |  TPen Pen Turtle
                              |  THide Turtle
                              |  TForward Numeric
                              |  TTurn Numeric
                              |  TPenUp
                              |  TPenDown
                              |  THome
                              |  TFork Turtle Turtle
                                 deriving Show

data TurtleDescr              =  TurtleDescr {tPos            :: (Numeric,Numeric),
                                              tOrientation    :: Numeric,
                                              tColor          :: Maybe Color,
                                              tPen            :: Maybe Pen,
                                              tPenDown        :: Bool }

instance HasDefault TurtleDescr where
      default'                =  TurtleDescr {tPos            = (0.0, 0.0),
                                              tOrientation    = 0,
                                              tColor          = Nothing,
                                              tPen            = Nothing,
                                              tPenDown        = True }

stdTurtleDescr                :: TurtleDescr
stdTurtleDescr                =  TurtleDescr {tPos            = (0.0, 0.0),
                                              tOrientation    = 0,
                                              tColor          = Nothing,
                                              tPen            = Nothing,
                                              tPenDown        = True }

data TurtleAttrib             =  TAttrib PathElemDescr Turtle
                              |  TAttribFork [TurtleAttrib] [TurtleAttrib]
                                 deriving Show

instance IsHideable Turtle where
      hide                    =  THide

instance HasConcat Turtle where
      (&)                     =  TConc

instance HasRelax Turtle where
      relax                   =  TTurn 0.0

instance HasPicture Turtle where
      fromPicture             =  TDropPic . toPicture

instance HasColor Turtle where
      setColor                =  TColor
      setDefaultColor         =  setColor DefaultColor
      getColor (TColor c _)   =  c
      getColor _              =  DefaultColor

instance HasPen Turtle where
      setPen                  =  TPen
      setDefaultPen           =  TPen DefaultPen
      getPen (TPen c _)       =  c
      getPen _                =  DefaultPen

instance IsPicture Turtle where
      toPicture tp            =  toPicture (cdraws paths & foldl (&) relax cs)
              where
              cs              =  [ cdrop pos pic | (pos, pic) <- pics ]
              (paths, pics)   =  figure tp stdPathElemDescr

turtle                        :: IsPicture a => a -> Picture
turtle                        =  toPicture

home                          :: Turtle
home                          =  THome

toleft                        :: Turtle
toleft                        =  turn   90.0

toright                       :: Turtle
toright                       =  turn (-90.0)

turn                          :: Numeric -> Turtle
turn                          =  TTurn

turnl                         :: Numeric -> Turtle
turnl a                       =  TTurn a

turnr                         :: Numeric -> Turtle
turnr a                       =  TTurn (-a)

forward                       :: Numeric -> Turtle
forward                       =  TForward

backward                      :: Numeric -> Turtle
backward a                    =  forward (-a)

penUp                         :: Turtle
penUp                         =  TPenUp

penDown                       :: Turtle
penDown                       =  TPenDown

plot                          :: [Turtle] -> Turtle
plot                          =  foldr (&) relax

fork                          :: Turtle -> Turtle -> Turtle
fork                          =  TFork


spreadAttrib                  :: PathElemDescr -> Turtle -> [TurtleAttrib]
                              -> [TurtleAttrib]
spreadAttrib ped (TConc p1 p2) ps
                              =  spreadAttrib ped p1 (spreadAttrib ped p2 ps)
spreadAttrib ped (TColor c p) ps
                              =  spreadAttrib (setColor c ped) p ps
spreadAttrib ped (TPen pen p) ps
                              =  spreadAttrib (setPen pen ped) p ps
spreadAttrib ped (THide p ) ps
                              =  spreadAttrib (hide ped) p ps
spreadAttrib ped (TFork p1 p2) ps
                              =  [TAttribFork (spreadAttrib ped p1 ps)
                                              (spreadAttrib ped p2 ps)]
spreadAttrib ped p ps         =  TAttrib ped p : ps

figure                        :: Turtle -> PathElemDescr -> ([Path], [PicPos])
figure t ped                  =  renderPath default' (spreadAttrib ped t []) ([], [])

renderPath                    :: TurtleDescr -> [TurtleAttrib] -> ([Path],[PicPos])
                              -> ([Path],[PicPos])
renderPath td (TAttrib _  (TTurn d):ps) tp
                              =  renderPath td' ps tp
              where
              td'             =  td{ tOrientation = tOrientation td + d }
renderPath td (TAttrib ped (TForward d):ps) tp
                              =  (PathJoin (actualPos td) ped' rp:rps, pics)
              where
              (rp:rps, pics)  =  renderPath td' ps tp
              td'             =  td{tPos = (x + d*cos phi, y + d*sin phi)}
              (x, y)          =  tPos td
              phi             =  tOrientation td
              ped'            =  if (tPenDown td)
                                      then ped
                                      else hide ped
renderPath td (TAttrib _ TPenUp:ps) tp
                              =  renderPath td{tPenDown = False} ps tp
renderPath td (TAttrib _ TPenDown:ps) tp
                              =  renderPath td{tPenDown = True} ps tp
renderPath td (TAttrib ped THome:ps) tp
                              =  (PathJoin (actualPos td) (hide ped)
                                              rp:rps, pics)
              where
              (rp:rps, pics)  =  renderPath td' ps tp
              td'             =  td{  tPos            = (0.0, 0.0),
                                      tOrientation    = 0 }
renderPath td (TAttrib _  (TDropPic p):ps) tp
                              =  (rps, (tPos td, p):pics)
              where
              (rps, pics)     =  renderPath td ps tp
renderPath td (TAttribFork ta1 ta2:ps) _
                              =  (actualPos td:rps1++rps2, pics1++pics2)
              where
              (rps1, pics1)   =  renderPath td ta1 (renderPath td ps ([], []))
              (rps2, pics2)   =  renderPath td ta2 (renderPath td ps ([], []))
renderPath td (TAttrib _ _:ps) tp
                              =  renderPath td ps tp
renderPath td [] tp           =  (actualPos td:fst tp, snd tp)


actualPos                     :: TurtleDescr -> Path
actualPos td                  =  toPath $ vec $ tPos td

