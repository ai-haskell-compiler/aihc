{- |
   Module      :  FMP.Frames
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

module FMP.Frames (
      drum, diamond, fuzzy, cloud
      ) where

import FMP.Types
import FMP.Picture

diamond                       :: Picture -> Picture
diamond p                     =  Frame stdFrameAttrib eqs path p
      where
      eqs                     =  [
              var "d"         .= 2+0.6*dist (ref ("last" <+ SW)) (ref ("last" <+ NE)),
              ref C           .= ref ("last" <+ C),
              ref N           .= ref ("last" <+ C) + vec(0,var "d"),
              ref E           .= ref ("last" <+ C) + vec(var "d",0),
              ref S           .= ref ("last" <+ C) - vec(0,var "d"),
              ref W           .= ref ("last" <+ C) - vec(var "d",0),
              ref NE          .= med 0.5 (ref N) (ref E),
              ref SE          .= med 0.5 (ref S) (ref E),
              ref SW          .= med 0.5 (ref S) (ref W),
              ref NW          .= med 0.5 (ref N) (ref W)
                                      ]
      path                    =  ref N .-. ref E .-. ref S .-. ref W .-. cycle'

fuzzy                         :: Int -> Int -> Picture -> Picture
fuzzy s1 s2 p                 =  Frame stdFrameAttrib eqs path p
      where
      eqs                     =  [
              var "d"         .= 0.5*dist (ref ("last" <+ NW)) (ref ("last" <+ SE)),
              ref C           .= ref ("last" <+ C),
              ref E           .= disort 0,
              ref SE          .= disort 1,
              ref S           .= disort 2,
              ref SW          .= disort 3,
              ref W           .= disort 4,
              ref NW          .= disort 5,
              ref N           .= disort 6,
              ref NE          .= disort 7
                                      ]
      path                    =  ((((((((ref E
                                      ... ref SE  # setEndAngle (90+r' 21))
                                      ... ref S   # setEndAngle (135+r' 22))
                                      ... ref SW  # setEndAngle (180+r' 23))
                                      ... ref W   # setEndAngle (225+r' 24))
                                      ... ref NW  # setEndAngle (270+r' 25))
                                      ... ref N   # setEndAngle (315+r' 26))
                                      ... ref NE  # setEndAngle (0+r' 26))
                                      ... cycle'  # setEndAngle (45+r' 26))
      r i                     =  realToFrac (randomDoubles s1 s2!!i)
      r' i                    =  realToFrac (40*(randomDoubles s1 s2!!i-0.5))
      disort i                =  ref ("last" <+ C)
                                 + (var "d"*(1+0.5*r (2*i+1)))
                                 .* dir (Numeric (fromIntegral i)*45+r' (2*i+2))


cloud                         :: Int -> Int -> Picture -> Picture
cloud s1 s2 p                 =  Frame stdFrameAttrib eqs path p
      where
      eqs                     =  [
              var "d"         .= 0.5*dist (ref ("last" <+ NW)) (ref ("last" <+ SE)),
              ref C           .= ref ("last" <+ C),
              ref E           .= disort 0,
              ref SE          .= disort 1,
              ref S           .= disort 2,
              ref SW          .= disort 3,
              ref W           .= disort 4,
              ref NW          .= disort 5,
              ref N           .= disort 6,
              ref NE          .= disort 7
                                      ]
      path                    =  ((((((((ref E
                                      ... ref SE  # setEndAngle (90+r' 21)# setStartAngle (r' 21))
                                      ... ref S   # setEndAngle (135+r' 22)# setStartAngle (r' 22))
                                      ... ref SW  # setEndAngle (180+r' 23)# setStartAngle (r' 23))
                                      ... ref W   # setEndAngle (225+r' 24)# setStartAngle (r' 24))
                                      ... ref NW  # setEndAngle (270+r' 25)# setStartAngle (r' 25))
                                      ... ref N   # setEndAngle (315+r' 26)# setStartAngle (r' 26))
                                      ... ref NE  # setEndAngle (0+r' 26))
                                      ... cycle'  # setEndAngle (45+r' 26))
      r i                     =  realToFrac (randomDoubles s1 s2!!i)
      r' i                    =  realToFrac (40*(randomDoubles s1 s2!!i-0.5))
      disort i                =  ref ("last" <+ C)
                                 + (var "d"*(1+0.5*r (2*i+1)))
                                 .* dir (Numeric (fromIntegral i)*45+r' (2*i+2))


drum                          :: IsPicture a => a -> Frame
drum p                        =  Frame' stdFrameAttrib
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
      eqsDY                   =  [ref N .= ref ("last" <+ N) + vec(0,var "dy"+var "d"),
                                  ref S .= ref ("last" <+ S) - vec(0,var "dy"+var "d") ]
      eqsWidth                =  [ref E .= ref W + vec(var "width",0),
                                  ref C - ref W .= ref E - ref C ]
      eqsHeight               =  [ref N .= ref S + vec(0,var "height"+2*var "d"),
                                  ref C - ref S .= ref N - ref C ]
      eqs                    =  [ref C .= ref ("last" <+ C),
                                 var "d" .= 0.10*xpart(ref E-ref W),
                                 xpart (ref NE)  .= xpart (ref SE),
                                 ypart (ref NW)  .= ypart (ref NE),
                                 ref W .= med 0.5 (ref NW) (ref SW),
                                 ref S .= med 0.5 (ref SW) (ref SE),
                                 ref E .= med 0.5 (ref NE) (ref SE),
                                 ref N .= med 0.5 (ref NE) (ref NW)]
      path                    = ref NW .--. (ref SW .... ref S+vec(0,-var "d").... ref SE # setJoin (joinTension (tensionAtLeast 1.7)))
                                      .--. (ref NE .... ref N+vec(0,var "d") .... ref NW
                                      .... ref N+vec(0,-var "d") .... ref NE  .... ref N+vec(0,var "d") .... cycle' # setJoin (joinTension (tensionAtLeast 1.7)))



random2Ints                   :: Int -> Int -> [Int]
random2Ints s1 s2             =  if 1 <= s1 && s1 <= 2147483562 then
                                     if 1 <= s2 && s2 <= 2147483398 then
                                         rands s1 s2
                                     else
                                         error "random2Ints: Bad second seed."
                                 else
                                     error "random2Ints: Bad first seed."

rands                         :: Int -> Int -> [Int]
rands s1 s2                   =  let
                      k       =  s1 `div` 53668
                      s1'     =  40014 * (s1 - k * 53668) - k * 12211
                      s1''    =  if s1' < 0 then s1' + 2147483563 else s1'
                      k'      =  s2 `div` 52774
                      s2'     =  40692 * (s2 - k' * 52774) - k' * 3791
                      s2''    =  if s2' < 0 then s2' + 2147483399 else s2'
                      z       =  s1'' - s2''
                                 in  if z < 1 then z + 2147483562 : rands s1'' s2''
                                               else z : rands s1'' s2''

randomDoubles                 :: Int -> Int -> [Double]
randomDoubles s1 s2           =  map (\x -> fromIntegral x * 4.6566130638969828e-10)
                                      (random2Ints s1 s2)

