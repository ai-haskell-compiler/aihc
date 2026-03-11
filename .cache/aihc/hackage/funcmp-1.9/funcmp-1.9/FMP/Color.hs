{- |
   Module      :  FMP.Color
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

module FMP.Color (
      Color(..), HasColor(..), HasBGColor(..),
      white, black, red,  green, blue, yellow, cyan, magenta,
      grey, color, hsv2rgb,
      graduateLow,  graduateMed, graduateHigh, graduate
      ) where

data Color                    =  DefaultColor
                              |  Color Double Double Double
                              |  Graduate Color Color Double Int
                                 deriving (Eq, Show, Read)

class HasColor a where
      setColor                :: Color -> a -> a
      setDefaultColor         :: a -> a
      getColor                :: a -> Color

class HasBGColor a where
      setBGColor              :: Color -> a -> a
      setDefaultBGColor       :: a -> a
      getBGColor              :: a -> Color

white, black, red, green, blue , yellow, cyan, magenta        :: Color
white                         =  Color 1 1 1
black                         =  Color 0 0 0
red                           =  Color 1 0 0
green                         =  Color 0 1 0
blue                          =  Color 0 0 1
yellow                        =  Color 1 1 0
cyan                          =  Color 0 1 1
magenta                       =  Color 1 0 1

grey                          :: Double -> Color
grey n                        =  Color n n n

color                         :: Double -> Double -> Double -> Color
color r g b                   =  Color r g b

graduateLow                   :: Color -> Color -> Double -> Color
graduateLow c1 c2 a           =  graduate c1 c2 a 16

graduateMed                   :: Color -> Color -> Double -> Color
graduateMed c1 c2 a           =  graduate c1 c2 a 64

graduateHigh                  :: Color -> Color -> Double -> Color
graduateHigh c1 c2 a          =  graduate c1 c2 a 256

graduate                      :: Color -> Color -> Double -> Int -> Color
graduate c1 c2 a n            =  Graduate c1 c2 a n

instance Num Color where
      Color r1 g1 b1 + Color r2 g2 b2
                              =  Color (r1+r2) (g1+g2) (b1+b2)
      Graduate c1 c2 a n +   c3@(Color _ _ _)
                              =  Graduate (c1+c3) (c2+c3) a n
      c3@(Color _ _ _) + Graduate c1 c2 a n
                              =  Graduate (c1+c3) (c2+c3) a n
      Graduate c1 c2 a n + Graduate c3 c4 a' n'
                              =  Graduate (c1+c3) (c2+c4)
                                      ((a+a')/2) (maximum [n,n'])
      a + DefaultColor        =  a
      DefaultColor + a        =  a

      Color r1 g1 b1 - Color r2 g2 b2
                              =  Color (r1-r2) (g1-g2) (b1-b2)
      Graduate c1 c2 a n -   c3@(Color _ _ _)
                              =  Graduate (c1-c3) (c2-c3) a n
      c3@(Color _ _ _) - Graduate c1 c2 a n
                              =  Graduate (c3-c1) (c3-c2) a n
      Graduate c1 c2 a n - Graduate c3 c4 a' n'
                              =  Graduate (c1-c3) (c2-c4)
                                      ((a+a')/2) (maximum [n,n'])
      a - DefaultColor        =  a
      DefaultColor - a        =  a

      Color r1 g1 b1 * Color r2 g2 b2
                              =  Color (r1*r2) (g1*g2) (b1*b2)
      Graduate c1 c2 a n *   c3@(Color _ _ _)
                              =  Graduate (c1*c3) (c2*c3) a n
      c3@(Color _ _ _) * Graduate c1 c2 a n
                              =  Graduate (c3*c1) (c3*c2) a n
      Graduate c1 c2 a n * Graduate c3 c4 a' n'
                              =  Graduate (c1*c3) (c2*c4)
                                      ((a+a')/2) (maximum [n,n'])
      a * DefaultColor        =  a
      DefaultColor * a        =  a

      negate (Color r g b)    =  Color (1-r) (1-g) (1-b)
      negate (Graduate c1 c2 a n)
                              =  Graduate (-c1) (-c2) a n
      negate DefaultColor     =  DefaultColor

      abs a                   =  a
      signum a                =  a
      fromInteger i           =  Color f f f
              where f         =  fromInteger i

instance Fractional Color where
      Color r1 g1 b1 / Color r2 g2 b2
                              =  Color (r1/r2) (g1/g2) (b1/b2)
      Graduate c1 c2 a n /      c3@(Color _ _ _)
                              =  Graduate (c1/c3) (c2/c3) a n
      c3@(Color _ _ _) / Graduate c1 c2 a n
                              =  Graduate (c3/c1) (c3/c2) a n
      Graduate c1 c2 a n / Graduate c3 c4 a' n'
                              =  Graduate (c1/c3) (c2/c4)
                                      ((a+a')/2) (maximum [n,n'])
      a / _                   =  a
      recip (Color r g b)     =  Color (recip r) (recip g) (recip b)
      recip a                 =  a
      fromRational i          =  Color f f f
              where f         =  fromRational i

hsv2rgb                       :: (Double,Double,Double) -> Color
hsv2rgb (_, 0, v)             =  Color v v v
hsv2rgb (h, s, v)             =  case i' of
                                      0 -> Color v t3 t1
                                      1 -> Color t2 v t1
                                      2 -> Color t1 v t3
                                      3 -> Color t1 t2 v
                                      4 -> Color t3 t1 v
                                      _ -> Color v t1 t2
              where
              h'              =  h / 60.0
              i'              =  mod (floor h') 6
              i               =  floor h'
              fract           =  h' - fromIntegral i
              t1              =  v * (1 - s)
              t2              =  v * (1 - s * fract)
              t3              =  v * (1 - s * (1 - fract))
