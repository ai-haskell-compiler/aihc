module Main where

import Data.Time.Clock (getCurrentTime, utctDayTime)
import Graphics.Ascii.Haha.Terminal
import Graphics.Ascii.Haha.Geometry
import Graphics.Ascii.Haha.Bitmap
import Graphics.Ascii.Haha.Plot

-- The viewport.

center :: Point Double
center = Point 40 40

screen :: Rect Integer
screen = Rect (Point 1 1) (Point 80 40)

view :: Rect Double
view = Rect (Point 1 1) (Point 80 80)

-- The renderer.

main :: IO ()
main = do
  tick <- getCurrentTime >>= return . fromRational . toRational . utctDayTime
  putStr $ move 1 (1::Int)
  putStr $ plot (myBmp tick)
  main

plot :: Bitmap Double Pixel -> String
plot = string False screen (Point 1 1) " " "" . list 0.5 view

-- The picture.

myBmp :: Double -> Bitmap Double Pixel
myBmp t =
    drawPoly (lambda   t) (Pixel 'x' magentaBold)
  $ drawPoly (darkstar t) (Pixel '@' blackBold)
  $ empty

darkstar :: Double -> Poly Double
darkstar t = 
    scale (1.2 + 0.6 * sin (t/2)) center
  $ rotate (-t*2) center
  $ star center 5 40 18

lambda :: Double -> Poly Double
lambda t =
    scale (0.7 + 0.6 * sin (t/4)) center
  $ rotate t center
  $ Poly [
    Point 25 22 , Point 24 22 , Point 25 15 , Point 27 13
  , Point 33 13 , Point 36 16 , Point 37 18 , Point 49 53
  , Point 52 57 , Point 56 57 , Point 58 55 , Point 58 53
  , Point 60 53 , Point 60 58 , Point 59 61 , Point 57 63
  , Point 55 64 , Point 52 64 , Point 50 63 , Point 48 60
  , Point 41 41 , Point 30 63 , Point 22 63 , Point 37 29
  , Point 36 25 , Point 35 22 , Point 34 20 , Point 31 18
  , Point 28 19 , Point 25 21 , Point 25 22
  ]

