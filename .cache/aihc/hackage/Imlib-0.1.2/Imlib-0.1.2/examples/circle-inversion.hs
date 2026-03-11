import Graphics.Imlib
import Data.Array
import System.Random

data Point = Point {x :: Double, y :: Double} deriving (Show, Eq)

data Circle = Circle { centre :: Point, radius :: Double } deriving (Show, Eq)

data Frame = Frame { top :: Double, left :: Double, bottom :: Double, right :: Double }

invert :: Circle -> Point -> Point
invert (Circle (Point a b) r) (Point x y) = Point (a + q*(x-a)) (b + q*(y-b))
      where q = r^2 / ((x-a)^2 + (y-b)^2)

invertRandomly :: StdGen -> [Circle] -> Point -> [Point]
invertRandomly g cs p = p : invertRandomly g' cs (invert (cs !! r) p)
    where (r,g') = randomR (0,length cs - 1) g

floorPoint :: Point -> (Int, Int)
floorPoint (Point x y) = (floor x, floor y)

main = do
     g <- getStdGen
     let cs = [Circle (Point (512+50) (512+50)) 50, Circle (Point 512 (512+50)) 50, Circle (Point (512+50) (512-1)) 50]
         p = Point 450 212
         ps = invertRandomly g cs p
         psf = filter (\(Point x y) -> x >= 0 && x < 1024 && y >= 0 && y < 1024) ps
         a = accumArray (\x y -> ImlibColor 0 255 255 255) 
                        (ImlibColor 0 0 0 0) 
                        ((0,0), (1024, 1024)) 
                        (zip (map floorPoint (take 50000 psf)) (repeat 0))
     buffer <- createImageUsingArray a
     contextSetImage buffer
     imageSetFormat "png"
     saveImage "hello.png"
