module Graphics.Ascii.Haha.Plot where

import Data.List (sortBy)

import qualified Graphics.Ascii.Haha.Bitmap as Bm
import Graphics.Ascii.Haha.Geometry
import Graphics.Ascii.Haha.Terminal

data Pixel = Pixel Char String
  deriving (Show, Eq)

--------[ helper functions ]---------------------------------------------------

-- Ordering function for pixels, top-left pixels are `less than' bottom-right
-- pixels.

orderPoint :: (Ord t) => Point t -> Point t -> Ordering
orderPoint (Point x0 y0) (Point x1 y1)
  | y0 > y1   = GT
  | y0 < y1   = LT
  | x0 > x1   = GT
  | x0 < x1   = LT
  | otherwise = EQ

-- Create an ordered list of all pixels in grid.
list :: (Integral i, RealFrac u) => u -> Rect u -> Bm.Bitmap u p -> [(Point i, p)]
list m r =
    sortBy (\a b -> orderPoint (fst a) (fst b))
  . Bm.toList
  . Bm.mapPoints discrete
  . Bm.mapPoints (\(Point x y) -> Point x (y * m))
  . Bm.clip r

string :: (Integral i, Show i) => Bool -> Rect i -> Point i -> String -> String -> [(Point i, Pixel)] -> String
string o rect@(Rect (Point x0 _) (Point x1 y1)) (Point x' y') nop prev p
  | ((x' > x1 && y' == y1) || (y' > y1)) = color reset
  | not (null p) && x  == x' && y  == y' = color b ++ [a] ++ lf ++ string o rect nextPos nop b xs
  | otherwise                            = color reset ++ nop ++ lf ++ string o rect nextPos nop reset p
  where
    (((Point x y), Pixel a b):xs) = p
    color c = if c == prev then "" else c
    lf   = if x' >= x1 && y' < y1 then (if o then "\n" else moveBack (x1 - x0 + 1) ++ moveDown (1::Int)) else ""
    nextPos = if x' >= x1 then Point x0 (y' + 1) else Point (x' + 1) y'

--------[ plotter functions ]--------------------------------------------------

{-plotPixel :: Plottable a => Point -> a -> IO ()
plotPixel (x, y) px = do
  putStr $ move x y
  putStr $ color clr
  putStr $ [chr]
  where Pixel chr clr = plot px-}

{-plotGrid :: Rect -> Bm.Bitmap Pixel -> String
plotGrid rect@(o, _) = string rect o "" . list rect-}

{-
plotGrids :: Area -> [G.Grid Pixel] -> String
plotGrids a ps = plotGrid a $ G.drawLayers ps

textToGrid :: Point -> String -> String -> G.Grid Pixel
textToGrid (xx, yy) t clr =
    G.fromList
  $ concat
  $ map f
  $ zip [yy..]
  $ lines t
  where
    f (y, s) = zipWith (\x c -> ((x, y), Pixel c clr)) [xx..] s

putGrid a g = do
  putStr $ plotGrid a g

putGrids a gs = do
  putStr $ plotGrids a gs-}

