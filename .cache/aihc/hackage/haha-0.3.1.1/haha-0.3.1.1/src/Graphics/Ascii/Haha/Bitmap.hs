module Graphics.Ascii.Haha.Bitmap where

import qualified Data.Map as M
import Prelude hiding (filter)

import Graphics.Ascii.Haha.Geometry

--------[ image data type ]----------------------------------------------------

data Bitmap u p = Bitmap { bits :: M.Map (Point u) p }
  deriving (Show, Eq)

withBits :: (M.Map (Point u) p -> M.Map (Point v) q) -> Bitmap u p -> Bitmap v q
withBits f = Bitmap . f . bits

empty :: Bitmap u p
empty = Bitmap M.empty

get :: Ord u => Point u -> Bitmap u p -> Maybe p
get p img = M.lookup p (bits img)

put :: Ord u => Point u -> p -> Bitmap u p -> Bitmap u p
put p px = withBits (M.insert p px)

erase :: Ord u => Point u -> Bitmap u p -> Bitmap u p
erase p = withBits (M.delete p)

--mapPt :: (Point u -> p -> q) -> Bitmap u p -> Bitmap u q
mapPoints :: (Ord v) => (Point u -> Point v) -> Bitmap u p -> Bitmap v p
mapPoints f = withBits (M.mapKeys f)

{-mapPt :: (Point u -> p -> q) -> Bitmap u p -> Bitmap u q
mapPt f = withBits (M.mapWithKey f)

mapPtMaybe :: Ord u => (Point u -> p -> Maybe q) -> Bitmap u p -> Bitmap u q
mapPtMaybe f = withBits (M.mapMaybeWithKey f)-}

filterPt :: Ord u => (Point u -> p -> Bool) -> Bitmap u p -> Bitmap u p
filterPt f = withBits (M.filterWithKey f)

toList :: Bitmap u p -> [(Point u, p)]
toList = M.toAscList . bits

{-
fromList = Bitmap . M.fromList
points = M.keys . gr
filter = withBits . M.filter
filterWithKey = withBits . M.filterWithKey
member x = M.member x . gr-}

instance Functor (Bitmap u) where
  fmap = withBits . M.map

{-instance Monoid (Bitmap a) where
  mempty      = empty
  mappend x y = Bitmap $ M.union (bits x) (bits y)-}

--------[ clipping and sub-imaging ]-------------------------------------------

clip :: Ord u => Rect u -> Bitmap u p -> Bitmap u p
clip r img = filterPt (\p _ -> inRect p r) img

--------[ primitive drawing on the bits ]--------------------------------------

drawPoint :: Ord u => Point u -> p -> Bitmap u p -> Bitmap u p
drawPoint = put

drawList :: Ord u => [Point u] -> p -> Bitmap u p -> Bitmap u p
drawList l v g = foldr (flip drawPoint v) g l

drawLine :: (Fractional u, Ord u, Enum u) => Line u -> p -> Bitmap u p -> Bitmap u p
drawLine (Line (Point x0 y0) (Point x1 y1))
  | xIsY = drawPoint (Point x0 y0)
  | xOrY = drawList [Point s (y0 + (s - x0) * (y1 - y0) / (x1 - x0)) | s <- range x0 x1 ]
  | True = drawList [Point (x0 + (s - y0) * (x1 - x0) / (y1 - y0)) s | s <- range y0 y1 ]
  where
    xIsY = x0 == x1 && y0 == y1
    xOrY = abs (x1-x0) > abs (y1-y0)
    range f t = if f < t then [f .. t] else reverse [t .. f]

drawPoly :: (Fractional u, Ord u, Enum u) => Poly u -> p -> Bitmap u p -> Bitmap u p
drawPoly (Poly (a:b:xs)) v =
    drawLine (Line a b) v
  . drawPoly (Poly (b:xs)) v
drawPoly _ _ = id

drawElipse :: (Floating u, Ord u, Enum u) => Elipse u -> u -> p -> Bitmap u p -> Bitmap u p
drawElipse (Elipse (Point x y) rx ry) s = drawPoly $ Poly
  [ Point (x + rx * cos (2 * pi / s * t))
          (y + ry * sin (2 * pi / s * t))
  | t <- [0 .. s]]

drawCircle :: (Floating u, Ord u, Enum u) => Circle u -> u -> p -> Bitmap u p -> Bitmap u p
drawCircle (Circle p r) = drawElipse $ Elipse p r r

drawRect :: (Ord u, Enum u) => Rect u -> p -> Bitmap u p -> Bitmap u p
drawRect (Rect (Point x0 y0) (Point x1 y1)) = drawList
   [Point x y | x <- [x0 .. x1], y <- [y0 .. y1]]

--------[ layers and masks functions ]-----------------------------------------

{-drawLayers :: [Bitmap p] -> Bitmap p
drawLayers = Bitmap . M.unions . map bits

drawMask :: Bitmap p -> Bitmap q -> Bitmap p
drawMask g m = mapPtMaybe (\p _ -> get p g) m-}

