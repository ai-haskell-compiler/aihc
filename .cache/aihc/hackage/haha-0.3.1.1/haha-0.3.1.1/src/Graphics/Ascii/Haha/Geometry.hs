module Graphics.Ascii.Haha.Geometry where

---------[ primitive geometries ]-----------------------------------------------

data Point u = Point  { _x :: u, _y :: u }
  deriving (Show, Eq, Ord)

data Line u = Line { _a :: Point u, _b :: Point u }
  deriving (Show, Eq, Ord)

data Tri u = Tri (Point u) (Point u) (Point u)
  deriving (Show, Eq, Ord)

data Poly u = Poly [Point u]
  deriving (Show, Eq, Ord)

data Mesh u = Mesh [Tri u]
  deriving (Show, Eq, Ord)

---------[ procedural geometrical objects ]-------------------------------------

data Rect u = Rect (Point u) (Point u)
  deriving (Show, Eq, Ord)

data Circle u = Circle (Point u) u
  deriving (Show, Eq, Ord)

data Elipse u = Elipse (Point u) u u
  deriving (Show, Eq, Ord)

---------[ primitive geometry class ]-------------------------------------------

class Geometry g where
  centroid  :: (Ord u, Floating u) => g u -> Point u
  bounds    :: (Ord u, Floating u) => g u -> Rect u
  translate :: (Ord u, Floating u) => u -> u -> g u -> g u
  rotate    :: (Ord u, Floating u) => u -> Point u -> g u -> g u
  scale     :: (Ord u, Floating u) => u -> Point u -> g u -> g u
  outline   :: g u -> Poly u
  mesh      :: g u -> Mesh u
  discrete  :: (RealFrac u, Integral i) => g u -> g i

-- Shortcut translations.

{-g +- d = translate (d, 0)  g
g +| d = translate (0, d)  g
g +\ d = translate (d, d)  g
g +/ d = translate (d, -d) g-}

-- Rotate geometry around own centroid.

rotateLocal :: (Geometry g, Ord u, Floating u) => u -> g u -> g u
rotateLocal u g = rotate u (centroid g) g

---------[ discrete 2-dimensional point type ]----------------------------------

instance Geometry Point where
  centroid  = centroidPoint
  bounds    = boundsPoint
  translate = translatePoint
  rotate    = rotatePoint
  scale     = scalePoint
  outline   = outlinePoint
  mesh      = meshPoint
  discrete  = discretePoint

centroidPoint :: Point u -> Point u
centroidPoint = id

boundsPoint :: Point u -> Rect u
boundsPoint p = Rect p p

translatePoint :: Num u => u -> u -> Point u -> Point u
translatePoint dx dy (Point x y) = Point (x + dx) (y + dy)

rotatePoint :: Floating u => u -> Point u -> Point u -> Point u
rotatePoint t (Point ox oy) (Point x y) = Point
  (ox + (x-ox) * cos t - (y-oy) * sin t)
  (oy + (x-ox) * sin t + (y-oy) * cos t)

scalePoint :: (Num u) => u -> Point u -> Point u -> Point u
scalePoint t (Point ox oy) (Point x y) = Point
  (ox + (x-ox) * t)
  (oy + (y-oy) * t)

outlinePoint :: Point u -> Poly u
outlinePoint p = Poly [p, p]

meshPoint :: Point u -> Mesh u
meshPoint p = Mesh [Tri p p p]

discretePoint :: (RealFrac u, Integral i) => Point u -> Point i
discretePoint (Point a b) = Point (round a) (round b)

--------[ discrete 2-dimensional line type ]-----------------------------------

instance Geometry Line where
  centroid  = centroidLine
  bounds    = boundsLine
  translate = translateLine
  rotate    = rotateLine
  scale     = scaleLine
  outline   = outlineLine
  mesh      = meshLine
  discrete  = discreteLine

centroidLine :: Fractional u => Line u -> Point u
centroidLine (Line (Point x0 y0) (Point x1 y1)) = Point ((x0+x1) / 2) ((y0+y1) / 2)

boundsLine :: Line u -> Rect u
boundsLine (Line a b) = Rect a b

translateLine :: (Ord u, Floating u) => u -> u -> Line u -> Line u
translateLine dx dy (Line a b) = Line (translate dx dy a) (translate dx dy b)

rotateLine :: (Ord u, Floating u) => u -> Point u -> Line u -> Line u
rotateLine d o (Line a b) = Line (rotate d o a) (rotate d o b)

scaleLine :: (Ord u, Floating u) => u -> Point u -> Line u -> Line u
scaleLine d o (Line a b) = Line (scale d o a) (scale d o b)

outlineLine :: Line u -> Poly u
outlineLine (Line a b) = Poly [a, b]

meshLine :: Line u -> Mesh u
meshLine (Line a b) = Mesh [Tri a a b]

discreteLine :: (RealFrac u, Integral i) => Line u -> Line i
discreteLine (Line a b) = Line (discrete a) (discrete b)

--------[ discrete 2-dimensional triangle type ]-------------------------------

instance Geometry Tri where
  centroid  = centroidTri
  bounds    = boundsTri
  translate = translateTri
  rotate    = rotateTri
  scale     = scaleTri
  outline   = outlineTri
  mesh      = meshTri
  discrete  = discreteTri

centroidTri :: (Ord u, Floating u) => Tri u -> Point u
centroidTri (Tri a b c) = centroid $ Line (centroid $ Line a b) c

boundsTri :: Ord u => Tri u -> Rect u
boundsTri (Tri (Point x0 y0) (Point x1 y1) (Point x2 y2)) = Rect
  (Point (min (min x0 x1) x2) (min (min y0 y1) y2))
  (Point (max (max x0 x1) x2) (max (max y0 y1) y2))

translateTri :: (Ord u, Floating u) => u -> u -> Tri u -> Tri u
translateTri dx dy (Tri a b c) = Tri (translate dx dy a) (translate dx dy b) (translate dx dy c)

rotateTri :: (Ord u, Floating u) => u -> Point u -> Tri u -> Tri u
rotateTri d o (Tri a b c) = Tri (rotate d o a) (rotate d o b) (rotate d o c)

scaleTri :: (Ord u, Floating u) => u -> Point u -> Tri u -> Tri u
scaleTri d o (Tri a b c) = Tri (scale d o a) (scale d o b) (scale d o c)

outlineTri :: Tri u -> Poly u
outlineTri (Tri a b c) = Poly [a, b, c, a]

meshTri :: Tri u -> Mesh u
meshTri t = Mesh [t]

discreteTri :: (RealFrac u, Integral i) => Tri u -> Tri i
discreteTri (Tri a b c) = Tri (discrete a) (discrete b) (discrete c)

--------[ discrete 2-dimensional polygon type ]--------------------------------

instance Geometry Poly where
  centroid  = centroidPoly
  bounds    = boundsPoly
  translate = translatePoly
  rotate    = rotatePoly
  scale     = scalePoly
  outline   = outlinePoly
  mesh      = meshPoly
  discrete  = discretePoly

centroidPoly :: Fractional u => Poly u -> Point u
centroidPoly (Poly xs) = Point (sum (map _x xs) / n) (sum (map _y xs) / n)
  where n = fromIntegral $ length xs

boundsPoly :: Ord u => Poly u -> Rect u
boundsPoly (Poly xs) = Rect
  (Point (minimum $ map _x xs) (minimum $ map _y xs))
  (Point (maximum $ map _x xs) (maximum $ map _y xs))

translatePoly :: (Ord u, Floating u) => u -> u -> Poly u -> Poly u
translatePoly dx dy (Poly xs) = Poly $ map (translate dx dy) xs

rotatePoly :: (Ord u, Floating u) => u -> Point u -> Poly u -> Poly u
rotatePoly d o (Poly xs) = Poly $ map (rotate d o) xs

scalePoly :: (Ord u, Floating u) => u -> Point u -> Poly u -> Poly u
scalePoly d o (Poly xs) = Poly $ map (scale d o) xs

outlinePoly :: Poly u -> Poly u
outlinePoly = id

meshPoly :: Poly u -> Mesh u
meshPoly = error "todo"

discretePoly :: (RealFrac u, Integral i) => Poly u -> Poly i
discretePoly (Poly xs) = Poly $ map discrete xs

--------[ discrete 2-dimensional rectangle utils ]-----------------------------

inRect :: (Ord u) => Point u -> Rect u -> Bool
inRect (Point x y) (Rect (Point x0 y0) (Point x1 y1)) =
  x >= x0 && x <= x1 && y >= y0 && y <= y1

intersectRect :: (Ord u, Num u) => Rect u -> Rect u -> Maybe (Rect u)
intersectRect (Rect (Point ax0 ay0) (Point ax1 ay1)) (Rect (Point bx0 by0) (Point bx1 by1)) =
  if ((x1-x0) <= 0 || (y1-y0) <= 0)
  then Nothing
  else Just $ Rect (Point x0 y0) (Point x1 y1)
    where
    x0 = max ax0 bx0 
    y0 = max ay0 by0 
    x1 = min ax1 bx1 
    y1 = min ay1 by1 

star :: (Enum u, Floating u) => Point u -> u -> u -> u -> Poly u
star (Point x y) s r0 r1 = Poly $ concat
  [[  Point
        (x + r0 * cos (2 * pi / s * t))
        (y + r0 * sin (2 * pi / s * t))
    , Point
        (x + r1 * cos (2 * pi / (s*2) * (t*2+1)))
        (y + r1 * sin (2 * pi / (s*2) * (t*2+1)))
  ] | t <- [0 .. s]]

