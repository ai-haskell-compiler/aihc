module Shape.Point (Point (..), pointSize, pointX) where

-- c2hs reads Shape/Kind.chi for this hook. Thus c2hs must process
-- Shape.Kind first.
{# import Shape.Kind #}

#include "shape.h"

newtype Point = Point PointPtr

pointSize :: Int
pointSize = {# sizeof shape_point #}

pointX :: PointPtr -> IO Int
pointX point = fromIntegral <$> {# get shape_point.x #} point
