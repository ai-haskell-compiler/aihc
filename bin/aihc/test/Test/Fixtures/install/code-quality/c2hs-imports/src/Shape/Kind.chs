module Shape.Kind (ShapeKind (..), PointPtr) where

#include "shape.h"

{# enum shape_kind as ShapeKind {underscoreToCase} deriving (Eq, Show) #}

{# pointer *shape_point as PointPtr #}
