module Graphics.Gnuplot.Graph.ThreeDimensional (
   T,
   Type,

--   deflt,

   lineSpec,

   impulses,
   vectors,
   Graph3DType.pm3d,

   Graph3DType.lines,
   points,
   ) where

import Graphics.Gnuplot.Private.Graph3D
import Graphics.Gnuplot.Private.Graph3DType as Graph3DType hiding (T, )
