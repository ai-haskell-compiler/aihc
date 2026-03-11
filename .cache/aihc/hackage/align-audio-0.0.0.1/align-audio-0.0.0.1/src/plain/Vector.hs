module Vector where

import qualified Numeric.Netlib.Class as Class
import qualified Data.Complex as Complex
import Data.Complex (Complex)

import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable (Array)


findPeak :: (Class.Real a) => Array (Shape.Cyclic Int) a -> (Int, Int)
findPeak xs =
   let (k,_) = Array.argMaximum $ Array.map abs xs
   in (k - Shape.size (Array.shape xs), k)

mulConj ::
   (Shape.C sh, Eq sh, Class.Real a) =>
   Array sh (Complex a) -> Array sh (Complex a) -> Array sh (Complex a)
mulConj = Array.zipWith (\x y -> x * Complex.conjugate y)
