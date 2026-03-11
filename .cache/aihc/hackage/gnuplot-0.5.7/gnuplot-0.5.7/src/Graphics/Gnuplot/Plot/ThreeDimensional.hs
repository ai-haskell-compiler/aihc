module Graphics.Gnuplot.Plot.ThreeDimensional (
   T,
   cloud,
--   function,
   mesh,
   surface,

   linearScale,
   functionToGraph,
   ) where

import qualified Graphics.Gnuplot.Private.Graph3DType as Type
import qualified Graphics.Gnuplot.Private.Graph3D as Graph
import qualified Graphics.Gnuplot.Private.Plot    as Plot
-- import qualified Graphics.Gnuplot.Value.ColumnSet as Col
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom  as Atom

import Graphics.Gnuplot.Utility
   (functionToGraph, linearScale, assembleCells, )

import Data.List.HT (outerProduct, )


{- |
Plots can be assembled using 'mappend' or 'mconcat'
or several functions from "Data.Foldable".
-}
type T x y z = Plot.T (Graph.T x y z)


-- * computed plots

cloud ::
   (Atom.C x, Atom.C y, Atom.C z,
    Tuple.C a) =>
   Type.T x y z a -> [a] -> T x y z
cloud typ ps =
   Plot.withUniqueFile
      (assembleCells (map Tuple.text ps))
      [Graph.deflt typ
         [1 .. case Type.tupleSize typ of Tuple.ColumnCount n -> n]]

{-
function ::
   (Atom.C x, Atom.C y, Atom.C z) =>
   Type.T (x,y,z) -> [x] -> [y] -> (x -> y -> z) -> T
function xArgs yArgs f =
   cloud (liftM2 (\x y -> (x, y, f x y)) xArgs yArgs)
-}


mesh ::
   (Atom.C x, Atom.C y, Atom.C z,
    Tuple.C x, Tuple.C y, Tuple.C z) =>
   [[(x,y,z)]] -> T x y z
mesh pss =
   Plot.withUniqueFile
      (assembleCells (concatMap (\ps -> map Tuple.text ps ++ [[]]) pss))
      [Graph.pm3d]

surface ::
   (Atom.C x, Atom.C y, Atom.C z,
    Tuple.C x, Tuple.C y, Tuple.C z) =>
   [x] -> [y] -> (x -> y -> z) -> T x y z
surface xArgs yArgs f =
   mesh (outerProduct (\x y -> (x, y, f x y)) xArgs yArgs)
