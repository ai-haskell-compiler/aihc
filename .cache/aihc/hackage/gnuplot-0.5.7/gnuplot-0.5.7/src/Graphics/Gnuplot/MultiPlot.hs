module Graphics.Gnuplot.MultiPlot (
   T,
   Part,
   partFromFrame,
   partFromPlot,
   simpleFromFrameArray,
   simpleFromPartArray,
   title,
   ) where

import qualified Graphics.Gnuplot.Private.Frame as Frame
import qualified Graphics.Gnuplot.Private.Plot as Plot

import qualified Graphics.Gnuplot.Private.Display as Display
import qualified Graphics.Gnuplot.Private.Graph as Graph

import Data.Monoid (mconcat, )
import Data.Foldable (foldMap, )

import Data.Array (Array, elems, bounds, )
import Data.Ix (Ix, rangeSize, )

import Graphics.Gnuplot.Utility (quote, )


data T =
   Cons {
      title_ :: Maybe String,
      numRows, numColumns :: Int,
      parts :: [Part]
   }

newtype Part = Part {scriptFromPart :: Display.Script}

{-
We could generalize this to Frame and Plot
but MultiPlot itself cannot be made a part.
Thus the parameter cannot be generalized to
@Display.C gfx => gfx@.
-}
partFromFrame :: Graph.C graph => Frame.T graph -> Part
partFromFrame =
   Part . Display.toScript

partFromPlot :: Graph.C graph => Plot.T graph -> Part
partFromPlot =
   Part . Display.toScript


{-
The @simple@ prefix is for functions
that don't accept custom options.
Options have to be implemented, yet.
-}
simpleFromFrameArray ::
   (Graph.C graph, Ix i, Ix j) =>
   Array (i,j) (Frame.T graph) -> T
simpleFromFrameArray =
   simpleFromPartArray . fmap partFromFrame

simpleFromPartArray ::
   (Ix i, Ix j) =>
   Array (i,j) Part -> T
simpleFromPartArray arr =
   let ((r0,c0), (r1,c1)) = bounds arr
   in  Cons Nothing
          (rangeSize (r0,r1))
          (rangeSize (c0,c1))
          (elems arr)


title :: String -> T -> T
title str mp =
   mp {title_ = Just str}


instance Display.C T where
   toScript mp =
      mconcat $
      (Display.pure $
         Display.Body []
            ["set multiplot layout " ++
             show (numRows mp) ++ ", " ++ show (numColumns mp) ++
             foldMap ((" title " ++) . quote) (title_ mp)]) :
      (map scriptFromPart $ parts mp) ++
      (Display.pure $
         Display.Body [] ["unset multiplot"]) :
      []
