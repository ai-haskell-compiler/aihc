module Main where

import qualified Plot2DExtra

import qualified Numeric.Interpolation.NodeList as Nodes
import qualified Numeric.Interpolation.Piecewise as Piecewise
import qualified Numeric.Interpolation.Basis as Basis
import qualified Numeric.Interpolation.Type as Type


import qualified Graphics.Gnuplot.Advanced as GP

import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D

import Control.Monad.HT (void)


xs :: [Double]
xs = [0, 1, 3, 4, 6, 7, 9, 10, 11, 13]

main :: IO ()
main = do
   let linearNodes = Nodes.fromList $ map (\x -> (x, sin x)) xs
       hermite1Nodes = Nodes.fromList $ map (\x -> (x, (sin x, cos x))) xs
       cubicLinearNodes = Basis.coefficientsToCubicLinear xs $ map sin xs
       cubicParabolaNodes = Basis.coefficientsToCubicParabola xs $ map sin xs
   void $ GP.plotDefault $
      Plot2DExtra.functions (Plot2D.linearScale 1000 (-2,15)) $
         let interpolate typ nodes = Piecewise.interpolateConstantExt typ nodes
         in ("linear",        interpolate Type.linear linearNodes) :
            ("hermite1",      interpolate Type.hermite1 hermite1Nodes) :
            ("cubicLinear",   interpolate Type.hermite1 cubicLinearNodes) :
            ("cubicParabola", interpolate Type.hermite1 cubicParabolaNodes) :
            ("sin", sin) :
            []
