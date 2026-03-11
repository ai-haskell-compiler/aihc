{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module Main where

import qualified Plot2DExtra

import qualified Numeric.Interpolation.NodeList as Nodes
import qualified Numeric.Interpolation.Piecewise as Piecewise
import qualified Numeric.Interpolation.Type as Type

import qualified Numeric.LAPACK.Singular as Singular
import qualified Numeric.LAPACK.Matrix.Shape as MatrixShape
import qualified Numeric.LAPACK.Matrix.BandedHermitianPositiveDefinite
                                                       as BandedSPD
import qualified Numeric.LAPACK.Matrix.BandedHermitian as BandedHermitian
import qualified Numeric.LAPACK.Matrix.Banded as Banded
import qualified Numeric.LAPACK.Matrix.Array as ArrMatrix
import qualified Numeric.LAPACK.Matrix as Matrix
import qualified Numeric.LAPACK.Vector as Vector
import Numeric.LAPACK.Matrix ((#\|))
import Numeric.LAPACK.Vector (Vector)

import qualified Type.Data.Num.Unary as Unary
import Type.Base.Proxy (Proxy)

import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Container as Container
import qualified Data.Array.Comfort.Shape.Static as ShapeStatic
import qualified Data.Array.Comfort.Shape as Shape

import qualified Graphics.Gnuplot.Advanced as GP
import qualified Graphics.Gnuplot.Frame as Frame
import qualified Graphics.Gnuplot.Frame.OptionSet as Opts
import qualified Graphics.Gnuplot.Plot.TwoDimensional as Plot2D
import qualified Graphics.Gnuplot.Graph.TwoDimensional as Graph2D
import qualified Graphics.Gnuplot.LineSpecification as LineSpec

import System.Random (randomRs, mkStdGen)
import Control.Monad.HT (void)
import Control.Applicative ((<$>))

import qualified Data.Foldable as Fold
import Data.Tuple.HT (mapFst)
import Data.Monoid ((<>))


{- |
We use @'Container.Shape' []@ for distinction.
We could use 'Matrix.ShapeInt' as well.
-}
type BasicMatrix = Matrix.General Matrix.ShapeInt (Container.Shape [])


noisy :: [(Double, Double)]
noisy =
   take 100 $
   zipWith
      (\x d -> (x, sin x + d))
      (randomRs (0,7) (mkStdGen 23))
      (randomRs (-0.2,0.2) (mkStdGen 42))

basisMatrixFull ::
   Type.T Double Double ny -> [Double] -> [Double] -> BasicMatrix Double
basisMatrixFull typ xs txs0 =
   let txs = Vector.autoFromList txs0
   in  Matrix.fromColumnContainer (Array.shape txs) $
       map (flip Array.map txs . Piecewise.interpolateConstantExt typ) $
       Type.basisFunctions typ xs

zipRowsWith :: (a -> b -> c) -> [a] -> [[b]] -> [c]
zipRowsWith f as bs = concat $ zipWith (map . f) as bs

basisMatrixSparse ::
   Type.T Double Double ny -> [Double] -> [Double] -> BasicMatrix Double
basisMatrixSparse typ xs txs =
   Matrix.fromRowMajor $
   Array.fromAssociations 0
      (Shape.ZeroBased $ length txs,
       Container.toShape $ Type.basisFunctions typ xs) $
   zipRowsWith (\k (j,x) -> ((k,j),x)) [0..] $
   map (Type.sampleBasisFunctions typ xs) txs

fit ::
   Type.T Double Double ny ->
   [Double] -> [(Double, Double)] -> Nodes.T Double ny
fit typ xs target =
   let (txs, tys) = unzip target
       matrix = basisMatrixSparse typ xs txs
   in  Type.coefficientsToInterpolator typ xs $
       Vector.toList $
       Matrix.unliftColumn MatrixShape.ColumnMajor
         (snd . Singular.leastSquaresMinimumNormRCond 1e-5 matrix) $
       Vector.autoFromList tys

matrixDiff ::
   Type.T Double Double ny -> [Double] -> [(Double, Double)] -> Double
matrixDiff typ xs target =
   let txs = map fst target
   in  Vector.normInf $ ArrMatrix.toVector $
       Matrix.sub
          (basisMatrixFull typ xs txs)
          (basisMatrixSparse typ xs txs)


mulSparseMatrixVector ::
   (Shape.Indexed shape) =>
   shape -> [[(Shape.Index shape, Double)]] ->
   [Double] -> Vector shape Double
mulSparseMatrixVector shape samples tys =
   Array.accumulate (+)
      (Vector.zero shape)
      (zipRowsWith (\ty (k,y) -> (k, y*ty)) tys samples)

shortVector ::
   (Unary.Natural k) =>
   Proxy k -> [(Int, Double)] -> (Int, Vector (ShapeStatic.ZeroBased k) Double)
shortVector width xs =
   let i0 = minimum $ map fst xs
   in (i0,
       Array.reshape Shape.static $
       Array.fromAssociations 0
          (Matrix.shapeInt $ Unary.integralFromProxy width) $
       map (mapFst (subtract i0)) xs)

bandedGramian ::
   (Shape.Indexed shape, Shape.Index shape ~ Int, Unary.Natural k) =>
   shape -> Proxy (Unary.Succ k) ->
   [[(Int, Double)]] -> Banded.HermitianPosSemidef k shape Double
bandedGramian shape width samples =
   BandedHermitian.sumRank1 MatrixShape.ColumnMajor shape $
   map ((,) 1 . shortVector width) samples

reifyPositive ::
   Integer -> (forall s. Unary.Natural s => Proxy (Unary.Succ s) -> w) -> w
reifyPositive n f = Unary.reifyNatural (n-1) (f . Unary.succ)

fitBanded ::
   Type.T Double Double ny ->
   [Double] -> [(Double, Double)] -> Nodes.T Double ny
fitBanded typ xs target =
   let shape = Container.toShape $ Type.basisFunctions typ xs
       (txs, tys) = unzip target
       samples = map (Type.sampleBasisFunctions typ xs) txs
   in reifyPositive
         (toInteger $ Type.basisOverlap typ)
         (\width ->
            Type.coefficientsToInterpolator typ xs $ Vector.toList $
            BandedSPD.assureFullRank (bandedGramian shape width samples)
            #\|
            mulSparseMatrixVector shape samples tys)

bandedDiff ::
   (ny -> ny -> Double) ->
   Type.T Double Double ny ->
   [Double] -> [(Double, Double)] -> Double
bandedDiff absDiff typ xs target =
   maximum $
   zipWith absDiff
      (Fold.toList $ fit typ xs target)
      (Fold.toList $ fitBanded typ xs target)

absDiffSingle :: Double -> Double -> Double
absDiffSingle x y = abs (x-y)

absDiffPair :: (Double,Double) -> (Double,Double) -> Double
absDiffPair (x,dx) (y,dy) = max (abs (x-y)) (abs (dx-dy))


plotBasisFunctions ::
   (String, Type.T Double Double ny) -> [Double] ->
   Frame.T (Graph2D.T Double Double)
plotBasisFunctions (title, nodeType) xs =
   let abscissa = Plot2D.linearScale 1000 (minimum xs, maximum xs)
   in  Frame.cons (Opts.title title $ Opts.key False Opts.deflt) $
       Plot2D.functions Graph2D.lines abscissa $
       map (Piecewise.interpolateConstantExt nodeType) $
       Type.basisFunctions nodeType xs


typeLinear :: (String, Type.T Double Double Double)
typeLinear        = ("linear",        Type.linear)
typeHermite1, typeCubicLinear, typeCubicParabola ::
   (String, Type.T Double Double (Double, Double))
typeHermite1      = ("hermite1",      Type.hermite1)
typeCubicLinear   = ("cubicLinear",   Type.cubicLinear)
typeCubicParabola = ("cubicParabola", Type.cubicParabola)


main :: IO ()
main = do
   let xs = [0, 1, 3, 4, 6, 7]
       exs = (-1) : xs ++ [8]
   void $ GP.plotDefault $ plotBasisFunctions typeLinear xs
   void $ GP.plotDefault $ plotBasisFunctions typeHermite1 xs
   void $ GP.plotDefault $ plotBasisFunctions typeCubicLinear exs
   void $ GP.plotDefault $ plotBasisFunctions typeCubicParabola exs
   let linearNodes = fit Type.linear xs noisy
       hermite1Nodes = fit Type.hermite1 xs noisy
       cubicLinearNodes = fit Type.cubicLinear exs noisy
       cubicParabolaNodes = fit Type.cubicParabola exs noisy
   void $ GP.plotDefault $
      (Graph2D.lineSpec (LineSpec.title "target" LineSpec.deflt)
         <$> Plot2D.list Graph2D.points noisy)
      <>
      (let interpolate (name,typ) nodes =
             (name, Piecewise.interpolateConstantExt typ nodes)
       in Plot2DExtra.functions (Plot2D.linearScale 1000 (-2,10)) $
            interpolate typeLinear linearNodes :
            interpolate typeHermite1 hermite1Nodes :
            interpolate typeCubicLinear cubicLinearNodes :
            interpolate typeCubicParabola cubicParabolaNodes :
            [])

   putStrLn "differences between matrices should be almost zero:"
   let printMatrixDiff (name,typ) ps =
         putStrLn $ name ++ ": " ++ show (matrixDiff typ ps noisy)
   printMatrixDiff typeLinear xs
   printMatrixDiff typeHermite1 xs
   printMatrixDiff typeCubicLinear exs
   printMatrixDiff typeCubicParabola exs

   putStrLn "differences between samples should be almost zero:"
   let printBandedDiff diff (name,typ) ps =
         putStrLn $ name ++ ": " ++ show (bandedDiff diff typ ps noisy)
   printBandedDiff absDiffSingle typeLinear xs
   printBandedDiff absDiffPair typeHermite1 xs
   printBandedDiff absDiffPair typeCubicLinear exs
   printBandedDiff absDiffPair typeCubicParabola exs
