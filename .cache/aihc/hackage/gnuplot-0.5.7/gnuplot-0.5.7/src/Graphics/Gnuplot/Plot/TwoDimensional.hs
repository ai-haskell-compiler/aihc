-- shall this also export Graph.Type, set plotType and so on?
module Graphics.Gnuplot.Plot.TwoDimensional (
   T,

   list,
   function,
   functions,
   functionsWithLineSpec,
   parameterFunction,

   listFromFile,
   pathFromFile,

   linearScale,
   functionToGraph,
   ) where

import qualified Graphics.Gnuplot.Private.Graph2DType as Type
import qualified Graphics.Gnuplot.Private.Graph2D as Graph
import qualified Graphics.Gnuplot.Private.Plot    as Plot
import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Value.ColumnSet as Col
import qualified Graphics.Gnuplot.Value.Tuple as Tuple
import qualified Graphics.Gnuplot.Value.Atom  as Atom

import Graphics.Gnuplot.Utility
   (functionToGraph, linearScale, assembleCells, )

import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT


{- |
Plots can be assembled using 'mappend' or 'mconcat'.
You can alter attributes of embedded graphs using 'fmap'.
-}
type T x y = Plot.T (Graph.T x y)


-- * computed plots

{- |
> list Type.listLines (take 30 (let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) in fibs))
> list Type.lines (take 30 (let fibs0 = 0 : fibs1; fibs1 = 1 : zipWith (+) fibs0 fibs1 in zip fibs0 fibs1))
-}
list ::
   (Atom.C x, Atom.C y, Tuple.C a) =>
   Type.T x y a -> [a] -> T x y
list typ ps =
   Plot.withUniqueFile
      (assembleCells (map Tuple.text ps))
      [Graph.deflt typ
         [1 .. case Type.tupleSize typ of Tuple.ColumnCount n -> n]]

{- |
> function Type.line (linearScale 1000 (-10,10)) sin
-}
function ::
   (Atom.C x, Atom.C y,
    Tuple.C a, Tuple.C b) =>
   Type.T x y (a,b) -> [a] -> (a -> b) -> T x y
function typ args f =
   list typ (functionToGraph args f)

{- |
> functions Type.line (linearScale 1000 (-10,10)) [sin, cos]
-}
functions ::
   (Atom.C x, Atom.C y,
    Tuple.C a, Tuple.C b) =>
   Type.T x y (a,b) -> [a] -> [a -> b] -> T x y
functions typ args =
   functionsWithLineSpec typ args . map ((,) LineSpec.deflt)

{- |
> functionsWithLineSpec Type.line (linearScale 1000 (-10,10)) $
>    map (mapFst (flip LineSpec.title LineSpec.deflt)) [("sin", sin), ("cos", cos)]
-}
functionsWithLineSpec ::
   (Atom.C x, Atom.C y,
    Tuple.C a, Tuple.C b) =>
   Type.T x y (a,b) -> [a] -> [(LineSpec.T, a -> b)] -> T x y
functionsWithLineSpec typ args fs =
   let dat = map (\x -> (x, map (($ x) . snd) fs)) args
       mapType :: (a -> b) -> Type.T x y a -> Type.T x y b
       mapType _ (Type.Cons str) = Type.Cons str
       Tuple.ColumnCount na = Type.tupleSize $ mapType fst typ
       Tuple.ColumnCount nb = Type.tupleSize $ mapType snd typ
   in  Plot.withUniqueFile
          (assembleCells
              (map (\(a,b) -> Tuple.text a ++ concatMap Tuple.text b) dat))
          (Match.take fs $
           zipWith
              (\(lineSpec,_f) ns ->
                 Graph.lineSpec lineSpec $ Graph.deflt typ ([1..na] ++ ns))
              fs $
           ListHT.sliceVertical nb [(na+1)..])


{- |
> parameterFunction Type.line (linearScale 1000 (0,2*pi)) (\t -> (sin (2*t), cos t))
-}
parameterFunction ::
   (Atom.C x, Atom.C y,
    Tuple.C a) =>
   Type.T x y a -> [t] -> (t -> a) -> T x y
parameterFunction typ args f = list typ (map f args)



-- * plot stored data

fromFile ::
   (Atom.C x, Atom.C y) =>
   Type.T x y a -> FilePath -> Col.T a -> T x y
fromFile typ filename (Col.Cons cs) =
   Plot.fromGraphs filename [Graph.deflt typ cs]

listFromFile ::
   (Atom.C i, Atom.C y) =>
   Type.T i y y -> FilePath -> Int -> T i y
listFromFile typ filename column =
   fromFile typ filename (Col.atom column)

pathFromFile ::
   (Atom.C x, Atom.C y) =>
   Type.T x y (x,y) -> FilePath -> Int -> Int -> T x y
pathFromFile typ filename columnX columnY =
   fromFile typ filename (Col.pair (Col.atom columnX) (Col.atom columnY))
