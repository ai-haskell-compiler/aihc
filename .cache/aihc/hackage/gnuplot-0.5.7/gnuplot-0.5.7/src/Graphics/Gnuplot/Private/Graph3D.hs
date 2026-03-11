module Graphics.Gnuplot.Private.Graph3D where

import qualified Graphics.Gnuplot.Private.FrameOptionSet as OptionSet
import qualified Graphics.Gnuplot.Private.FrameOption as Option
import qualified Graphics.Gnuplot.Private.LineSpecification as LineSpec
import qualified Graphics.Gnuplot.Private.Graph3DType as GraphType
import qualified Graphics.Gnuplot.Private.Graph as Graph
import qualified Graphics.Gnuplot.Value.Atom as Atom
import qualified Data.Map as Map

import Graphics.Gnuplot.Private.Graph2D (Columns, columnToString, )

import Prelude hiding (lines, )


data T x y z =
   Cons {
      column_   :: Columns,
      type_     :: Type,
      lineSpec_ :: LineSpec.T
   }

type Type = String


toString :: T x y z -> String
toString (Cons c t l) =
   "using " ++ columnToString c ++
   " with " ++ t ++
   " " ++ LineSpec.toString l


type AxisOption x y z a =
   OptionSet.T (T x y z) -> Atom.OptionSet a

defltOptions :: (Atom.C x, Atom.C y, Atom.C z) => OptionSet.T (T x y z)
defltOptions =
   let mk ::
          Option.T -> Option.T ->
          Atom.OptionSet a ->
          [(Option.T, [String])]
       mk optData optFormat opts =
          (optData, Atom.optData opts) :
          (optFormat, Atom.optFormat opts) :
          Atom.optOthers opts
       result ::
          Atom.OptionSet x ->
          Atom.OptionSet y ->
          Atom.OptionSet z ->
          OptionSet.T (T x y z)
       result optX optY optZ =
          OptionSet.Cons $
          flip Map.union OptionSet.deflt $
          Map.fromList $
          mk Option.xData Option.xFormat optX ++
          mk Option.yData Option.yFormat optY ++
          mk Option.yData Option.yFormat optZ ++
          []
   in  result Atom.options Atom.options Atom.options


instance (Atom.C x, Atom.C y, Atom.C z) => Graph.C (T x y z) where
   command = Graph.Command "splot"
   toString = toString
   defltOptions = defltOptions


pm3d :: T x y z
pm3d = Cons (1:2:3:[]) "pm3d" LineSpec.deflt

deflt :: GraphType.T x y z a -> Columns -> T x y z
deflt t c = Cons c (GraphType.toString t) LineSpec.deflt

typ :: Type -> T x y z -> T x y z
typ t gr = gr{type_ = t}

{-
for 3D plots not all line attributes are supported like:
   pointsize
   pointtype

pm3d and impulses allow:
   linestyle
   linewidth
   linecolor
   linetype
   title

FIXME:
Do we need a separate LineSpec3D type or a type parameter for LineSpec?
-}
lineSpec :: LineSpec.T -> T x y z -> T x y z
lineSpec ls gr = gr{lineSpec_ = ls}
