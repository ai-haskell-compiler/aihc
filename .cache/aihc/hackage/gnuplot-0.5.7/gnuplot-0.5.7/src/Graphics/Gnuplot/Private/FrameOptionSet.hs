{-
This controls gnuplot variables for a plot.
We use a separate data type for that purpose
in order to be able to share a set of options between different plots.
(If we would only allow to set options of a frame,
e.g. border :: Bool -> Frame -> Frame,
we could of course still build
an options setting function of type Frame -> Frame
from smaller option setters.)
-}
module Graphics.Gnuplot.Private.FrameOptionSet where

import qualified Graphics.Gnuplot.Private.FrameOption as Option
import Graphics.Gnuplot.Utility (formatBool, )

import qualified Data.Map as Map

import Data.Maybe.HT (toMaybe, )
import Data.Maybe (mapMaybe, )


type Plain = Map.Map Option.T [String]

newtype T graph =
   Cons {decons :: Plain}


{- |
The default options contain what we expect as default value in gnuplot.
We need an entry for every option that cannot be reset by @unset@.
-}
deflt :: Plain
deflt =
   Map.fromList $
   (Option.keyShow, []) :
--   (Option.border, []) :
   (Option.sizeRatio, ["noratio"]) :
   (Option.xLabelText, []) :
   (Option.yLabelText, []) :
   (Option.zLabelText, []) :
   (Option.xRangeBounds, ["[*:*]"]) :
   (Option.yRangeBounds, ["[*:*]"]) :
   (Option.zRangeBounds, ["[*:*]"]) :
   (Option.xFormat, []) :
   (Option.yFormat, []) :
   (Option.zFormat, []) :
   (Option.xTickLabels, []) :
   (Option.yTickLabels, []) :
   (Option.zTickLabels, []) :
   (Option.gridXTicks, ["noxtics"]) :
   (Option.gridYTicks, ["noytics"]) :
   (Option.gridZTicks, ["noztics"]) :
   (Option.styleFillSolid, ["0"]) :
   (Option.styleFillBorder, []) :
   (Option.styleHistogram, ["clustered"]) :
--   (Option.timeFmt, [quote "%s"]) :
   (Option.boxwidth, []) :
   []

initial :: Plain
initial =
   flip Map.union deflt $
   Map.fromList $
   (Option.xData, []) :
   (Option.yData, []) :
   (Option.zData, []) :
--   (Option.timeFmt, []) :
   []

{- |
Add (set) an option with arguments as plain strings.

This is very flexible, but not very safe.
Use it only as fall-back,
if there is no specific setter function in "Graphics.Gnuplot.Frame.OptionSet".
-}
add :: Option.T -> [String] -> T graph -> T graph
add opt args (Cons m) =
   Cons (Map.insert opt args m)

{- |
Remove (unset) an option.

This is very flexible, but not very safe.
Use it only as fall-back,
if there is no specific setter function in "Graphics.Gnuplot.Frame.OptionSet".
-}
remove :: Option.T -> T graph -> T graph
remove opt (Cons m) =
   Cons (Map.delete opt m)

{- |
Convert the difference between the first and the second option set
into a sequence of 'set' and 'unset' commands.
-}
diffToString :: Plain -> Plain -> [String]
diffToString m0 m1 =
   mapMaybe
      (\(Option.Cons opt _, (old,new)) ->
         toMaybe (old/=new) $
         maybe
            ("unset " ++ opt)
            (\args -> "set " ++ opt ++ concatMap (' ':) args)
            new) $
   Map.toList $
   Map.unionWith
      (\(old,_) (_,new) -> (old,new))
      (fmap (\x -> (Just x, Nothing)) m0)
      (fmap (\x -> (Nothing, Just x)) m1)

{- |
Set or unset option according to a 'Bool'.
This is for switches that can be disabled using @unset@.

This is very flexible, but not very safe.
Use it only as fall-back,
if there is no specific setter function in "Graphics.Gnuplot.Frame.OptionSet".

See also: 'addBool', 'add', 'remove'.
-}
boolean :: Option.T -> Bool -> T graph -> T graph
boolean opt on =
   if on
     then add opt []
     else remove opt

{- |
Add an option with boolean value
that is formatted like @set style fill border@ and @set style fill noborder@.
The name of the internal state (i.e. @border@)
must be stored in the second field of the option.

This is very flexible, but not very safe.
Use it only as fall-back,
if there is no specific setter function in "Graphics.Gnuplot.Frame.OptionSet".

See also 'boolean'.
-}
addBool :: Option.T -> Bool -> T graph -> T graph
addBool opt@(Option.Cons _ state) arg =
   add opt [formatBool state arg]
