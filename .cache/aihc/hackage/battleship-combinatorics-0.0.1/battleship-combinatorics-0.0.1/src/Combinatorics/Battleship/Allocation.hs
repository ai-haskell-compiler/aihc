module Combinatorics.Battleship.Allocation where


import Data.Set (Set, )
import qualified Data.Set as Set

import Data.Tuple.HT (mapPair, )


type T = Set (Int, Int)


fatten, fattenHorizontal, fattenVertical :: T -> T
fatten = fattenHorizontal . fattenVertical

fattenHorizontal set =
   Set.mapMonotonic (\(x,y) -> (x-1,y)) set
   `Set.union`
   set
   `Set.union`
   Set.mapMonotonic (\(x,y) -> (x+1,y)) set

fattenVertical set =
   Set.mapMonotonic (\(x,y) -> (x,y-1)) set
   `Set.union`
   set
   `Set.union`
   Set.mapMonotonic (\(x,y) -> (x,y+1)) set

sizes :: T -> (Int, Int)
sizes =
   let size cs = maximum cs - minimum cs + 1
   in  mapPair (size, size) . unzip . Set.toList

boundingBox :: T -> ((Int, Int), (Int, Int))
boundingBox =
   (\(xs,ys) -> ((minimum xs, minimum ys), (maximum xs, maximum ys))) .
   unzip . Set.toList

normalize :: T -> T
normalize set =
   let (minx,miny) = fst $ boundingBox set
   in  Set.mapMonotonic (\(x,y) -> (x-minx,y-miny)) set
