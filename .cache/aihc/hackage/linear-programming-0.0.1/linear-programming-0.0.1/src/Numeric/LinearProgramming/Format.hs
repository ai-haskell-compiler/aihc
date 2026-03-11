{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Numeric.LinearProgramming.Format (
   Identifier,
   mathProg,
   ) where

import qualified Numeric.LinearProgramming.Common as LP
import Numeric.LinearProgramming.Common
         (Bound(..), Inequality(Inequality),
          Bounds, Direction(..), Objective, (.*))

import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import qualified Data.List as List

import Text.Printf (printf)

import Prelude hiding (sum)



type Term = LP.Term Double

type Constraints ix = LP.Constraints Double ix


class Identifier ix where
   identifier :: ix -> String

instance Identifier Char where
   identifier x = [x]

instance Identifier c => Identifier [c] where
   identifier = concatMap identifier

instance Identifier Int where
   identifier = printf "x%d"

instance Identifier Integer where
   identifier = printf "x%d"


bound :: (Identifier ix) => Inequality ix -> String
bound (Inequality ix bnd) =
   printf "var %s%s;" (identifier ix) $
   case bnd of
      LessEqual up -> printf ", <=%f" up
      GreaterEqual lo -> printf ", >=%f" lo
      Between lo up -> printf ", >=%f, <=%f" lo up
      Equal x -> printf ", =%f" x
      Free -> ""


sum :: (Identifier ix) => [Term ix] -> String
sum [] = "0"
sum xs =
   let formatTerm (LP.Term c ix) = printf "%f*%s" c (identifier ix) in
   List.intercalate "+" $ map formatTerm xs

constraint :: (Identifier ix) => Inequality [Term ix] -> String
constraint (Inequality terms bnd) =
   let sumStr = sum terms in
   case bnd of
      LessEqual up -> printf "%s <= %f" sumStr up
      GreaterEqual lo -> printf "%f <= %s" lo sumStr
      Between lo up -> printf "%f <= %s <= %f" lo sumStr up
      Equal x -> printf "%s = %f" sumStr x
      Free -> sumStr

direction :: Direction -> String
direction Minimize = "minimize"
direction Maximize = "maximize"

objective ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Identifier ix) =>
   Objective sh -> String
objective =
   sum . map (\(ix,c) -> c .* ix) . Array.toAssociations

mathProg ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Identifier ix) =>
   Bounds ix -> Constraints ix ->
   (Direction, Objective sh) -> [String]
mathProg bounds constrs (dir,obj) =
   map bound bounds ++
   "" :
   direction dir :
   printf "value: %s;" (objective obj) :
   "" :
   "subject to" :
   zipWith
      (\k constr -> printf "constr%d: %s;" k $ constraint constr)
      [(0::Int)..] constrs ++
   "" :
   "end;" :
   []
