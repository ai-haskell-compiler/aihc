{-# LANGUAGE RebindableSyntax #-}
{- |
Convert a physical value to a human readable string.
-}

module Number.Physical.Show where

import qualified Number.Physical              as Value
import qualified Number.Physical.UnitDatabase as Db
import Number.Physical.UnitDatabase
          (UnitSet, Scale, reciprocal, magnitude, symbol, scales)

import qualified Algebra.NormedSpace.Maximum as NormedMax
import qualified Algebra.Field               as Field
import qualified Algebra.Ring                as Ring

import Data.List(find)
import Data.Maybe(mapMaybe)

import NumericPrelude.Numeric
import NumericPrelude.Base


mulPrec :: Int
mulPrec = 7

{-| Show the physical quantity in a human readable form
    with respect to a given unit data base. -}
showNat :: (Ord i, Show v, Field.C a, Ord a, NormedMax.C a v) =>
   Db.T i a -> Value.T i v -> String
showNat db x =
   let (y, unitStr) = showSplit db x
   in  if null unitStr
       then show y
       else showsPrec mulPrec y unitStr

{-| Returns the rescaled value as number
    and the unit as string.
    The value can be used re-scale connected values
    and display them under the label of the unit -}
showSplit :: (Ord i, Show v, Field.C a, Ord a, NormedMax.C a v) =>
   Db.T i a -> Value.T i v -> (v, String)
showSplit db (Value.Cons xu x) =
   showScaled x (Db.positiveToFront (Db.decompose xu db))


showScaled :: (Ord i, Show v, Ord a, Field.C a, NormedMax.C a v) =>
   v -> [UnitSet i a] -> (v, String)
showScaled x [] = (x, "")
showScaled x (us:uss) =
  let (scaledX, sc) = chooseScale x us
  in  (scaledX, showUnitPart False (reciprocal us) sc ++
                   concatMap (\us' ->
                      showUnitPart True (reciprocal us') (defScale us')) uss)

{-| Choose a scale where the number becomes handy
    and return the scaled number and the corresponding scale. -}
chooseScale :: (Ord i, Show v, Ord a, Field.C a, NormedMax.C a v) =>
   v -> UnitSet i a -> (v, Scale a)
chooseScale x us =
   let sc = findCloseScale (NormedMax.norm x) (
               {- you should not reverse earlier,
                  otherwise the index of the default unit is wrong -}
               if reciprocal us
               then scales us
               else reverse (scales us))
   in  ((1 / magnitude sc) *> x, sc)


showUnitPart :: Bool -> Bool -> Scale a -> String
showUnitPart multSign rec sc =
   if rec
   then "/" ++ symbol sc
   else -- the multiplication sign can be omitted before the first unit component
        (if multSign then "*" else " ") ++ symbol sc

defScale :: UnitSet i v -> Scale v
defScale Db.UnitSet{Db.defScaleIx=def, Db.scales=scs} = scs!!def

findCloseScale :: (Ord a, Field.C a) => a -> [Scale a] -> Scale a
findCloseScale _ [sc]     = sc
findCloseScale x (sc:scs) =
   if 0.9 * magnitude sc < x
   then sc
   else findCloseScale x scs
findCloseScale _ _        =
   error "There must be at least one scale for a unit."

{-| unused -}
totalDefScale :: Ring.C a => Db.T i a -> a
totalDefScale =
   foldr (\us -> (magnitude (defScale us) *)) 1

{-| unused -}
getUnit :: Ring.C a => String -> Db.T i a -> Value.T i a
getUnit sym = Db.extractOne .
   (mapMaybe (\Db.UnitSet{Db.unit=u, scales=scs} ->
      fmap (Value.Cons u . magnitude) (find ((sym==) . symbol) scs)))
