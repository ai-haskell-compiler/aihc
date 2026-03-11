{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Data.Array.Comfort.Shape.Test (tests) where

import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Shape.Utility (isRight)

import Control.Applicative (pure)
import Data.Tuple.HT (mapSnd)

import qualified Test.QuickCheck as QC


inBounds :: (Shape.Indexed sh) => sh -> Bool
inBounds sh  =  all (Shape.inBounds sh) $ Shape.indices sh


forAllIndices ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Show ix, QC.Testable prop) =>
   sh -> (ix -> prop) -> QC.Property
forAllIndices sh f =
   let ixs = Shape.indices sh
   in not (null ixs)  QC.==>  QC.forAll (QC.elements ixs) f

-- ToDo: we need to check for indices outside of bounds, too
inBoundsOffset ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Show ix) => sh -> QC.Property
inBoundsOffset sh =
   forAllIndices sh $ \ix ->
      Shape.inBounds sh ix ==
      isRight (Shape.getChecked (Shape.unifiedOffset sh ix))

inBoundsSizeOffset ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Show ix) => sh -> QC.Property
inBoundsSizeOffset sh =
   forAllIndices sh $ \ix ->
      Shape.inBounds sh ix ==
      isRight (Shape.getChecked (snd (Shape.unifiedSizeOffset sh) ix))

sizeOffset ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Show ix) => sh -> QC.Property
sizeOffset sh =
   forAllIndices sh $ \ix ->
      mapSnd ($ix) (Shape.sizeOffset sh)
      ==
      (Shape.size sh, Shape.offset sh ix)

uncheckedSizeOffset ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Show ix) => sh -> QC.Property
uncheckedSizeOffset sh =
   forAllIndices sh $ \ix ->
      mapSnd ($ix) (Shape.uncheckedSizeOffset sh) ==
         (Shape.size sh, Shape.uncheckedOffset sh ix)

unifiedSizeOffsetA ::
   (Shape.Checking check, Shape.Indexed sh, Shape.Index sh ~ ix, Show ix) =>
   Shape.CheckSingleton check -> sh -> QC.Property
unifiedSizeOffsetA check sh =
   forAllIndices sh $ \ix ->
      mapSnd ($ix) (Shape.unifiedSizeOffset sh) ==
         (Shape.size sh, Shape.requireCheck check $ Shape.unifiedOffset sh ix)

unifiedSizeOffsetB ::
   (Shape.Checking check, Shape.Indexed sh, Shape.Index sh ~ ix, Show ix) =>
   Shape.CheckSingleton check -> sh -> QC.Property
unifiedSizeOffsetB check sh =
   forAllIndices sh $ \ix ->
      (mapSnd (Shape.requireCheck check . ($ix)) $ Shape.unifiedSizeOffset sh)
      ==
      case check of
         Shape.Checked ->
            mapSnd (pure . ($ix)) (Shape.sizeOffset sh)
         Shape.Unchecked ->
            mapSnd (pure . ($ix)) (Shape.uncheckedSizeOffset sh)

uncheckedOffset ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Show ix) => sh -> QC.Property
uncheckedOffset sh =
   forAllIndices sh $ \ix ->
      Shape.offset sh ix == Shape.uncheckedOffset sh ix

unifiedOffset ::
   (Shape.Checking check, Shape.Indexed sh, Shape.Index sh ~ ix, Show ix) =>
   Shape.CheckSingleton check -> sh -> QC.Property
unifiedOffset check sh =
   forAllIndices sh $ \ix ->
      Shape.requireCheck check (Shape.unifiedOffset sh ix) ==
      case check of
         Shape.Checked -> pure $ Shape.offset sh ix
         Shape.Unchecked -> pure $ Shape.uncheckedOffset sh ix

lengthIndices :: (Shape.Indexed sh) => sh -> Bool
lengthIndices sh  =  length (Shape.indices sh) == Shape.size sh

indexOffsets :: (Shape.Indexed sh) => sh -> Bool
indexOffsets sh =
   map (Shape.offset sh) (Shape.indices sh) == take (Shape.size sh) [0..]

invIndices :: (Shape.InvIndexed sh, Shape.Index sh ~ ix, Eq ix) => sh -> Bool
invIndices sh =
   Shape.indices sh ==
   map (Shape.indexFromOffset sh) (take (Shape.size sh) [0..])

uncheckedInvIndices ::
   (Shape.InvIndexed sh, Shape.Index sh ~ ix, Eq ix) => sh -> Bool
uncheckedInvIndices sh =
   Shape.indices sh ==
   map (Shape.uncheckedIndexFromOffset sh) (take (Shape.size sh) [0..])

unifiedInvIndicesA ::
   (Shape.Checking check, Shape.InvIndexed sh, Shape.Index sh ~ ix, Eq ix) =>
   Shape.CheckSingleton check -> sh -> Bool
unifiedInvIndicesA check sh =
   map pure (Shape.indices sh) ==
   map (Shape.requireCheck check . Shape.unifiedIndexFromOffset sh)
      (take (Shape.size sh) [0..])

unifiedInvIndicesB ::
   (Shape.Checking check, Shape.InvIndexed sh, Shape.Index sh ~ ix, Eq ix) =>
   Shape.CheckSingleton check -> sh -> QC.Property
unifiedInvIndicesB check sh =
   let n = Shape.size sh in n>0 QC.==>
   QC.forAll (QC.choose (0, n-1)) $ \k ->
   Shape.requireCheck check (Shape.unifiedIndexFromOffset sh k) ==
   case check of
      Shape.Checked -> pure $ Shape.indexFromOffset sh k
      Shape.Unchecked -> pure $ Shape.uncheckedIndexFromOffset sh k


unifiedTests ::
   (Shape.Checking check,
    Shape.InvIndexed sh, Show sh, Shape.Index sh ~ ix, Eq ix, Show ix) =>
   Shape.CheckSingleton check -> QC.Gen sh -> [(String, QC.Property)]
unifiedTests check gen =
   ("unifiedSizeOffsetA", QC.forAll gen (unifiedSizeOffsetA check)) :
   ("unifiedSizeOffsetB", QC.forAll gen (unifiedSizeOffsetB check)) :
   ("unifiedOffset", QC.forAll gen (unifiedOffset check)) :
   ("unifiedInvIndicesA", QC.forAll gen (unifiedInvIndicesA check)) :
   ("unifiedInvIndicesB", QC.forAll gen (unifiedInvIndicesB check)) :
   []

-- cf. Test.Utility
prefix :: String -> [(String, test)] -> [(String, test)]
prefix msg =
   map (\(str,test) -> (msg ++ "." ++ str, test))

tests ::
   (Shape.InvIndexed sh, Show sh, Shape.Index sh ~ ix, Eq ix, Show ix) =>
   QC.Gen sh -> [(String, QC.Property)]
tests gen =
   ("inBounds", QC.forAll gen inBounds) :
   ("inBoundsOffset", QC.forAll gen inBoundsOffset) :
   ("inBoundsSizeOffset", QC.forAll gen inBoundsSizeOffset) :
   ("sizeOffset", QC.forAll gen sizeOffset) :
   ("uncheckedSizeOffset", QC.forAll gen uncheckedSizeOffset) :
   ("uncheckedOffset", QC.forAll gen uncheckedOffset) :
   ("lengthIndices", QC.forAll gen lengthIndices) :
   ("indexOffsets", QC.forAll gen indexOffsets) :
   ("invIndices", QC.forAll gen invIndices) :
   ("uncheckedInvIndices", QC.forAll gen uncheckedInvIndices) :
   prefix "Checked" (unifiedTests Shape.Checked gen) ++
   prefix "Unchecked" (unifiedTests Shape.Unchecked gen) ++
   []
