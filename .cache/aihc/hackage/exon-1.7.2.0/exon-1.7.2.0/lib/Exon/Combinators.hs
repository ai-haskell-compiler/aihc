-- | Description: Internal
module Exon.Combinators where

import qualified Data.List.NonEmpty as NonEmpty
import Prelude hiding (intersperse)

import Exon.Class.Exon (Exon (exonProcess))
import qualified Exon.Data.Segment as Segment

-- | Combine the elements in the list using 'Exon', interspersing the separator between each pair of elements.
intercalate1 ::
  Exon a =>
  a ->
  NonEmpty a ->
  a
intercalate1 sep ta =
  exonProcess (Segment.Expression <$> NonEmpty.intersperse sep ta)

-- | Combine the elements in the list using 'Exon', interspersing the separator between each pair of elements.
--
-- Returns 'Nothing' when the list is empty.
intercalateMay ::
  Exon a =>
  Foldable t =>
  a ->
  t a ->
  Maybe a
intercalateMay sep ta =
  intercalate1 sep <$> nonEmpty (toList ta)

-- | Combine the elements in the list using 'Exon', interspersing the separator between each pair of elements.
--
-- Returns 'mempty' for empty lists; use 'intercalate1' for 'NonEmpty' or 'intercalateMay' to avoid the 'Monoid'
-- requirement.
intercalate ::
  Exon a =>
  Monoid a =>
  Foldable t =>
  a ->
  t a ->
  a
intercalate sep ta =
  fromMaybe mempty (intercalateMay sep ta)
