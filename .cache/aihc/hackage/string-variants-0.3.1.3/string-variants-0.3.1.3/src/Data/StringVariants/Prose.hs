-- | Prose type: arbitrary length non-empty text that is trimmed.
module Data.StringVariants.Prose
  ( Prose,
    mkProse,
    compileProse,
    IsProse,
    literalProse,
    proseToText,
    proseFromNonEmptyText
  )
where

import Data.StringVariants.Prose.Internal
import Prelude ()
