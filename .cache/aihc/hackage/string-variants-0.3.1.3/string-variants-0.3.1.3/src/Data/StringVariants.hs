module Data.StringVariants
  ( -- * Non empty text
    NonEmptyText,

    -- ** Constructing
    mkNonEmptyText,
    unsafeMkNonEmptyText,
    mkNonEmptyTextWithTruncate,
    compileNonEmptyText,
    compileNonEmptyTextKnownLength,

    -- ** Converting
    nonEmptyTextToText,
    convertEmptyTextToNothing,
    maybeTextToTruncateNullableNonEmptyText,
    type (<=),
    widen,

    -- * Operations
    takeNonEmptyText,
    takeNonEmptyTextEnd,
    chunksOfNonEmptyText,
    filterNonEmptyText,
    (<>|),

    -- * Conversions between 'Refined' and 'NonEmptyText'.
    ContainsNonWhitespaceCharacters (..),
    exactLengthRefinedToRange,
    nonEmptyTextFromRefined,
    refinedFromNonEmptyText,

    -- * Non-empty, whitespace-trimmed text with no character limit
    Prose,
    mkProse,
    compileProse,
    proseToText,
    proseFromNonEmptyText,

    -- * Nullable non empty text
    NullableNonEmptyText (..),
    mkNullableNonEmptyText,
    nullNonEmptyText,
    compileNullableNonEmptyText,

    -- ** Converting
    nonEmptyTextToNullable,
    maybeNonEmptyTextToNullable,
    nullableNonEmptyTextToMaybeText,
    nullableNonEmptyTextToMaybeNonEmptyText,
    parseNullableNonEmptyText,
    fromMaybeNullableText,

    -- ** Information
    isNullNonEmptyText,
  )
where

import Data.StringVariants.NonEmptyText
import Data.StringVariants.NullableNonEmptyText
import Data.StringVariants.Prose
import Prelude ()
