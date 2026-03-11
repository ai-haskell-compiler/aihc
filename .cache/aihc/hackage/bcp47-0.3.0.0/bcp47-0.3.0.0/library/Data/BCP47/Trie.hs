-- | A trie like data structure for defining maps from 'BCP47' tags to values.
--
-- This structure supports collection and lookup of language tagged values. Its
-- semantics are based on those defined in the BCP 47 specification.
module Data.BCP47.Trie
  ( Trie
  , fromList
  , fromNonEmpty
  , singleton
  , lookup
  , match
  , elem
  , union
  , unionWith
  , mapMaybe
  )
where

import Prelude hiding (elem, lookup, null)

import Data.BCP47
import Data.BCP47.Trie.Internal
import qualified Data.Map as Map
import Data.Maybe (isJust)

-- | Lookup the most relevant item for a tag
--
-- "Lookup is used to select the single language tag that best matches the
-- language priority list for a given request...For example, if the language
-- range is 'de-ch', a lookup operation can produce content with the tags 'de'
-- or 'de-CH' but never content with the tag 'de-CH-1996'."
--
-- https://tools.ietf.org/html/bcp47#page-2-12
lookup :: BCP47 -> Trie a -> Maybe a
lookup tag trie = lookup2 tag =<< Map.lookup (language tag) (unLanguage trie)

-- | Lookup an exact match for a tag
match :: BCP47 -> Trie a -> Maybe a
match tag trie = match2 tag =<< Map.lookup (language tag) (unLanguage trie)

-- | Check if a tag exists in the 'Trie'
elem :: BCP47 -> Trie a -> Bool
elem tag = isJust . match tag
