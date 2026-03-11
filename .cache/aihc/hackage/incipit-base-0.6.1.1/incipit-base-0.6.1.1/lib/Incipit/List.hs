-- |Variants of the uncons and unsnoc combinators for lists that return 'Maybe'.
module Incipit.List where

import qualified Data.List as List
import Data.Maybe (Maybe (..))

-- |Return 'Just' the head of a list.
head :: [a] -> Maybe a
head = \case
  [] -> Nothing
  a : _ -> Just a
{-# inline head #-}

-- |Return 'Just' the last element of a list.
last :: [a] -> Maybe a
last = \case
  [] -> Nothing
  as -> Just (List.last as)
{-# inline last #-}

-- |Return 'Just' the non-last elements of a list.
init :: [a] -> Maybe [a]
init = \case
  [] -> Nothing
  as -> Just (List.init as)
{-# inline init #-}

-- |Return 'Just' the non-head elements of a list.
tail :: [a] -> Maybe [a]
tail = \case
  [] -> Nothing
  as -> Just (List.tail as)
{-# inline tail #-}
