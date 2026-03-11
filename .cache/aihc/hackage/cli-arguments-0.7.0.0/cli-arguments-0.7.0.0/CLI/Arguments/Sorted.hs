{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  CLI.Arguments.Sorted
-- Copyright   :  (c) OleksandrZhabenko 2021-2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A library to process command line arguments in some more convenient way.

module CLI.Arguments.Sorted where

import GHC.Base
import Data.Tuple (fst)
import Data.Monoid (mappend)
import Data.Maybe (fromJust)
import Data.List (sortBy)
import CLI.Arguments
import CLI.Arguments.Parsing

takeArgsSortedBy
  :: (Arguments -> Bool) -- ^ A predicate to check which 'Arguments' must be kept in the result.
  -> (Arguments -> Arguments -> Ordering) -- ^ A 'compare'-like implementation for 'Arguments'. If needed you can implement your own 'Ord' instance for 'Arguments' and use it here. Here it can be partial, just for 'C's.
  -> CLSpecifications
  -> [String]
  -> Args
takeArgsSortedBy g f ts yss = sortBy f . fst . args2ArgsFilteredGR g ts $ ([],yss)
{-# INLINABLE takeArgsSortedBy #-}

takeArgs1SortedBy
  :: FirstChars -- ^ A pair of the first characters of the starting group delimiter (the same for all 'String's in the all 'CLSpecifications') and the probable its modification (the first character of the last delimiter).
  -> (Arguments -> Bool) -- ^ A predicate to check which 'Arguments' must be kept in the result.
  -> (Arguments -> Arguments -> Ordering) -- ^ A 'compare'-like implementation for 'Arguments'. If needed you can implement your own 'Ord' instance for 'Arguments' and use it here. Here it can be partial, just for 'C's.
  -> CLSpecifications
  -> [String]
  -> Args
takeArgs1SortedBy (x1,x2) g f ts yss = sortBy f . fst . args2ArgsFilteredG1R (x1,x2) g ts $ ([],yss)
{-# INLINABLE takeArgs1SortedBy #-}

takeCsSortedBy
  :: (Arguments -> Arguments -> Ordering) -- ^ A 'compare'-like implementation for 'Arguments'. If needed you can implement your own 'Ord' instance for 'Arguments' and use it here. Here it can be partial, just for 'C's.
  -> CLSpecifications
  -> [String]
  -> Args
takeCsSortedBy = takeArgsSortedBy (\x -> notNullArguments x && isC x)
{-# INLINABLE takeCsSortedBy #-}

takeCs1SortedBy
  :: FirstChars -- ^ A pair of the first characters of the starting group delimiter (the same for all 'String's in the all 'CLSpecifications') and the probable its modification (the first character of the last delimiter).
  -> (Arguments -> Arguments -> Ordering) -- ^ A 'compare'-like implementation for 'Arguments'. If needed you can implement your own 'Ord' instance for 'Arguments' and use it here. Here it can be partial, just for 'C's.
  -> CLSpecifications
  -> [String]
  -> Args
takeCs1SortedBy (x1,x2) = takeArgs1SortedBy (x1,x2) (\x -> notNullArguments x && isC x)
{-# INLINABLE takeCs1SortedBy #-}

takeBsSortedBy
  :: (Arguments -> Arguments -> Ordering) -- ^ A 'compare'-like implementation for 'Arguments'. If needed you can implement your own 'Ord' instance for 'Arguments' and use it here. Here it can be partial, just for 'B's.
  -> CLSpecifications
  -> [String]
  -> Args
takeBsSortedBy = takeArgsSortedBy (\x -> notNullArguments x && isB x)
{-# INLINABLE takeBsSortedBy #-}

takeAsSortedBy
  :: (Arguments -> Arguments -> Ordering) -- ^ A 'compare'-like implementation for 'Arguments'. If needed you can implement your own 'Ord' instance for 'Arguments' and use it here. Here it can be partial, just for 'A's.
  -> CLSpecifications
  -> [String]
  -> Args
takeAsSortedBy = takeArgsSortedBy (\x -> notNullArguments x && isA x)
{-# INLINABLE takeAsSortedBy #-}
