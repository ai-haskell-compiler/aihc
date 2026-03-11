{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  CLI.Arguments.Arr
-- Copyright   :  (c) OleksandrZhabenko 2021-2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A library to process command line arguments in some more convenient way.

module CLI.Arguments.Arr where

import GHC.Base
import Data.Tuple
import GHC.Arr
import GHC.List (length)
import GHC.Num ((-))
import CLI.Arguments
import CLI.Arguments.Parsing
import CLI.Arguments.Sorted

-- | For empty list of 'String's returns empty array that has no elements. Trying to index it always returns error and can cause
-- segmentation fault in the running program or interpreter (GHCi).
takeABCsArr
  :: (CLSpecifications -> [String] -> Args) -- ^ A function to collect the 'Args'
  -> CLSpecifications
  -> [String]
  -> Array Int Arguments
takeABCsArr f ts xss = listArray (0,l-1) js
     where js = f ts xss
           l = length js
{-# INLINABLE takeABCsArr #-}

-- | For empty list of 'String's returns empty array that has no elements. Trying to index it always returns error and can cause
-- segmentation fault in the running program or interpreter (GHCi).
takeCsArr
  :: CLSpecifications
  -> [String]
  -> Array Int Arguments
takeCsArr = takeABCsArr (\us zss -> fst . takeCsR us $ zss)
{-# INLINABLE takeCsArr #-}

-- | For empty list of 'String's returns empty array that has no elements. Trying to index it always returns error and can cause
-- segmentation fault in the running program or interpreter (GHCi).
takeCs1Arr
  :: FirstChars -- ^ A pair of the first characters of the starting group delimiter (the same for all 'String's in the all 'CLSpecifications') and the probable its modification (the first character of the last delimiter).
  -> CLSpecifications
  -> [String]
  -> Array Int Arguments
takeCs1Arr (x1,x2) = takeABCsArr (\us zss -> fst . takeCs1R (x1,x2) us $ zss)
{-# INLINABLE takeCs1Arr #-}

-- | For empty list of 'String's returns empty array that has no elements. Trying to index it always returns error and can cause
-- segmentation fault in the running program or interpreter (GHCi).
takeBsArr
  :: CLSpecifications
  -> [String]
  -> Array Int Arguments
takeBsArr = takeABCsArr (\us zss -> fst . takeBsR us $ zss)
{-# INLINABLE takeBsArr #-}

-- | For empty list of 'String's returns empty array that has no elements. Trying to index it always returns error and can cause
-- segmentation fault in the running program or interpreter (GHCi).
takeAsArr
  :: CLSpecifications
  -> [String]
  -> Array Int Arguments
takeAsArr  = takeABCsArr (\us zss -> fst . takeAsR us $ zss)
{-# INLINABLE takeAsArr #-}

---------------------------------------------------

-- | For empty list of 'String's returns empty array that has no elements. Trying to index it always returns error and can cause
-- segmentation fault in the running program or interpreter (GHCi).
takeABCsArrSortedBy
  :: ((Arguments -> Arguments -> Ordering) -> CLSpecifications -> [String] -> Args)
  -> (Arguments -> Arguments -> Ordering) -- ^ A 'compare'-like implementation for 'Arguments'. If needed you can implement your own 'Ord' instance for 'Arguments' and use it here. Here can be partial, just for 'C's.
  -> CLSpecifications
  -> [String]
  -> Array Int Arguments
takeABCsArrSortedBy g f ts xss = listArray (0,l-1) js
     where js = g f  ts xss
           l = length js
{-# INLINABLE takeABCsArrSortedBy #-}

-- | For empty list of 'String's returns empty array that has no elements. Trying to index it always returns error and can cause
-- segmentation fault in the running program or interpreter (GHCi).
takeCsArrSortedBy
  :: (Arguments -> Arguments -> Ordering) -- ^ A 'compare'-like implementation for 'Arguments'. If needed you can implement your own 'Ord' instance for 'Arguments' and use it here. Here can be partial, just for 'C's.
  -> CLSpecifications
  -> [String]
  -> Array Int Arguments
takeCsArrSortedBy = takeABCsArrSortedBy (takeArgsSortedBy (\x -> notNullArguments x && isC x))
{-# INLINABLE takeCsArrSortedBy #-}

-- | For empty list of 'String's returns empty array that has no elements. Trying to index it always returns error and can cause
-- segmentation fault in the running program or interpreter (GHCi).
takeCs1ArrSortedBy
  :: FirstChars -- ^ A pair of the first characters of the starting group delimiter (the same for all 'String's in the all 'CLSpecifications') and the probable its modification being also the first character.
  -> (Arguments -> Arguments -> Ordering) -- ^ A 'compare'-like implementation for 'Arguments'. If needed you can implement your own 'Ord' instance for 'Arguments' and use it here. Here can be partial, just for 'C's.
  -> CLSpecifications
  -> [String]
  -> Array Int Arguments
takeCs1ArrSortedBy (x1,x2) = takeABCsArrSortedBy (takeArgs1SortedBy (x1,x2) (\x -> notNullArguments x && isC x))
{-# INLINABLE takeCs1ArrSortedBy #-}

-- | For empty list of 'String's returns empty array that has no elements. Trying to index it always returns error and can cause
-- segmentation fault in the running program or interpreter (GHCi).
takeBsArrSortedBy
  :: (Arguments -> Arguments -> Ordering) -- ^ A 'compare'-like implementation for 'Arguments'. If needed you can implement your own 'Ord' instance for 'Arguments' and use it here. Here can be partial, just for 'B's.
  -> CLSpecifications
  -> [String]
  -> Array Int Arguments
takeBsArrSortedBy = takeABCsArrSortedBy (takeArgsSortedBy (\x -> notNullArguments x && isB x))
{-# INLINABLE takeBsArrSortedBy #-}

-- | For empty list of 'String's returns empty array that has no elements. Trying to index it always returns error and can cause
-- segmentation fault in the running program or interpreter (GHCi).
takeAsArrSortedBy
  :: (Arguments -> Arguments -> Ordering) -- ^ A 'compare'-like implementation for 'Arguments'. If needed you can implement your own 'Ord' instance for 'Arguments' and use it here. Here can be partial, just for 'A's.
  -> CLSpecifications
  -> [String]
  -> Array Int Arguments
takeAsArrSortedBy = takeABCsArrSortedBy (takeArgsSortedBy (\x -> notNullArguments x && isA x))
{-# INLINABLE takeAsArrSortedBy #-}


