--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Partial
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2024 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  H98
--
--  This module provides methods to support operations on partially ordered
--  collections.  The partial ordering relationship is represented by
--  'Maybe' 'Ordering'.
--
--  Thanks to members of the haskell-cafe mailing list -
--    Robert (rvollmert-lists\@gmx.net) and
--    Tom Pledger (Tom.Pledger\@peace.com) -
--  who suggested key ideas on which some of the code in this module is based.
--
--------------------------------------------------------------------------------

-- at present the only user of this module is Swish.RDF.ClassRestrictionRule

module Data.Ord.Partial
    ( PartCompare
      -- * Finding the range of a part-ordered list
    , minima
    , maxima
      -- * Comparing part-ordered containers
    , partCompareEq
    , partComparePair
    , partCompareListMaybe
    , partCompareListSubset
    )
    where

import Data.Foldable (Foldable(..))

-- Avoid messages added in GHC 9.10 about foldl' import from Data.List
-- being redundant.
import Prelude hiding (Foldable(..))

------------------------------------------------------------
--  Type of partial compare function
------------------------------------------------------------

-- | Partial comparison function.
type PartCompare a = a -> a -> Maybe Ordering

------------------------------------------------------------
--  Functions for minima and maxima of a part-ordered list
------------------------------------------------------------

-- |This function finds the maxima in a list of partially
--  ordered values, preserving the sequence of retained
--  values from the supplied list.
--
--  It returns all those values in the supplied list
--  for which there is no larger element in the list.
--
maxima :: PartCompare a -> [a] -> [a]
maxima cmp = foldl' add [] 
    where
        add []     e = [e]
        add ms@(m:mr) e = case cmp m e of
            Nothing -> m : add mr e
            Just GT -> ms
            Just EQ -> ms
            Just LT -> add mr e

-- |This function finds the minima in a list of partially
--  ordered values, preserving the sequence of retained
--  values from the supplied list.
--
--  It returns all those values in the supplied list
--  for which there is no smaller element in the list.
--
minima :: PartCompare a -> [a] -> [a]
minima cmp = maxima (flip cmp)

------------------------------------------------------------
--  Partial ordering comparison functions
------------------------------------------------------------

-- |Partial ordering for Eq values
partCompareEq :: (Eq a) => PartCompare a
partCompareEq a1 a2 = if a1 == a2 then Just EQ else Nothing

-- |Part-ordering comparison on pairs of values,
--  where each has a part-ordering relationship
partComparePair ::
    PartCompare a
    -> PartCompare b
    -> (a,b) 
    -> (a,b)
    -> Maybe Ordering
partComparePair cmpa cmpb (a1,b1) (a2,b2) = case (cmpa a1 a2,cmpb b1 b2) of
    (_,Nothing)       -> Nothing
    (jc1,Just EQ)     -> jc1
    (Nothing,_)       -> Nothing
    (Just EQ,jc2)     -> jc2
    (Just c1,Just c2) -> if c1 == c2 then Just c1 else Nothing

-- |Part-ordering comparison on lists of partially ordered values, where:
--
--  [@as==bs@]  if members of as are all equal to corresponding members of bs
--    
--  [@as<=bs@]  if members of as are all less than or equal to corresponding
--          members of bs
--    
--  [@as>=bs@]  if members of as are all greater than or equal to corresponding
--          members of bs
--    
--  [otherwise] as and bs are unrelated
--
--  The comparison is restricted to the common elements in the two lists.
--
partCompareListPartOrd :: PartCompare a -> [a] -> [a] -> Maybe Ordering
partCompareListPartOrd cmp a1s b1s = pcomp a1s b1s EQ
    where
        pcomp (a:as) (b:bs) ordp = case cmp a b of
            Just rel  -> pcomp1 as bs rel ordp
            _         -> Nothing
        pcomp _      _      ordp = Just ordp
        -- pcomp []     []     ordp = Just ordp
        
        pcomp1 as bs ordn EQ   = pcomp as bs ordn
        pcomp1 as bs EQ   ordp = pcomp as bs ordp
        pcomp1 as bs ordn ordp =
            if ordn == ordp then pcomp as bs ordp else Nothing
                                                       
-- |Part-ordering comparison for Maybe values.
partCompareMaybe :: (Eq a) => Maybe a -> Maybe a -> Maybe Ordering
partCompareMaybe Nothing  Nothing  = Just EQ
partCompareMaybe (Just _) Nothing  = Just GT
partCompareMaybe Nothing  (Just _) = Just LT
partCompareMaybe (Just a) (Just b) = if a == b then Just EQ else Nothing

-- |Part-ordering comparison on lists of Maybe values.
partCompareListMaybe :: (Eq a) => [Maybe a] -> [Maybe a] -> Maybe Ordering
partCompareListMaybe = partCompareListPartOrd partCompareMaybe

-- |Part-ordering comparison on lists based on subset relationship
partCompareListSubset :: (Eq a) => [a] -> [a] -> Maybe Ordering
partCompareListSubset a b
    | aeqvb     = Just EQ
    | asubb     = Just LT
    | bsuba     = Just GT
    | otherwise = Nothing
    where
        asubb = a `subset` b
        bsuba = b `subset` a
        aeqvb = asubb && bsuba
        x `subset` y = and [ ma `elem` y | ma <- x ]

------------------------------------------------------------
--  Test cases
------------------------------------------------------------

{-

notTrueFalse  = Nothing :: Maybe Bool

-- partCompareListOrd
test01 = partCompareListOrd [1,2,3] [1,2,3] == Just EQ
test02 = partCompareListOrd [1,2,3] [2,3,4] == Just LT
test03 = partCompareListOrd [1,2,4] [1,2,3] == Just GT
test04 = partCompareListOrd [1,2,3] [2,1,3] == Nothing

-- partCompareMaybe
test11 = partCompareMaybe (Just True)  (Just True)  == Just EQ
test12 = partCompareMaybe (Just True)  (Just False) == Nothing
test13 = partCompareMaybe notTrueFalse (Just False) == Just LT
test14 = partCompareMaybe (Just True)  notTrueFalse == Just GT
test15 = partCompareMaybe notTrueFalse notTrueFalse == Just EQ

-- partCompareListMaybe
test21 = partCompareListMaybe [Just True,Just False]
                              [Just True,Just False]
            == Just EQ
test22 = partCompareListMaybe [Just True,Just False]
                              [Just True,Just True]
            == Nothing
test23 = partCompareListMaybe [Just False,Just True]
                              [Just False,Just True]
            == Just EQ
test24 = partCompareListMaybe [Nothing,   Just True]
                              [Just False,Just True]
            == Just LT
test25 = partCompareListMaybe [Just False,Just True]
                              [Just False,Nothing]
            == Just GT
test26 = partCompareListMaybe [Nothing,   Just True]
                              [Just False,Nothing]
            == Nothing
test27 = partCompareListMaybe [Nothing,Just True]
                              [Nothing,Nothing]
            == Just GT
test28 = partCompareListMaybe [notTrueFalse,notTrueFalse]
                              [notTrueFalse,notTrueFalse]
            == Just EQ

--  minima, maxima
test31a = maxima partCompareListMaybe ds1a == ds1b
test31b = minima partCompareListMaybe ds1a == ds1c
ds1a =
    [ [Just 'a',Just 'b',Just 'c']
    , [Just 'a',Just 'b',Nothing ]
    , [Just 'a',Nothing ,Just 'c']
    , [Just 'a',Nothing ,Nothing ]
    , [Nothing ,Just 'b',Just 'c']
    , [Nothing ,Just 'b',Nothing ]
    , [Nothing ,Nothing ,Just 'c']
    , [Nothing ,Nothing ,Nothing ]
    ]
ds1b =
    [ [Just 'a',Just 'b',Just 'c']
    ]
ds1c =
    [ [Nothing ,Nothing ,Nothing ]
    ]

test32a = maxima partCompareListMaybe ds2a == ds2b
test32b = minima partCompareListMaybe ds2a == ds2c
ds2a =
    [ [Just 'a',Just 'b',Nothing ]
    , [Just 'a',Nothing ,Just 'c']
    , [Just 'a',Nothing ,Nothing ]
    , [Nothing ,Just 'b',Just 'c']
    , [Nothing ,Just 'b',Nothing ]
    , [Nothing ,Nothing ,Just 'c']
    ]
ds2b =
    [ [Just 'a',Just 'b',Nothing ]
    , [Just 'a',Nothing ,Just 'c']
    , [Nothing ,Just 'b',Just 'c']
    ]
ds2c =
    [ [Just 'a',Nothing ,Nothing ]
    , [Nothing ,Just 'b',Nothing ]
    , [Nothing ,Nothing ,Just 'c']
    ]

test33a = maxima partCompareListMaybe ds3a == ds3b
test33b = minima partCompareListMaybe ds3a == ds3c
ds3a =
    [ [Just "a1",Just "b1",Just "c1"]
    , [Just "a2",Just "b2",Nothing  ]
    , [Just "a3",Nothing  ,Just "c3"]
    , [Just "a4",Nothing  ,Nothing  ]
    , [Nothing  ,Just "b5",Just "c5"]
    , [Nothing  ,Just "b6",Nothing  ]
    , [Nothing  ,Nothing  ,Just "c7"]
    ]
ds3b =
    [ [Just "a1",Just "b1",Just "c1"]
    , [Just "a2",Just "b2",Nothing  ]
    , [Just "a3",Nothing  ,Just "c3"]
    , [Just "a4",Nothing  ,Nothing  ]
    , [Nothing  ,Just "b5",Just "c5"]
    , [Nothing  ,Just "b6",Nothing  ]
    , [Nothing  ,Nothing  ,Just "c7"]
    ]
ds3c =
    [ [Just "a1",Just "b1",Just "c1"]
    , [Just "a2",Just "b2",Nothing  ]
    , [Just "a3",Nothing  ,Just "c3"]
    , [Just "a4",Nothing  ,Nothing  ]
    , [Nothing  ,Just "b5",Just "c5"]
    , [Nothing  ,Just "b6",Nothing  ]
    , [Nothing  ,Nothing  ,Just "c7"]
    ]


test34a = maxima partCompareListMaybe ds4a == ds4b
test34b = minima partCompareListMaybe ds4a == ds4c
ds4a =
    [ [Just 1, Just 1 ]
    , [Just 2, Nothing]
    , [Nothing,Just 3 ]
    , [Nothing,Nothing]
    ]
ds4b =
    [ [Just 1, Just 1 ]
    , [Just 2, Nothing]
    , [Nothing,Just 3 ]
    ]
ds4c =
    [ [Nothing,Nothing]
    ]

-- Check handling of equal values
test35a = maxima partCompareListMaybe ds5a == ds5b
test35b = minima partCompareListMaybe ds5a == ds5c
ds5a =
    [ [Just 1, Just 1 ]
    , [Just 2, Nothing]
    , [Nothing,Just 3 ]
    , [Nothing,Nothing]
    , [Just 1, Just 1 ]
    , [Just 2, Nothing]
    , [Nothing,Just 3 ]
    , [Nothing,Nothing]
    ]
ds5b =
    [ [Just 1, Just 1 ]
    , [Just 2, Nothing]
    , [Nothing,Just 3 ]
    ]
ds5c =
    [ [Nothing,Nothing]
    ]

-- test case 32 with different ordering of values
test36a = maxima partCompareListMaybe ds6a == ds6b
test36b = minima partCompareListMaybe ds6a == ds6c
ds6a =
    [ [Just 'a',Just 'b',Nothing ]
    , [Nothing ,Nothing ,Just 'c']
    , [Nothing ,Just 'b',Nothing ]
    , [Nothing ,Just 'b',Just 'c']
    , [Just 'a',Nothing ,Nothing ]
    , [Just 'a',Nothing ,Just 'c']
    ]
ds6b =
    [ [Just 'a',Just 'b',Nothing ]
    , [Nothing ,Just 'b',Just 'c']
    , [Just 'a',Nothing ,Just 'c']
    ]
ds6c =
    [ [Nothing ,Nothing ,Just 'c']
    , [Nothing ,Just 'b',Nothing ]
    , [Just 'a',Nothing ,Nothing ]
    ]

test = and
    [ test01, test02, test03, test04
    , test11, test12, test13, test14, test15
    , test21, test22, test23, test24, test25, test26, test27, test28
    , test31a, test31b, test32a, test32b, test33a, test33b
    , test34a, test34b, test35a, test35b, test36a, test36b
    ]

-}

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012 Douglas Burke
--  All rights reserved.
--
--  This file is part of Swish.
--
--  Swish is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Swish is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Swish; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--------------------------------------------------------------------------------
