{- |
A simple purely functional circular list, or ring, data type.

Lets describe what we mean by 'ring'. A ring is a circular data structure
such that if you continue rotating the ring, you'll eventually return to
the element you first observed.

All of our analogies involve sitting at a table who's top surface rotates
about its center axis (think of those convenient rotating platforms one
often finds in an (Americanized) Chinese Restaurant).

Only the closest item on the table is avialable to us. In order to reach
other elements on the table, we need to rotate the table to the left or
the right.

Our convention for this problem says that rotations to the right are a
forward motion while rotations to the left are backward motions.

We'll use the following circular list for our examples:

>   8 7 6
>  9     5
> A       4
> B       3
>  C     2
>   D 0 1
>     ^

The pointer at the bottom represents our position at the table. The element
currently in front of is is referred to as the `focus`. So, in this case,
our focus is 0.

If we were to rotate the table to the right using the `rotR` operation, we'd
have the following table.

>   9 8 7
>  A     6
> B       5
> C       4
>  D     3
>   0 1 2
>     ^

This yields 1 as our new focus. Rotating this table left would return 0 to
the focus position.

-}
module Data.CircularList (
    -- * Data Types
    CList,
    -- * Functions
    -- ** Creation of CLists
    empty, fromList, singleton,
    -- ** Update of CList
    update, reverseDirection,
    -- ** Converting CLists to Lists
    leftElements, rightElements, toList, toInfList,
    -- ** Extraction and Accumulation
    focus, insertL, insertR,
    removeL, removeR,
    -- ** Manipulation of Focus
    allRotations, rotR, rotL, rotN, rotNR, rotNL,
    rotateTo, findRotateTo,
    -- ** List-like functions
    filterR, filterL, foldrR, foldrL, foldlR, foldlL,
    -- ** Manipulation of Packing
    balance, packL, packR,
    -- ** Information
    isEmpty, size,
) where

import Data.CircularList.Internal
