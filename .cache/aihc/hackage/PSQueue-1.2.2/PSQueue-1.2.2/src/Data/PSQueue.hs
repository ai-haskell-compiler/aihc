{- |

A /priority search queue/ (henceforth /queue/) efficiently supports the
operations of both a search tree and a priority queue. A 'Binding' is a
product of a key and a priority. Bindings can be inserted, deleted, modified
and queried in logarithmic time, and the binding with the least priority can be
retrieved in constant time. A queue can be built from a list of bindings,
sorted by keys, in linear time.

This implementation is due to Ralf Hinze.

* [Hinze, R., A Simple Implementation Technique for Priority Search Queues, ICFP 2001, pp. 110-121](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.18.1149)

-}

-- Some modifications by Scott Dillard


module Data.PSQueue
    (
    -- * Binding Type
    Binding((:->))
    , key
    , prio
    -- * Priority Search Queue Type
    , PSQ
    -- * Query
    , size
    , null
    , lookup
    -- * Construction
    , empty
    , singleton
    -- * Insertion
    , insert
    , insertWith
    -- * Delete/Update
    , delete
    , adjust
    , adjustWithKey
    , update
    , updateWithKey
    , alter
    -- * Conversion
    , keys
    , toList
    , toAscList
    , toDescList
    , fromList
    , fromAscList
    , fromDistinctAscList
    -- * Priority Queue
    , findMin
    , deleteMin
    , minView
    , atMost
    , atMostRange
    -- * Fold
    , foldr
    , foldl
) where

import Prelude ()
import Data.PSQueue.Internal
