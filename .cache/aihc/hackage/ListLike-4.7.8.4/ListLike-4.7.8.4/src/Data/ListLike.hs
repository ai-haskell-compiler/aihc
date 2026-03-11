{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.ListLike
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : BSD3

   Maintainer : David Fox <dsf@seereason.com>, Andreas Abel
   Stability  : stable
   Portability: portable

Generic operations over list-like structures

Written by John Goerzen, jgoerzen\@complete.org

Please start with the introduction at "Data.ListLike#intro".
-}

module Data.ListLike
                (-- * Introduction
                 -- $intro

                 -- * Creation & Basic Functions
                 empty, singleton,
                 cons, snoc, append, uncons, head, last, tail, init, null, length,
                 -- * List transformations
                 map, rigidMap, reverse, intersperse,
                 -- ** Conversions
                 toList, fromList, fromListLike,
                 -- * Reducing lists (folds), from "FoldableLL"
                 foldl, foldl', foldl1, foldr, foldr', foldr1,
                 -- ** Special folds
                 concat, concatMap, rigidConcatMap,
                 and, or,
                 any, all,
                 sum, product,
                 maximum, minimum,
                 fold, foldMap,
                 -- * Building lists
                 -- ** Scans
                 -- ** Accumulating maps
                 -- ** Infinite lists
                 iterate, repeat, replicate, cycle,
                 -- ** Unfolding
                 -- * Sublists
                 -- ** Extracting sublists
                 take, drop, splitAt, takeWhile, dropWhile, dropWhileEnd, span, break,
                 group, inits, tails,
                 -- ** Predicates
                 isPrefixOf, isSuffixOf, isInfixOf,
                 -- ** Modify based on predicate
                 stripPrefix, stripSuffix,
                 -- * Searching lists
                 -- ** Searching by equality
                 elem, notElem,
                 -- ** Searching with a predicate
                 find, filter, partition,
                 -- * Indexing lists
                 index, elemIndex, elemIndices, findIndex, findIndices,
                 -- * Zipping and unzipping lists
                 zip, zipWith, unzip,
                 -- * Monadic Operations
                 sequence, sequence_, mapM, rigidMapM, mapM_,
                 -- * Input and Output
                 ListLikeIO(..),
                 -- * Special lists
                 -- ** Strings
                 toString, fromString, lines, words, show,
                 fromStringLike, fromText, fromLazyText,
                 -- ** \"Set\" operations
                 nub, delete, deleteFirsts, union, intersect,
                 -- ** Ordered lists
                 sort, insert,
                 -- * Generalized functions
                 -- ** The \"By\" operations
                 -- *** User-supplied equality (replacing an Eq context)
                 nubBy, deleteBy, deleteFirstsBy, unionBy, intersectBy,
                 groupBy,
                 -- *** User-supplied comparison (replacing an Ord context)
                 sortBy, insertBy, -- maximumBy, minimumBy,
                 -- ** The \"generic\" operations
                 genericLength, genericTake, genericDrop, genericSplitAt,
                 -- genericIndex,
                 genericReplicate,
                 -- * Notes on specific instances
                 -- ** Lists
                 -- $noteslist

                 -- ** Arrays
                 -- $notesarray

                 -- ** ByteStrings
                 -- $notesbytestring
                 Chars(..),
                 CharString (..),
                 CharStringLazy (..),

                 -- * Base Typeclasses
                 -- ** The ListLike class
                 ListLike, ListOps,
                 -- ** The FoldableLL class
                 FoldableLL,
                 -- ** The StringLike class
                 StringLike,
                 -- ** The InfiniteListLike class
                 InfiniteListLike
                )
       where
import Prelude hiding (Foldable(..), head, last, tail, map, filter, concat,
                       any, lookup, init, all,
                       iterate, span, break, takeWhile,
                       dropWhile, {-dropWhileEnd,-} reverse, zip, zipWith, sequence,
                       sequence_, mapM, mapM_, concatMap, and, or,
                       repeat, replicate, cycle, take, drop,
                       splitAt, notElem, unzip, lines, words,
                       unlines, unwords, show)
import Data.ListLike.Base
import Data.ListLike.Chars
import Data.ListLike.CharString
import Data.ListLike.FoldableLL
import Data.ListLike.Instances ()
import Data.ListLike.DList ()
import Data.ListLike.FMList ()
import Data.ListLike.String
import Data.ListLike.Utils
import Data.ListLike.IO

{- $intro
 #intro#
Welcome to ListLike.

This module provides abstractions over typical list operations.
It is designed to let you freely interchange different ways to represent
sequences of data.  It works with lists, various types of ByteStrings,
and much more.

In this module, you'll find generic versions of most of the functions
you're used to using in the "Prelude", "Data.List", and "System.IO".
They carry the
same names, too.  Therefore, you'll want to be careful how you import
the module.  I suggest using:

>import qualified Data.ListLike as LL

Then, you can use LL.fold, LL.map, etc. to get the generic version of
the functions you want.  Alternatively, you can hide the other versions
from Prelude and import specific generic functions from here, such as:

>import Prelude hiding (map)
>import Data.ListLike (map)

The module "Data.ListLike" actually simply re-exports the items found
in a number of its sub-modules.  If you want a smaller subset of
"Data.ListLike", look at the documentation for its sub-modules and import
the relevant one.

In most cases, functions here can act as drop-in replacements for their
list-specific counterparts.  They will use the same underlying implementations
for lists, so there should be no performance difference.

You can make your own types instances of 'ListLike' as well.  For more
details, see the notes for the 'ListLike' typeclass.
-}

{- $noteslist

Functions for operating on regular lists almost all use the native
implementations in "Data.List", "Prelude", or similar standard
modules.  The exceptions are:

* 'mapM' uses the default 'ListLike' implementation

* 'hGet' does not exist for 'String' in the Haskell modules.
  It is implemented in terms of "Data.ByteString.Lazy".

* 'hGetNonBlocking' is the same way. -}

{- $notesarray

'Data.Array.Array' is an instance of 'ListLike'.  Here are some notes about it:

* The index you use must be an integral

* 'ListLike' functions that take an index always take a 0-based index
  for compatibility with other 'ListLike' instances.
  This is translated by the instance functions into the proper offset from
  the bounds in the Array.

* 'ListLike' functions preserve the original Array index numbers when
  possible.  Functions such as 'cons' will reduce the lower bound to do
  their job.  'snoc' and 'append' increase the upper bound.  'drop' raises
  the lower bound and 'take' lowers the upper bound.

* Functions that change the length of the array by an amount not known
  in advance, such as 'filter', will generate a new array with the lower
  bound set to 0.  Furthermore, these functions cannot operate on infinite
  lists because they must know their length in order to generate the
  array.  'hGetContents' and its friends will therefore require the
  entire file to be read into memory before processing is possible.

* 'empty', 'singleton', and 'fromList' also generate an array with the
  lower bound set to 0.

* Many of these functions will generate runtime exceptions if you have
  not assigned a value to every slot in the array.
-}

{- $notesbytestring

Both strict and lazy ByteStreams can be used with 'ListLike'.

ByteString ListLike instances operate on 'Word8' elements.  This is because
both Data.ByteString.ByteString and Data.ByteString.Char8.ByteString have
the same underlying type.  If you wish to use the Char8 representation,
the newtype wrappers 'CharString' and 'CharStringLazy' are available.

Most 'ListLike' operations map directly to ByteStream options.  Notable
exceptions:

* 'map' uses the 'ListLike' implementation.  'rigidMap' is more efficient.
  The same goes for 'concatMap' vs. 'rigidConcatMap'.

* 'isInfixOf', 'sequence', 'mapM' and similar monad operations, 'insert',
  'union', 'intersect', 'sortBy', and similar functions are not implemented
  in 'ByteStream' and use a naive default implementation.

* The lazy ByteStream module implements fewer funtions than the strict
  ByteStream module.  In some cases, default implementations are used.
  In others, notably related to I\/O, the lazy ByteStreams are converted
  back and forth to strict ones as appropriate.

-}
