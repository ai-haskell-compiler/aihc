{-# LANGUAGE FlexibleContexts #-}


{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.ListLike.Utils
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : BSD3

   Maintainer : David Fox <dsf@seereason.com>, Andreas Abel
   Stability  : stable
   Portability: portable

Utilities for 'Data.ListLike.ListLike' and friends.  More functions
similar to 'Data.List' but not part of the main typeclass.

Written by John Goerzen, jgoerzen\@complete.org
-}

module Data.ListLike.Utils
    (and, or, sum, product, zip, zipWith, unzip, sequence_, toMonadPlus, list,
     intercalate
    ) where
import Prelude hiding (length, head, last, null, tail, map, filter, concat,
                       any, lookup, init, all, foldl, foldr, foldl1, foldr1,
                       maximum, minimum, iterate, span, break, takeWhile,
                       dropWhile, reverse, zip, zipWith, sequence,
                       sequence_, mapM, mapM_, concatMap, and, or, sum,
                       product, repeat, replicate, cycle, take, drop,
                       splitAt, elem, notElem, unzip, lines, words,
                       unlines, unwords, foldMap)
import Control.Monad (MonadPlus(..))
import Data.ListLike.Base
import Data.ListLike.FoldableLL
import Data.Monoid

-- | Returns True if all elements are True
and :: ListLike full Bool => full -> Bool
and = all (== True)

-- | Returns True if any element is True
or :: ListLike full Bool => full -> Bool
or = any (== True)

-- | The sum of the list
sum :: (Num a, ListLike full a) => full -> a
sum = getSum . foldMap Sum

-- | The product of the list
product :: (Num a, ListLike full a) => full -> a
product = getProduct . foldMap Product

------------------------------ Zipping
-- zip, zipWith  in Base
{- | Converts a list of pairs into two separate lists of elements -}
unzip :: (ListLike full (itema, itemb),
          ListLike ra itema,
          ListLike rb itemb) => full -> (ra, rb)
unzip inp = foldr convert (empty, empty) inp
    where convert (a, b) (as, bs) = ((cons a as), (cons b bs))

-- | Converts to a MonadPlus instance
toMonadPlus :: (MonadPlus m, ListLike full a) => full -> m (a, full)
toMonadPlus = maybe mzero return . uncons

-- | List-like destructor (like Data.Maybe.maybe)
list :: ListLike full a => b -> (a -> full -> b) -> full -> b
list d f = maybe d (uncurry f) . toMonadPlus

-- | 'intercalate' @xs xss@ is equivalent to @('concat' ('intersperse'
-- xs xss))@.  It inserts the list @xs@ in between the lists in @xss@
-- and concatenates the result.
intercalate :: (ListLike a item, ListLike b a)
            => a -> b -> a
intercalate x = concat . intersperse x
