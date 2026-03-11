{-
Copyright (C) 2007 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file COPYRIGHT

-}

{- |
   Module     : Data.ListLike.String
   Copyright  : Copyright (C) 2007 John Goerzen
   License    : BSD3

   Maintainer : David Fox <dsf@seereason.com>, Andreas Abel
   Stability  : stable
   Portability: portable

String-like functions

Written by John Goerzen, jgoerzen\@complete.org
-}

{-# LANGUAGE FlexibleContexts #-}

module Data.ListLike.String
    ( StringLike(..)
    , fromString
    )
       where
import Prelude hiding (length, head, last, null, tail, map, filter, concat,
                       any, lookup, init, all, foldl, foldr, foldl1, foldr1,
                       maximum, minimum, iterate, span, break, takeWhile,
                       dropWhile, reverse, zip, zipWith, sequence,
                       sequence_, mapM, mapM_, concatMap, and, or, sum,
                       product, repeat, replicate, cycle, take, drop,
                       splitAt, elem, notElem, unzip, lines, words,
                       unlines, unwords)
import qualified Data.List as L
import Data.ListLike.Base
import Data.String
import Data.Text (Text)
import qualified Data.Text.Lazy as Lazy (Text)

{- | An extension to 'ListLike' for those data types that are similar
to a 'String'.  Minimal complete definition is 'toString' and
'fromString'. -}
class IsString s => StringLike s where
    {- | Converts the structure to a 'String' -}
    toString :: s -> String

    {- | Breaks a string into a list of strings -}
    lines :: (ListLike full s) => s -> full
    --lines = map fromString . L.lines . toString
    lines = myLines

    {- | Breaks a string into a list of words -}
    words :: ListLike full s => s -> full
    words = myWords

    {- | Joins lines -}
    unlines :: ListLike full s => full -> s
    unlines = myUnlines

    {- | Joins words -}
    unwords :: ListLike full s => full -> s
    unwords = myUnwords

    {- | Generalize the 'Show' method t return any 'StringLike'. -}
    show :: Show a => a -> s
    show = fromString . Prelude.show

    fromStringLike :: StringLike s' => s -> s'
    fromStringLike = fromString . toString

    {- | Override this to avoid extra 'String' conversions. -}
    fromText :: StringLike Text => Text -> s
    fromText = fromString . toString
    {- | Override this to avoid extra 'String' conversions. -}
    fromLazyText :: StringLike Lazy.Text => Lazy.Text -> s
    fromLazyText = fromString . toString

{-# DEPRECATED fromStringLike "Use fromString . toString or something more efficient using local knowledge" #-}

-- For some reason, Hugs required splitting these out into
-- separate functions.
myLines :: (StringLike s, ListLike full s) => s -> full
myLines = map fromString . L.lines . toString

myWords :: (StringLike s, ListLike full s) => s -> full
myWords = map fromString . L.words . toString

myUnlines :: (StringLike s, ListLike full s) => full -> s
myUnlines = fromString . L.unlines . map toString

myUnwords :: (StringLike s, ListLike full s) => full -> s
myUnwords = fromString . L.unwords . map toString
