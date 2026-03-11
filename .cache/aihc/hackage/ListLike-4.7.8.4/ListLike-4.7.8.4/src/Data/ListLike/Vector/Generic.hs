{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}  -- for GHC >= 9.4

{-# OPTIONS -Wno-orphans #-}

-- | ListLike instance for any type supporting the @Data.Vector.Generic@
-- interface.  To avoid collisions with other Vector instances, this module
-- must be imported directly.
module Data.ListLike.Vector.Generic ()

where

import           Prelude as P
import qualified Data.Vector.Generic as V
import           Data.Vector.Generic ((!))
import           Data.ListLike.Base
import           Data.ListLike.FoldableLL
import           Data.ListLike.String
import           Data.String (IsString)
import           GHC.Exts (IsList(..))

instance {-# OVERLAPPABLE #-} V.Vector v a => FoldableLL (v a) a where
    foldl = V.foldl
    foldl' = V.foldl'
    foldl1 = V.foldl1
    foldr = V.foldr
    foldr' = V.foldr'
    foldr1 = V.foldr1

#if 0
instance {-# OVERLAPPABLE #-} (Monoid (v a), Eq (v a), V.Vector v a) => IsList (v a) where
    type Item (v a) = a
    toList = V.toList
    fromList = V.fromList
#endif

instance {-# OVERLAPPABLE #-} (IsList (v a), Item (v a) ~ a, Monoid (v a), Eq (v a), V.Vector v a) => ListLike (v a) a where
    empty = V.empty
    singleton = V.singleton
    cons = V.cons
    snoc = V.snoc
    append = mappend
    head = V.head
    last = V.last
    tail = V.tail
    init = V.init
    null = V.null
    length = V.length
    rigidMap = V.map
    reverse = V.reverse
    --intersperse =
    concat = V.concat . toList
    rigidConcatMap = V.concatMap
    any = V.any
    all = V.all
    maximum = V.maximum
    minimum = V.minimum
    replicate = V.replicate
    take = V.take
    drop = V.drop
    --splitAt =
    takeWhile = V.takeWhile
    dropWhile = V.dropWhile
    span = V.span
    break = V.break
    --group =
    --inits =
    --tails =
    isPrefixOf = isPrefixOf'
    isSuffixOf = isSuffixOf'
    elem = V.elem
    find = V.find
    filter = V.filter
    index = (!)
    findIndex = V.findIndex
    --toList = V.toList
    --fromList = V.fromList
    --fromListLike = fromList . toList
    --groupBy f =
    genericLength = fromInteger . fromIntegral . V.length
    genericTake i = V.take (fromIntegral i)
    genericDrop i = V.drop (fromIntegral i)
    --genericSplitAt i =
    genericReplicate i = V.replicate (fromIntegral i)

    sequence  = fmap fromList . P.sequenceA  . toList
    mapM func = fmap fromList . P.traverse func . toList

instance (Eq (v Char), V.Vector v Char) => IsString (v Char) where
    fromString = V.fromList

instance (Eq (v Char), V.Vector v Char) => StringLike (v Char) where
    toString = V.toList
    --words =
    --lines =
    unwords = let sp = V.singleton ' ' in V.concat . intersperse sp . toList
    unlines = let eol = V.singleton '\n' in V.concat . intersperse eol . toList

isPrefixOf' :: (V.Vector v a, Eq (v a)) => v a -> v a -> Bool
isPrefixOf' needle haystack
  | V.null needle = True
  | V.length needle < V.length haystack =
            needle == V.slice 0 (V.length needle) haystack
  | V.length needle == V.length haystack = needle == haystack
  | otherwise = False
isSuffixOf' :: (V.Vector v a, Eq (v a)) => v a -> v a -> Bool
isSuffixOf' needle haystack
  | V.null needle = True
  | V.length needle < V.length haystack =
        needle == V.slice (V.length haystack - V.length needle)
                          (V.length needle)
                          haystack
  | V.length needle == V.length haystack = needle == haystack
  | otherwise = False
