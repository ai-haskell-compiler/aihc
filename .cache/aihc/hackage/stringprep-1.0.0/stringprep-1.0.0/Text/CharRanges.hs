{-# LANGUAGE PatternGuards #-}
module Text.CharRanges
  ( Range(..)
  , range
  , single
  , CharSet
  , toSet
  , member
  ) where

import           Data.List
import           Data.Set (Set)
import qualified Data.Set as Set

data Range = Single {-# UNPACK #-} !Char
           | Range  {-# UNPACK #-} !Char {-# UNPACK #-} !Char
             deriving (Eq, Show)

newtype CharRange = CR { unCR :: Range }

-- | A rather hacked-up instance.
--   This is to support fast lookups using 'Data.Set' (see 'toSet').
--   x == y iff x and y overlap
instance Eq CharRange where
    CR (Single x)    == CR (Single y) = x == y
    CR (Single a)    == CR (Range x y) = x <= a && a <= y
    CR (Range x y)   == CR (Single a) = x <= a && a <= y
    CR (Range lx ux) == CR (Range ly uy) = (lx <= uy && ly <= ux)
                                        || (lx <= uy && ly <= ux) -- INTENTIONAL
                                           -- For some strange reason GHC
                                           -- (7.6.3) seems to have problems
                                           -- optimizing this expressions
                                           -- without the additional or

instance Ord CharRange where
    CR (Single x)  <= CR (Single y) = x <= y
    CR (Single x)  <= CR (Range y _) = x <= y
    CR (Range _ x) <= CR (Single y) = x <= y
    CR (Range _ x) <= CR (Range y _) = x <= y

newtype CharSet = CharSet (Set CharRange)

-- | Allows quick lookups using ranges.
toSet :: [Range] -> CharSet
toSet = CharSet . Set.fromDistinctAscList . prepareRanges
  where prepareRanges :: [Range] -> [CharRange]
        prepareRanges =  go . sort . map CR -- we could use unsafeCoerce to
                                            -- avoid the cost of mapping
        go (r1:r2:rs) | Just r' <- maybeMergeRanges r1 r2 = go (r':rs)
                      | rss@(r3:rs') <- go (r2:rs) =
            case maybeMergeRanges r1 r3 of
                Nothing -> r1:rss
                Just r' -> r':rs'
        go rs = rs

maybeMergeRanges :: CharRange -> CharRange -> Maybe CharRange
maybeMergeRanges x y = if x == y -- overlap
                       then Just . CR $ minMax (unCR x) (unCR y)
                       else Nothing
{-# INLINE maybeMergeRanges #-}

minMax :: Range -> Range -> Range
minMax (Range lx ux) (Range ly uy) = Range (min lx ly) (max ux uy)
minMax (Single _) y = y
minMax x (Single _) = x
{-# INLINE minMax #-}

range :: Char -> Char -> Range
range x y = if x < y then Range x y
            else error "range: x not smaller than y"
{-# INLINE range #-}

single :: Char -> Range
single = Single
{-# INLINE single #-}

member :: Char -> CharSet -> Bool
member x (CharSet cs) = Set.member (CR $ Single x) cs
