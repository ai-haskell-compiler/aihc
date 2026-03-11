-- |
-- = Example
--
-- You have a diff between two versions of a file. Given a source location
-- in one version, find the corresponding location in the other version.
--
-- For example, here is a diff @d@ between a source string "abcdefgh" and a target
-- string "appcfgzzh", with deletions and insertions in the middle:
--
-- >  ab cdefg  h
-- > - b  de
-- > + pp     zz
-- >  appc  fgzzh
--
-- Diffs are represented by the type 'Diff'.
-- Only locations and lengths are recorded, not the actual characters.
--
-- >>> :{
--   let d :: Diff N
--       d = addDiff (Replace 1 (offset 1) (offset 2))  -- at location 1, replace "b" (length 1) with "pp" (length 2)
--         $ addDiff (Replace 3 (offset 2) (offset 0))  -- at location 3, replace "de" with ""
--         $ addDiff (Replace 7 (offset 0) (offset 2))  -- at location 7, replace "" with "zz"
--         $ emptyDiff
--   -- N.B.: replacements should be inserted right to left, starting from 'emptyDiff'.
-- :}
--
-- The span @s@ of "fg" in the first string starts at location 5 and has length 2.
--
-- >>> let s = 5 :.. offset 2 :: Interval N
--
-- >  a b c d e f g h
-- > 0 1 2 3 4 5 6 7 8
-- >           ^f+g+ length 2
-- >           ^
-- >           start 5
--
-- After applying the diff, the resulting span has been shifted to location 4.
--
-- >>> mapDiff d (5 :.. offset 2)
-- Just (4 :.. offset 2)
--
-- >  a p p c f g q q h
-- > 0 1 2 3 4 5 6 7 8 9
-- >         ^f+g+ length 2
-- >         ^
-- >         start 4
--
-- Conversely, we can map spans from the target string to the source string of the diff:
--
-- >>> comapDiff d (4 :.. offset 2)
-- Just (5 :.. offset 2)
--
-- If part of the input span is modified by the diff, there is no
-- corresponding output span.
--
-- >>> mapDiff d (1 :.. offset 2)  -- "bc" contains "b" which is edited by the diff
-- Nothing
module DiffLoc
  ( -- * API

    -- ** Overview

    -- |
    -- @
    --                                       "DiffLoc.Diff"
    -- +------------------------------------------------+
    -- |  data 'Diff' r                                   |
    -- |       'addDiff' :: r -> Diff r -> Diff r         |
    -- |       'mapDiff' :: Diff r -> Block r -> Block r  |
    -- +------------------------------------------------+
    --         | requires
    --         v                                                "DiffLoc.Shift"
    -- **********************************************************************
    -- *  class 'Shift' r                                                     *
    -- *  type  'Block' r                                                     *
    -- *        'src', 'tgt' :: r -> Block r                                    *
    -- *        'shiftBlock', 'coshiftBlock' :: r -> Block r -> Maybe (Block r) *
    -- *        'shiftR', 'coshiftR' :: r -> r -> Maybe r                       *
    -- **********************************************************************
    --         ^
    --         | implements with
    --         |          r = 'Replace' p
    --         |    'Block' r = 'Interval' p
    --         |
    --         |
    --         |  "DiffLoc.Interval"
    -- +-------------------+
    -- |  data 'Interval' p  |
    -- |  data 'Replace' p   |
    -- +-------------------+
    --         | requires
    --         v                           "DiffLoc.Shift"
    -- *************************************************
    -- *  class 'Amor' p                                 *
    -- *  type  'Trans' p                                *
    -- *  class Ord p                                  *
    -- *  class Ord ('Trans' p)                          *
    -- *  class Monoid ('Trans' p)                       *<---+
    -- *        ('.+') :: p -> Trans p -> p              *    |
    -- *        ('.-.?') :: p -> p -> Maybe (Trans p)    *    |
    -- *************************************************    |
    --         ^                               ^            |
    --         | implements with               |            | requires from
    --         |          p = 'Plain' a          |            |        l as p
    --         |    or    p = 'IndexFrom' n a    |            |    and c as p
    --         |    'Trans' p = 'Offset' a         |            |
    --         |                               |            |
    --         |                               |            |  "DiffLoc.Colline"
    --         |      "DiffLoc.Index"            |       +---------------------+
    -- +--------------------------+            |       |  data 'Colline' l c   |
    -- |  newtype 'Plain' a         |            |_______|  data 'Vallee' l' c'  |
    -- |  newtype 'IndexFrom' n a   |                    +---------------------+
    -- |  newtype 'Offset' a        |     implements with
    -- +--------------------------+                p = 'Colline' l c
    --         | requires                    'Trans' p = 'Vallee' ('Trans' l) ('Trans' c)
    --         v
    -- *****************
    -- *  class Num a  *
    -- *  class Ord a  *
    -- *****************
    -- @

    -- ** Diffs
    module DiffLoc.Diff

    -- ** Interfaces
    --
    -- - 'Shift', 'BlockOrder'
    -- - 'Amor', 'Origin'
  , module DiffLoc.Shift

    -- ** Intervals and replacements
  , module DiffLoc.Interval

    -- ** Plain indices
  , module DiffLoc.Index

    -- ** Lines and columns
  , module DiffLoc.Colline

    -- ** Basic configurations to get started
  , module DiffLoc.Starter

    -- $unsafe
  ) where

import DiffLoc.Colline
import DiffLoc.Diff
import DiffLoc.Index
import DiffLoc.Interval
import DiffLoc.Shift
import DiffLoc.Starter

-- $unsafe
-- The module "DiffLoc.Unsafe" is not reexported here.
-- You can import it separately.

-- $setup
-- >>> import DiffLoc.Unsafe
