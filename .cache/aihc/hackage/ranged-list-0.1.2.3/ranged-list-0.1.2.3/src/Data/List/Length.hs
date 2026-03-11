{-# LANGUAGE BlockArguments, TupleSections #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.List.Length (
	-- * LENGTHED LIST LEFT
	-- ** Type
	LengthL, RangeL(NilL, (:.)),
	-- ** AddL
	AddL, (++.),
	-- ** Unfoldr
	-- *** class
	Unfoldr,
	-- *** without monad
	repeatL, fillL, unfoldr, unfoldrWithBase,
	-- *** with monad
	unfoldrM, unfoldrMWithBase,
	-- ** ZipL
	ZipL, zipL, zipWithL, zipWithML,
	-- ** ListToLengthL
	ListToLengthL, splitL, chunksL, chunksL',
	-- * LENGTHED LIST RIGHT
	-- ** Type
	LengthR, RangeR(NilR, (:+)),
	-- ** AddR
	AddR, (+++),
	-- ** Unfoldl
	-- *** class
	Unfoldl,
	-- *** without monad
	repeatR, fillR, unfoldl, unfoldlWithBase,
	-- *** with monad
	unfoldlM, unfoldlMWithBase,
	-- ** ZipR
	ZipR, zipR, zipWithR, zipWithMR,
	-- ** ListToLengthR
	ListToLengthR, listToLengthR, chunksR, chunksR',
	-- * LEFT TO RIGHT
	LeftToRight, (++.+), leftToRight,
	-- * RIGHT TO LEFT
	RightToLeft, (++..), rightToLeft ) where

import Control.Arrow (first, (***))
import Data.List.Range (
	RangeL(..), AddL, (++.), LoosenLMax, loosenLMax, Unfoldr,
	ZipL, zipL, zipWithL, zipWithML,
	RangeR(..), AddR, (+++), LoosenRMax, loosenRMax, Unfoldl,
	ZipR, zipR, zipWithR, zipWithMR,
	LeftToRight, (++.+), leftToRight, RightToLeft, (++..), rightToLeft )
import Data.List.Length.LengthL (
	LengthL, unfoldr, unfoldrWithBase, unfoldrM, unfoldrMWithBase,
	ListToLengthL, splitL )
import Data.List.Length.LengthR (
	LengthR, unfoldl, unfoldlWithBase, unfoldlM, unfoldlMWithBase,
	ListToLengthR, listToLengthR )

import GHC.TypeNats

---------------------------------------------------------------------------

-- LENGTH LEFT
-- LENGTH RIGHT

---------------------------------------------------------------------------
-- LENGTH LEFT
---------------------------------------------------------------------------

repeatL :: (0 <= n, Unfoldr 0 n n) => a -> LengthL n a
repeatL = fillL NilL

{-^

To repeat a value of type @a@ to construct a list of type @LengthL n a@.

>>> :set -XDataKinds
>>> repeatL 'c' :: LengthL 5 Char
'c' :. ('c' :. ('c' :. ('c' :. ('c' :. NilL))))

-}

fillL :: Unfoldr n m m => RangeL n m a -> a -> LengthL m a
fillL = (`unfoldrWithBase` \x -> (x, x))

{-^

To fill a list of type @LengthL n a@ with a value of type @a@.

>>> :set -XDataKinds
>>> fillL ('a' :. 'b' :.. NilL) 'c' :: LengthL 5 Char
'a' :. ('b' :. ('c' :. ('c' :. ('c' :. NilL))))

-}

chunksL :: ListToLengthL n => [a] -> ([LengthL n a], RangeL 0 (n - 1) a)
chunksL = either ([] ,) (uncurry first . ((:) *** chunksL)) . splitL

{-^

To separate a list to multiple lengthed lists.
It return separeted lengthed lists and a not enough length fragment.

>>> :set -XTypeApplications -XDataKinds
>>> chunksL @3 "foo bar"
(['f' :. ('o' :. ('o' :. NilL)),' ' :. ('b' :. ('a' :. NilL))],'r' :.. NilL)

-}

chunksL' :: (Unfoldr 0 n n, LoosenLMax 0 (n - 1) n, ListToLengthL n) =>
	a -> [a] -> [LengthL n a]
chunksL' z xs = case chunksL xs of
	(cs, NilL) -> cs; (cs, rs) -> cs ++ [loosenLMax rs `fillL` z]

{-^

It is like chunksL.
But if there is a not enough length fragment, then it fill with a default value.

>>> :set -XTypeApplications -XDataKinds
>>> print `mapM_` chunksL' @3 '@' "foo bar"
'f' :. ('o' :. ('o' :. NilL))
' ' :. ('b' :. ('a' :. NilL))
'r' :. ('@' :. ('@' :. NilL))

-}

---------------------------------------------------------------------------
-- LENGTH RIGHT
---------------------------------------------------------------------------

repeatR :: (0 <= n, Unfoldl 0 n n) => a -> LengthR n a
repeatR = (`fillR` NilR)

{-^

To repeat a value of type @a@ to construct a list of type @LengthR n a@.

>>> :set -XDataKinds
>>> repeatR 'c' :: LengthR 5 Char
((((NilR :+ 'c') :+ 'c') :+ 'c') :+ 'c') :+ 'c'

-}

fillR :: Unfoldl n m m => a -> RangeR n m a -> LengthR m a
fillR = unfoldlWithBase \x -> (x, x)

{-^

To fill a list of type @LengthR n a@ with a default value.

>>> :set -XDataKinds
>>> fillR 'c' (NilR :++ 'a' :+ 'b') :: LengthR 5 Char
((((NilR :+ 'c') :+ 'c') :+ 'c') :+ 'a') :+ 'b'

-}

chunksR :: ListToLengthR n => [a] -> ([LengthR n a], RangeR 0 (n - 1) a)
chunksR = either ([] ,) (uncurry first . ((:) *** chunksR)) . listToLengthR

{-^

To separate a list to multiple lengthed lists.
It return separated lengthed lists and a not enough length fragment.

>>> :set -XTypeApplications -XDataKinds
>>> chunksR @3 "foo bar"
([((NilR :+ 'o') :+ 'o') :+ 'f',((NilR :+ 'a') :+ 'b') :+ ' '],NilR :++ 'r')

-}

chunksR' :: (Unfoldl 0 n n, LoosenRMax 0 (n - 1) n, ListToLengthR n) =>
	a -> [a] -> [LengthR n a]
chunksR' z xs = case chunksR xs of
	(cs, NilR) -> cs; (cs, rs) -> cs ++ [z `fillR` loosenRMax rs]

{-^

It is like @chunksR@.
But if there is a not enough length fragment, then it fill with a default value.

>>> :set -XTypeApplications -XDataKinds
>>> print `mapM_` chunksR' @3 '@' "foo bar"
((NilR :+ 'o') :+ 'o') :+ 'f'
((NilR :+ 'a') :+ 'b') :+ ' '
((NilR :+ '@') :+ '@') :+ 'r'

-}
