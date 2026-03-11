{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

{-# LANGUAGE GADTs #-}

module Data.List.Range.Nat (
	-- * RangedNatL
	RangedNatL, natL, toIntL, fromIntL, splitAtL,
	-- * RangedNatR
	RangedNatR, natR, toIntR, fromIntR, splitAtR ) where

import Data.Bool (bool)
import Data.List.Length (repeatL, repeatR)
import Data.List.Range (
	RangeL, Unfoldr, unfoldrRangeMaybe, ZipL, zipWithL,
	RangeR, Unfoldl, unfoldlRangeMaybe, ZipR, zipWithR )

import GHC.TypeNats

---------------------------------------------------------------------------

-- * RANGED NAT LEFT
-- * RANGED NAT RIGHT

---------------------------------------------------------------------------
-- RANGED NAT LEFT
---------------------------------------------------------------------------

type RangedNatL n m = RangeL n m ()

natL :: (0 <= n, Unfoldr 0 n n) => RangedNatL n n
natL = repeatL ()

{-^

To make @RangedNatL@.

>>> :set -XTypeApplications -XDataKinds
>>> :module + Data.List.Range
>>> loosenL (natL @5) :: RangedNatL 3 8
() :. (() :. (() :. (() :.. (() :.. NilL))))

-}

toIntL :: Foldable (RangeL n m) => RangedNatL n m -> Int
toIntL = length

{-^

To convert from @RangedNatL@ to @Int@.

>>> :set -XTypeApplications -XDataKinds
>>> :module + Data.List.Range
>>> toIntL (loosenL (natL @5) :: RangedNatL 3 8)
5

-}

fromIntL :: (0 <= m, Unfoldr 0 n m) => Int -> Maybe (RangedNatL n m)
fromIntL = unfoldrRangeMaybe \s -> bool Nothing (Just ((), s - 1)) (s > 0)

{-^

To convert from @Int@ to @RangedNatL@.

>>> :set -XDataKinds
>>> fromIntL 5 :: Maybe (RangedNatL 3 8)
Just (() :. (() :. (() :. (() :.. (() :.. NilL)))))

-}

splitAtL :: ZipL n m v w => RangedNatL n m ->
	RangeL v w a -> (RangeL n m a, RangeL (v - m) (w - n) a)
splitAtL = zipWithL $ flip const

{-^

To split a list at position which is specified by number.

>>> :set -XTypeApplications -XDataKinds
>>> :module + Data.List.Range
>>> xs = 'h' :. 'e' :. 'l' :. 'l' :.. 'o' :.. NilL :: RangeL 3 8 Char
>>> splitAtL (natL @2) xs
('h' :. ('e' :. NilL),'l' :. ('l' :.. ('o' :.. NilL)))
>>> :type splitAtL (natL @2) xs
splitAtL (natL @2) xs :: (RangeL 2 2 Char, RangeL 1 6 Char)

-}

---------------------------------------------------------------------------
-- RANGED NAT RIGHT
---------------------------------------------------------------------------

type RangedNatR n m = RangeR n m ()

natR :: (0 <= n, Unfoldl 0 n n) => RangedNatR n n
natR = repeatR ()

{-^

To make @RangedNatR@.

>>> :set -XTypeApplications -XDataKinds
>>> :module + Data.List.Range
>>> loosenR (natR @5) :: RangedNatR 3 8
((((NilR :++ ()) :++ ()) :+ ()) :+ ()) :+ ()

-}

toIntR :: Foldable (RangeR n m) => RangedNatR n m -> Int
toIntR = length

{-^

To convert from @RangedNatR@ to @Int@.

>>> :set -XTypeApplications -XDataKinds
>>> :module + Data.List.Range
>>> toIntR (loosenR (natR @5) :: RangedNatR 3 8)
5

-}

fromIntR :: (0 <= m, Unfoldl 0 n m) => Int -> Maybe (RangedNatR n m)
fromIntR = unfoldlRangeMaybe \s -> bool Nothing (Just (s - 1, ())) (s > 0)

{-^

To convert @Int@ to @RangedNatR@.

>>> :set -XDataKinds
>>> fromIntR 5 :: Maybe (RangedNatR 3 8)
Just (((((NilR :++ ()) :++ ()) :+ ()) :+ ()) :+ ())

-}

splitAtR :: ZipR n m v w => RangeR n m a ->
	RangedNatR v w -> (RangeR (n - w) (m - v) a, RangeR v w a)
splitAtR = zipWithR const

{-^

To split a list at position which is specified by number.

>>> :set -XTypeApplications -XDataKinds
>>> :module + Data.List.Range
>>> xs = NilR :++ 'h' :++ 'e' :+ 'l' :+ 'l' :+ 'o' :: RangeR 3 8 Char
>>> splitAtR xs (natR @2)
(((NilR :++ 'h') :++ 'e') :+ 'l',(NilR :+ 'l') :+ 'o')
>>> :type splitAtR xs (natR @2)
splitAtR xs (natR @2) :: (RangeR 1 6 Char, RangeR 2 2 Char)

-}
