{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Data.Sequence.ToolsYj (

	cons, snoc, uncons, unsnoc,

	splitAt'

	) where

import Data.Sequence qualified as Seq

cons :: a -> Seq.Seq a -> Seq.Seq a
cons = (Seq.:<|)

snoc :: Seq.Seq a -> a -> Seq.Seq a
snoc = (Seq.:|>)

uncons :: Seq.Seq a -> Maybe (a, Seq.Seq a)
uncons = \case Seq.Empty -> Nothing; x Seq.:<| s -> Just (x, s)

unsnoc :: Seq.Seq a -> Maybe (Seq.Seq a, a)
unsnoc = \case Seq.Empty -> Nothing; s Seq.:|> x -> Just (s, x)

splitAt' :: Int -> Seq.Seq a -> Maybe (Seq.Seq a, Seq.Seq a)
splitAt' n s
	| Seq.length s < n = Nothing
	| otherwise = Just $ Seq.splitAt n s
