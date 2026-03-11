{-# LANGUAGE DeriveGeneric #-}

module Pipes.Fluid.Merge where

import qualified GHC.Generics as G
import Data.Semigroup
import Data.Foldable
import qualified Data.List.NonEmpty as NE

-- | Differentiates whether a value from either or both producers.
-- In the case of one producer, additional identify if the other producer is live or dead.
data Source = FromBoth | FromLeft OtherStatus | FromRight OtherStatus
    deriving (Eq, Show, Ord, G.Generic)

-- | The other producer can be live (still yielding values), or dead
data OtherStatus = OtherLive | OtherDead
    deriving (Eq, Show, Ord, G.Generic)

-- | Differentiates when only one side is available (due to initial merge values of Nothing)
-- or if two values (one of which may be a previous values) are availabe.
data Merged a b =
    Coupled Source a b
    | LeftOnly OtherStatus a
    | RightOnly OtherStatus b
    deriving (Eq, Show, Ord, G.Generic)

-- | This can be used with 'Pipes.Prelude.takeWhile'
isBothLive :: Merged x y -> Bool
isBothLive (Coupled FromBoth _ _) = True
isBothLive (Coupled (FromLeft OtherLive) _ _) = True
isBothLive (Coupled (FromRight OtherLive) _ _) = True
isBothLive (LeftOnly OtherLive _) = True
isBothLive (RightOnly OtherLive _) = True
isBothLive _ = False

-- | This can be used with 'Pipes.Prelude.takeWhile'
isLeftLive :: Merged x y -> Bool
isLeftLive (Coupled FromBoth _ _) = True
isLeftLive (Coupled (FromLeft _) _ _) = True
isLeftLive (Coupled (FromRight OtherLive) _ _) = True
isLeftLive (LeftOnly _ _) = True
isLeftLive (RightOnly OtherLive _) = True
isLeftLive _ = False

-- | This can be used with 'Pipes.Prelude.takeWhile'
isRightLive :: Merged x y -> Bool
isRightLive (Coupled FromBoth _ _) = True
isRightLive (Coupled (FromRight _) _ _) = True
isRightLive (Coupled (FromLeft OtherLive) _ _) = True
isRightLive (RightOnly _ _) = True
isRightLive (LeftOnly OtherLive _) = True
isRightLive _ = False

-- | This can be used with 'Pipes.Prelude.takeWhile'
isRightDead :: Merged x y -> Bool
isRightDead (Coupled (FromLeft OtherDead) _ _) = True
isRightDead (LeftOnly OtherDead _) = True
isRightDead _ = False

-- | This can be used with 'Pipes.Prelude.takeWhile'
isLeftDead :: Merged x y -> Bool
isLeftDead (Coupled (FromRight OtherDead) _ _) = True
isLeftDead (RightOnly OtherDead _) = True
isLeftDead _ = False

class Merge f where
    merge' :: Maybe x -> Maybe y -> f x -> f y -> f (Merged x y)

merge :: Merge f => f x -> f y -> f (Merged x y)
merge = merge' Nothing Nothing

-- | Keep only the values originated from the left, replacing other yields with Nothing.
-- This is useful when React is based on STM, since filtering with Producer STM results in
-- larger STM transactions which may result in blocking.
discreteLeft :: Merged x y -> Maybe x
discreteLeft (LeftOnly _ x) = Just x
discreteLeft (Coupled FromBoth  x _) = Just x
discreteLeft (Coupled (FromLeft _) x _) = Just x
discreteLeft _ = Nothing

-- | Keep only the values originated from the right, replacing other yields with Nothing.
-- This is useful when React is based on STM, since filtering with Producer STM results in
-- larger STM transactions which may result in blocking.
discreteRight :: Merged x y -> Maybe y
discreteRight (RightOnly _ y) = Just y
discreteRight (Coupled FromBoth  _ y) = Just y
discreteRight (Coupled (FromRight _) _ y) = Just y
discreteRight _ = Nothing

-- | Keep only the values originated from both, replacing other yields with Nothing.
-- This is useful when React is based on STM, since filtering with Producer STM results in
-- larger STM transactions which may result in blocking.
discreteBoth :: Merged x y -> Maybe (x, y)
discreteBoth (Coupled FromBoth x y) = Just (x, y)
discreteBoth _ = Nothing

-- | Keep only the "new" values
discrete' :: Merged x x -> NE.NonEmpty x
discrete' (Coupled FromBoth x y) = x  NE.:| [y]
discrete' (Coupled (FromRight _) _ y) = y NE.:| []
discrete' (Coupled (FromLeft _) x _) = x NE.:| []
discrete' (RightOnly _ y) = y NE.:| []
discrete' (LeftOnly _ x) = x  NE.:| []

-- | Keep only the "new" values (using semigroup <> when both values were active)
discrete :: Semigroup x => Merged x x -> x
discrete = nonEmptyFoldl1' . discrete'
    where
      nonEmptyFoldl1' :: Semigroup b => NE.NonEmpty b -> b
      nonEmptyFoldl1' (x  NE.:| xs) = foldl' (<>) x xs

-- | merge two producers of the same type together.
mergeDiscrete' :: (Merge f, Functor f) => f x -> f x -> f (NE.NonEmpty x)
mergeDiscrete' x y = discrete' <$> (x `merge` y)

-- | merge two producers of the same type together (using semigroup <> when both values were active)
mergeDiscrete :: (Semigroup x, Merge f, Functor f) => f x -> f x -> f x
mergeDiscrete x y = discrete <$> (x `merge` y)
