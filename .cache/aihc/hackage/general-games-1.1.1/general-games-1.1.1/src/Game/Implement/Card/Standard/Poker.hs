{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Module      : Game.Implement.Card.Standard.Poker
-- Copyright   : (c) 2017 Christopher A. Gorski
-- License     : MIT
-- Maintainer  : Christopher A. Gorski <cgorski@cgorski.org>
--
-- The Game.Implement.Card.Standard.Poker module defines data types and type class instances
-- for ordered operations on PlayingCard cards.
module Game.Implement.Card.Standard.Poker
  (
    Order(..),
    ValueType(..),

  )
  where

import Game.Implement.Card
import Game.Implement.Card.Standard

-- |
-- 'Order' defines an order to use when sorting a card. 'AceHighRankOrder' sorts under the
-- assumption that an Ace is a high card, and 'AceLowRankOrder' under the assumption that an
-- Ace is a low card. 'SuitOrder' sorts cards by suit, irrespective of their rank.
data Order = AceHighRankOrder | AceLowRankOrder | SuitOrder deriving (Eq)

-- |
-- 'ValueType' indicates the type Int value to be assigned to a card when the card
-- is evaluated by game value.
data ValueType = RankValueType | SuitValueType

instance OrderedCard PlayingCard Order where
  compareCardBy AceHighRankOrder (PlayingCard Ace _) (PlayingCard _ _) = GT
  compareCardBy AceHighRankOrder (PlayingCard _ _) (PlayingCard Ace _) = LT
  compareCardBy AceHighRankOrder (PlayingCard r1 _) (PlayingCard r2 _) =
    if r1 == r2
    then EQ
    else r1 `compare` r2

  compareCardBy AceLowRankOrder (PlayingCard Ace _) (PlayingCard _ _) = LT
  compareCardBy AceLowRankOrder (PlayingCard _ _) (PlayingCard Ace _) = GT
  compareCardBy AceLowRankOrder (PlayingCard r1 _) (PlayingCard r2 _) =
    if r1 == r2
    then EQ
    else r1 `compare` r2
    
  compareCardBy SuitOrder (PlayingCard _ s1) (PlayingCard _ s2) = s1 `compare` s2


instance OrderedValuedCard PlayingCard Order ValueType where
  toOrderedValue AceLowRankOrder RankValueType (PlayingCard r _) = (fromEnum r) + 1
  toOrderedValue AceHighRankOrder RankValueType (PlayingCard r _) =
    case r of 
      Ace -> 14
      n -> (fromEnum n) + 1
  toOrderedValue _ _ (PlayingCard _ s) = fromEnum s

