{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Module      : Game.Implement.Card
-- Copyright   : (c) 2017 Christopher A. Gorski
-- License     : MIT
-- Maintainer  : Christopher A. Gorski <cgorski@cgorski.org>
--
-- The Game.Implement.Card module provides fundamental operations for a deck of cards.
module Game.Implement.Card
  (
    Card (..)
  , ValuedCard (..)
  , OrderedCard (..)
  , OrderedValuedCard (..)
  )
  where

import Control.Monad.Random
import System.Random.Shuffle (shuffleM)
import Data.List (nub, maximumBy, minimumBy, sortBy, foldl1', tails)

-- |
-- Represents a physical card with no order and no value.
-- Inherited Enum, Eq, Ord and Bounded typeclasses are used to
-- distingish cards for the purposes of manipulation within lists.
-- Game value functions are provided by other typeclasses.
class (Enum c, Eq c, Ord c, Bounded c) => Card c where
  -- |
  -- Return all combinations of size n of a deck of cards
  choose :: Int -> [c] -> [[c]]
  choose 0 _ = [[]]
  choose n lst = do
    (x:xs) <- tails lst
    rest <- choose (n-1) xs
    return $ x : rest

  -- |
  -- Return a full deck of cards. Cards are unique. Order is not guaranteed.
  --
  -- >>> fullDeck :: [PlayingCard]
  -- [Ace of Clubs,Two of Clubs,Three of Clubs,Four of Clubs,Five of Clubs,Six of Clubs,Seven of Clubs,Eight of Clubs,Nine of Clubs,Ten of Clubs,Jack of Clubs,Queen of Clubs,King of Clubs,Ace of Diamonds,Two of Diamonds,Three of Diamonds,Four of Diamonds,Five of Diamonds,Six of Diamonds,Seven of Diamonds,Eight of Diamonds,Nine of Diamonds,Ten of Diamonds,Jack of Diamonds,Queen of Diamonds,King of Diamonds,Ace of Hearts,Two of Hearts,Three of Hearts,Four of Hearts,Five of Hearts,Six of Hearts,Seven of Hearts,Eight of Hearts,Nine of Hearts,Ten of Hearts,Jack of Hearts,Queen of Hearts,King of Hearts,Ace of Spades,Two of Spades,Three of Spades,Four of Spades,Five of Spades,Six of Spades,Seven of Spades,Eight of Spades,Nine of Spades,Ten of Spades,Jack of Spades,Queen of Spades,King of Spades]
  fullDeck :: [c]

  -- |
  -- Returns all unique cards in a list. All duplicates are removed.
  dedupe :: [c] -> [c]
  
  -- |
  -- Draws cards from a deck, and groups them based on the list provided
  -- in the first argument. Returns the grouped hands and the remaining deck.
  -- Arguments that are negative or exceed bounds return Nothing.
  --
  -- For instance, to simulate a three player Hold'em game, one might wish
  -- to draw two cards for each player, and five cards for the community:
  -- 
  -- >>> deck <- evalRandIO $ shuffle $ (fullDeck :: [PlayingCard])
  -- >>> draw [2,2,2,5] deck
  -- Just ([[Ace of Spades,Jack of Spades],[Queen of Hearts,Seven of Clubs],[Jack of Diamonds,Six of Hearts],[Jack of Hearts,Five of Spades,Three of Spades,Two of Diamonds,Ace of Hearts]],[Four of Clubs,Six of Diamonds,Four of Diamonds,Eight of Spades,Six of Clubs,Seven of Spades,Three of Diamonds,Ten of Diamonds,Eight of Hearts,Nine of Diamonds,Three of Clubs,Six of Spades,King of Clubs,Nine of Clubs,Four of Spades,Five of Diamonds,Nine of Spades,Queen of Spades,Ace of Diamonds,Four of Hearts,Two of Clubs,Five of Clubs,Two of Hearts,King of Diamonds,Ten of Spades,Eight of Clubs,Seven of Hearts,Three of Hearts,Queen of Diamonds,Queen of Clubs,Ten of Clubs,King of Hearts,Eight of Diamonds,Jack of Clubs,Ten of Hearts,Seven of Diamonds,Two of Spades,Nine of Hearts,King of Spades,Ace of Clubs,Five of Hearts])
  draw :: [Int] -> [c] -> Maybe ([[c]],[c])
  
  -- |
  -- The same as 'draw', except throw away the deck
  --
  -- >>> deck <- evalRandIO $ shuffle $ (fullDeck :: [PlayingCard])
  -- >>> draw_ [2,2,2,5] deck
  -- Just [[Eight of Hearts,Queen of Hearts],[Two of Clubs,Seven of Diamonds],[Ten of Clubs,Three of Hearts],[Ace of Spades,Nine of Spades,Five of Spades,Four of Diamonds,Two of Spades]]
  draw_ :: [Int] -> [c] -> Maybe [[c]]
  
  -- |
  -- The same as 'draw', except draw only one hand of specified size.
  --
  -- >>> deck <- evalRandIO $ shuffle $ (fullDeck :: [PlayingCard])
  -- >>> draw1 5 deck
  -- Just ([Six of Clubs,Ace of Hearts,Nine of Hearts,Four of Hearts,Two of Diamonds],[King of Diamonds,Queen of Spades,Four of Spades,Seven of Hearts,Five of Hearts,Seven of Clubs,Three of Hearts,Ace of Spades,Three of Diamonds,Seven of Diamonds,Two of Clubs,Five of Spades,King of Hearts,Jack of Hearts,Queen of Hearts,Ten of Clubs,Five of Clubs,Eight of Spades,Ace of Clubs,King of Clubs,Five of Diamonds,Queen of Diamonds,Eight of Hearts,Four of Clubs,Three of Clubs,Jack of Clubs,Jack of Diamonds,Ten of Diamonds,Queen of Clubs,Eight of Diamonds,Six of Diamonds,Eight of Clubs,Three of Spades,Two of Hearts,Six of Spades,King of Spades,Ten of Hearts,Nine of Spades,Nine of Diamonds,Two of Spades,Ten of Spades,Nine of Clubs,Four of Diamonds,Ace of Diamonds,Six of Hearts,Seven of Spades,Jack of Spades])
  draw1 :: Int -> [c] -> Maybe ([c],[c])

  -- |
  -- The same as 'draw1', except throw away the deck.
  --
  -- >>> deck <- evalRandIO $ shuffle $ (fullDeck :: [PlayingCard])
  -- >>> draw1_ 5 deck
  -- Just [Five of Hearts,Ace of Diamonds,Ten of Hearts,Two of Spades,Six of Clubs]
  draw1_ :: Int -> [c] -> Maybe [c]

  -- |
  -- Shuffle a deck of cards.
  --
  -- >>> deck <- evalRandIO $ shuffle $ (fullDeck :: [PlayingCard])
  -- >>> [Three of Clubs,Nine of Spades,Five of Clubs,Two of Hearts,Four of Spades,King of Hearts,Ten of Hearts,Two of Clubs,Ace of Hearts,Eight of Diamonds,Six of Diamonds,Seven of Diamonds,Jack of Spades,Three of Hearts,Three of Spades,Queen of Clubs,Ten of Diamonds,Six of Spades,Two of Diamonds,Nine of Clubs,Five of Diamonds,Five of Spades,Seven of Spades,Jack of Clubs,Six of Hearts,Jack of Diamonds,Four of Hearts,Ace of Spades,Nine of Diamonds,King of Clubs,Two of Spades,Four of Clubs,Eight of Hearts,Queen of Hearts,Ace of Clubs,Five of Hearts,Ten of Spades,Six of Clubs,Ten of Clubs,Four of Diamonds,Three of Diamonds,Seven of Hearts,King of Diamonds,Ace of Diamonds,Nine of Hearts,Queen of Spades,Seven of Clubs,Jack of Hearts,King of Spades,Eight of Spades,Queen of Diamonds,Eight of Clubs]
  shuffle :: RandomGen m => [c] -> Rand m [c]

  -- |
  -- Return a random card.
  --
  -- >>> card :: PlayingCard <- evalRandIO $ randomCard
  -- >>> card
  -- Four of Diamonds
  randomCard :: RandomGen m => Rand m c

  fullDeck = [minBound .. maxBound]
  dedupe l = nub l
  shuffle deck = shuffleM deck
  randomCard =
    let
      minB = minBound :: (Bounded c, Card c) => c
      maxB = maxBound :: (Bounded c, Card c) => c in
      do
        randomd <- getRandomR(fromEnum minB, fromEnum maxB)
        return $ toEnum randomd
      
  draw handSizeLst deck 
    | let
        total = (foldl1' (+) handSizeLst)
        anyNeg = (length (filter (\n -> n < 0) handSizeLst)) > 0
      in
        (total > (length deck)) || (total < 1) || anyNeg = Nothing
    | otherwise = let
        draw2 [] (houtput, doutput) = ((reverse houtput), doutput)
        draw2 (nToTake:hst) (handOutput, deckOutput) = let
          newHand = take nToTake deckOutput
          newDeck = drop nToTake deckOutput in
            draw2 hst (newHand:handOutput, newDeck)
        in Just (draw2 handSizeLst ([],deck))

  draw_ handSizes deck =
    let f (Just (h, _)) = Just h
        f _ = Nothing
    in
      f $ draw handSizes deck
  
  draw1 handSize deck =
    let
      f (Just ([h], d)) = Just (h,d)
      f _ = Nothing
    in
      f $ draw [handSize] deck

  draw1_ handSize deck =
    let
      f (Just ([h], _)) = Just h
      f _ = Nothing
    in
      f $ draw [handSize] deck
        
-- |
-- Represents a playing card with a game value. For instance,
-- a standard playing card with a type representing
-- rank and suit.
class (Card c) => ValuedCard c v where
  -- |
  -- Return a value associated with a card.
  --
  -- >>> card = PlayingCard Six Clubs
  -- >>> toValue card :: Rank
  -- Six
  -- >>> toValue card :: Suit
  -- Clubs
  toValue :: c -> v

  -- |
  -- Return values associated with multiple cards.
  --
  -- >>> cards = [PlayingCard Six Clubs, PlayingCard Four Diamonds]
  -- >>> toValueLst cards :: [Rank]
  -- [Six,Four]
  -- >>> toValueLst cards :: [Suit]
  -- [Clubs,Diamonds]
  toValueLst :: [c] -> [v]
  toValueLst l = map toValue l

-- |
-- Orderings independent of a specific value
-- type of a Card.
class (Card c) => OrderedCard c o where
  highestCardBy :: o -> [c] -> c
  lowestCardBy :: o -> [c] -> c
  compareCardBy :: o -> c -> c -> Ordering
  sortCardsBy :: o -> [c] -> [c]
  highestCardBy o cl = maximumBy (compareCardBy o) cl
  lowestCardBy o cl = minimumBy (compareCardBy o) cl
  sortCardsBy o cl = sortBy (compareCardBy o) cl

-- |
-- Orderings dependent on the specific value type of a Card
class (OrderedCard c o) => OrderedValuedCard c o vt where
  -- |
  -- Return an Int based on a card, an ordering and a value type.
  toOrderedValue :: o -> vt -> c -> Int


