{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
-- |
-- Module      : Game.Game.Poker
-- Copyright   : (c) 2017 Christopher A. Gorski
-- License     : MIT
-- Maintainer  : Christopher A. Gorski <cgorski@cgorski.org>
--
-- The Game.Game.Poker module provides operations for five card poker.
module Game.Game.Poker
  (
    -- * Poker Hand Types
    PokerHand
  , PokerHandType(..)
  , AceRank (..)

  , cardsOfPokerHand
  , typeOfPokerHand

  -- * Building Hands
  , mkHand

  -- * Hand Type Existence Checks
  , isHand
  
  -- * Sets of Hand Types
  , allPossibleHands
  , allRoyalFlush
  , allStraightFlush
  , allFourOfAKind
  , allFullHouse
  , allFlush
  , allStraight
  , allThreeOfAKind
  , allTwoPair
  , allPair
  , allHighCard


  -- * Random Hands
  , randomHighCard 
  , randomPair
  , randomTwoPair
  , randomThreeOfAKind
  , randomStraight
  , randomFlush
  , randomFullHouse
  , randomFourOfAKind
  , randomStraightFlush
  , randomRoyalFlush

  -- * Additional Hand Building Functions
  , mkHighCard
  , mkPair
  , mkTwoPair
  , mkThreeOfAKind
  , mkStraight
  , mkFlush
  , mkFullHouse
  , mkFourOfAKind
  , mkStraightFlush
  , mkRoyalFlush
  
  -- * Additional Hand Type Existence Checks
  , isHighCard
  , isPair
  , isTwoPair
  , isThreeOfAKind
  , isStraight
  , isFlush
  , isFullHouse
  , isFourOfAKind
  , isStraightFlush
  , isRoyalFlush

  )

   
  where


import Control.Monad.Loops
import Control.Monad.Random
import Game.Implement.Card
import Game.Implement.Card.Standard
import Game.Implement.Card.Standard.Poker
import Data.List (nub,find) 
import Data.Maybe (isJust, fromJust, catMaybes)



-- |
-- Indicates if a poker hand uses the Ace as a high card or a low card. AceLow is only
-- used when an Ace is in a hand. Any hand without an Ace is considered AceHigh.
--
-- >>>
data AceRank = AceHigh | AceLow deriving (Eq, Show, Ord, Enum, Bounded)

-- |
-- Return the cards in a 'PokerHand'
cardsOfPokerHand :: PokerHand -> [PlayingCard]
cardsOfPokerHand (PokerHand _ h) = h

-- |
-- Return the 'PokerHandType' of a 'PokerHand'
typeOfPokerHand :: PokerHand -> PokerHandType
typeOfPokerHand (PokerHand t _) = t

-- |
-- The type of a 'PokerHand'. 
data PokerHandType =
  HighCard 
  | Pair 
  | TwoPair 
  | ThreeOfAKind 
  | Straight AceRank
  | Flush 
  | FullHouse 
  | FourOfAKind 
  | StraightFlush AceRank
  | RoyalFlush 
  deriving(Eq,Show)


-- |
-- A poker hand. Constructors are hidden, so any hand encapsulated in this type
-- can be considered a valid hand.
--
-- >>> deck <- evalRandIO $ shuffle $ (fullDeck :: [PlayingCard])
-- >>> hand = draw1_ 5 deck
-- >>> hand
-- [Five of Diamonds,Jack of Spades,Queen of Spades,Queen of Diamonds,Jack of Hearts]
--
-- >>> pokerhand = fromJust $ mkHand hand
-- >>> pokerhand
-- PokerHand TwoPair [Five of Diamonds,Jack of Spades,Queen of Spades,Queen of Diamonds,Jack of Hearts]
--
-- >>> typeOfPokerHand pokerhand
-- TwoPair
--
-- >>> cardsOfPokerHand pokerhand
-- [Five of Diamonds,Jack of Spades,Queen of Spades,Queen of Diamonds,Jack of Hearts]
data PokerHand = PokerHand PokerHandType [PlayingCard] deriving(Eq,Show)

-- |
-- Return a random hand that is not any other hand, also known as "High Card"
randomHighCard :: RandomGen g => Rand g PokerHand
randomHighCard =
  let r = do
        randHand <- replicateM 5 randomCard
        return randHand 
  in
    do 
      hand <- iterateUntil (\h -> isHighCard h) r
      return $ PokerHand HighCard hand
    
-- |
-- Return a random hand that is a Pair
randomPair :: RandomGen g => Rand g PokerHand
randomPair =
  do
    numLstR <- uniqueNumList 4 0 12
    rank1pair <- return $ replicate 2 $ toEnum $ (fromJust numLstR) !! 0
    rank2 <- return $ toEnum $ (fromJust numLstR) !! 1
    rank3 <- return $ toEnum $ (fromJust numLstR) !! 2
    rank4 <- return $ toEnum $ (fromJust numLstR) !! 3
    rankLst <- return $ rank4:rank3:rank2:rank1pair
    numLstS1 <- uniqueNumList 2 0 3
    suitLst1 <- return $ map (\r -> toEnum r) $ fromJust numLstS1
    suit2 <- randomSuit
    suit3 <- randomSuit
    suit4 <- randomSuit
    suitLst <- return $ suit4:suit3:suit2:suitLst1
    cardset <- zipWithM (\r s -> return(PlayingCard r s)) rankLst suitLst
    shuffleset <- shuffle cardset
    return $ PokerHand Pair shuffleset

-- |
-- Return a random hand that is a Two Pair
randomTwoPair :: RandomGen g => Rand g PokerHand
randomTwoPair =
  do
    numLstR <- uniqueNumList 3 0 12
    rank1 <- return $ replicate 2 $ toEnum $ (fromJust numLstR) !! 0
    rank2 <- return $ replicate 2 $ toEnum $ (fromJust numLstR) !! 1
    rank3 <- return $ toEnum $ (fromJust numLstR) !! 2
    rankLst :: [Rank] <- return $ rank3:(rank1 ++ rank2)
    numLstS1 <- uniqueNumList 2 0 3
    numLstS2 <- uniqueNumList 2 0 3
    numS3 <- randomSuit
    suitLst1 <- return $ map (\r -> toEnum r) $ fromJust numLstS1
    suitLst2 <- return $ map (\r -> toEnum r) $ fromJust numLstS2
    suitLst <- return $ numS3:(suitLst1 ++ suitLst2)
    cardset <- zipWithM (\r s -> return(PlayingCard r s)) rankLst suitLst
    shuffleset <- shuffle cardset
    return $ PokerHand TwoPair shuffleset


-- |
-- Return a random hand that is a Three of a Kind
randomThreeOfAKind :: RandomGen g => Rand g PokerHand
randomThreeOfAKind =
  do
    numLst <- uniqueNumList 3 0 12
    rank1 <- return $ replicate 3 $ toEnum $ (fromJust numLst) !! 0
    rank2 <- return $ map (\r -> toEnum r) $ drop 1 (fromJust numLst)
    rankLst :: [Rank] <- return $ rank1 ++ rank2
    numLstS1 <- uniqueNumList 3 0 3
    suitLst1 <- return $ map (\r -> toEnum r) $ fromJust numLstS1
    suitLst2 <- replicateM 2 randomSuit
    suitLst <- return $ suitLst1 ++ suitLst2
    cardset <- zipWithM (\r s -> return(PlayingCard r s)) rankLst suitLst
    shuffleset <- shuffle cardset
    return $ PokerHand ThreeOfAKind shuffleset

-- |
-- Return a random hand that is a Straight
randomStraight :: RandomGen g => Rand g PokerHand
randomStraight =
  let
    mkRanklst :: Int -> [Rank]
    mkRanklst n = map (\m -> toEnum ((m+n) `mod` 13) ) [0..4] 
    mergelst r s = return(PlayingCard r s)
    l = do
      startRank :: Int <- getRandomR(0,9)

      ranklst <- return (mkRanklst startRank)
      suitlst :: [Suit] <- replicateM 5 randomSuit
      cardset <- zipWithM mergelst ranklst suitlst
      return cardset
  in
    do
      hand <- iterateUntil (\h -> (not $ isStraightFlush h) && (not $ isRoyalFlush h)) l
      aceRank <- return (if (toRank $  hand !! 0) == Ace then AceLow else AceHigh)
      shuffledHand <- shuffle hand
      return $ PokerHand (Straight aceRank) shuffledHand

-- |
-- Return a random hand that is a Flush
randomFlush :: RandomGen g => Rand g PokerHand
randomFlush =
  let
    l = do
      numLst <- uniqueNumList 5 0 12
      rankLst :: [Rank] <- return $ map (\n -> toEnum n) $ fromJust $ numLst
      randSuit <- randomSuit
      suitLst :: [Suit] <- return $ replicate 5 randSuit
      cardset <- zipWithM (\r s -> return(PlayingCard r s)) rankLst suitLst 
      return cardset
  in
    do
      hand <- iterateUntil (\h -> (not $ isRoyalFlush h) && (not $ isStraightFlush h)) l
      return $ PokerHand Flush hand

-- |
-- Return a random hand that is a Full House
randomFullHouse :: RandomGen g => Rand g PokerHand
randomFullHouse =
  do
    numLstR <- uniqueNumList 2 0 12
    rank1 <- return $ toEnum $ (fromJust numLstR) !! 0
    rank2 <- return $ toEnum $ (fromJust numLstR) !! 1      
    rankLst :: [Rank] <- return [rank1, rank1, rank1, rank2, rank2]
    numLstS1 <- uniqueNumList 3 0 3
    numLstS2 <- uniqueNumList 2 0 3
    suitLst1 <- return $ map (\r -> toEnum r) $ fromJust numLstS1
    suitLst2 <- return $ map (\r -> toEnum r) $ fromJust numLstS2
    suitLst <- return $ suitLst1 ++ suitLst2
    cardset <- zipWithM (\r s -> return(PlayingCard r s)) rankLst suitLst
    shuffleset <- shuffle cardset
    return $ PokerHand FullHouse shuffleset

-- |
-- Return a random hand that is a Four of a Kind
randomFourOfAKind :: RandomGen g => Rand g PokerHand
randomFourOfAKind =
  do
    randRank4 <- randomRank
    randRank <- iterateUntil (\r -> r /= randRank4) randomRank
    randRanks <- return $ randRank:(replicate 4 randRank4)
    randSuit <- randomSuit
    randSuits <- return [randSuit, Clubs, Diamonds, Hearts, Spades]
    mergedLst <- zipWithM (\r s -> return(PlayingCard r s)) randRanks randSuits
    shuffleSet <- shuffle mergedLst
    return $ PokerHand FourOfAKind $ shuffleSet

-- |
-- Return a random hand that is a Straight Flush
randomStraightFlush :: RandomGen g => Rand g PokerHand
randomStraightFlush =
  let
    mkRanklst :: Int -> [Rank]
    mkRanklst n = map (\m -> toEnum ((m+n) `mod` 13) ) [0..4] 
    mergelst r s = return(PlayingCard r s)
    l = do 
      startRank :: Int <- getRandomR(0,9)
      ranklst <- return (mkRanklst startRank)
      randSuit <- randomSuit
      suitlst :: [Suit] <- return (replicate 5 randSuit)
      cardset <- zipWithM mergelst ranklst suitlst
      return cardset 
  in
    do
      hand <- iterateUntil (\h -> (not $ isRoyalFlush h)) l
      aceRank <- return (if (toRank $  hand !! 0) == Ace then AceLow else AceHigh)
      shuffledHand <- shuffle hand
      return $ PokerHand (StraightFlush aceRank) shuffledHand

-- |
-- Return a random hand that is a Royal Flush
randomRoyalFlush :: RandomGen g => Rand g PokerHand
randomRoyalFlush =
  let
    mkRanklst :: [Rank]
    mkRanklst = Ace : (map (\m -> toEnum m) [9..12])
    mergelst r s = return(PlayingCard r s) in
    do 
      randSuit <- randomSuit
      suitlst :: [Suit] <- return (replicate 5 randSuit)
      cardset <- zipWithM mergelst mkRanklst suitlst
      shuffledHand <- shuffle cardset
      return $ PokerHand RoyalFlush shuffledHand

-- |
-- Given a list of cards, find the best hand in the set. If the number
-- of cards is not equal to five, or there are duplicate cards, mkHand returns
-- Nothing.
mkHand :: [PlayingCard] -> Maybe PokerHand
mkHand hand =
  let checks =
        [mkHighCard
        ,mkPair
        ,mkTwoPair
        ,mkThreeOfAKind
        ,mkStraight
        ,mkFlush
        ,mkFullHouse
        ,mkFourOfAKind
        ,mkStraightFlush
        ,mkRoyalFlush]
      cat = catMaybes $ map (\f -> f hand) checks
  in 
    if length cat == 0
    then Nothing
    else Just $ cat !! 0

isSameSuit :: [PlayingCard] -> Bool
isSameSuit hand =
  let
    ff (Just c0) (Just c1) =
      if (toSuit c0) == (toSuit c1)
      then Just c1
      else Nothing
    ff _ _ = Nothing
  in
    case foldl1 ff $ map (\x -> Just x) hand of
      Nothing -> False
      Just _ -> True

hasConsecutiveRanks :: Order -> [PlayingCard] -> Bool
hasConsecutiveRanks order hand =
  let handlst = map (\x -> Just x) $ sortCardsBy order hand
      ff (Just c0) (Just c1) =
        case (toOrderedValue order RankValueType c1)-(toOrderedValue order RankValueType c0) of
          1 -> Just c1
          _ -> Nothing
      ff _ _ = Nothing
  in
    case foldl1 ff handlst of
      Nothing -> False
      _ -> True

nOfRank :: [PlayingCard] -> [(Rank, Int)]
nOfRank hand =
  let
    rlst = toRankLst hand
    uniquelst = nub hand
    countel :: PlayingCard -> (Rank, Int)
    countel card = ((toRank card), length [x | x <- rlst, (toRank card)==x])
  in
    nub $ map countel uniquelst
  
hasNOfRank :: Int -> [PlayingCard] -> Bool
hasNOfRank i hand =
  case (find (\(_,n) -> i == n) (nOfRank hand)) of
    Just _ -> True
    Nothing -> False

hasNumNOfRank :: Int -> Int -> [PlayingCard] -> Bool
hasNumNOfRank i num hand =
  if (length (filter  (\(_,n) -> i == n) (nOfRank hand))) == num
  then True
  else False


-- |
-- Verify that the best hand of a set of cards is a high card hand,
-- and if so, return a 'PokerHand'. Otherwise, return Nothing.
mkHighCard :: [PlayingCard] -> Maybe PokerHand
mkHighCard hand
  | isValidPokerHand hand =
      if (not $ isPair hand)
         && (not $ isTwoPair hand)
         && (not $ isThreeOfAKind hand)
         && (not $ isStraight hand)
         && (not $ isFlush hand)
         && (not $ isFullHouse hand)
         && (not $ isFourOfAKind hand)
         && (not $ isStraightFlush hand)
         && (not $ isRoyalFlush hand)
      then Just (PokerHand HighCard hand)
      else Nothing
  | otherwise = Nothing

-- |
-- Return True if a hand matches a specific PokerHandType. False otherwise.
isHand :: PokerHandType -> [PlayingCard] -> Bool
isHand HighCard cards = if isHighCard cards then True else False
isHand Pair cards = if isPair cards then True else False
isHand TwoPair cards = if isTwoPair cards then True else False
isHand ThreeOfAKind cards = if isThreeOfAKind cards then True else False
isHand (Straight AceHigh) cards =
  let f (Just (PokerHand (Straight AceHigh) _)) = True
      f _ = False in f $ mkStraight cards
isHand (Straight AceLow) cards =
  let f (Just (PokerHand (Straight AceLow) _)) = True
      f _ = False in f $ mkStraight cards
isHand Flush cards = if isFlush cards then True else False
isHand FullHouse cards = if isFullHouse cards then True else False
isHand FourOfAKind cards = if isFourOfAKind cards then True else False
isHand (StraightFlush AceHigh) cards =
  let f (Just (PokerHand (StraightFlush AceHigh) _)) = True
      f _ = False in f $ mkStraightFlush cards
isHand (StraightFlush AceLow) cards =
  let f (Just (PokerHand (StraightFlush AceLow) _)) = True
      f _ = False in f $ mkStraightFlush cards
isHand RoyalFlush cards = if isRoyalFlush cards then True else False

-- |
-- Verify that the best hand of a set of cards is a high card hand,
-- and if so, return True. Otherwise, return False.
isHighCard :: [PlayingCard] -> Bool
isHighCard hand
  | isJust $ mkHighCard hand = True
  | otherwise = False


-- |
-- Verify that the best hand of a set of cards is a pair hand,
-- and if so, return a 'PokerHand'. Otherwise, return Nothing.
mkPair :: [PlayingCard] -> Maybe PokerHand
mkPair hand
  | isValidPokerHand hand =
      if (hasNumNOfRank 2 1 hand)
         && (not $ isFullHouse hand)
      then Just (PokerHand Pair hand)
      else Nothing
  | otherwise = Nothing

-- |
-- Verify that the best hand of a set of cards is a pair hand,
-- and if so, return True. Otherwise, return False.
isPair :: [PlayingCard] -> Bool
isPair hand
  | isJust $ mkPair hand = True
  | otherwise = False

-- |
-- Verify that the best hand of a set of cards is a two pair,
-- and if so, return a 'PokerHand'. Otherwise, return Nothing.
mkTwoPair :: [PlayingCard] -> Maybe PokerHand
mkTwoPair hand
  | isValidPokerHand hand =
      if (hasNumNOfRank 2 2 hand)
         && (not $ isFullHouse hand) 
      then Just (PokerHand TwoPair hand)
      else Nothing
  | otherwise = Nothing

-- |
-- Verify that the best hand of a set of cards is a two pair hand,
-- and if so, return True. Otherwise, return False.
isTwoPair :: [PlayingCard] -> Bool
isTwoPair hand
  | isJust $ mkTwoPair hand = True
  | otherwise = False

-- |
-- Verify that the best hand of a set of cards is a three-of-a-kind hand,
-- and if so, return a 'PokerHand'. Otherwise, return Nothing.
mkThreeOfAKind :: [PlayingCard] -> Maybe PokerHand
mkThreeOfAKind hand
  | isValidPokerHand hand =
      if (hasNOfRank 3 hand)
         && (not $ isFullHouse hand)
      then Just (PokerHand ThreeOfAKind hand)
      else Nothing
  | otherwise = Nothing

-- |
-- Verify that the best hand of a set of cards is a three-of-a-kind hand,
-- and if so, return True. Otherwise, return False.
isThreeOfAKind :: [PlayingCard] -> Bool
isThreeOfAKind hand
  | isJust $ mkThreeOfAKind hand = True
  | otherwise = False

mkConsecutiveRanks :: [PlayingCard] -> Maybe ([PlayingCard], AceRank)
mkConsecutiveRanks hand =
  let consecHigh h = (hasConsecutiveRanks AceHighRankOrder h)
      consecLow h = (hasConsecutiveRanks AceLowRankOrder h)
      f h2
        | consecHigh h2 = Just (sortCardsBy AceHighRankOrder h2, AceHigh)
        | consecLow h2 = Just (sortCardsBy AceLowRankOrder h2, AceLow)
        | otherwise = Nothing
  in f hand

-- |
-- Verify that the best hand of a set of cards is a straight hand,
-- and if so, return a 'PokerHand'. Otherwise, return Nothing.
mkStraight :: [PlayingCard] -> Maybe PokerHand
mkStraight hand
  | isValidPokerHand hand =
      let consecRanks  = mkConsecutiveRanks hand
          isConsecRanks = isJust consecRanks in
        if isConsecRanks
        && (not $ isRoyalFlush hand)
        && (not $ isStraightFlush hand)
      then Just (PokerHand (Straight $ snd $ fromJust consecRanks) hand)
      else Nothing
  | otherwise = Nothing

-- |
-- Verify that the best hand of a set of cards is a straight hand,
-- and if so, return True. Otherwise, return False.
isStraight :: [PlayingCard] -> Bool
isStraight hand
  | isJust $ mkStraight hand = True
  | otherwise = False
    
-- |
-- Verify that the best hand of a set of cards is a flush hand,
-- and if so, return a 'PokerHand'. Otherwise, return Nothing.
mkFlush :: [PlayingCard] -> Maybe PokerHand
mkFlush hand
  | isValidPokerHand hand =
      if (isSameSuit hand)
         && (not $ isRoyalFlush hand)
         && (not $ isStraightFlush hand) 
      then Just (PokerHand Flush hand)
      else Nothing
  | otherwise = Nothing

-- |
-- Verify that the best hand of a set of cards is a flush hand,
-- and if so, return True. Otherwise, return False.
isFlush :: [PlayingCard] -> Bool
isFlush hand
  | isJust $ mkFlush hand = True
  | otherwise = False

-- |
-- Verify that the best hand of a set of cards is a full house hand,
-- and if so, return a 'PokerHand'. Otherwise, return Nothing.
mkFullHouse :: [PlayingCard] -> Maybe PokerHand
mkFullHouse hand
  | isValidPokerHand hand =
      if (hasNOfRank 3 hand)
         && (hasNOfRank 2 hand)
      then Just (PokerHand FullHouse hand)
      else Nothing
  | otherwise = Nothing

-- |
-- Verify that the best hand of a set of cards is a full house hand,
-- and if so, return True. Otherwise, return False.
isFullHouse :: [PlayingCard] -> Bool
isFullHouse hand
  | isJust $ mkFullHouse hand = True
  | otherwise = False

-- |
-- Verify that the best hand of a set of cards is a four-of-a-kind hand,
-- and if so, return a 'PokerHand'. Otherwise, return Nothing.
mkFourOfAKind :: [PlayingCard] -> Maybe PokerHand
mkFourOfAKind hand
  | isValidPokerHand hand = 
      if (hasNOfRank 4 hand)
      then Just (PokerHand FourOfAKind hand)
      else Nothing
  | otherwise = Nothing

-- |
-- Verify that the best hand of a set of cards is a four-of-a-kind hand,
-- and if so, return True. Otherwise, return False.
isFourOfAKind :: [PlayingCard] -> Bool
isFourOfAKind hand
  | isJust $ mkFourOfAKind hand = True
  | otherwise = False

-- |
-- Verify that the best hand of a set of cards is a straight flush hand,
-- and if so, return a 'PokerHand'. Otherwise, return Nothing.
mkStraightFlush :: [PlayingCard] -> Maybe PokerHand
mkStraightFlush hand
  | isValidPokerHand hand =
      let consecRanks  = mkConsecutiveRanks hand
          isConsecRanks = isJust consecRanks in
        if isConsecRanks
        && (isSameSuit hand)
        && (not $ isRoyalFlush hand)
      then Just (PokerHand (StraightFlush $ snd $ fromJust consecRanks) hand)
      else Nothing
  | otherwise = Nothing

-- |
-- Verify that the best hand of a set of cards is a straight flush hand,
-- and if so, return True. Otherwise, return False.
isStraightFlush :: [PlayingCard] -> Bool
isStraightFlush hand
  | isJust $ mkStraightFlush hand = True
  | otherwise = False

               
-- |
-- Verify that the best hand of a set of cards is a royal flush hand,
-- and if so, return a 'PokerHand'. Otherwise, return Nothing.
mkRoyalFlush :: [PlayingCard] -> Maybe PokerHand
mkRoyalFlush hand 
  | isValidPokerHand hand =
      if (isSameSuit hand)
      then
        let
          slst :: [PlayingCard] = sortCardsBy AceHighRankOrder hand
          rlst = toValueLst slst
        in
          if (rlst == [Ten, Jack, Queen, King, Ace])
          then Just (PokerHand RoyalFlush hand)
          else Nothing
      else Nothing
  | otherwise = Nothing

-- |
-- Verify that the best hand of a set of cards is a royal flush hand,
-- and if so, return True. Otherwise, return False.
isRoyalFlush :: [PlayingCard] -> Bool
isRoyalFlush hand
  | isJust $ mkRoyalFlush hand = True
  | otherwise = False

isValidPokerHand :: [PlayingCard] -> Bool
isValidPokerHand hand 
   | ((length hand) == 5) && ((dedupe hand) == hand) = True
   | otherwise = False


-- |
-- All possible hands of a full deck of playing cards
allPossibleHands :: [[PlayingCard]]
allPossibleHands = choose 5 fullDeck

-- |
-- All royal flushes in a full deck of playing cards.
-- The current implementation traverses the entire list of allPossibleHands,
-- and is not efficient.
allRoyalFlush :: [[PlayingCard]]
allRoyalFlush = [x | x <- allPossibleHands, isRoyalFlush x]

-- |
-- All straight flushes in a full deck of playing cards.
-- The current implementation traverses the entire list of allPossibleHands,
-- and is not efficient.
allStraightFlush :: [[PlayingCard]]
allStraightFlush = [x | x <- allPossibleHands, isStraightFlush x]

-- |
-- All four-of-a-kinds in a full deck of playing cards.
-- The current implementation traverses the entire list of allPossibleHands,
-- and is not efficient.
allFourOfAKind :: [[PlayingCard]]
allFourOfAKind = [x | x <- allPossibleHands, isFourOfAKind x]

-- |
-- All full houses in a full deck of playing cards.
-- The current implementation traverses the entire list of allPossibleHands,
-- and is not efficient.
allFullHouse :: [[PlayingCard]]
allFullHouse = [x | x <- allPossibleHands, isFullHouse x]

-- |
-- All flushes in a full deck of playing cards.
-- The current implementation traverses the entire list of allPossibleHands,
-- and is not efficient.
allFlush :: [[PlayingCard]]
allFlush = [x | x <- allPossibleHands, isFlush x]

-- |
-- All straights in a full deck of playing cards.
-- The current implementation traverses the entire list of allPossibleHands,
-- and is not efficient.
allStraight :: [[PlayingCard]]
allStraight = [x | x <- allPossibleHands, isStraight x]

-- |
-- All three-of-a-kind in a full deck of playing cards.
-- The current implementation traverses the entire list of allPossibleHands,
-- and is not efficient.
allThreeOfAKind :: [[PlayingCard]]
allThreeOfAKind = [x | x <- allPossibleHands, isThreeOfAKind x]

-- |
-- All two pairs in a full deck of playing cards.
-- The current implementation traverses the entire list of allPossibleHands,
-- and is not efficient.
allTwoPair :: [[PlayingCard]]
allTwoPair = [x | x <- allPossibleHands, isTwoPair x]

-- |
-- All pairs in a full deck of playing cards.
-- The current implementation traverses the entire list of allPossibleHands,
-- and is not efficient.
allPair :: [[PlayingCard]]
allPair = [x | x <- allPossibleHands, isPair x]

-- |
-- All high card hands in a full deck of playing cards.
-- The current implementation traverses the entire list of allPossibleHands,
-- and is not efficient.
allHighCard :: [[PlayingCard]]
allHighCard = [x | x <- allPossibleHands, isHighCard x]


