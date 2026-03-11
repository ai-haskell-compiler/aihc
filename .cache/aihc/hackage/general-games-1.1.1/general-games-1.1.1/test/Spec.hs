import Control.Monad.Random
import Test.Hspec
import Game.Game.Poker
import Game.Implement.Card
import Game.Implement.Card.Standard
import Game.Implement.Card.Standard.Poker


allHandsCount :: Int
allHandsCount = length allPossibleHands
allHandsCountExpected :: Int
allHandsCountExpected = 2598960
allRoyalFlushCountExpected :: Int
allRoyalFlushCountExpected = 4
allStraightFlushCountExpected :: Int
allStraightFlushCountExpected = 36
allFourOfAKindCountExpected :: Int
allFourOfAKindCountExpected = 624
allFullHouseCountExpected :: Int
allFullHouseCountExpected = 3744
allFlushCountExpected :: Int
allFlushCountExpected = 5108
allStraightCountExpected :: Int
allStraightCountExpected = 10200
allThreeOfAKindCountExpected :: Int
allThreeOfAKindCountExpected = 54912
allTwoPairCountExpected :: Int
allTwoPairCountExpected = 123552
allPairCountExpected :: Int
allPairCountExpected = 1098240
allHighCardCountExpected :: Int
allHighCardCountExpected = 1302540

royalFlush :: [PlayingCard]
royalFlush =
  [PlayingCard Ace Hearts,
   PlayingCard Queen Hearts,
   PlayingCard King Hearts,
   PlayingCard Jack Hearts,
   PlayingCard Ten Hearts]

royalFlushNot :: [PlayingCard]
royalFlushNot =
  [PlayingCard Ace Hearts,
   PlayingCard Queen Hearts,
   PlayingCard Eight Hearts,
   PlayingCard Jack Hearts,
   PlayingCard Ten Hearts]

drawDeck :: [PlayingCard]
drawDeck =
  [PlayingCard Five Diamonds,
   PlayingCard Seven Clubs,
   PlayingCard Two Spades,
   PlayingCard King Spades,
   PlayingCard King Hearts,
   PlayingCard Ace Diamonds,
   PlayingCard Seven Diamonds,
   PlayingCard Three Clubs,
   PlayingCard Four Clubs]


drawDeckSizes :: [Int]
drawDeckSizes = [1,4,2]

drawDeckSizesFail :: [Int]
drawDeckSizesFail = [5,9]

drawDeckSizesFailNeg :: [Int]
drawDeckSizesFailNeg = [-3,4]


drawDeckExpectedOutput :: Maybe ([[PlayingCard]],[PlayingCard])
drawDeckExpectedOutput = Just
  ([[PlayingCard Five Diamonds],
   [PlayingCard Seven Clubs,
   PlayingCard Two Spades,
   PlayingCard King Spades,
   PlayingCard King Hearts],
   [PlayingCard Ace Diamonds,
   PlayingCard Seven Diamonds]],
   [PlayingCard Three Clubs,
   PlayingCard Four Clubs])

confirmDisjoint :: (Int, Bool)
confirmDisjoint =
  let mfunc1 hand = [mkRoyalFlush hand,
                     mkStraightFlush hand,
                     mkFourOfAKind hand,
                     mkFullHouse hand,
                     mkFlush hand,
                     mkStraight hand,
                     mkThreeOfAKind hand,
                     mkTwoPair hand,
                     mkPair hand,
                     mkHighCard hand]
      maybem (Just _) = 1
      maybem Nothing = 0
      countJust hand = sum $ map maybem $ mfunc1 hand
      allSums = map countJust allPossibleHands
      collect _  (outsum, False) = (outsum, False)
      collect (x:xs) (outsum, _) =
        collect xs (x+outsum, if x==0 || x==1 then True else False)
      collect [] output = output 
  in
    collect allSums (0, True)

isUnique :: Eq a => [a] -> Bool
isUnique lst = f lst True where
  f _ False = False
  f [] result = result
  f (x:xs) _ = if x `elem` xs then f xs False else f xs True

shuffledDeck :: RandomGen g => Rand g [PlayingCard]
shuffledDeck = shuffle $ fullDeck

clubsDefaultSortAceLow :: [PlayingCard]
clubsDefaultSortAceLow =
  let clubs = flip PlayingCard Clubs <$> ranks
  in sortCardsBy AceLowRankOrder $ clubs

clubsDefaultSortAceHigh :: [PlayingCard]
clubsDefaultSortAceHigh =
  let clubs = flip PlayingCard Clubs <$> ranks
  in sortCardsBy AceHighRankOrder $ clubs


main :: IO ()
main =
  do
    randdecks <- evalRandIO $ replicateM 10000 shuffledDeck;

    randHighCards <-
      let
        r = do
          h <- randomHighCard
          return (h, isHighCard $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r

    randPairs <-
      let
        r = do
          h <- randomPair
          return (h, isPair $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r

    randTwoPairs <-
      let
        r = do
          h <- randomTwoPair
          return (h, isTwoPair $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r

    randThreeOfAKinds <-
      let
        r = do
          h <- randomThreeOfAKind
          return (h, isThreeOfAKind $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r

    randStraights <-
      let
        r = do
          h <- randomStraight
          return (h, isStraight $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r

    randFlushes <-
      let
        r = do
          h <- randomFlush
          return (h, isFlush $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r

    randFullHouses <-
      let
        r = do
          h <- randomFullHouse
          return (h, isFullHouse $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r

    randFourOfAKinds <-
      let
        r = do
          h <- randomFourOfAKind
          return (h, isFourOfAKind $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r

    randStraightFlushes <-
      let
        r = do
          h <- randomStraightFlush
          return (h, isStraightFlush $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r

    randRoyalFlushes <-
      let
        r = do
          h <- randomRoyalFlush
          return (h, isRoyalFlush $ cardsOfPokerHand h)
      in evalRandIO $ replicateM 100000 r

    hspec $ do
      describe "Game.Implement.Card.draw (PlayingCard)" $ do
        it "returns drawn hands from a deck, plus the remaining deck" $ do
          (draw drawDeckSizes drawDeck) `shouldBe` drawDeckExpectedOutput
        it "returns Nothing when trying to return more cards than in deck" $ do
          (draw drawDeckSizesFail drawDeck) `shouldBe` Nothing
        it "returns Nothing when trying to return negative cards" $ do
          (draw drawDeckSizesFailNeg drawDeck) `shouldBe` Nothing

      describe "Game.Implement.Card.fullDeck (PlayingCard)" $ do
        it "returns 52 cards" $ do
          length (fullDeck :: [PlayingCard]) `shouldBe` 52
        it "returns unique cards" $ do
          isUnique (fullDeck :: [PlayingCard]) `shouldBe` True

      describe "Game.Game.Poker.compareCardBy" $ do
        it "orders low to high by default, check ace low" $ do
          clubsDefaultSortAceLow `shouldBe` [PlayingCard Ace Clubs, PlayingCard Two Clubs, PlayingCard Three Clubs, PlayingCard Four Clubs, PlayingCard Five Clubs, PlayingCard Six Clubs, PlayingCard Seven Clubs, PlayingCard Eight Clubs, PlayingCard Nine Clubs, PlayingCard Ten Clubs, PlayingCard Jack Clubs, PlayingCard Queen Clubs, PlayingCard King Clubs]
        it "orders low to high by default, check ace high" $ do
          clubsDefaultSortAceHigh `shouldBe` [PlayingCard Two Clubs, PlayingCard Three Clubs, PlayingCard Four Clubs, PlayingCard Five Clubs, PlayingCard Six Clubs, PlayingCard Seven Clubs, PlayingCard Eight Clubs, PlayingCard Nine Clubs, PlayingCard Ten Clubs, PlayingCard Jack Clubs, PlayingCard Queen Clubs, PlayingCard King Clubs, PlayingCard Ace Clubs]

          

      describe "Game.Game.Poker.isHand" $ do
        it "confirms that an Ace low straight flush exists" $ do
          isHand (StraightFlush AceLow) [PlayingCard Ace Spades, PlayingCard Two Spades, PlayingCard Three Spades, PlayingCard Four Spades, PlayingCard Five Spades] `shouldBe` True
        it "confirms that an Ace high straight flush is not Ace low" $ do
          isHand (StraightFlush AceLow) [PlayingCard Six Spades, PlayingCard Two Spades, PlayingCard Three Spades, PlayingCard Four Spades, PlayingCard Five Spades] `shouldBe` False
        it "confirms that an Ace high straight flush exists" $ do
          isHand (StraightFlush AceHigh) [PlayingCard Six Spades, PlayingCard Two Spades, PlayingCard Three Spades, PlayingCard Four Spades, PlayingCard Five Spades] `shouldBe` True
  
      describe "Game.Implement.Card.Standard.Poker.isRoyalFlush" $ do
        it "confirms that [AH, QH, KH, JH, TH] is a Royal Flush" $ do
          (isRoyalFlush royalFlush) `shouldBe` True
        it "confirms that [AH, QH, 8H, JH, TH] is not a Royal Flush" $ do
          (isRoyalFlush royalFlushNot) `shouldBe` False

      describe "Game.Game.Poker.randomHighCard" $ do
        it "returns a HighCard of typeOfPokerHand HighCard" $ do
           hand <- evalRandIO randomHighCard
           typeOfPokerHand hand `shouldBe` HighCard
        it "returns 100000 random HighCards" $ do 
          randHighCards `shouldBe` (map (\(h,_) -> (h,True)) randHighCards) 

      describe "Game.Game.Poker.randomPair" $ do
        it "returns a Pair of typeOfPokerHand Pair" $ do
           hand <- evalRandIO randomPair
           typeOfPokerHand hand `shouldBe` Pair
        it "returns 100000 random Pairs" $ do 
          randPairs `shouldBe` (map (\(h,_) -> (h,True)) randPairs) 

      describe "Game.Game.Poker.randomTwoPair" $ do
        it "returns a TwoPair of typeOfPokerHand TwoPair" $ do
           hand <- evalRandIO randomTwoPair
           typeOfPokerHand hand `shouldBe` TwoPair
        it "returns 100000 random TwoPairs" $ do 
          randTwoPairs `shouldBe` (map (\(h,_) -> (h,True)) randTwoPairs) 
        
      describe "Game.Game.Poker.randomThreeOfAKind" $ do
        it "returns a ThreeOfAKind of typeOfPokerHand ThreeOfAKind" $ do
           hand <- evalRandIO randomThreeOfAKind
           typeOfPokerHand hand `shouldBe` ThreeOfAKind
        it "returns 100000 random ThreeOfAKinds" $ do 
          randThreeOfAKinds `shouldBe` (map (\(h,_) -> (h,True)) randThreeOfAKinds) 
        
      describe "Game.Game.Poker.randomStraight" $ do
        it "returns a Straight of typeOfPokerHand Straight" $ do
           hand <- evalRandIO randomStraight
           typeOfPokerHand hand `shouldSatisfy`
             \t -> t == Straight AceHigh || t == Straight AceLow
        it "returns 100000 random Straights" $ do 
          randStraights `shouldBe` (map (\(h,_) -> (h,True)) randStraights) 

      describe "Game.Game.Poker.randomFlush" $ do
        it "returns a Flush of typeOfPokerHand Flush" $ do
           hand <- evalRandIO randomFlush
           typeOfPokerHand hand `shouldBe` Flush
        it "returns 100000 random Flushes" $ do 
          randFlushes `shouldBe` (map (\(h,_) -> (h,True)) randFlushes) 
        
      describe "Game.Game.Poker.randomFullHouse" $ do
        it "returns a FullHouse of typeOfPokerHand FullHouse" $ do
           hand <- evalRandIO randomFullHouse
           typeOfPokerHand hand `shouldBe` FullHouse
        it "returns 100000 random Full Houses" $ do 
          randFullHouses `shouldBe` (map (\(h,_) -> (h,True)) randFullHouses)

      describe "Game.Game.Poker.randomFourOfAKind" $ do
        it "returns a FourOfAKind of typeOfPokerHand FourOfAKind" $ do
           hand <- evalRandIO randomFourOfAKind
           typeOfPokerHand hand `shouldBe` FourOfAKind
        it "returns 100000 random Four-of-a-Kinds" $ do 
          randFourOfAKinds `shouldBe` (map (\(h,_) -> (h,True)) randFourOfAKinds)


      describe "Game.Game.Poker.randomStraightFlush" $ do
        it "returns a StraightFlush of typeOfPokerHand StraightFlush" $ do
           hand <- evalRandIO randomStraightFlush
           typeOfPokerHand hand `shouldSatisfy`
             \t -> t == StraightFlush AceHigh || t == StraightFlush AceLow
        it "returns 100000 random Straight Flushes" $ do 
          randStraightFlushes `shouldBe` (map (\(h,_) -> (h,True)) randStraightFlushes) 

      describe "Game.Game.Poker.randomRoyalFlush" $ do
        it "returns a RoyalFlush of typeOfPokerHand RoyalFlush" $ do
           hand <- evalRandIO randomRoyalFlush
           typeOfPokerHand hand `shouldBe` RoyalFlush
        it "returns 100000 random Royal Flushes" $ do 
          randRoyalFlushes `shouldBe` (map (\(h,_) -> (h,True)) randRoyalFlushes) 



      describe "Game.Implement.Card.shuffle (PlayingCard)" $ do
        it "returns 10000 different fullDeck shuffles using the global random generator" $ do
          (isUnique randdecks) `shouldBe` True

          
      describe "Game.Game.Poker allPossibleHands / mkHand / isHand functions" $ do
        it "confirms that sets of each hand are disjoint and that total count correct" $ do
          confirmDisjoint `shouldBe` (allHandsCountExpected, True)
        it "confirms the total number of poker hands" $ do
          allHandsCount `shouldBe` allHandsCountExpected
        it "confirms the total number of royal flushes" $ do
          (length allRoyalFlush) `shouldBe` allRoyalFlushCountExpected
        it "confirms the total number of straight flushes" $ do
          (length allStraightFlush) `shouldBe` allStraightFlushCountExpected
        it "confirms the total number of four-of-a-kinds" $ do
          (length allFourOfAKind) `shouldBe` allFourOfAKindCountExpected
        it "confirms the total number of full houses" $ do
          (length allFullHouse) `shouldBe` allFullHouseCountExpected
        it "confirms the total number of flushes" $ do
          (length allFlush) `shouldBe` allFlushCountExpected
        it "confirms the total number of straights" $ do
          (length allStraight) `shouldBe` allStraightCountExpected
        it "confirms the total number of three-of-a-kinds" $ do
          (length allThreeOfAKind) `shouldBe` allThreeOfAKindCountExpected
        it "confirms the total number of two-pairs" $ do
          (length allTwoPair) `shouldBe` allTwoPairCountExpected
        it "confirms the total number of pairs" $ do
          (length allPair) `shouldBe` allPairCountExpected
        it "confirms the total number of high card hands" $ do
          (length allHighCard) `shouldBe` allHighCardCountExpected
          
        

