{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- | Implementation of the modified Borda count election method.
--
-- The implementation implements the modified Borda count election
-- method, optionally with different weights for different participants.
--
-- See <https://en.wikipedia.org/wiki/Borda_count>.
--
-- The election runs in two phases. First individual 'Vote's on options
-- from a certain participant are gathered and ranked into the
-- participant's 'Ballot' using the function 'ballot'.
--
-- Then all ballots are gathered and an 'election' is held, resulting in
-- a list of election 'Result's, one for each option.
--
-- Except for the 'Vote' data constructor, you should not use other data
-- constructors, as they do not reflect invariants correctly. However, the
-- 'ballot' and 'election' will enforce variants.
module Data.Voting.BordaCount (
    -- * Voting
    Vote(..)
  , Ballot(..)
  , BallotError(..)
  , ballot

    -- * Election
  , Result(..)
  , Score(..)
  , Zeros(..)
  , ElectionError(..)
  , election
  , election'

    -- * Internal
  , findFirstDuplicateBy
) where

import Data.Function (on)
import Data.List (find, foldl', nubBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

-- | A vote is an attribution of a number of points to an option
data Vote o = Vote {
    voteRanking :: Int
  , voteOption :: o
  } deriving (Eq, Show)


-- | A ballot with options of type @o@ filled in by a participant of type @p@.
data Ballot p o = Ballot {
    ballotParticipant :: p
  , ballotVotes :: [Vote o]
  } deriving (Eq, Show)

-- | Construct a new ballot for a participant from a collection of votes.
--
-- This function constructs a valid ballot or returns an error if the
-- collection of votes is invalid.
ballot :: Eq o => p -> [Vote o] -> Either (BallotError o) (Ballot p o)
ballot p votes = do
    () <- checkDoubleVotes
    () <- checkNonZeros
    return $ Ballot p votes
  where
    zeros    = filter ((== 0) . voteRanking) votes
    nonZeros = filter ((/= 0) . voteRanking) votes

    checkNonZeros =
        case findFirstDuplicateBy ((==) `on` voteRanking) nonZeros of
            Just (v, v') -> Left $ DuplicateRanking (voteOption v) (voteOption v')
            Nothing      -> Right ()

    checkDoubleVotes =
        case findFirstDuplicateBy ((==) `on` voteOption) votes of
            Just (v, _) -> Left $ DuplicateOption (voteOption v)
            Nothing     -> Right ()

-- | Result of a certain option of type @o@ of an election.
data Result o = Result o Score Zeros deriving (Eq, Show)

-- | The weighted score of a 'Result'.
newtype Score = Score Double deriving (Eq, Num, Ord, Real, Show)

-- | The number of weighted zeros a 'Result' got.
newtype Zeros = Zeros Double deriving (Eq, Num, Ord, Real, Show)

-- | Hold an election, collect all the ballots and produce the 'Result'.
--
-- This functions assumes that the 'Ballot's are well formed, as
-- returned by the function 'ballot'.
--
-- Thee function holds the election, and might return an 'ElectionError'
-- on any irregularity.
election :: (Eq p, Ord o)
         => (p -> Double)                        -- ^ Weighing function for participants.
         -> [Ballot p o]                         -- ^ All ballots.
         -> Either (ElectionError p) [Result o]  -- ^ Election result
election weigh ballots = do
     () <- checkUniqueParticipants
     return . M.elems $ foldl' process M.empty ballots
   where
     checkUniqueParticipants =
        case findFirstDuplicateBy ((==) `on` ballotParticipant) ballots of
            Just (b, _) -> Left $ DuplicateParticipant (ballotParticipant b)
            Nothing     -> Right ()

     process m b =
        let w = weigh (ballotParticipant b) in
        foldl' (register w) m (ballotVotes b)

     register w m v
        | 0 <- voteRanking v
        = M.insertWith plus (voteOption v) (Result (voteOption v) 0 (Zeros w)) m

        | n <- voteRanking v
        , s <- fromIntegral n * w
        = M.insertWith plus (voteOption v) (Result (voteOption v) (Score s) 0) m

     plus (Result o s z) (Result _ s' z') = Result o (s + s') (z + z')

-- | Hold an 'election', but with a weight of @1@ for all participants.
--
-- See 'election' for more information.
election' :: (Eq p, Ord o)
          => [Ballot p o]                        -- ^ All ballots
          -> Either (ElectionError p) [Result o] -- ^ Election result
election' = election (const 1.0)

-- | Find the first duplicates, filtered by a function.
findFirstDuplicateBy :: (a -> a -> Bool) -> [a] -> Maybe (a, a)
findFirstDuplicateBy _ [] = Nothing
findFirstDuplicateBy f (x:xs) = case find (f x) xs of
    Just x' -> Just (x, x')
    Nothing -> findFirstDuplicateBy f xs

-- | Errors that can occur when creating a 'Ballot' using 'ballot'.
data BallotError o = DuplicateRanking o o -- ^ The given options have the same ranking
                   | DuplicateOption  o   -- ^ The given option was voted on twice.
                   deriving (Eq, Show)

-- | Error that can occur when holding an 'election'.
newtype ElectionError p = DuplicateParticipant p -- ^ The given participant voted twice.
                        deriving (Eq, Show)
