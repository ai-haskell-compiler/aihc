{-# LANGUAGE OverloadedLists #-}

module Math.OEIS (
  -- * Functions
  searchSeq,  searchSeq',
  lookupSeq,  lookupSeq',
  getSeqData, getSeqData',
  extendSeq,  extendSeq',

  -- * Types
  SeqData,
  SearchStatus(..),
  Keyword(..),
  OEISSeq(..)
  ) where

import           Data.Functor
import           Data.List
import           Data.Maybe         (fromMaybe, listToMaybe)
import qualified Data.Vector        as V
import           System.IO.Unsafe   (unsafePerformIO)

import           Math.OEIS.Internal
import           Math.OEIS.Types


-- | Get all search results on OEIS
--
-- e.g.
--
-- > ghci>searchSeq (ID "A000027") 0
-- > [OEIS {number = "A000027", ids = ["M0472","N0173"], seqData = [1,2,3,4,5,6,7,...
--
-- > ghci>searchSeq (SubSeq [1,2,3,4]) 0
-- > [OEIS {number = "A000027", ids = ["M0472","N0173"], seqData = [1,2,3,4,5,6,7,...
-- > ghci>length it
-- > 53
-- > ghci>searchSeq (SubSeq [1,2,3,4]) 17
-- > [OEIS {number = "A000027", ids = ["M0472","N0173"], seqData = [1,2,3,4,5,6,7,8,9,
-- > ghci>length it
-- > 17
--
-- > ghci>searchSeq (SubSeq [1,1,4,5,1,4,1,9,1,9,8,9,3]) 0
-- > []
searchSeq :: SearchStatus -> Int -> V.Vector OEISSeq
searchSeq ss = unsafePerformIO . searchSeq' ss

-- | searchSeq in IO
searchSeq' :: SearchStatus -> Int -> IO (V.Vector OEISSeq)
searchSeq' ss bound = do
  results' <- getResults ss 0 bound []
  let seqs
        | V.null results' = []
        | otherwise       = parseOEIS <$> results'
  return seqs


-- | Look up a sequence on OEIS.
--
-- e.g.
--
-- > ghci>lookupSeq (ID "A000027")
-- > Just (OEIS {number = "A000027", ids = ["M0472","N0173"], seqData = [1,2,3,4,5,6,7,...
--
-- > ghci>lookupSeq (SubSeq [1,2,3,4])
-- > Just (OEIS {number = "A000027", ids = ["M0472","N0173"], seqData = [1,2,3,4,5,6,7,...
lookupSeq :: SearchStatus -> Maybe OEISSeq
lookupSeq = unsafePerformIO . lookupSeq'

-- | lookupSeq in IO
lookupSeq' :: SearchStatus -> IO (Maybe OEISSeq)
lookupSeq' ss = do
  result <- getResult ss 0
  return $ case result of
             Just result' -> Just $ parseOEIS result'
             _            -> Nothing


-- | Get sub-sequence on OEIS.
--
-- e.g.
--
-- > ghci>getSeqData (ID "A000027")
-- > Just [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77]
--
-- > ghci>getSeqData (SubSeq [1,2,3,4])
-- > Just [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77]
--
getSeqData :: SearchStatus -> Maybe SeqData
getSeqData = unsafePerformIO . getSeqData'

-- | getSeqData in IO
getSeqData' :: SearchStatus -> IO (Maybe SeqData)
getSeqData' = ((seqData <$>) <$>) . lookupSeq'


-- | Extend from sub-sequence.
--
-- e.g.
--
-- > ghci>extendSeq [1,2,3,4]
-- > [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77]
--
-- > ghci> extendSeq [1,3,2,5,6,1,6]
-- > [1,3,2,5,6,1,6]
extendSeq :: SeqData -> SeqData
extendSeq = unsafePerformIO . extendSeq'

-- | extendSeq in IO
extendSeq' :: [Integer] -> IO [Integer]
extendSeq' [] = return []
extendSeq' sd = do
  oeis <- lookupSeq' (SubSeq sd)
  return $ case oeis of
    Just s -> extend sd (seqData s)
    _      -> sd
  where
    extend :: SeqData -> SeqData -> SeqData
    extend sd ext = fromMaybe sd . find (sd `isPrefixOf`) $ tails ext
