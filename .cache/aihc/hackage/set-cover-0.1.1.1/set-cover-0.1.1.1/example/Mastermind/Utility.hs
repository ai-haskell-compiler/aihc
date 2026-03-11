module Mastermind.Utility where

import qualified Data.Map as Map; import Data.Map (Map, )


histogram :: (Ord a) => [a] -> Map a Int
histogram = Map.fromListWith (+) . attach 1

{-# INLINE attach #-}
attach :: b -> [a] -> [(a, b)]
attach a = map (flip (,) a)
