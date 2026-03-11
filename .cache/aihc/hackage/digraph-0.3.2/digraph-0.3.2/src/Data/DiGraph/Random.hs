{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Module: Data.DiGraph.Random
-- Copyright: Copyright © 2019 - 2020 Kadena LLC.
-- License: MIT
-- Maintainer: Lars Kuhtz <lars@kadena.io>
-- Stability: experimental
--
-- Throughout the module an undirected graph is a directed graph that is
-- symmetric and irreflexive.
--
--
module Data.DiGraph.Random
(
-- * Random Regular Graph
  UniformRng
, rrgIO
, rrg

-- * Random Graphs in the \(G_{n,p}\) model
, gnp
) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe

import qualified Data.Set as S

import Numeric.Natural

import qualified Streaming.Prelude as S

import qualified System.Random.MWC as MWC

-- internal modules

import Data.DiGraph

-- -------------------------------------------------------------------------- --
-- Utils

-- | Type of a random number generator that uniformily chooses an element from a
-- range.
--
type UniformRng m = (Int, Int) -> m Int

int :: Integral a => Num b => a -> b
int = fromIntegral
{-# INLINE int #-}

-- -------------------------------------------------------------------------- --
-- Random Regular Graph

-- | Undirected, irreflexive random regular graph.
--
-- The algorithm here is incomplete. For a complete approach see for instance
-- https://users.cecs.anu.edu.au/~bdm/papers/RandRegGen.pdf
--
rrgIO
    :: Natural
    -> Natural
    -> IO (Maybe (DiGraph Int))
rrgIO n d = do
    gen <- MWC.createSystemRandom
    rrg @IO (`MWC.uniformR` gen) n d

-- | Undirected, irreflexive random regular graph.
--
-- The algorithm here is incomplete. For a complete approach see for instance
-- https://users.cecs.anu.edu.au/~bdm/papers/RandRegGen.pdf
--
rrg
    :: Monad m
    => UniformRng m
        -- ^ a uniform random number generator
    -> Natural
    -> Natural
    -> m (Maybe (DiGraph Int))
rrg gen n d = go 0 (S.fromList c) (emptyGraph n)
  where
    v = [0 .. int n - 1]
    c = [(x, y) | x <- v, y <- [0 :: Int .. int d - 1]]

    go i s g
        | S.null s = return $ Just g
        | (fst . fst <$> S.minView s) == (fst . fst <$> S.maxView s) = return Nothing
        | otherwise = sampleEdge s g >>= \case
            Nothing -> if i < n then go (i + 1) s g else return Nothing
            Just (s', g') -> go 0 s' g'

    sampleEdge s graph = runMaybeT $ do
        (s', v₁) <- lift $ uniformSample gen s
        (s'', v₂) <- lift $ uniformSample gen s'
        let e₁ = (fst v₁, fst v₂)
        let e₂ = (fst v₂, fst v₁)
        guard $ fst v₁ /= fst v₂ && not (isEdge e₁ graph)
        return (s'', insertEdge e₁ $ insertEdge e₂ graph)

-- | Uniformily sample an element from the input set. Returns the set with the
-- sampled element removed and the sampled element.
--
uniformSample :: Monad m => UniformRng m -> S.Set a -> m (S.Set a, a)
uniformSample gen s = do
    p <- gen (0, S.size s - 1)
    return (S.deleteAt p s, S.elemAt p s)

-- -------------------------------------------------------------------------- --
-- Gnp

-- | Undirected irreflexive random graph in the \(G_{n,p}\) model.
--
gnp
    :: forall m
    . Monad m
    => UniformRng m
    -> Natural
    -> Double
    -> m (DiGraph Int)
gnp gen n p = S.fold_ (flip insertEdge) (emptyGraph n) id
    $ S.concat
    $ S.filterM (const choice)
    $ S.each
        [ [(a,b), (b,a)]
        | a <- [0..int n - 1]
        , b <- [0..a-1]
        ]
  where
    choice = do
        v <- gen (0, maxBound)
        return $ int v <= p * int (maxBound :: Int)
