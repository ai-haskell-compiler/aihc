{-# LANGUAGE TupleSections #-}

module Test.Util where

import qualified Test.QuickCheck as Q

import qualified Control.Exception as E
import qualified Control.Monad     as M

import qualified Data.List as L

orElse :: IO x -> IO x -> IO x
orElse io1 io2 = io1 `E.catch` \ (E.SomeException _) -> io2

oneOf :: [IO x] -> IO x
oneOf (io:[])  = io
oneOf (io:ios) = io `orElse` oneOf ios
oneOf []       = error "actions must include at least one element"

oneOfWithIndex :: [IO x] -> IO (x, Int)
oneOfWithIndex = go 0
  where
    go _ []       = error "actions must include at least one element"
    go x (io:[])  = (, x) <$> io
    go x (io:ios) = ((, x) <$> io)
        `E.catch` \ (E.SomeException _) -> go (x+1) ios

genSatisfy :: Q.Arbitrary a => Int -> (a -> Bool) -> IO [a]
genSatisfy num f = M.replicateM num . Q.generate $ Q.arbitrary `Q.suchThat` f

pickUp :: Int -> [x] -> (x, [x])
pickUp idx xs = (rx, hxs ++ drop 1 txs)
  where
    (hxs, txs) = L.splitAt idx xs
    rx = case take 1 txs of
        r:_ -> r
        _   -> error "invalid index to pickUp"

