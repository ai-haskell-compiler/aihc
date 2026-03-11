module Main (main) where

import Control.Exception
import Data.Semigroup
import Data.Tuple.Strict

infixr 5 ===
(===) :: Eq a => a -> a -> IO ()
x === y
  | x == y = pure ()
  | otherwise = throwIO (userError "")

times2 :: Semigroup a => a -> a
times2 = stimes (2 :: Int)

main :: IO ()
main = do
  T1 "" === mempty
  T2 "" "" === mempty
  T3 "" "" "" === mempty
  T4 "" "" "" "" === mempty
  T5 "" "" "" "" "" === mempty
  T6 "" "" "" "" "" "" === mempty
  T7 "" "" "" "" "" "" "" === mempty
  T8 "" "" "" "" "" "" "" "" === mempty
  T9 "" "" "" "" "" "" "" "" "" === mempty
  T10 "" "" "" "" "" "" "" "" "" "" === mempty
  T11 "" "" "" "" "" "" "" "" "" "" "" === mempty
  T12 "" "" "" "" "" "" "" "" "" "" "" "" === mempty
  T13 "" "" "" "" "" "" "" "" "" "" "" "" "" === mempty
  T14 "" "" "" "" "" "" "" "" "" "" "" "" "" "" === mempty
  T15 "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" === mempty
  T16 "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" === mempty
  T17 "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" === mempty
  T18 "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" === mempty
  T19 "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" === mempty

  T1 "a1a2" === T1 "a1" <> T1 "a2"
  T2 "a1a2" "b1b2" === T2 "a1" "b1" <> T2 "a2" "b2"
  T3 "a1a2" "b1b2" "c1c2" === T3 "a1" "b1" "c1" <> T3 "a2" "b2" "c2"
  T4 "a1a2" "b1b2" "c1c2" "d1d2" === T4 "a1" "b1" "c1" "d1" <> T4 "a2" "b2" "c2" "d2"
  T5 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" === T5 "a1" "b1" "c1" "d1" "e1" <> T5 "a2" "b2" "c2" "d2" "e2"
  T6 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" === T6 "a1" "b1" "c1" "d1" "e1" "f1" <> T6 "a2" "b2" "c2" "d2" "e2" "f2"
  T7 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" === T7 "a1" "b1" "c1" "d1" "e1" "f1" "g1" <> T7 "a2" "b2" "c2" "d2" "e2" "f2" "g2"
  T8 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" "h1h2" === T8 "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" <> T8 "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2"
  T9 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" "h1h2" "i1i2" === T9 "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1" <> T9 "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2"
  T10 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" "h1h2" "i1i2" "j1j2" === T10 "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1" "j1" <> T10 "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2" "j2"
  T11 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" "h1h2" "i1i2" "j1j2" "k1k2" === T11 "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1" "j1" "k1" <> T11 "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2" "j2" "k2"
  T12 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" "h1h2" "i1i2" "j1j2" "k1k2" "l1l2" === T12 "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1" "j1" "k1" "l1" <> T12 "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2" "j2" "k2" "l2"
  T13 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" "h1h2" "i1i2" "j1j2" "k1k2" "l1l2" "m1m2" === T13 "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1" "j1" "k1" "l1" "m1" <> T13 "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2" "j2" "k2" "l2" "m2"
  T14 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" "h1h2" "i1i2" "j1j2" "k1k2" "l1l2" "m1m2" "n1n2" === T14 "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1" "j1" "k1" "l1" "m1" "n1" <> T14 "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2" "j2" "k2" "l2" "m2" "n2"
  T15 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" "h1h2" "i1i2" "j1j2" "k1k2" "l1l2" "m1m2" "n1n2" "o1o2" === T15 "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1" "j1" "k1" "l1" "m1" "n1" "o1" <> T15 "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2" "j2" "k2" "l2" "m2" "n2" "o2"
  T16 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" "h1h2" "i1i2" "j1j2" "k1k2" "l1l2" "m1m2" "n1n2" "o1o2" "p1p2" === T16 "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1" "j1" "k1" "l1" "m1" "n1" "o1" "p1" <> T16 "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2" "j2" "k2" "l2" "m2" "n2" "o2" "p2"
  T17 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" "h1h2" "i1i2" "j1j2" "k1k2" "l1l2" "m1m2" "n1n2" "o1o2" "p1p2" "q1q2" === T17 "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1" "j1" "k1" "l1" "m1" "n1" "o1" "p1" "q1" <> T17 "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2" "j2" "k2" "l2" "m2" "n2" "o2" "p2" "q2"
  T18 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" "h1h2" "i1i2" "j1j2" "k1k2" "l1l2" "m1m2" "n1n2" "o1o2" "p1p2" "q1q2" "r1r2" === T18 "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1" "j1" "k1" "l1" "m1" "n1" "o1" "p1" "q1" "r1" <> T18 "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2" "j2" "k2" "l2" "m2" "n2" "o2" "p2" "q2" "r2"
  T19 "a1a2" "b1b2" "c1c2" "d1d2" "e1e2" "f1f2" "g1g2" "h1h2" "i1i2" "j1j2" "k1k2" "l1l2" "m1m2" "n1n2" "o1o2" "p1p2" "q1q2" "r1r2" "s1s2" === T19 "a1" "b1" "c1" "d1" "e1" "f1" "g1" "h1" "i1" "j1" "k1" "l1" "m1" "n1" "o1" "p1" "q1" "r1" "s1" <> T19 "a2" "b2" "c2" "d2" "e2" "f2" "g2" "h2" "i2" "j2" "k2" "l2" "m2" "n2" "o2" "p2" "q2" "r2" "s2"

  times2 (T1 "a") === T1 "aa"
  times2 (T2 "a" "a") === T2 "aa" "aa"
  times2 (T3 "a" "a" "a") === T3 "aa" "aa" "aa"
  times2 (T4 "a" "a" "a" "a") === T4 "aa" "aa" "aa" "aa"
  times2 (T5 "a" "a" "a" "a" "a") === T5 "aa" "aa" "aa" "aa" "aa"
  times2 (T6 "a" "a" "a" "a" "a" "a") === T6 "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T7 "a" "a" "a" "a" "a" "a" "a") === T7 "aa" "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T8 "a" "a" "a" "a" "a" "a" "a" "a") === T8 "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T9 "a" "a" "a" "a" "a" "a" "a" "a" "a") === T9 "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T10 "a" "a" "a" "a" "a" "a" "a" "a" "a" "a") === T10 "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T11 "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a") === T11 "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T12 "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a") === T12 "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T13 "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a") === T13 "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T14 "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a") === T14 "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T15 "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a") === T15 "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T16 "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a") === T16 "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T17 "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a") === T17 "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T18 "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a") === T18 "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa"
  times2 (T19 "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a" "a") === T19 "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa" "aa"
