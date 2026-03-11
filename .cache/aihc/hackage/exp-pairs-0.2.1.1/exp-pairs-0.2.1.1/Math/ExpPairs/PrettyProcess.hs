{-|
Module      : Math.ExpPairs.PrettyProcess
Copyright   : (c) Andrew Lelechenko, 2015-2020
License     : GPL-3
Maintainer  : andrew.lelechenko@gmail.com

Transforms sequences of 'Process' into most compact (by the means of typesetting) representation using brackets and powers.
E. g., AAAABABABA -> A^4(BA)^3.
-}
{-# LANGUAGE LambdaCase      #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Math.ExpPairs.PrettyProcess
  ( prettify,
    uglify,
    PrettyProcess) where

import Data.List                (minimumBy, inits, tails, uncons)
import Data.Monoid              (mempty)
import Data.Ord                 (comparing)
import Prettyprinter

import qualified Data.Map as M
import qualified Data.Set as S

import Math.ExpPairs.ProcessMatrix

-- | Compact representation of the sequence of 'Process',
-- using brackets and powers.
data PrettyProcess
  = Simply [Process]
  | Repeat PrettyProcess Int
  | Sequence PrettyProcess PrettyProcess
  deriving (Show)

data PrettyProcessWithWidth = PPWL { ppwlProcess :: PrettyProcess, ppwlWidth :: Int }

instance Pretty PrettyProcess where
  pretty = \case
    Simply xs    -> hsep (map (pretty . show) xs)
    Repeat _  0  -> mempty
    Repeat xs 1  -> pretty xs
    Repeat (Simply [A]) n -> pretty (show A) <> pretty "^" <> pretty n
    Repeat xs n  -> parens (pretty xs) <> pretty "^" <> pretty n
    Sequence a b -> pretty a <+> pretty b

-- | Width of the bracket symbol.
bracketWidth :: Int
bracketWidth = 4

-- | Width of the subscript-sized digit (e. g., power).
subscriptWidth :: Int
subscriptWidth = 4

-- | Width of the process symbols.
processWidth :: Process -> Int
processWidth  A = 10
processWidth BA = 20

-- | Compute the width of the 'PrettyProcess'
-- according to 'bracketWidth', 'subscriptWidth' and 'printedWidth''.
printedWidth :: PrettyProcess -> Int
printedWidth = \case
  Simply xs             -> sum (map processWidth xs)
  Repeat _ 0            -> 0
  Repeat xs 1           -> printedWidth xs
  Repeat (Simply [A]) _ -> processWidth A + subscriptWidth
  Repeat xs _           -> printedWidth xs + bracketWidth * 2 + subscriptWidth
  Sequence a b          -> printedWidth a + printedWidth b

-- | Convert 'PrettyProcess' to 'PrettyProcessWithWidth'.
annotateWithWidth :: PrettyProcess -> PrettyProcessWithWidth
annotateWithWidth p = PPWL p (printedWidth p)

-- | List divisors.
divisors :: Int -> [Int]
divisors n = ds1 ++ reverse ds2 where
  (ds1, ds2) = unzip [ (a, n `div` a) | a <- [1 .. sqrtint n], n `mod` a == 0 ]
  sqrtint = round . sqrt . fromIntegral

-- | Try to represent list as a replication of list.
asRepeat :: [Process] -> ([Process], Int)
asRepeat [] = ([], 0)
asRepeat xs = pair where
  l = length xs
  candidates = [ (take d xs, l `div` d) | d <- divisors l ]
  pair = maybe (xs, 1) fst $ uncons $
    filter (\(ys, n) -> concat (replicate n ys) == xs) candidates

-- | Find the most compact representation of the sequence of processes.
prettify :: [Process] -> PrettyProcess
prettify = ppwlProcess . prettifyP

-- | Find the most compact representation of the sequence of processes, keeping track of widthess.
prettifyP :: [Process] -> PrettyProcessWithWidth
prettifyP ps = (M.!) cache ps
  where
    keys = S.fromList $ concatMap inits (tails ps)
    cache = M.fromSet alg keys

    alg :: [Process] -> PrettyProcessWithWidth
    alg = \case
      []   -> annotateWithWidth (Simply [])
      [A]  -> annotateWithWidth (Simply [A])
      [BA] -> annotateWithWidth (Simply [BA])
      xs@(xsHd : xsTl) -> minimumBy (comparing ppwlWidth) yss where
        xs'' = case asRepeat xs of
          (_, 1)   -> annotateWithWidth (Simply xs)
          (xs', n) -> annotateWithWidth (Repeat (ppwlProcess $ (M.!) cache xs') n)

        yss :: [PrettyProcessWithWidth]
        yss = xs'' : map f bcs

        bcs :: [([Process], [Process])]
        bcs = takeWhile (not . null . snd) $ iterate bcf ([xsHd], xsTl)

        bcf :: ([a], [a]) -> ([a], [a])
        bcf (_, [])    = error "prettifyP: unexpected second argument of bcf"
        bcf (zs, y:ys) = (zs++[y], ys)

        f (bs, cs) = PPWL (Sequence bsP csP) (bsW + csW) where
          PPWL bsP bsW = (M.!) cache  bs
          PPWL csP csW = (M.!) cache  cs

-- | Unfold back 'PrettyProcess' into the sequence of 'Process'.
uglify :: PrettyProcess -> [Process]
uglify = \case
  Simply xs      -> xs
  Repeat xs n    -> concat . replicate n . uglify $ xs
  Sequence xs ys -> uglify xs ++ uglify ys
