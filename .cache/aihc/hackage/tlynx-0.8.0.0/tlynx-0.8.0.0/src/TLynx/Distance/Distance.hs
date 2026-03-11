{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description :  Compute distances between trees
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Wed May 29 18:09:39 2019.
--
-- - Symmetric (Robinson-Foulds) distance.
-- - Incompatible splits distance.
module TLynx.Distance.Distance
  ( distance,
  )
where

import Control.Monad
  ( unless,
    when,
  )
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader hiding (local)
import Data.Bifunctor
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List hiding (intersect)
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector.Unboxed as V
import ELynx.Tools.ByteString
import ELynx.Tools.ELynx
import ELynx.Tools.Environment
import ELynx.Tools.Logger
import ELynx.Tree
import Statistics.Sample
import System.IO
import TLynx.Distance.Options
import TLynx.Parsers
import Text.Printf

median :: (Ord a) => [a] -> a
median xs = sort xs !! l2 where l2 = length xs `div` 2

pf :: String
pf = "%.3f"

header :: Int -> Int -> DistanceMeasure -> BL.ByteString
header n m d =
  alignLeft (n + 2) "Tree 1"
    <> alignLeft (n + 2) "Tree 2"
    <> alignRight
      (m + 2)
      (BL.pack $ show d)

showTriplet ::
  (PrintfArg a) => Int -> Int -> [String] -> (Int, Int, a) -> BL.ByteString
showTriplet n m args (i, j, d) = i' <> j' <> d'
  where
    i' = alignLeft (n + 2) $ BL.pack (args !! i)
    j' = alignLeft (n + 2) $ BL.pack (args !! j)
    d' = alignRight (m + 2) $ BL.pack (printf pf d)

-- Compute pairwise distances of a list of input trees. Use given distance
-- measure. Returns a triple, the first two elements are the indices of the
-- compared trees, the third is the distance.
pairwise ::
  -- | Distance function
  (a -> a -> b) ->
  -- | Input trees
  [a] ->
  -- | (index i, index j, distance i j)
  [(Int, Int, b)]
pairwise dist trs =
  [ (i, j, dist x y)
    | (i : is, x : xs) <- zip (tails [0 ..]) (tails trs),
      (j, y) <- zip is xs
  ]

-- | Compute distance functions between phylogenetic trees.
distance :: ELynx DistanceArguments ()
distance = do
  l <- localArguments <$> ask
  let nwFormat = argsNewickFormat l
  -- Determine output handle (stdout or file).
  outH <- outHandle "results" ".out"
  -- Master tree (in case it is given).
  let mname = argsMasterTreeFile l
  mtree <- case mname of
    Nothing -> return Nothing
    Just f -> do
      logInfoS $ "Read master tree from file: " <> f <> "."
      t <- liftIO $ parseTree nwFormat f
      logInfoS "Compute distances between all trees and master tree."
      return $ Just t
  let tfps = argsInFiles l
  (trees, names) <- case tfps of
    [] -> error "No tree input files given."
    [tf] -> do
      logInfoS "Read trees from single file."
      ts <- liftIO $ parseTrees nwFormat tf
      logInfoS $ show (length ts) <> " trees found in file."
      logInfoS "Trees are indexed with integers."
      return (ts, map show [0 .. length ts - 1])
    _ -> do
      logInfoS "Read trees from files."
      ts <- liftIO $ mapM (parseTree nwFormat) tfps
      logInfoS "Trees are named according to their file names."
      return (ts, tfps)
  when (null trees) (error "Not enough trees found in files.")
  when
    (isNothing mtree && length trees == 1)
    (error "Not enough trees found in files.")
  -- when (isNothing mtree) $ logInfoS
  --   "Compute pairwise distances between trees from different files."
  logDebugS "The trees are:"
  logDebugB $ BL.unlines $ map toNewick trees
  -- Set the distance measure.
  let dist = argsDistance l
  case argsDistance l of
    Symmetric -> logInfoS "Use symmetric (Robinson-Foulds) distance."
    IncompatibleSplit val -> do
      logInfoS "Use incompatible split distance."
      logInfoS $
        "Collapse nodes with support less than "
          ++ show val
          ++ "."
    BranchScore -> logInfoS "Use branch score distance."
  let distanceMeasure' ::
        Tree Phylo Name ->
        Tree Phylo Name ->
        Double
      distanceMeasure' t1 t2 = either error id $ case dist of
        Symmetric -> second fromIntegral $ symmetric t1 t2
        IncompatibleSplit val ->
          second fromIntegral $
            incompatibleSplits
              (collapse val $ normalizeBranchSupport $ either error id $ toSupportTree t1)
              (collapse val $ normalizeBranchSupport $ either error id $ toSupportTree t2)
        BranchScore ->
          branchScore
            (normalizeF $ either error id $ toLengthTree t1)
            (normalizeF $ either error id $ toLengthTree t2)
        where
          normalizeF = if argsNormalize l then normalizeBranchLengths else id
  -- Possibly intersect trees before distance calculation.
  when (argsIntersect l) $
    logInfoS "Intersect trees before calculation of distances."
  let distanceMeasure =
        if argsIntersect l
          then
            ( \t1 t2 -> case either error id $ intersect [t1, t2] of
                [t1', t2'] -> distanceMeasure' t1' t2'
                _ -> error "distance: Could not intersect trees."
            )
          else distanceMeasure'
  -- Possibly normalize trees.
  when (argsNormalize l) $
    logInfoS "Normalize trees before calculation of distances."
  let dsTriplets = case mtree of
        Nothing -> pairwise distanceMeasure trees
        Just masterTree -> [(0, i, distanceMeasure masterTree t') | (i, t') <- zip [1 ..] trees]
      ds = map (\(_, _, x) -> x) dsTriplets
      dsVec = V.fromList ds
  liftIO $
    hPutStrLn outH $
      "Summary statistics of "
        ++ show dist
        ++ " Distance:"
  liftIO $
    T.hPutStrLn outH $
      T.justifyLeft 10 ' ' "Mean: "
        <> T.pack
          (printf pf (mean dsVec))
  liftIO $
    T.hPutStrLn outH $
      T.justifyLeft 10 ' ' "Median: "
        <> T.pack
          (printf pf (median ds))
  liftIO $
    T.hPutStrLn outH $
      T.justifyLeft 10 ' ' "Variance: "
        <> T.pack
          (printf pf (variance dsVec))

  unless
    (argsSummaryStatistics l)
    ( do
        let n = maximum $ 6 : map length names
            m = length $ show dist
        lift $ hPutStrLn outH ""
        lift $ BL.hPutStrLn outH $ header n m dist
        case mname of
          Nothing ->
            lift $
              BL.hPutStr outH $
                BL.unlines
                  (map (showTriplet n m names) dsTriplets)
          Just mn ->
            lift $
              BL.hPutStr outH $
                BL.unlines
                  (map (showTriplet n m (mn : names)) dsTriplets)
    )
  liftIO $ hClose outH
