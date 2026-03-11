{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      :  TLynx.Connect.Connect
-- Description :  Connect two phylogenies
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Sep 19 15:01:52 2019.
module TLynx.Connect.Connect
  ( connectCmd,
  )
where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Default
import qualified Data.Set as S
import ELynx.Tools.ELynx
import ELynx.Tools.Environment
import ELynx.Tools.Logger
import ELynx.Tree
import System.IO
import TLynx.Connect.Options
import TLynx.Parsers

-- Connect two trees with a branch in all possible ways.
--
-- Introduce a branch between two trees. If the trees have @n>2@, and @m>2@
-- nodes, respectively, there are (n-2)*(m-2) ways to connect them.
--
-- A base node label has to be given which will be used wherever the new node is
-- introduced.
--
-- Return 'Left' if one tree has a non-bifurcating root node.
connect ::
  (Semigroup e, Splittable e, Default a) =>
  e ->
  a ->
  Tree e a ->
  Tree e a ->
  Either String (Forest e a)
connect br lb l r = do
  ls <- roots l
  rs <- roots r
  return [Node br lb [x, y] | x <- ls, y <- rs]

-- | Connect two trees honoring possible constraints.
--
-- Introduce a branch between two trees. If the trees have @n>2@, and @m>2@
-- nodes, respectively, there are (n-2)*(m-2) ways to connect them.
connectCmd :: ELynx ConnectArguments ()
connectCmd = do
  lArgs <- localArguments <$> ask
  outH <- outHandle "results" ".out"
  -- Do we have constraints or not?
  let cs = constraints lArgs
      l = inFileA lArgs
      r = inFileB lArgs
  case cs of
    Nothing -> connectOnly outH l r
    Just c -> connectAndFilter outH c l r
  liftIO $ hClose outH

connectTrees ::
  Tree Length Name ->
  Tree Length Name ->
  Forest Length Name
connectTrees t = either error id . connect 0 "root" t

type Constraint a = S.Set a

-- Get groups induced by multifurcations. Collect the leaves of all trees
-- induced by multifurcations.
multifurcatingGroups :: Tree e a -> [[a]]
multifurcatingGroups (Node _ _ []) = []
multifurcatingGroups (Node _ _ [x]) = multifurcatingGroups x
multifurcatingGroups (Node _ _ [x, y]) = multifurcatingGroups x ++ multifurcatingGroups y
multifurcatingGroups t = leaves t : concatMap multifurcatingGroups (forest t)

compatibleAll :: (Show a, Ord a) => Tree e a -> [Constraint a] -> Bool
compatibleAll t@(Node _ _ [l, r]) cs =
  all (compatible partitionLeft . getP) cs
    && all (compatible partitionRight . getP) cs
  where
    lvs = S.fromList $ leaves t
    getP x = either error id $ pt [x, lvs S.\\ x]
    partitionLeft = either error id $ partition l
    partitionRight = either error id $ partition r
compatibleAll _ _ = error "Tree is not bifurcating."

compatibleWith ::
  (Show b, Ord b) => (a -> b) -> [Constraint a] -> Tree e a -> Bool
compatibleWith f cs t = compatibleAll (fmap f t) (map (S.map f) cs)

parseTreeTuple ::
  FilePath ->
  FilePath ->
  ELynx
    ConnectArguments
    (Tree Length Name, Tree Length Name)
parseTreeTuple l r = do
  nwF <- nwFormat . localArguments <$> ask
  tl <- liftIO $ parseTree nwF l
  tr <- liftIO $ parseTree nwF r
  logInfoS "Tree 1:"
  logInfoB $ toNewick tl
  logInfoS "Tree 2:"
  logInfoB $ toNewick tr
  return (either error id $ toLengthTree tl, either error id $ toLengthTree tr)

connectOnly :: Handle -> FilePath -> FilePath -> ELynx ConnectArguments ()
connectOnly h l r = do
  (tl, tr) <- parseTreeTuple l r
  let ts = connectTrees tl tr
  logInfoS $ "Connected trees: " <> show (length ts)
  liftIO $ BL.hPutStr h $ BL.unlines $ map (toNewick . lengthToPhyloTree) ts

connectAndFilter ::
  Handle -> FilePath -> FilePath -> FilePath -> ELynx ConnectArguments ()
connectAndFilter h c l r = do
  nwF <- nwFormat . localArguments <$> ask
  cts <- liftIO $ parseTrees nwF c
  logInfoS "Constraints:"
  logInfoB $ BL.intercalate "\n" $ map toNewick cts
  (tl, tr) <- parseTreeTuple l r
  let ts = connectTrees tl tr
      cs = map S.fromList $ concatMap multifurcatingGroups cts :: [Constraint Name]
      -- Only collect trees that are compatible with the constraints.
      ts' = filter (compatibleWith getName cs) ts
  logInfoS $ "Connected  trees: " <> show (length ts)
  logInfoS $ "Compatible trees: " <> show (length ts')
  liftIO $ BL.hPutStr h $ BL.unlines $ map (toNewick . lengthToPhyloTree) ts'
