{-# LANGUAGE OverloadedStrings #-}

-- |
-- Description :  Analyze trees
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri May 24 13:47:56 2019.
module TLynx.Examine.Examine
  ( examine,
  )
where

import Control.Comonad
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ask)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Containers.ListUtils (nubOrd)
import Data.List (foldl', (\\))
import qualified Data.Map as M
import ELynx.Tools.ByteString
import ELynx.Tools.ELynx
import ELynx.Tools.Environment
import ELynx.Tools.Logger
import ELynx.Tree
import System.IO
  ( Handle,
    hPutStrLn,
  )
import TLynx.Examine.Options
import TLynx.Parsers
import Text.Printf

pretty :: Length -> String
pretty = printf "%.5f" . fromLength

prettyRow :: String -> String -> BL.ByteString
prettyRow name val = alignLeft 33 n <> alignRight 8 v
  where
    n = BL.pack name
    v = BL.pack val

-- | Examine branches of a tree.
summarizeLengths :: (HasLength e) => Tree e a -> BL.ByteString
summarizeLengths t =
  BL.intercalate
    "\n"
    [ prettyRow "Origin height: " $ pretty h,
      prettyRow "Mean distance origin leaves: " $ pretty h',
      prettyRow "Total branch length: " $ pretty b
    ]
  where
    n = length $ leaves t
    h = height t
    h' = sum (distancesOriginLeaves t) / fromIntegral n
    b = totalBranchLength t

readTrees :: FilePath -> ELynx ExamineArguments (Forest Phylo Name)
readTrees fp = do
  logInfoS $ "Read tree(s) from file " <> fp <> "."
  nf <- argsNewickFormat . localArguments <$> ask
  liftIO $ parseTrees nf fp

countElements :: (Ord a, Foldable f) => f a -> M.Map a Int
countElements = foldl' f M.empty
  where
    f m x = M.alter g x m
    g Nothing = Just 1
    g (Just x) = Just $ x + 1

examineTree :: (HasName a) => Handle -> Tree Phylo a -> IO ()
examineTree h t = do
  hPutStrLn h $ "Number of leaves: " ++ show (length lvs)
  hPutStrLn h $ "Degree of root node: " ++ show (degree t)
  if bifurcating t
    then hPutStrLn h "Tree is bifurcating."
    else
      let degrees = extend degree t
          degreeMax = maximum degrees
       in do
            if degreeMax > 2
              then hPutStrLn h "Tree is multifurcating."
              else hPutStrLn h "Tree is bifurcating but has degree two nodes."
            hPutStrLn h $ "List of degrees with counts: " <> show (M.toList $ countElements degrees)
  let l = toLengthTree t
  case l of
    Left _ -> hPutStrLn h "Branch lengths not available."
    Right t' -> BL.hPutStrLn h $ summarizeLengths t'
  unless (null dups) $ do
    hPutStrLn h ""
    hPutStrLn h ("Duplicate leaves: " ++ show dups)
  BL.hPutStrLn h $ "Leave names: " <> BL.intercalate " " lvs
  where
    lvs = map (fromName . getName) $ leaves t
    dups = lvs \\ nubOrd lvs

-- | Examine phylogenetic trees.
examine :: ELynx ExamineArguments ()
examine = do
  l <- localArguments <$> ask
  let inFn = argsInFile l
  trs <- readTrees inFn
  outH <- outHandle "results" ".out"
  liftIO $ mapM_ (examineTree outH) trs
