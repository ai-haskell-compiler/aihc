
module Biobase.Newick.StaticForest where

import           Data.Tree (drawForest,flatten)
import qualified Data.Tree as T
import qualified Data.Vector as V

import           Data.Forest.Static

import           Biobase.Newick.Types

import           Data.Graph.Inductive.Basic   -- only for @test@
import           Biobase.Newick.Import



test = do
  let ts = map (addIndices 0 . getNewickTree) $ either error id $ newicksFromText t
  let ss = either error id $ newicksFromText t
  mapM_ (putStrLn . T.drawTree . fmap show) ts
  putStrLn ""
  mapM_ (mapM_ (putStrLn . show) . postorder) ts
  putStrLn ""
  mapM_ (mapM_ (putStrLn . show) . preorder) ts
  putStrLn ""
  mapM_ (mapM_ print . T.levels) ts
  putStrLn ""
  print (forestPre $ map getNewickTree ss :: Forest Pre V.Vector Info)
  putStrLn ""
  print (forestPost $ map getNewickTree ss :: Forest Post V.Vector Info)
  where t = "((raccoon:19.19959,bear:6.80041):0.84600,((sea_lion:11.99700, seal:12.00300):7.52973,((monkey:100.85930,cat:47.14069):20.59201, weasel:18.87953):2.09460):3.87382,dog:25.46154)Root;"

