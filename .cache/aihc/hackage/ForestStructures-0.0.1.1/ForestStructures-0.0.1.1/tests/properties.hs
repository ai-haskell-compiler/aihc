
module Main where

import           Debug.Trace
import qualified Data.Tree as T
import qualified Data.Vector.Unboxed as VU
import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

import           Data.Forest.Static

-- Finite tree ?

--prop_finite :: [QCTree ()] -> Bool
--prop_finite qs = True -- traceShow qs True

---- Given trees, create a pre-order forest and then generate the trees from
---- the pre-order forest again.
--
--prop_bla :: [QCTree ()] -> Bool
--prop_bla qs = ts == xs
--  where xs = forestToTrees f
--        ts = map getTree qs
--        f  = forestPre ts :: Forest Pre VU.Vector ()

-- Same, but with post-order.

main :: IO ()
main = $(defaultMainGenerator)

