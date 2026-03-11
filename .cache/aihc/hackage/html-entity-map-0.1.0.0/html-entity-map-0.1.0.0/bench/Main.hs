module Main (main) where

import Criterion.Main
import Data.HTML.Entities
import qualified Data.HashMap.Strict as M
import qualified Data.Text           as T

-- | I used this to decide whether using @unordered-containers@ makes sense
-- or @containers@ will do.

main :: IO ()
main = defaultMain
  [ bnch "AMP"
  , bnch "Bernoullis"
  , bnch "HumpDownHump"
  , bnch "Star"
  , bnch "downharpoonright" ]

-- | Benchmark looking up an entity with given name.

bnch :: String -> Benchmark
bnch entity = env (return (T.pack entity)) $
  bench entity . nf (`M.lookup` htmlEntityMap)
