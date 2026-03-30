{- ORACLE_TEST
id: import-qualified-post-as
category: modules
expected: pass
-}
{-# LANGUAGE ImportQualifiedPost #-}

module ImportQualifiedPostAs where

import Data.Map qualified as M

fromPairs :: [(Int, Int)] -> M.Map Int Int
fromPairs = M.fromList
