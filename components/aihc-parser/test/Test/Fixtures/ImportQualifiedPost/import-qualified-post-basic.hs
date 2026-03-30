{- ORACLE_TEST
id: import-qualified-post-basic
category: modules
expected: pass
-}
{-# LANGUAGE ImportQualifiedPost #-}

module ImportQualifiedPostBasic where

import Data.List qualified as List

sorted :: [Int] -> [Int]
sorted = List.sort
