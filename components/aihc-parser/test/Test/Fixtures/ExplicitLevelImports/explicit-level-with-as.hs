{- ORACLE_TEST
id: explicit-level-with-as
category: modules
expected: pass
reason: parser now supports explicit-level imports with aliases
-}
{-# LANGUAGE ExplicitLevelImports #-}

module ExplicitLevelWithAs where

import quote Data.List as L
import splice Data.Maybe as M

useAliases :: [a] -> a -> a
useAliases xs fallback = M.fromMaybe fallback (L.listToMaybe xs)
