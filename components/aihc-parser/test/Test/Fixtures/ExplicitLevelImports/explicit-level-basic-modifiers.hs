{- ORACLE_TEST
id: explicit-level-basic-modifiers
category: modules
expected: pass
reason: parser now supports explicit-level import modifiers
-}
{-# LANGUAGE ExplicitLevelImports #-}

module ExplicitLevelBasicModifiers where

import quote Data.List
import splice Data.Maybe

useMaybe :: a -> Maybe a
useMaybe = Just
