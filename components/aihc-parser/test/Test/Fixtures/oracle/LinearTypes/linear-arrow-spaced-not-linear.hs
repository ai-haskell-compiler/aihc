{- ORACLE_TEST pass -}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE DataKinds #-}
module LinearArrowSpacedNotLinear where

-- GHC parses "% 1" (space between % and 1) as the type operator % applied to
-- 1, making the arrow unrestricted: fn :: (a % 1) -> b.
-- Only "%1" (no space) is parsed as a linear multiplicity annotation.
fn :: a % 1 -> b
fn = undefined
