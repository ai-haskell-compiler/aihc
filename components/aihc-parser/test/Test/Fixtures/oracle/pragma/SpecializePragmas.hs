{- ORACLE_TEST xfail SPECIALIZE pragmas not preserved in pretty-printer roundtrip -}
{-# LANGUAGE TypeApplications #-}

module SpecializePragmas where

hammeredLookup :: Ord key => [(key, value)] -> key -> Maybe value
hammeredLookup [] _ = Nothing
hammeredLookup ((k, v):xs) key
  | k == key = Just v
  | otherwise = hammeredLookup xs key
{-# INLINABLE hammeredLookup #-}
{-# SPECIALIZE hammeredLookup :: [(Int, value)] -> Int -> Maybe value #-}
{-# SPECIALIZE hammeredLookup @Int #-}
{-# SPECIALIZE [0] hammeredLookup :: [(Char, value)] -> Char -> Maybe value #-}

fn :: Bool -> Int -> Int
fn b i = if b then i + 1 else i - 1
{-# SPECIALIZE fn True #-}
{-# SPECIALIZE fn False #-}

specInline :: Num a => a -> a
specInline x = x + x
{-# SPECIALIZE INLINE specInline :: Int -> Int #-}

specNoInline :: Num a => a -> a
specNoInline x = x * x
{-# SPECIALIZE NOINLINE [0] specNoInline :: Int -> Int #-}
