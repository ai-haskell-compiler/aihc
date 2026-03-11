{-# LANGUAGE CPP, MultiWayIf #-}

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 2 of the License, or
-- (at your option) any later version.

-- String-ified version of vercmp from codec-rpm-0.2.2/Codec/RPM/Version.hs
-- Copyright 2016-2018 Red Hat
-- Copyright 2021 Jens Petersen

-- | Compare versions or releases using rpm's vercmp algorithm
module Data.RPM.VerCmp (rpmVerCompare)
where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (isPrefixOf)
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif

-- | Compare two version numbers and return an 'Ordering'.
--
-- Native implementation of rpm's C vercmp
rpmVerCompare :: String -> String -> Ordering
rpmVerCompare a b =
  if a == b then EQ
  else
    -- strip out all non-version characters
    -- keep in mind the strings may be empty after this
    let a' = dropSeparators a
        b' = dropSeparators b
        -- rpm compares strings by digit and non-digit components,
        -- so grab the first component of one type
        fn = if isDigit (head a') then isDigit else isAsciiAlpha
        (prefixA, suffixA) = span fn a'
        (prefixB, suffixB) = span fn b'
    in
    if | a' == b'                                       -> EQ
       -- Nothing left means the versions are equal
       {- null a' && null b'                             -> EQ -}
       -- tilde is less than everything, including an empty string
       | ("~" `isPrefixOf` a') && ("~" `isPrefixOf` b') -> rpmVerCompare (tail a') (tail b')
       | ("~" `isPrefixOf` a')                            -> LT
       | ("~" `isPrefixOf` b')                            -> GT
       -- caret is more than everything, except .
       | ("^" `isPrefixOf` a') && ("^" `isPrefixOf` b') -> rpmVerCompare (tail a') (tail b')
       | ("^" `isPrefixOf` a') && null b'               -> GT
       | null a' && ("^" `isPrefixOf` b')               -> LT
       | ("^" `isPrefixOf` a')                            -> LT
       | ("^" `isPrefixOf` b')                            -> GT
       -- otherwise, if one of the strings is null, the other is greater
       | (null a')                                        -> LT
       | (null b')                                        -> GT
       -- Now we have two non-null strings starting with
       -- a non-tilde version character.
       -- If one prefix is a number and the other is a string,
       -- the one that is a number is greater.
       | isDigit (head a') && (not . isDigit) (head b') -> GT
       | (not . isDigit) (head a') && isDigit (head b') -> LT
       | isDigit (head a')                                -> (prefixA `compareAsInts` prefixB) <> (suffixA `rpmVerCompare` suffixB)
       | otherwise                                          -> (prefixA `compare` prefixB) <> (suffixA `rpmVerCompare` suffixB)
 where
    compareAsInts :: String -> String -> Ordering
    -- the version numbers can overflow Int, so strip leading 0's and do a string compare,
    -- longest string wins
    compareAsInts x y =
      if | x == y -> EQ
         | null x -> LT
         | null y -> GT
         | otherwise ->
           let x' = dropWhile (== '0') x
               y' = dropWhile (== '0') y
           in
             case compare (length x') (length y') of
               EQ ->
                 (read x' :: Int) `compare` read y'
               o -> o

    -- isAlpha returns any unicode alpha, but we just want ASCII characters
    isAsciiAlpha :: Char -> Bool
    isAsciiAlpha x = isAsciiLower x || isAsciiUpper x

    -- RPM only cares about ascii digits, ascii alpha, and ~ ^
    isVersionChar :: Char -> Bool
    isVersionChar x = isDigit x || isAsciiAlpha x || x == '~' || x == '^'

    dropSeparators :: String -> String
    dropSeparators = dropWhile (not . isVersionChar)
