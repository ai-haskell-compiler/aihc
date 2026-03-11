
-- |
-- Module      :  Data.ReversedScientific
-- Copyright   :  (c) OleksandrZhabenko 2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Provides a function that shows the somewhat "reversed" scientific notation of the big 'Integer' number so that it is easier (more likely exact) to compare at quick glance two consequent numbers in such a notation by their order and values if they are located in the orderd list.

{-# LANGUAGE NoImplicitPrelude #-}

{-# OPTIONS_HADDOCK -show-extensions #-}

module Data.ReversedScientific where

import GHC.Base
import GHC.Num ((-),Integer)
import Text.Show (Show(..))
import Data.List (length,dropWhileEnd)


-- | Two big positive 'Integer' numbers in the ordered list of them are somewhat \'quickly\' compared by values from left to right. If the number
-- after \"e\" is greater than the other one respectively, the number is also greater than the
-- other. If these values are equal, then the next shown 'Double' values are compared (with the same
-- rule). If they are also equal, then the difference between them can be likely find from the last
-- two values (nevertheless, the numbers can differ if the values here are all pairwise equal! But
-- the numbers are close in values one to another and so can represent just a subtle difference).
-- This notation allows to reduce the difference in the lengths of big numbers in general case and
-- to read the standard scientific notation not from right to left, but in a \'more usual\' way —
-- from left to right. If two numbers have different results of the function, then they are
-- definitely unequal numbers. 
--
-- Is intended to be used in the PhLADiPreLiO for big numbers output.
--
-- Example:
--
-- > let a = 58702730574235423475087390582374507304750234705847047403750375034750723045720387502357280345
-- > let b = 58738475082532745724057082370582054703750278402705730573074052705780857034750537507053278
-- > showBignum 7 a == "e91~5.87...d92~345"
-- > showBignum 7 b == "e88~5.87...d89~278"
--
-- As it is seen from the numbers after \"e\" the first one is greater than the second one.
-- Nevertheless, if you append the three digits 345 (hence \"d\" in the notation meanitg a number of significant digits) to the end of the second one, the output here 
-- will be equivalent, but the modified second number will be greater than the first one.
showBignum :: Int -> Integer -> String
showBignum n x 
  | l0 < 6 = xShow
  | l >= n = mconcat ["e", show (l - 1), "~", (c1:"."), k2s, "...d", show l0, "~", k3s]
  | otherwise = xShow
      where xShow = show x
            l = length xShow
            k = dropWhileEnd (== '0') xShow 
            l0 = length k
            f (p:q:r:s:t:u:v:vs) = f (p:q:r:t:u:v:vs)
            f (p:q:r:s:t:u:_) = (p, [q, r], [s, t, u])
            (c1,k2s,k3s) = f k

