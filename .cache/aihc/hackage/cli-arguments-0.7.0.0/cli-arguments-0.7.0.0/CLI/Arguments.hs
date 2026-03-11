{-# OPTIONS_HADDOCK show-extensions #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module      :  CLI.Arguments
-- Copyright   :  (c) OleksandrZhabenko 2021-2023
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- A library to process command line arguments in some more convenient way.

module CLI.Arguments where

import Data.Monoid (mappend)
import GHC.Base
import Text.Show
import GHC.List

-- | Data to encode the possible cli-arguments of some logics below. The arguments are separated
-- by the spaces characters one from another.
data Arguments =
  A String -- ^ If for encoding of the argument just one 'String' is used without spaces.
  | B GQtyArgs Delimiter [String] -- ^ If there is a certain quantity of consequent arguments that indicates the one semantical value.
  | C Delimiter [String] -- ^ If the quantity of the consequent arguments that forms a semantic value is not known, but they are enclosed by the delimiter or its modification.
      deriving Eq

type Args = [Arguments]

type Specification = (Delimiter,GQtyArgs)

type CLSpecifications = [Specification]

type Delimiter = String

type GQtyArgs = Int

type FirstCharacter = Char

type FirstChars = (Char,Char)

instance Show Arguments where
  show (A xs) = xs
  show (B n ys yss) = ' ':ys `mappend` concatMap (\xs ->' ':show xs) (take n yss)
  show (C xs xss) = ' ':xs `mappend` concatMap (\ys ->' ':show ys) xss `mappend` (' ':xs)

isA :: Arguments -> Bool
isA (A _) = True
isA _ = False

isB :: Arguments -> Bool
isB (B _ _ _) = True
isB _ = False

isC :: Arguments -> Bool
isC (C _ _) = True
isC _ = False

nullArguments :: Arguments -> Bool
nullArguments (A xs) = null xs
nullArguments (B n ys yss) = n /= length yss || null ys  || null yss
nullArguments (C xs xss) = null xs || null xss

notNullArguments :: Arguments -> Bool
notNullArguments (A (_:_)) =  True
notNullArguments (A _) = False
notNullArguments (B n (_:_) yss@(_:_)) = n == length yss
notNullArguments (B _ _ _) = False
notNullArguments (C (_:_) (_:_)) = True
notNullArguments _ = False

b1Args2AArgs :: Arguments -> Arguments
b1Args2AArgs b@(B n _ [ys])
 | n < 1 = A ys
 | otherwise = b
b1Args2AArgs x = x

