{- |
Module      : Data.Ratio.Slash
Copyright   : (c) Jun Narumi 2018
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

Handle fractions described in /, not%

-}

module Data.Ratio.Slash (
  Slash(..),
  ) where

import Control.Monad (guard)
import Control.Applicative ((<|>))
import Data.Ratio
import Numeric

-- | Type of read and show slash form rational
--
-- >>> getRatio . read $ "1/2"
-- 1 % 2
--
-- >>> map getRatio . read $ "[1/2,3/4,5/6]"
-- [1 % 2,3 % 4,5 % 6]
--
-- >>> Slash (1 % 2)
-- 1/2
--
-- >>> map Slash [1%2,3%4,5%6]
-- [1/2,3/4,5/6]
--
newtype Slash a
  = Slash {
    getRatio :: Ratio a
    } deriving (Eq,Ord)

instance (Integral a) => Show (Slash a) where
  showsPrec _ (Slash n)
    | denominator n == 1
      = showSigned showInt 0 (numerator n)
    | otherwise
      = showSigned showInt 0 (numerator n) . showString "/" . showInt (denominator n)

instance (Integral a) => Read (Slash a) where
  readsPrec _ n = do
    ( ( n, d ), st ) <- slashOrInteger n
    guard $ d /= 0
    return ( Slash $ n % d , st )

integer :: (Integral a) => ReadS (a,a)
integer n = do
  ( n, st ) <- readSigned readDec n
  return ( ( n, 1 ), st )

slash :: (Integral a) => ReadS (a,a)
slash n = do
  ( numer, st  )  <- readSigned readDec n
  ( "/"  , st1 ) <- lex st
  ( denom, st2 ) <- readDec st1
  return ( ( numer, denom ), st2)

slashOrInteger :: (Integral a) => ReadS (a,a)
slashOrInteger n = take 1 $ slash n <|> integer n
