module Currency.Rates where

import Prelude hiding (lookup)
import Data.Map (Map, empty, lookup, insert)

-- | A map from currency to exchange rate against some reference currency
data Rates a b = Rates {
		reference :: a,
		rates :: Map a b
	} deriving (Show, Read, Eq)

-- | Change the reference currency to a different one found in the 'Map'
rebase :: (Ord a, Fractional b) => a -> Rates a b -> Rates a b
rebase new rs@(Rates old m)
	| new == old = rs
	| otherwise = Rates new $ maybe empty
		(\newRate -> fmap (/newRate) $ insert old 1 m) $
		lookup new m

-- | Convenience function for getting a single exchange rate
--
-- If you're doing a lot of conversions, use 'rebase' and 'lookup'
exchangeRate :: (Ord a, Fractional b) =>
	Rates a b
	-> a -- ^ Source currency
	-> a -- ^ Target currency
	-> Maybe b
exchangeRate rs source target = lookup target $ rates (rebase source rs)
