{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Internal.Test.QuickCheck.Quid.Representations
    where

import Data.List.NonEmpty
    ( NonEmpty )
import Data.Proxy
    ( Proxy (..) )
import Internal.Test.QuickCheck.Quid
    ( Quid (..) )
import Numeric.Natural
    ( Natural )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

nonEmptyListToQuid :: forall a. (Bounded a, Enum a) => NonEmpty a -> Quid
nonEmptyListToQuid xs = Quid $
    F.foldl' f 0 xs - 1
  where
    f !acc !x = acc * base + 1 + fromIntegral (fromEnum x)
    base = fromIntegral @Int @Natural $ boundedEnumCardinality $ Proxy @a

nonEmptyListFromQuid :: forall a. (Bounded a, Enum a) => Quid -> NonEmpty a
nonEmptyListFromQuid (Quid q) =
    NE.fromList $ go [] q
  where
    go :: [a] -> Natural -> [a]
    go !acc !n
        | n < base =
            toEnum (fromIntegral n) : acc
        | otherwise =
            go (toEnum (fromIntegral (n `mod` base)) : acc) (n `div` base - 1)
    base = fromIntegral @Int @Natural $ boundedEnumCardinality $ Proxy @a

boundedEnumCardinality :: forall a. (Bounded a, Enum a) => Proxy a -> Int
boundedEnumCardinality _ = fromEnum (maxBound @a) - fromEnum (minBound @a) + 1
