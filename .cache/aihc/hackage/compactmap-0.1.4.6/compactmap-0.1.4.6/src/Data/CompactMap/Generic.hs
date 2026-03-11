module Data.CompactMap.Generic
    ( CompactMap
    , fromList
    , toVector
    , lookup
    , getLE
    , getIndex
    ) where

import qualified Data.Vector.Generic as V
import GHC.Exts (sortWith)
import Control.Applicative
import Prelude hiding (lookup)

data CompactMap vec k v = CompactMap { getMap :: vec v
                                     , _gkf   :: v -> k
                                     }

instance (V.Vector vec v, Show v) => Show (CompactMap vec k v) where
    show = show . V.toList . getMap

fromList :: (V.Vector vec v, Ord k) => [v] -> (v -> k) -> CompactMap vec k v
fromList lst f = CompactMap (V.fromList $ sortWith f lst) f

toVector :: V.Vector vec v => CompactMap vec k v -> vec v
toVector (CompactMap v _) = v

lookup :: (V.Vector vec v, Ord k) => k -> CompactMap vec k v -> Maybe v
lookup k cm@(CompactMap _ f) = getLE k cm >>= \(_,r) -> if f r == k
                                                            then Just r
                                                            else Nothing

getLE :: (V.Vector vec v, Ord k) => k -> CompactMap vec k v -> Maybe (Int, v)
getLE k (CompactMap lst f) = go 0 (sz - 1)
    where
        sz = V.length lst
        go l h
            | m < l     = Nothing
            | m > h     = Nothing
            | k' == k   = Just (m, x)
            | l >= h    = checkLower <|> Just (m, x)
            | k < k'    = checkLower <|> go l (m - 1)
            | otherwise = go (m + 1) h
            where
                checkLower
                    | m < 1 = Nothing
                    | k > k_1 = Just (m - 1, x_1)
                    | otherwise = Nothing
                m = (l + h) `div` 2
                x = lst V.! m
                k' = f x
                x_1 = lst V.! (m - 1)
                k_1 = f x_1

getIndex :: V.Vector vec v => Int -> CompactMap vec k v -> Maybe v
getIndex i (CompactMap lst _) = lst V.!? i
