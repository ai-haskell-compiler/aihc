{-# LANGUAGE DataKinds #-}

module Data.KVITable.Internal.Helpers where

import Data.Name ( Name )


type Keys = [Name "Key"]


single :: e -> [e]
single = (:[])

snoc :: [a] -> a -> [a]
snoc l e = l <> [e]

maxOf :: (Foldable t, Ord e) => e -> t e -> e
maxOf v l = if null l then v else max v $ maximum l
