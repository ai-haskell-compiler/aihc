{- ORACLE_TEST pass -}
{-# LANGUAGE TransformListComp #-}

module BareIfBoundaries where

thenIf = [[] | then if [] then [] else [] by []]

groupByIf = [[] | then group by if [] then [] else [] using []]
