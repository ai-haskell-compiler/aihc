{- ORACLE_TEST pass -}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TransformListComp #-}
{-# LANGUAGE TypeApplications #-}

module ThenByClosedBoundaries where

closedApp xs = [x | x <- xs, then f x by x]

closedNegate xs = [x | x <- xs, then -x by x]

closedTypeApp xs = [x | x <- xs, then f @T by x]

closedValueNameQuote xs = [x | x <- xs, then '[] by x]

closedTypeNameQuote xs = [x | x <- xs, then ''T by x]

groupByClosedApp xs = [x | x <- xs, then group by f x using g]

type T = ()
