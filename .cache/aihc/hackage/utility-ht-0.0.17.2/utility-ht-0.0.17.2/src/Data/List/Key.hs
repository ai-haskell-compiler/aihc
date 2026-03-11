{- |
Variant of "Data.List" functions like 'Data.List.group', 'Data.List.sort'
where the comparison is performed on a key computed from the list elements.
In principle these functions could be replaced by e.g. @sortBy (compare `on` f)@,
but @f@ will be re-computed for every comparison.
If the evaluation of @f@ is expensive,
our functions are better, since they buffer the results of @f@.
-}
module Data.List.Key (
   L.nub,
   L.sort,
   L.minimum,
   L.maximum,
   L.group,
   L.merge,
   ) where

import qualified Data.List.Key.Private as L
