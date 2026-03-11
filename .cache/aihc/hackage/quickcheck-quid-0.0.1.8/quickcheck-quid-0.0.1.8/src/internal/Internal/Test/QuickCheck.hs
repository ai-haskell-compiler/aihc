module Internal.Test.QuickCheck where

import Data.List.NonEmpty
    ( NonEmpty )
import Data.Maybe
    ( mapMaybe )
import Test.QuickCheck
    ( shrinkList )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE

--------------------------------------------------------------------------------
-- Shrinking
--------------------------------------------------------------------------------

shrinkListNonEmpty :: (a -> [a]) -> NonEmpty a -> [NonEmpty a]
shrinkListNonEmpty shrinkFn =
    mapMaybe NE.nonEmpty . shrinkList shrinkFn . F.toList
