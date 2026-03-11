-- |
-- Module      :  Data.InsertLeft.Unfold
-- Copyright   :  (c) OleksandrZhabenko 2021-2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Generalization of the 'Data.List.unfoldr' for the data type that has 'InsertLeft' and 'Monoid' instances.
-- Inspired by: <https://www.works-hub.com/learn/number-anamorphisms-aka-unfolds-explained-50e1a> by Marty Stumpf.
-- Is a fork of <https://hackage.haskell.org/package/subG-0.6.1.0>

{-# LANGUAGE NoImplicitPrelude #-}

module Data.InsertLeft.Unfold (
  unfoldG
  , iterateG
) where

import GHC.Base
import Data.InsertLeft
import Data.Monoid

-- | Inspired by: https://hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.OldList.html#words
-- and: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
-- Also inspired by: https://www.works-hub.com/learn/number-anamorphisms-aka-unfolds-explained-50e1a by Marty Stumpf.
-- Generalizes the 'Data.List.unfoldr' function not only for lists, but for the data type that has 'InsertLeft' and 'Monoid' instances.
unfoldG :: (InsertLeft t a, Monoid (t a)) => (b -> Maybe (a, b)) -> b -> t a
unfoldG p x =
 case p x of
   Just (y, z) -> y %@ unfoldG p z
   Nothing -> mempty

-- | Inspired by: https://hackage.haskell.org/package/base-4.14.0.0/docs/src/Data.OldList.html#words
-- and: Graham Hutton. A tutorial on the universality and expressiveness of fold. /J. Functional Programming/ 9 (4): 355–372, July 1999.
-- that is available at the URL: https://www.cs.nott.ac.uk/~pszgmh/fold.pdf.
-- Also inspired by: https://www.works-hub.com/learn/number-anamorphisms-aka-unfolds-explained-50e1a by Marty Stumpf.
-- Generalizes the 'Prelude.iterate' function not only for lists, but for the data type that has 'InsertLeft' and 'Monoid' instances.
iterateG :: (InsertLeft t a, Monoid (t a)) => (a -> a) -> a -> t a
iterateG f = unfoldG (\x -> Just (x, f x))
{-# INLINE iterateG #-}
