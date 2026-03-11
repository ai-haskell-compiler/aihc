{-# LANGUAGE AllowAmbiguousTypes  #-} -- due to tag type class design
{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6

module Generic.Data.Function.FoldMap.NonSum where

import GHC.Generics
import Generic.Data.Function.FoldMap.Constructor
  ( GFoldMapC(gFoldMapC)
  , GenericFoldMap(type GenericFoldMapM) )
import Generic.Data.Function.Common.Error
import Generic.Data.Function.Common.Generic ( absurdV1 )

{- | 'foldMap' over generic product data types.

Take a generic representation, map each field in the data type to a 'Monoid',
and combine the results with ('<>').
-}
class GFoldMapNonSum tag gf where gFoldMapNonSum :: gf p -> GenericFoldMapM tag

instance GFoldMapNonSum tag gf => GFoldMapNonSum tag (D1 c gf) where
    gFoldMapNonSum = gFoldMapNonSum @tag . unM1

instance GFoldMapC tag gf => GFoldMapNonSum tag (C1 c gf) where
    gFoldMapNonSum (M1 a) = gFoldMapC @tag a

instance GFoldMapNonSum tag (l :+: r) where gFoldMapNonSum = error eNoSum
instance GFoldMapNonSum tag V1        where gFoldMapNonSum = absurdV1
