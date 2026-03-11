{-# LANGUAGE UndecidableInstances #-} -- required below GHC 9.6
{-# LANGUAGE AllowAmbiguousTypes #-} -- due to type class design

module Generic.Data.Function.Traverse.NonSum where

import GHC.Generics
import GHC.TypeLits ( Symbol )
import Generic.Data.Function.Traverse.Constructor
import Generic.Data.Function.Common.Error

class GTraverseNonSum tag gf where
    gTraverseNonSum :: GenericTraverseF tag (gf p)

instance (Functor (GenericTraverseF tag), GTraverseNonSumD tag dtName gf)
  => GTraverseNonSum tag (D1 (MetaData dtName _md2 _md3 _md4) gf) where
    gTraverseNonSum = M1 <$> gTraverseNonSumD @tag @dtName

class GTraverseNonSumD tag (cd :: Symbol) gf where
    gTraverseNonSumD :: GenericTraverseF tag (gf p)

instance (Functor (GenericTraverseF tag), GTraverseC tag cd cstrName 0 gf)
  => GTraverseNonSumD tag cd (C1 (MetaCons cstrName _mc2 _mc3) gf) where
    gTraverseNonSumD = M1 <$> gTraverseC @tag @cd @cstrName @0

instance GTraverseNonSumD tag cd (l :+: r) where gTraverseNonSumD = error eNoSum
instance GenericTraverse tag => GTraverseNonSumD tag cd V1 where
    gTraverseNonSumD = genericTraverseV1 @tag
