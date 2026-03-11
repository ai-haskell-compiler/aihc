{-# LANGUAGE UndecidableInstances #-} -- due to type families in constraints
{-# LANGUAGE AllowAmbiguousTypes  #-} -- due to class design

module Generic.Data.Function.FoldMap.Sum where

import GHC.Generics
import Generic.Data.Function.Common.Generic ( absurdV1 )
import Generic.Data.Function.FoldMap.Constructor
  ( GFoldMapC(gFoldMapC)
  , GenericFoldMap(type GenericFoldMapM) )
import Generic.Data.MetaParse.Cstr
import GHC.Exts ( proxy# )
import GHC.TypeLits ( Symbol )

class GFoldMapSum tag sumtag gf where
    gFoldMapSum
        :: ParseCstrTo sumtag (GenericFoldMapM tag)
        -> gf p -> GenericFoldMapM tag

instance GFoldMapSumD tag sumtag dtName gf
  => GFoldMapSum tag sumtag (D1 (MetaData dtName _md2 _md3 _md4) gf) where
    gFoldMapSum f = gFoldMapSumD @tag @sumtag @dtName f . unM1

class GFoldMapSumD tag sumtag dtName gf where
    gFoldMapSumD
        :: ParseCstrTo sumtag (GenericFoldMapM tag)
        -> gf p -> GenericFoldMapM tag

instance GFoldMapSumD tag sumtag dtName V1 where
    gFoldMapSumD _ = absurdV1

instance GFoldMapCSum tag sumtag dtName (C1 c gf)
  => GFoldMapSumD tag sumtag dtName (C1 c gf) where
    gFoldMapSumD = gFoldMapCSum @tag @sumtag @dtName

instance GFoldMapCSum tag sumtag dtName (l :+: r)
  => GFoldMapSumD tag sumtag dtName (l :+: r) where
    gFoldMapSumD = gFoldMapCSum @tag @sumtag @dtName

class GFoldMapCSum tag sumtag (dtName :: Symbol) gf where
    gFoldMapCSum
        :: ParseCstrTo sumtag (GenericFoldMapM tag)
        -> gf p -> GenericFoldMapM tag

instance (GFoldMapCSum tag sumtag dtName l, GFoldMapCSum tag sumtag dtName r)
  => GFoldMapCSum tag sumtag dtName (l :+: r) where
    gFoldMapCSum f = \case L1 l -> gFoldMapCSum @tag @sumtag @dtName f l
                           R1 r -> gFoldMapCSum @tag @sumtag @dtName f r

-- TODO could play with this. Perhaps Unsatisfiable (GHC 9.8) is better?
instance
  ( Semigroup (GenericFoldMapM tag), GFoldMapC tag gf
  , ReifyCstrParseResult sumtag cstrParsed
  , ForceGCParse dtName cstr (ParseCstr sumtag cstr) ~ cstrParsed
  ) => GFoldMapCSum tag sumtag dtName (C1 (MetaCons cstr _mc2 _mc3) gf) where
    gFoldMapCSum mapReifyCstr (M1 a) =
        mapReifyCstr (proxy# @cstrParsed) <> gFoldMapC @tag a
