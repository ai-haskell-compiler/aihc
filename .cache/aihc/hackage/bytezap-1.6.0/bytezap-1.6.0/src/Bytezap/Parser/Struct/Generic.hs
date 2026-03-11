{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-} -- thanks to type manipulation

-- TODO pass metadata to parser for errors. not hard just cba

-- TODO copies a lot of stuff from generic-data-functions. some should be kept
-- in a separate library (ReifyMaybeSymbol, ProdArity, natVal'' etc.)

module Bytezap.Parser.Struct.Generic where

import Bytezap.Parser.Struct
import GHC.Generics
import GHC.Exts
import Data.Kind
import GHC.TypeNats
import GHC.TypeLits ( KnownSymbol, symbolVal' )
import Bytezap.Common.TypeNats ( natValInt )
import Bytezap.Common.Generic ( type GTFoldMapCAddition )
import DeFun.Core ( type (~>) )

class GParseBase tag where
    -- | The state token of the parser.
    type GParseBaseSt tag :: ZeroBitType
    type GParseBaseC tag a :: Constraint
    type GParseBaseE tag :: Type

    -- unlike the serializer we stay newtyped because we want our Functor
    --
    -- TODO this is where we need to pass a bunch of metadata. see gdf
    gParseBase
        :: GParseBaseC tag a
        => String       {- ^ data type name -}
        -> String       {- ^ constructor name -}
        -> Maybe String {- ^ record name (if present) -}
        -> Natural      {- ^ field index -}
        -> ParserT (GParseBaseSt tag) (GParseBaseE tag) a

    -- | Defunctionalization symbol for a type family turning 'Type's into
    --   'Natural's. (Needed as we can't partially apply type families.)
    type GParseBaseLenTF tag :: Type ~> Natural

class GParse tag gf where
    gParse :: ParserT (GParseBaseSt tag) (GParseBaseE tag) (gf p)

instance GParseC tag dtName cstrName 0 gf
  => GParse tag (D1 (MetaData dtName _md2 _md3 _md4) (C1 (MetaCons cstrName _mc2 _mc3) gf)) where
    gParse = M1 <$> M1 <$> gParseC @tag @dtName @cstrName @0

class GParseC tag (cd :: Symbol) (cc :: Symbol) (si :: Natural) gf where
    gParseC :: ParserT (GParseBaseSt tag) (GParseBaseE tag) (gf p)

instance
  ( GParseC tag cd cc si                 l
  , GParseC tag cd cc (si + ProdArity r) r
  , GParseBase tag
  , lenL ~ GTFoldMapCAddition (GParseBaseLenTF tag) l
  , KnownNat lenL
  ) => GParseC tag cd cc si (l :*: r) where
    gParseC = sequenceParsers len (:*:)
        (gParseC @tag @cd @cc @si)
        (gParseC @tag @cd @cc @(si + ProdArity r))
      where
        len = natValInt @lenL

instance
  ( GParseBase tag, GParseBaseC tag a
  , KnownNat si, ReifyMaybeSymbol mSelName, KnownSymbol cc, KnownSymbol cd
  ) => GParseC tag cd cc si (S1 (MetaSel mSelName _ms2 _ms3 _ms4) (Rec0 a)) where
    gParseC = (M1 . K1) <$> gParseBase @tag cd cc cs si
      where
        cs = reifyMaybeSymbol @mSelName
        cd = symbolVal'' @cd
        cc = symbolVal'' @cc
        si = natVal'' @si

-- | Wow, look! Nothing!
instance GParseC tag cd cc 0 U1 where gParseC = constParse U1

type family ProdArity (f :: Type -> Type) :: Natural where
    ProdArity (S1 c f)  = 1
    ProdArity (l :*: r) = ProdArity l + ProdArity r

class ReifyMaybeSymbol (mstr :: Maybe Symbol) where
    reifyMaybeSymbol :: Maybe String
instance ReifyMaybeSymbol Nothing where reifyMaybeSymbol = Nothing
instance KnownSymbol str => ReifyMaybeSymbol (Just str) where
    reifyMaybeSymbol = Just (symbolVal'' @str)

natVal'' :: forall n. KnownNat n => Natural
natVal'' = natVal' (proxy# :: Proxy# n)
{-# INLINE natVal'' #-}

symbolVal'' :: forall sym. KnownSymbol sym => String
symbolVal'' = symbolVal' (proxy# :: Proxy# sym)
{-# INLINE symbolVal'' #-}
