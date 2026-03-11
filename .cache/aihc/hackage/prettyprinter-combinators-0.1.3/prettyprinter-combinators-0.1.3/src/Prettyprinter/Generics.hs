----------------------------------------------------------------------------
-- |
-- Module      :  Prettyprinter.Generics
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Prettyprinter.Generics
  ( ppGeneric
  , PPGeneric(..)
  , PPGenericOverride(..)
  -- * Reexports
  , Pretty(..)
  , Generic
  ) where

import Data.Bimap (Bimap)
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy.Char8 qualified as CL8
import Data.ByteString.Short qualified as ShortBS
import Data.DList (DList)
import Data.DList qualified as DL
#ifdef HAVE_ENUMMAPSET
import Data.EnumMap (EnumMap)
import Data.EnumSet (EnumSet)
#endif
import Data.Foldable
import Data.Functor.Compose
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Data.Int
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Kind
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Proxy
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Vector (Vector)
import Data.Void
import Data.Word
import GHC.ForeignPtr (ForeignPtr(..))
import GHC.Generics
import GHC.Real (Ratio(..))
import GHC.Stack (CallStack)
import GHC.TypeLits

import Prettyprinter
import Prettyprinter.Combinators
import Prettyprinter.MetaDoc

import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Syntax qualified as TH

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> :set -XDerivingVia
-- >>> :set -XImportQualifiedPost
-- >>> import Data.List.NonEmpty (NonEmpty(..))
-- >>> import Data.List.NonEmpty qualified as NonEmpty
-- >>> import Data.IntMap (IntMap)
-- >>> import Data.IntMap qualified as IntMap
-- >>> import Data.IntSet (IntSet)
-- >>> import Data.IntSet qualified as IntSet
-- >>> import Data.Map.Strict (Map)
-- >>> import Data.Map.Strict qualified as Map
-- >>> import Data.Set (Set)
-- >>> import Data.Set qualified as Set
-- >>> import Data.Vector (Vector)
-- >>> import Data.Vector qualified as Vector
-- >>> import GHC.Generics (Generic)
--
-- >>> :{
-- data Test = Test
--   { testSet         :: Maybe (Set Int)
--   , testMap         :: Map String (Set Double)
--   , testIntSet      :: IntSet
--   , testIntMap      :: IntMap String
--   , testInt         :: Int
--   , testComplexMap  :: Map (Maybe (Set Int)) (IntMap (Set String))
--   , testComplexMap2 :: Map (Maybe (Set Int)) (Map (NonEmpty Int) (Vector String))
--   } deriving (Generic)
-- :}

-- | Helper to use 'GHC.Generics.Generic'-based prettyprinting with DerivingVia.
--
-- >>> :{
-- data TestWithDeriving a b = TestWithDeriving
--   { testSet         :: Maybe (Set a)
--   , testB           :: b
--   , testIntMap      :: IntMap String
--   , testComplexMap  :: Map (Maybe (Set Int)) (IntMap (Set String))
--   }
--   deriving (Generic)
--   deriving Pretty via PPGeneric (TestWithDeriving a b)
-- :}
--
newtype PPGeneric a = PPGeneric { unPPGeneric :: a }

instance (Generic a, GPretty (Rep a)) => Pretty (PPGeneric a) where
  pretty = ppGeneric . unPPGeneric

-- | Prettyprint using 'GHC.Generics.Generic' instance.
--
-- >>> :{
-- test = Test
--   { testSet         = Just $ Set.fromList [1..3]
--   , testMap         =
--       Map.fromList [("foo", Set.fromList [1.5]), ("foo", Set.fromList [2.5, 3, 4])]
--   , testIntSet      = IntSet.fromList [1, 2, 4, 5, 7]
--   , testIntMap      = IntMap.fromList $ zip [1..] ["3", "2foo", "11"]
--   , testInt         = 42
--   , testComplexMap  = Map.fromList
--       [ ( Nothing
--         , IntMap.fromList $ zip [0..] $ map Set.fromList
--             [ ["foo", "bar"]
--             , ["baz"]
--             , ["quux", "frob"]
--             ]
--         )
--       , ( Just (Set.fromList [1])
--         , IntMap.fromList $ zip [0..] $ map Set.fromList
--             [ ["quux"]
--             , ["fizz", "buzz"]
--             ]
--         )
--       , ( Just (Set.fromList [3, 4])
--         , IntMap.fromList $ zip [0..] $ map Set.fromList
--             [ ["quux", "5"]
--             , []
--             , ["fizz", "buzz"]
--             ]
--         )
--       ]
--   , testComplexMap2 =
--       Map.singleton
--         (Just (Set.fromList [1..5]))
--         (Map.fromList
--            [ (NonEmpty.fromList [1, 2],  Vector.fromList ["foo", "bar", "baz"])
--            , (NonEmpty.fromList [3],     Vector.fromList ["quux"])
--            , (NonEmpty.fromList [4..10], Vector.fromList ["must", "put", "something", "in", "here"])
--            ])
--   }
-- :}
--
-- >>> ppGeneric test
-- Test
--   { testSet         -> Just ({1, 2, 3})
--   , testMap         -> {foo -> {2.5, 3.0, 4.0}}
--   , testIntSet      -> {1, 2, 4, 5, 7}
--   , testIntMap      -> {1 -> 3, 2 -> 2foo, 3 -> 11}
--   , testInt         -> 42
--   , testComplexMap  ->
--       { Nothing -> {0 -> {bar, foo}, 1 -> {baz}, 2 -> {frob, quux}}
--       , Just ({1}) -> {0 -> {quux}, 1 -> {buzz, fizz}}
--       , Just ({3, 4}) -> {0 -> {5, quux}, 1 -> {}, 2 -> {buzz, fizz}}
--       }
--   , testComplexMap2 ->
--       { Just ({1, 2, 3, 4, 5}) ->
--           { [1, 2] -> [foo, bar, baz]
--           , [3] -> [quux]
--           , [4, 5, 6, 7, 8, 9, 10] -> [must, put, something, in, here]
--           } }
--   }
ppGeneric :: (Generic a, GPretty (Rep a)) => a -> Doc ann
ppGeneric = mdPayload . gpretty . from

class GPretty (a :: Type -> Type) where
  gpretty :: a ix -> MetaDoc ann

instance GPretty V1 where
  gpretty _ = error "gpretty for V1"

instance GPretty U1 where
  gpretty U1 = mempty

instance (GPretty f, GPretty g) => GPretty (f :+: g) where
  gpretty = \case
    L1 x -> gpretty x
    R1 y -> gpretty y

-- 'PPGenericDeriving' to give it a chance to fire before standard 'Pretty'.
instance PPGenericOverride a => GPretty (K1 i a) where
  gpretty = ppGenericOverride . unK1


-- | A class to override 'Pretty' when calling 'ppGeneric' without introducing
-- orphans for standard types.
class PPGenericOverride a where
  ppGenericOverride :: a -> MetaDoc ann

ppGenericOverrideDoc :: PPGenericOverride a => a -> Doc ann
ppGenericOverrideDoc = mdPayload . ppGenericOverride

newtype PPGenericOverrideToPretty a = PPGenericOverrideToPretty { unPPGenericOverrideToPretty :: a }

instance PPGenericOverride a => Pretty (PPGenericOverrideToPretty a) where
  pretty = mdPayload . ppGenericOverride . unPPGenericOverrideToPretty


-- | Fall back to standard 'Pretty' instance when no override is available.
instance Pretty a => PPGenericOverride a where
  ppGenericOverride = compositeMetaDoc . pretty


instance {-# OVERLAPS #-} PPGenericOverride Int where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocInt

instance {-# OVERLAPS #-} PPGenericOverride Float where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocFloat

instance {-# OVERLAPS #-} PPGenericOverride Double where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocDouble

instance {-# OVERLAPS #-} PPGenericOverride Integer where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocInteger

instance {-# OVERLAPS #-} PPGenericOverride Natural where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocNatural

instance {-# OVERLAPS #-} PPGenericOverride Word where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocWord

instance {-# OVERLAPS #-} PPGenericOverride Word8 where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocWord8

instance {-# OVERLAPS #-} PPGenericOverride Word16 where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocWord16

instance {-# OVERLAPS #-} PPGenericOverride Word32 where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocWord32

instance {-# OVERLAPS #-} PPGenericOverride Word64 where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocWord64

instance {-# OVERLAPS #-} PPGenericOverride Int8 where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocInt8

instance {-# OVERLAPS #-} PPGenericOverride Int16 where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocInt16

instance {-# OVERLAPS #-} PPGenericOverride Int32 where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocInt32

instance {-# OVERLAPS #-} PPGenericOverride Int64 where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocInt64

instance {-# OVERLAPS #-} PPGenericOverride () where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocUnit

instance {-# OVERLAPS #-} PPGenericOverride Bool where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocBool

instance {-# OVERLAPS #-} PPGenericOverride Char where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = metaDocChar

instance {-# OVERLAPS #-} PPGenericOverride a => PPGenericOverride (Ratio a) where
  {-# INLINABLE ppGenericOverride #-}
  ppGenericOverride (x :% y) =
    ppGenericOverride x <> atomicMetaDoc "/" <> ppGenericOverride y

instance {-# OVERLAPS #-} PPGenericOverride CallStack where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride =
    compositeMetaDoc . ppCallStack


instance {-# OVERLAPS #-} PPGenericOverride (Doc Void) where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride =
    compositeMetaDoc . fmap absurd

instance {-# OVERLAPS #-} PPGenericOverride String where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = stringMetaDoc

instance {-# OVERLAPS #-} PPGenericOverride T.Text where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = strictTextMetaDoc

instance {-# OVERLAPS #-} PPGenericOverride TL.Text where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = lazyTextMetaDoc

instance {-# OVERLAPS #-} PPGenericOverride C8.ByteString where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = strictByteStringMetaDoc

instance {-# OVERLAPS #-} PPGenericOverride CL8.ByteString where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = lazyByteStringMetaDoc

instance {-# OVERLAPS #-} PPGenericOverride ShortBS.ShortByteString where
  {-# INLINE ppGenericOverride #-}
  ppGenericOverride = shortByteStringMetaDoc

instance {-# OVERLAPS #-} PPGenericOverride (ForeignPtr a)        where ppGenericOverride = atomicMetaDoc . pretty . show

instance {-# OVERLAPS #-} PPGenericOverride a => PPGenericOverride (TH.TyVarBndr a) where ppGenericOverride = gpretty . from

instance {-# OVERLAPS #-} PPGenericOverride TH.OccName            where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.NameFlavour        where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.PkgName            where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.NameSpace          where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.ModName            where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Name               where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.TyLit              where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Type               where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.SourceUnpackedness where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.SourceStrictness   where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Bang               where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Con                where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Lit                where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Bytes              where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Stmt               where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Guard              where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Body               where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Match              where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Range              where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Exp                where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Pat                where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Clause             where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.DerivStrategy      where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.DerivClause        where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.FunDep             where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Overlap            where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Callconv           where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Safety             where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Foreign            where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.FixityDirection    where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Fixity             where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Inline             where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.RuleMatch          where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Phases             where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.RuleBndr           where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.AnnTarget          where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Pragma             where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.TySynEqn           where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.FamilyResultSig    where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.InjectivityAnn     where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.TypeFamilyHead     where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Role               where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.PatSynArgs         where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.PatSynDir          where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Dec                where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Info               where ppGenericOverride = gpretty . from
instance {-# OVERLAPS #-} PPGenericOverride TH.Specificity        where ppGenericOverride = gpretty . from

#if MIN_VERSION_template_haskell(2, 21, 0)
instance {-# OVERLAPS #-} PPGenericOverride TH.BndrVis            where ppGenericOverride = gpretty . from
#endif

#if MIN_VERSION_template_haskell(2, 22, 0)
instance {-# OVERLAPS #-} PPGenericOverride TH.NamespaceSpecifier where ppGenericOverride = gpretty . from
#endif

instance {-# OVERLAPS #-}
  ( PPGenericOverride a
  , PPGenericOverride b
  ) => PPGenericOverride (a, b) where
  ppGenericOverride (a, b) = atomicMetaDoc $ pretty
    ( PPGenericOverrideToPretty a
    , PPGenericOverrideToPretty b
    )

instance {-# OVERLAPS #-}
  ( PPGenericOverride a
  , PPGenericOverride b
  , PPGenericOverride c
  ) => PPGenericOverride (a, b, c) where
  ppGenericOverride (a, b, c) = atomicMetaDoc $ pretty
    ( PPGenericOverrideToPretty a
    , PPGenericOverrideToPretty b
    , PPGenericOverrideToPretty c
    )

instance {-# OVERLAPS #-} PPGenericOverride v => PPGenericOverride (Maybe v) where
  ppGenericOverride =
    gpretty . from . fmap PPGenericOverrideToPretty

instance {-# OVERLAPS #-} PPGenericOverride v => PPGenericOverride [v] where
  ppGenericOverride =
    atomicMetaDoc . ppListWith ppGenericOverrideDoc

instance {-# OVERLAPS #-} (PPGenericOverride k, PPGenericOverride v) => PPGenericOverride [(k, v)] where
  ppGenericOverride =
    atomicMetaDoc . ppAssocListWith ppGenericOverrideDoc ppGenericOverrideDoc

instance {-# OVERLAPS #-} PPGenericOverride k => PPGenericOverride (NonEmpty k) where
  ppGenericOverride =
    atomicMetaDoc . ppNEWith ppGenericOverrideDoc

instance {-# OVERLAPS #-} PPGenericOverride v => PPGenericOverride (Vector v) where
  ppGenericOverride =
    atomicMetaDoc . ppVectorWith ppGenericOverrideDoc


instance {-# OVERLAPS #-} (PPGenericOverride k, PPGenericOverride v) => PPGenericOverride (Map k v) where
  ppGenericOverride =
    atomicMetaDoc . ppMapWith ppGenericOverrideDoc ppGenericOverrideDoc

instance {-# OVERLAPS #-} PPGenericOverride v => PPGenericOverride (Set v) where
  ppGenericOverride =
    atomicMetaDoc . ppSetWith ppGenericOverrideDoc

instance {-# OVERLAPS #-} (PPGenericOverride k, PPGenericOverride v) => PPGenericOverride (Bimap k v) where
  ppGenericOverride =
    atomicMetaDoc . ppBimapWith ppGenericOverrideDoc ppGenericOverrideDoc

instance {-# OVERLAPS #-} PPGenericOverride IntSet where
  ppGenericOverride =
    atomicMetaDoc . ppIntSetWith ppGenericOverrideDoc

instance {-# OVERLAPS #-} PPGenericOverride v => PPGenericOverride (IntMap v) where
  ppGenericOverride =
    atomicMetaDoc . ppIntMapWith ppGenericOverrideDoc ppGenericOverrideDoc

#ifdef HAVE_ENUMMAPSET
instance {-# OVERLAPS #-} (Enum a, PPGenericOverride a) => PPGenericOverride (EnumSet a) where
  ppGenericOverride =
    atomicMetaDoc . ppEnumSetWith ppGenericOverrideDoc

instance {-# OVERLAPS #-} (Enum k, PPGenericOverride k, PPGenericOverride v) => PPGenericOverride (EnumMap k v ) where
  ppGenericOverride =
    atomicMetaDoc . ppEnumMapWith ppGenericOverrideDoc ppGenericOverrideDoc
#endif

instance {-# OVERLAPS #-} PPGenericOverride v => PPGenericOverride (HashSet v) where
  ppGenericOverride =
    atomicMetaDoc . ppHashSetWith ppGenericOverrideDoc

instance {-# OVERLAPS #-} (PPGenericOverride k, PPGenericOverride v) => PPGenericOverride (HashMap k v) where
  ppGenericOverride =
    atomicMetaDoc . ppHashMapWith ppGenericOverrideDoc ppGenericOverrideDoc

instance {-# OVERLAPS #-} PPGenericOverride (f (g a)) => PPGenericOverride (Compose f g a) where
  ppGenericOverride =
    ppGenericOverride . getCompose


instance (GPretty f, GPretty g) => GPretty (f :*: g) where
  gpretty (x :*: y) =
    compositeMetaDoc $ mdPayload x' <+> mdPayload y'
    where
      x' = gpretty x
      y' = gpretty y

instance GPretty x => GPretty (M1 D ('MetaData a b c d) x) where
  gpretty = gpretty . unM1

instance GPretty x => GPretty (M1 S ('MetaSel 'Nothing b c d) x) where
  gpretty = gpretty . unM1

instance (KnownSymbol name, GFields x) => GPretty (M1 C ('MetaCons name _fixity 'False) x) where
  gpretty (M1 x) =
    constructorAppMetaDoc constructor args
    where
      constructor :: MetaDoc ann
      constructor = atomicMetaDoc $ pretty $ symbolVal $ Proxy @name
      args :: [MetaDoc ann]
      args = toList $ gfields x

class GFields a where
  gfields :: a ix -> DList (MetaDoc ann)

instance GFields U1 where
  {-# INLINE gfields #-}
  gfields = const mempty

instance GPretty x => GFields (M1 S ('MetaSel a b c d) x) where
  {-# INLINABLE gfields #-}
  gfields = DL.singleton . gpretty . unM1

instance (GFields f, GFields g) => GFields (f :*: g) where
  {-# INLINABLE gfields #-}
  gfields (f :*: g) = gfields f <> gfields g


instance (KnownSymbol name, GCollectRecord f) => GPretty (M1 C ('MetaCons name _fixity 'True) f) where
  gpretty (M1 x) =
    compositeMetaDoc $
      ppDictHeader
        (pretty (symbolVal (Proxy @name)))
        (map (fmap mdPayload) (toList (gcollectRecord x)))

class GCollectRecord a where
  gcollectRecord :: a ix -> DList (MapEntry Text (MetaDoc ann))

instance (KnownSymbol name, GPretty a) => GCollectRecord (M1 S ('MetaSel ('Just name) su ss ds) a) where
  {-# INLINABLE gcollectRecord #-}
  gcollectRecord (M1 x) =
    DL.singleton (T.pack (symbolVal (Proxy @name)) :-> gpretty x)

instance (GCollectRecord f, GCollectRecord g) => GCollectRecord (f :*: g) where
  {-# INLINABLE gcollectRecord #-}
  gcollectRecord (f :*: g) = gcollectRecord f <> gcollectRecord g

instance GCollectRecord U1 where
  {-# INLINABLE gcollectRecord #-}
  gcollectRecord = const mempty
