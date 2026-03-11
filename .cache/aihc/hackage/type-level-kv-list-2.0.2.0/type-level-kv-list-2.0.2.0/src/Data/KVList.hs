{-| This library provide a brief implementation for extensible records.
  It is sensitive to the ordering of key-value items, but has simple type constraints and provides short compile time.
-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ApplicativeDo #-}

module Data.KVList
  (
  -- * Constructors
  -- $setup
    KVList
  , (:=)((:=))
  , (&=)
  , (&=>)
  , kvcons
  , empty
  , singleton
  , ListKey(..)

  -- * Operators
  , get
  , HasKey
  , (&.)
  , (&.?)
  , (&.??)
  )
where

import Prelude

import Data.Functor ((<&>))
import qualified Data.List as List
import Data.Kind (Constraint, Type)
import Data.Typeable (Typeable, typeOf)
import Data.Proxy (Proxy(Proxy))
import GHC.TypeLits (KnownSymbol, Symbol, TypeError, ErrorMessage(Text), symbolVal)
import GHC.OverloadedLabels (IsLabel(..))
import Unsafe.Coerce (unsafeCoerce)


-- Constructors

{- $setup #constructors#
  We can create type level KV list as follows.

  >>> :set -XOverloadedLabels -XTypeOperators
  >>> import Prelude
  >>> import Data.KVList (empty, KVList, (:=)((:=)), (&.), (&=))
  >>> import qualified Data.KVList as KVList
  >>> let sampleList = KVList.empty &= #foo := "str" &= #bar := 34
  >>> type SampleList = KVList '[ "foo" := String, "bar" := Int ]
-}

{-| A value with type level key.
-}
data KVList (kvs :: [Type]) where
  KVNil :: KVList '[]
  KVCons :: (KnownSymbol key) => key := v -> KVList xs -> KVList ((key := v) ': xs)

instance Eq (KVList '[]) where
  (==) _ _ = True

instance (Eq v, Eq (KVList kvs)) => Eq (KVList ((k := v) ': kvs)  ) where
  (==) (KVCons (_ := v1) next1) (KVCons (_ := v2) next2) = v1 == v2 && next1 == next2

{-| -}
instance ShowFields (KVList kvs) => Show (KVList kvs) where
  show kvs =
    ( List.unwords $
      "(KVList.empty" : showFields kvs
    ) ++ ")"

class ShowFields a where
  showFields :: a -> [String]

instance ShowFields (KVList '[]) where
  showFields _ = []

instance ( ShowFields (KVList kvs)
         , Show v
         ) => ShowFields (KVList ((k := v) ': kvs)) where
  showFields (KVCons (k := v) next) =
    let
      firstLine str =
        List.unwords
          [ "&="
          , "#" <> show k
          , ":="
          , str
          ]
    in
    ( case List.lines $ show v of
        [] -> [ firstLine "" ]
        [a] -> [ firstLine a ]
        as ->
          List.concat
            [ [ firstLine "(" ]
            , as <&> \x -> "  " ++ x
            , [ ")" ]
            ]
    ) ++ showFields next

{-| -}
empty :: KVList '[]
empty = KVNil

{-| -}
(&=) :: (KnownSymbol k, Appended kvs '[k := v] ~ appended) => KVList kvs -> (k := v) -> KVList appended
(&=) kvs kv = append kvs (singleton kv)
{-# INLINE (&=) #-}

infixl 1 &=

{-| Applicative version of '(&=)'.

>>> import Data.KVList ((&=>))
>>> :{
  pure KVList.empty
    &=> #foo := (Just 3)
    &=> #bar := (Just "bar")
:}
Just (KVList.empty &= #foo := 3 &= #bar := "bar")

>>> :{
  pure KVList.empty
    &=> #foo := (Just 3)
    &=> #bar := Nothing
:}
Nothing

-}
(&=>) :: (Applicative f, KnownSymbol k, Appended kvs '[k := v] ~ appended) => f (KVList kvs) -> (k := f v) -> f (KVList appended)
(&=>) fkvs (k := fv) = do
  kvs <- fkvs
  v <- fv
  pure $ (&=) kvs (k := v)
{-# INLINE (&=>) #-}

infixl 1 &=>

{-| -}
kvcons :: (KnownSymbol k) => (k := v) -> KVList kvs -> KVList ((k := v) ': kvs)
kvcons = KVCons

{-| -}
data (key :: Symbol) := (value :: Type) where
  (:=) :: ListKey a -> b -> a := b
infix 2 :=

deriving instance (KnownSymbol key, Show value) => Show (key := value)

{-| -}
type HasKey (key :: Symbol) (kvs :: [Type]) (v :: Type) = HasKey_ key kvs kvs v

type family HasKey_ (key :: Symbol) (kvs :: [Type]) (orig :: [Type]) (v :: Type) :: Constraint where
  HasKey_ key '[] '[] v = TypeError ('Text "The KVList is empty.")
  HasKey_ key '[] orig v = TypeError ('Text "The Key is not in the KVList.")
  HasKey_ key ((key := val) ': _) _ v = (val ~ v)
  HasKey_ key (_ ': kvs) orig v = HasKey_ key kvs orig v

{-| -}
type family Appended kvs1 kv2 :: [Type] where
  Appended '[] kv2 = kv2
  Appended (kv ': kvs) kv2 =
    kv ': Appended kvs kv2

{-| -}
append :: (Appended kvs1 kvs2 ~ appended) => KVList kvs1 -> KVList kvs2 -> KVList appended
append KVNil kvs2 = kvs2
append (KVCons kv kvs) kvs2 = KVCons kv (append kvs kvs2)


{-| -}
singleton :: (KnownSymbol k) => (k := v) -> KVList '[ k := v ]
singleton kv = KVCons kv KVNil


{-| -}
get :: (KnownSymbol key, HasKey key kvs v) => ListKey key -> KVList kvs -> v
get p kvs = get_ p kvs kvs

get_ :: (KnownSymbol key, HasKey key orig v) => ListKey key -> KVList kvs -> KVList orig -> v
get_ _ KVNil KVNil = error "Unreachable: The KVList is empty."
get_ _ KVNil _ = error "Unreachable: The Key is not in the KVList."
get_ p (KVCons (k := v) kvs) orig =
  if typeOf p == typeOf k then
    unsafeCoerce v
  else
    get_ p kvs orig


{-| -}
(&.) :: (KnownSymbol key, HasKey key kvs v) => KVList kvs -> ListKey key -> v
(&.) kvs k = get k kvs
infixl 9 &.

{-| Helper operator for optional chain.

@
(&.?) mkvs k = fmap (&. k) mkvs
@

>>> import Data.KVList ((&.?))
>>> :{
  ( KVList.empty
    &= #foo := Just
      (KVList.empty
        &= #bar := "bar"
      )
  ) &. #foo &.? #bar
:}
Just "bar"

-}
(&.?) :: (KnownSymbol key, HasKey key kvs v, Functor f) => f (KVList kvs) -> ListKey key -> f v
(&.?) mkvs k = fmap (&. k) mkvs
infixl 9 &.?

{-| Helper operator for optional chain.

@
(&.??) mkvs k = (&. k) =<< mkvs
@

>>> import Data.KVList ((&.??))
>>> :{
  ( KVList.empty
    &= #foo := Just
      (KVList.empty
        &= #bar := Just "bar"
      )
  ) &. #foo &.?? #bar
:}
Just "bar"

-}
(&.??) :: (KnownSymbol key, HasKey key kvs (m v), Monad m) => m (KVList kvs) -> ListKey key -> m v
(&.??) mkvs k = (&. k) =<< mkvs
infixl 9 &.??

{-| 'ListKey' is just a proxy, but needed to implement a non-orphan 'IsLabel' instance.
In most cases, you only need to create a `ListKey` instance with @OverloadedLabels@, such as `#foo`.
-}
data ListKey (t :: Symbol)
    = ListKey
    deriving (Eq, Typeable)

instance (KnownSymbol t) => Show (ListKey t) where
  show _ = symbolVal (Proxy :: Proxy t)

instance l ~ l' => IsLabel (l :: Symbol) (ListKey l') where
#if MIN_VERSION_base(4, 10, 0)
    fromLabel = ListKey
#else
    fromLabel _ = ListKey
#endif
