{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.JsonSpec.Encode (
  HasJsonEncodingSpec(..),
  StructureToJSON(..),
  encode,
) where


import Data.Aeson (ToJSON(toJSON), Value)
import Data.JsonSpec.Spec
  ( Field(Field), Ref(unRef), Specification(JsonArray), JSONStructure, JStruct
  , Tag, sym
  )
import Data.Proxy (Proxy(Proxy))
import Data.Scientific (Scientific)
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.TypeLits (KnownSymbol)
import Prelude
  ( Either(Left, Right), Functor(fmap), Maybe(Just, Nothing), Monoid(mempty)
  , (.), Bool, Int, id, maybe
  )
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Set as Set


{- |
  Types of this class can be encoded to JSON according to a type-level
  'Specification'.
-}
class HasJsonEncodingSpec a where
  {- | The encoding specification. -}
  type EncodingSpec a :: Specification

  {- | Encode the value into the structure appropriate for the specification. -}
  toJSONStructure :: a -> JSONStructure (EncodingSpec a)
instance (HasJsonEncodingSpec a) => HasJsonEncodingSpec (Set a) where
  type EncodingSpec (Set a) = JsonArray (EncodingSpec a)
  toJSONStructure = fmap toJSONStructure . Set.toList


{- |
  This is like 'ToJSON', but specialized for our custom "json
  representation" types (i.e. the 'JSONStructure' type family). It is
  also closed (i.e. not exported, so the user can't add instances),
  because our json representation is closed.

  see 'StructureFromJSON' for an explaination about why we don't just use
  'ToJSON'.
-}
class StructureToJSON a where
  reprToJSON :: a -> Value
instance StructureToJSON Value where
  reprToJSON = id
instance StructureToJSON () where
  reprToJSON () = A.object []
instance StructureToJSON Bool where
  reprToJSON = toJSON
instance StructureToJSON Text where
  reprToJSON = toJSON
instance StructureToJSON Scientific where
  reprToJSON = toJSON
instance StructureToJSON Int where
  reprToJSON = toJSON
instance (ToJSONObject (a, b)) => StructureToJSON (a, b) where
  reprToJSON = A.Object . toJSONObject
instance (StructureToJSON left, StructureToJSON right) => StructureToJSON (Either left right) where
  reprToJSON = \case
    Left val -> reprToJSON val
    Right val -> reprToJSON val
instance (KnownSymbol const) => StructureToJSON (Tag const) where
  reprToJSON _proxy = toJSON (sym @const @Text)
instance (StructureToJSON a) => StructureToJSON [a] where
  reprToJSON = toJSON . fmap reprToJSON
instance StructureToJSON UTCTime where
  reprToJSON = toJSON
instance (StructureToJSON a) => StructureToJSON (Maybe a) where
  reprToJSON = maybe A.Null reprToJSON
instance
    (StructureToJSON (JStruct env spec))
  =>
    StructureToJSON (Ref env spec)
  where
    reprToJSON = reprToJSON . unRef


{- |
  This class is to help 'StructureToJSON' recursively encode objects, and
  is mutually recursive with 'StructureToJSON'. If we tried to "recurse
  on the rest of the object" directly in 'StructureToJSON' we would end
  up with a partial function, because 'reprToJSON' returns a 'Value'
  not an 'Object'. We would therefore have to pattern match on 'Value'
  to get the 'Object' back out, but we would have to call 'error' if the
  'Value' mysteriously somehow wasn't an 'Object' after all. Instead of
  calling error because "it can't ever happen", we use this helper so
  the compiler can prove it never happens.
-}
class ToJSONObject a where
  toJSONObject :: a -> A.Object
instance ToJSONObject () where
  toJSONObject _ = mempty
instance (KnownSymbol key, StructureToJSON val, ToJSONObject more) => ToJSONObject (Field key val, more) where
  toJSONObject (Field val, more) =
    KM.insert
      (sym @key)
      (reprToJSON val)
      (toJSONObject more)
instance (KnownSymbol key, StructureToJSON val, ToJSONObject more) => ToJSONObject (Maybe (Field key val), more) where
  toJSONObject (mval, more) =
    case mval of
      Nothing -> toJSONObject more
      Just (Field val) ->
        KM.insert
          (sym @key)
          (reprToJSON val)
          (toJSONObject more)


{-|
  Given a raw Haskell structure, directly encode it directly into an
  aeson Value without having to go through any To/FromJSON instances.

  See also: `Data.JsonSpec.eitherDecode`.
-}
encode :: StructureToJSON (JSONStructure spec) => Proxy spec -> JSONStructure spec -> Value
encode Proxy = reprToJSON


