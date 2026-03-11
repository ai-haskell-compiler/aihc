{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Decoding using specs. -}
module Data.JsonSpec.Decode (
  StructureFromJSON(..),
  HasJsonDecodingSpec(..),
  eitherDecode,
) where


import Control.Applicative (Alternative((<|>)))
import Data.Aeson.Types
  ( FromJSON(parseJSON), Value(Null, Object), Parser, parseEither, withArray
  , withObject, withScientific, withText
  )
import Data.JsonSpec.Spec
  ( Field(Field), Ref(Ref), Tag(Tag), JSONStructure, JStruct, Specification, sym
  )
import Data.Proxy (Proxy)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.TypeLits (KnownSymbol)
import Prelude
  ( Applicative(pure), Either(Left, Right), Eq((==)), Functor(fmap)
  , Maybe(Just, Nothing), MonadFail(fail), Semigroup((<>))
  , Traversable(traverse), ($), (.), (<$>), Bool, Int, String
  )
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as Vector


{- |
  Types of this class can be JSON decoded according to a type-level
  'Specification'.
-}
class HasJsonDecodingSpec a where
  {- | The decoding 'Specification'. -}
  type DecodingSpec a :: Specification

  {- |
    Given the structural encoding of the JSON data, parse the structure
    into the final type. The reason this returns a @'Parser' a@ instead of
    just a plain @a@ is because there may still be some invariants of the
    JSON data that the 'Specification' language is not able to express,
    and so you may need to fail parsing in those cases. For instance,
    'Specification' is not powerful enough to express "this field must
    contain only prime numbers".
  -}
  fromJSONStructure :: JSONStructure (DecodingSpec a) -> Parser a


{- |
  Analog of 'Data.Aeson.FromJSON', but specialized for decoding our
  "json representations", and closed to the user because the haskell
  representation scheme is fixed and not extensible by the user.

  We can't just use 'Data.Aeson.FromJSON' because the types we are using
  to represent "json data" (i.e. the 'JSONStructure' type family) already
  have 'ToJSON' instances. Even if we were to make a bunch of newtypes
  or whatever to act as the json representation (and therefor also force
  the user to do a lot of wrapping and unwrapping), that still wouldn't
  be sufficient because someone could always write an overlapping (or
  incoherent) 'ToJSON' instance of our newtype! This way we don't have
  to worry about any of that, and the types that the user must deal with
  when implementing 'fromJSONRepr' can be simple tuples and such.
-}
class StructureFromJSON a where
  reprParseJSON :: Value -> Parser a
instance StructureFromJSON Value where
  reprParseJSON = pure
instance StructureFromJSON Text where
  reprParseJSON = withText "string" pure
instance StructureFromJSON Scientific where
  reprParseJSON = withScientific "number" pure
instance StructureFromJSON Int where
  reprParseJSON = parseJSON
instance StructureFromJSON () where
  reprParseJSON =
    withObject "empty object" $ \_ -> pure ()
instance StructureFromJSON Bool where
  reprParseJSON = parseJSON
instance (KnownSymbol key, StructureFromJSON val, StructureFromJSON more) => StructureFromJSON (Field key val, more) where
  reprParseJSON =
    withObject "object" $ \o -> do
      more <- reprParseJSON (Object o)
      case KM.lookup (sym @key) o of
        Nothing -> fail $ "could not find key: " <> sym @key
        Just rawVal -> do
          val <- reprParseJSON rawVal
          pure (Field val, more)
instance (KnownSymbol key, StructureFromJSON val, StructureFromJSON more) => StructureFromJSON (Maybe (Field key val), more) where
  reprParseJSON =
    withObject "object" $ \o -> do
      more <- reprParseJSON (Object o)
      case KM.lookup (sym @key) o of
        Nothing ->
          pure (Nothing, more)
        Just rawVal -> do
          val <- reprParseJSON rawVal
          pure (Just (Field val), more)
instance (StructureFromJSON left, StructureFromJSON right) => StructureFromJSON (Either left right) where
  reprParseJSON v =
    (Left <$> reprParseJSON v)
    <|> (Right <$> reprParseJSON v)
instance (KnownSymbol const) => StructureFromJSON (Tag const) where
  reprParseJSON =
    withText "constant" $ \c ->
      if c == sym @const then pure Tag
      else fail "unexpected constant value"
instance (StructureFromJSON a) => StructureFromJSON [a] where
  reprParseJSON =
    withArray
      "list"
      (fmap Vector.toList . traverse reprParseJSON)
instance StructureFromJSON UTCTime where
  reprParseJSON = parseJSON
instance (StructureFromJSON a) => StructureFromJSON (Maybe a) where
  reprParseJSON val = do
    case val of
      Null -> pure Nothing
      _ -> Just <$> reprParseJSON val
instance
    (StructureFromJSON (JStruct env spec))
  =>
    StructureFromJSON (Ref env spec)
  where
  reprParseJSON val =
    Ref <$> reprParseJSON val


{-|
  Directly decode some JSON accoring to a spec without going through
  any To/FromJSON instances.
-}
eitherDecode
  :: forall spec.
     (StructureFromJSON (JSONStructure spec))
   => Proxy (spec :: Specification)
  -> Value
  -> Either String (JSONStructure spec)
eitherDecode _spec =
  parseEither reprParseJSON


