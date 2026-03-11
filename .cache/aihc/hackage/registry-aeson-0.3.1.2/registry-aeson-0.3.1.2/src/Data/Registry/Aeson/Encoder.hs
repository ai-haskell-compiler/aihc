{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-
  An Encoder is used to encode a specific data type into a Aeson Object
  This module provides several functions to create encoders and assemble them into a registry of encoders.
-}

module Data.Registry.Aeson.Encoder
  ( module Data.Registry.Aeson.Encoder,
    module Data.Registry.Aeson.TH.Encoder,
    module Data.Registry.Aeson.TH.ThOptions,
  )
where

import Data.Aeson
import Data.Aeson.Encoding.Internal
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as BL (toStrict)
import Data.Functor.Contravariant
import Data.Map qualified as M
import Data.Registry
import Data.Registry.Aeson.TH.Encoder
import Data.Registry.Aeson.TH.ThOptions
import Data.Set qualified as S
import Data.Vector qualified as V
import Protolude hiding (Type, list)
import Prelude (String)

-- * ENCODER DATA TYPE

newtype Encoder a = Encoder {encode :: a -> (Value, Encoding)}

instance Contravariant Encoder where
  contramap f (Encoder a) = Encoder (a . f)

newtype KeyEncoder a = KeyEncoder {encodeAsKey :: a -> Key}

instance Contravariant KeyEncoder where
  contramap f (KeyEncoder a) = KeyEncoder (a . f)

-- * ENCODE VALUES

encodeByteString :: Encoder a -> a -> ByteString
encodeByteString (Encoder e) = BL.toStrict . encodingToLazyByteString . snd . e

encodeValue :: Encoder a -> a -> Value
encodeValue (Encoder e) = fst . e

-- * CREATE KEY ENCODERS

-- | Make a key encoder from a function returning some text
encodeKey :: forall a. Typeable a => (a -> Text) -> Typed (KeyEncoder a)
encodeKey f = fun (keyEncoder f)

keyEncoder :: (a -> Text) -> KeyEncoder a
keyEncoder f = KeyEncoder $ K.fromString . toS . f

-- * CREATE ENCODERS

-- | Create an Encoder from a function returning a Value
fromValue :: (a -> Value) -> Encoder a
fromValue f = Encoder $ \a -> let v = f a in (v, value v)

-- | Create an encoder from a Aeson instance
jsonEncoder :: forall a. (ToJSON a, Typeable a) => Typed (Encoder a)
jsonEncoder = fun (jsonEncoderOf @a)

jsonEncoderOf :: ToJSON a => Encoder a
jsonEncoderOf = Encoder $ \a -> (toJSON a, toEncoding a)

-- * COMBINATORS

-- | Create an Encoder for a (Maybe a)
encodeMaybeOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder (Maybe a))
encodeMaybeOf = fun (maybeOfEncoder @a)

maybeOfEncoder :: Encoder a -> Encoder (Maybe a)
maybeOfEncoder (Encoder e) = Encoder $ \case
  Nothing -> (Null, null_)
  Just a -> e a

-- | Create an Encoder for a pair (a, b)
encodePairOf :: forall a b. (Typeable a, Typeable b) => Typed (Encoder a -> Encoder b -> Encoder (a, b))
encodePairOf = fun (pairOfEncoder @a @b)

pairOfEncoder :: Encoder a -> Encoder b -> Encoder (a, b)
pairOfEncoder (Encoder ea) (Encoder eb) =
  Encoder $ \(a, b) -> do
    let (ls1, ls2) = unzip [ea a, eb b]
    (array ls1, list identity ls2)

-- | Create an Encoder for a tripe (a, b, c)
encodeTripleOf :: forall a b c. (Typeable a, Typeable b, Typeable c) => Typed (Encoder a -> Encoder b -> Encoder c -> Encoder (a, b, c))
encodeTripleOf = fun (tripleOfEncoder @a @b @c)

tripleOfEncoder :: Encoder a -> Encoder b -> Encoder c -> Encoder (a, b, c)
tripleOfEncoder (Encoder ea) (Encoder eb) (Encoder ec) =
  Encoder $ \(a, b, c) -> do
    let (ls1, ls2) = unzip [ea a, eb b, ec c]
    (array ls1, list identity ls2)

-- | Create an Encoder for a Set a
encodeSetOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder (Set a))
encodeSetOf = fun (setOfEncoder @a)

setOfEncoder :: Encoder a -> Encoder (Set a)
setOfEncoder (Encoder ea) = Encoder $ \as -> do
  let (ls1, ls2) = unzip (ea <$> S.toList as)
  (array ls1, list identity ls2)

-- | Create an Encoder for a list [a]
encodeListOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder [a])
encodeListOf = fun (listOfEncoder @a)

listOfEncoder :: Encoder a -> Encoder [a]
listOfEncoder (Encoder ea) = Encoder $ \as -> do
  let (ls1, ls2) = unzip (ea <$> as)
  (array ls1, list identity ls2)

-- | Create an Encoder for a map a b
encodeMapOf :: forall a b. (Typeable a, Typeable b) => Typed (KeyEncoder a -> Encoder b -> Encoder (Map a b))
encodeMapOf = fun (mapOfEncoder @a @b)

mapOfEncoder :: KeyEncoder a -> Encoder b -> Encoder (Map a b)
mapOfEncoder ea (Encoder eb) = Encoder $ \ms -> do
  let ks = encodeAsKey ea <$> M.keys ms
  let (vs1, vs2) = unzip (eb <$> toList ms)
  (Object $ KM.fromList $ zip ks vs1, pairs $ foldMap (uncurry pair) (zip ks vs2))

-- | Create an Encoder for a non-empty list (NonEmpty a)
encodeNonEmptyOf :: forall a. (Typeable a) => Typed (Encoder a -> Encoder (NonEmpty a))
encodeNonEmptyOf = fun (nonEmptyOfEncoder @a)

nonEmptyOfEncoder :: Encoder a -> Encoder (NonEmpty a)
nonEmptyOfEncoder = contramap toList . listOfEncoder

-- | Shortcut function to create arrays
array :: [Value] -> Value
array = Array . V.fromList

-- * DEFAULT VALUES

defaultEncoderOptions :: Registry _ _
defaultEncoderOptions =
  fun defaultConstructorEncoder
    <: fun textKeyEncoder
    <: fun stringKeyEncoder
    <: val defaultOptions

textKeyEncoder :: KeyEncoder Text
textKeyEncoder = KeyEncoder K.fromText

stringKeyEncoder :: KeyEncoder String
stringKeyEncoder = KeyEncoder K.fromString

-- * BUILDING ENCODERS

-- | A ConstructorEncoder uses configuration options + type information extracted from
--   a given data type (with TemplateHaskell) in order to produce a Value and an Encoding
newtype ConstructorEncoder = ConstructorEncoder
  { encodeConstructor :: Options -> FromConstructor -> (Value, Encoding)
  }

-- | Default implementation, it can be overridden in a registry
defaultConstructorEncoder :: ConstructorEncoder
defaultConstructorEncoder = ConstructorEncoder makeEncoderFromConstructor

-- | Minimum set of data extracted from a given type with Template Haskell
--   in order to create the appropriate encoder given an Options value
data FromConstructor = FromConstructor
  { -- | names of all the constructors of the type
    fromConstructorNames :: [Text],
    -- | types of all the constructors of the type
    fromConstructorTypes :: [Text],
    -- | name of the constructor for the value to encode
    fromConstructorName :: Text,
    -- | name of all the constructor fields
    fromConstructorFieldNames :: [Text],
    -- | encoded values of all the constructor fields
    fromConstructorValues :: [(Value, Encoding)]
  }
  deriving (Eq, Show)

-- | Make an Encoder from Options and the representation of a constructor for a given value to encode
makeEncoderFromConstructor :: Options -> FromConstructor -> (Value, Encoding)
makeEncoderFromConstructor options fromConstructor = do
  let fc = modifyFromConstructorWithOptions options fromConstructor
  case fc of
    -- nullary constructors
    FromConstructor _ [] name _ _ ->
      if allNullaryToStringTag options
        then (String name, string $ toS name)
        else makeSumEncoding options fc
    -- single constructor
    FromConstructor [_] _ _ names values ->
      if tagSingleConstructors options
        then case (names, values) of
          (_, [v]) | sumEncoding options == UntaggedValue && unwrapUnaryRecords options -> v
          _ -> makeSumEncoding options fc
        else do
          case values of
            [(v, e)] ->
              if unwrapUnaryRecords options || null names
                then (v, e)
                else valuesToObject names values
            _ ->
              if null names
                then do
                  let (vs, es) = unzip values
                  (array vs, list identity es)
                else valuesToObject names values
    -- sum constructor
    _ ->
      makeSumEncoding options fc

makeSumEncoding :: Options -> FromConstructor -> (Value, Encoding)
makeSumEncoding options (FromConstructor _constructorNames _constructorTypes constructorTag fieldNames values) = do
  let fieldNamesKeys = K.fromText <$> fieldNames
  case sumEncoding options of
    UntaggedValue ->
      if null fieldNames
        then do
          let (vs, es) = unzip values
          case (vs, es) of
            ([], []) -> (String $ toS constructorTag, string $ toS constructorTag)
            ([v], [e]) -> (v, e)
            _ -> (array vs, list identity es)
        else do
          let (vs, es) = unzip values
          case (vs, es) of
            ([v], [e]) | unwrapUnaryRecords options -> (v, e)
            _ -> valuesToObject fieldNames values
    TwoElemArray ->
      if null fieldNames
        then do
          let (vs, es) = unzip values
          case (vs, es) of
            ([], []) -> (String constructorTag, string $ toS constructorTag)
            ([v], [e]) -> (array [String constructorTag, v], list identity [string $ toS constructorTag, e])
            _ -> (array [String constructorTag, array vs], list identity [string $ toS constructorTag, list identity es])
        else do
          let (vs, es) = unzip values
          case (vs, es) of
            ([v], [e])
              | unwrapUnaryRecords options ->
                  (array [String constructorTag, v], list identity [string $ toS constructorTag, e])
            _ -> do
              let (vs', es') = valuesToObject fieldNames values
              (array [String constructorTag, vs'], list identity [string $ toS constructorTag, es'])
    ObjectWithSingleField -> do
      if null fieldNames
        then do
          let (vs, es) = unzip values
          case (vs, es) of
            ([], []) -> (String constructorTag, string $ toS constructorTag)
            ([v], [e]) -> (Object $ KM.singleton (K.fromText constructorTag) v, pairs (pair (K.fromText constructorTag) e))
            _ -> (Object $ KM.singleton (K.fromText constructorTag) (array vs), pairs (pair (K.fromText constructorTag) $ list identity es))
        else do
          let (vs, es) = unzip values
          case (vs, es) of
            ([v], [e])
              | unwrapUnaryRecords options ->
                  (Object $ KM.singleton (K.fromText constructorTag) v, pairs (pair (K.fromText constructorTag) e))
            _ -> do
              let (vs', es') = valuesToObject fieldNames values
              (Object $ KM.singleton (K.fromText constructorTag) vs', pairs (pair (K.fromText constructorTag) es'))
    TaggedObject tagFieldName contentsFieldName ->
      if null values
        then
          ( Object $ KM.fromList [(K.fromText $ toS tagFieldName, String constructorTag)],
            pairs (pair (K.fromText $ toS tagFieldName) (string $ toS constructorTag))
          )
        else
          if null fieldNames
            then case unzip values of
              ([v], [e]) ->
                ( Object $ KM.fromList [(K.fromText $ toS tagFieldName, String constructorTag), (K.fromText $ toS contentsFieldName, v)],
                  pairs $ pair (K.fromText $ toS tagFieldName) (string $ toS constructorTag) <> pair (K.fromText $ toS contentsFieldName) e
                )
              (vs, es) ->
                ( Object $ KM.fromList [(K.fromText $ toS tagFieldName, String constructorTag), (K.fromText $ toS contentsFieldName, array vs)],
                  pairs $ pair (K.fromText $ toS tagFieldName) (string $ toS constructorTag) <> pair (K.fromText $ toS contentsFieldName) (list identity es)
                )
            else do
              let (vs, es) = unzip values
              ( Object . KM.fromList $ (K.fromText $ toS tagFieldName, String constructorTag) : zip fieldNamesKeys vs,
                pairs (foldMap identity $ pair (K.fromText $ toS tagFieldName) (string $ toS constructorTag) : (uncurry pair <$> zip fieldNamesKeys es))
                )

-- | Apply Options to the constructor name + field names
--   and remove Nothing values if necessary
modifyFromConstructorWithOptions :: Options -> FromConstructor -> FromConstructor
modifyFromConstructorWithOptions options fc = do
  let (fn, fv) =
        if omitNothingFields options && length (fromConstructorFieldNames fc) == length (fromConstructorValues fc)
          then unzip $ filter ((/= Null) . fst . snd) $ zip (fromConstructorFieldNames fc) (fromConstructorValues fc)
          else (fromConstructorFieldNames fc, fromConstructorValues fc)
  fc
    { fromConstructorName = toS . constructorTagModifier options . toS $ fromConstructorName fc,
      fromConstructorFieldNames = toS . fieldLabelModifier options . toS <$> fn,
      fromConstructorValues = fv
    }

-- | Create an Object from a list of field names and a list of Values
--   both as a Value and as an Encoding
valuesToObject :: [Text] -> [(Value, Encoding)] -> (Value, Encoding)
valuesToObject fieldNames values = do
  let (vs, es) = unzip values
  let fieldNamesKeys = K.fromText <$> fieldNames
  (Object $ KM.fromList (zip fieldNamesKeys vs), pairs $ foldMap (uncurry pair) (zip fieldNamesKeys es))
