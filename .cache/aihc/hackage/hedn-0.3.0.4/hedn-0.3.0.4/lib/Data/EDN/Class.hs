{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedLists      #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.EDN.Class
  ( ToEDN(..)
  , toEDNtagged
  , FromEDN(..)
  , fromEDN
  , withTagged
  , withNoTag
  , withNil
  , withBoolean
  , withString
  , withCharacter
  , withSymbol
  , withKeyword
  , withTextual
  , withInteger
  , withIntegral
  , withFloating
  , withFractional
  , withList
  , withVec
  , withMap
  , withSet
  , unexpected
  , DP.Expected
  , DP.Label
  , vecGet
  , mapGetP
  , mapGetKeyword
  , mapGetString
  , mapGetSymbol
  , mapGetSymbolNS
  ) where

import Control.Applicative ((<|>))
import Data.Map (Map)
#if MIN_VERSION_base(4,12,0)
#else
import Data.Semigroup ((<>))
#endif
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, parseTimeM)
import Data.UUID.Types (UUID)
import Data.Vector (Vector)
import Data.Void (Void, absurd)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.UUID.Types as UUID
import qualified Data.Vector as Vector

import Data.EDN.AST.Printer (renderText)

import qualified Data.EDN.AST.Types as EDN
import qualified Data.EDN.Class.Parser as DP

-- * Encoding

-- | A type that can be converted to EDN AST.
class ToEDN a where
  {-# MINIMAL toEDN | toEDNv #-}

  toEDN :: a -> EDN.TaggedValue
  toEDN = EDN.NoTag . toEDNv
  {-# INLINE toEDN #-}

  toEDNv :: a -> EDN.Value
  toEDNv = EDN.stripTag . toEDN
  {-# INLINE toEDNv #-}

toEDNtagged :: ToEDN a => Text -> Text -> a -> EDN.TaggedValue
toEDNtagged tagNS tag = EDN.Tagged tagNS tag . toEDNv

instance ToEDN EDN.TaggedValue where
  toEDN = id

instance ToEDN EDN.Value where
  toEDNv = id

instance ToEDN Void where
  toEDNv = absurd

instance ToEDN () where
  toEDN () = EDN.NoTag EDN.Nil

instance ToEDN Bool where
  toEDNv = EDN.Boolean

instance ToEDN Text where
  toEDNv = EDN.String

instance ToEDN LText.Text where
  toEDNv = EDN.String . LText.toStrict

instance ToEDN Char where
  toEDNv = EDN.Character

instance ToEDN Int where
  toEDNv = EDN.Integer

instance ToEDN Double where
  toEDNv = EDN.Floating

instance ToEDN a => ToEDN (Maybe a) where
  toEDN Nothing  = EDN.NoTag EDN.Nil
  toEDN (Just a) = toEDN a

instance ToEDN a => ToEDN [a] where
  toEDNv = EDN.List . map toEDN

instance ToEDN a => ToEDN (Vector a) where
  toEDNv = EDN.Vec . fmap toEDN

instance ToEDN a => ToEDN (Set a) where
  toEDNv = EDN.Set . Set.fromList . map toEDN . Set.toList

instance (ToEDN k, ToEDN v) => ToEDN (Map k v) where
  toEDNv
    = EDN.Map
    . Map.fromList
    . map (\(k, v) -> (toEDN k, toEDN v))
    . Map.toList

instance (ToEDN a, ToEDN b) => ToEDN (a, b) where
  toEDNv (a, b) = EDN.Vec $ Vector.fromList
    [ toEDN a
    , toEDN b
    ]

instance (ToEDN a, ToEDN b, ToEDN c) => ToEDN (a, b, c) where
  toEDNv (a, b, c) = EDN.Vec $ Vector.fromList
    [ toEDN a
    , toEDN b
    , toEDN c
    ]

instance (ToEDN a, ToEDN b, ToEDN c, ToEDN d) => ToEDN (a, b, c, d) where
  toEDNv (a, b, c, d) = EDN.Vec $ Vector.fromList
    [ toEDN a
    , toEDN b
    , toEDN c
    , toEDN d
    ]

instance ToEDN UTCTime where
  toEDN
    = toEDNtagged "" "inst"
    . Text.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%EZ"

instance ToEDN UUID where
  toEDN = toEDNtagged "" "uuid" . UUID.toText

-- * Decoding

withTagged
  :: Text
  -> Text
  -> (EDN.Value -> DP.Parser a)
  -> EDN.TaggedValue
  -> DP.Parser a
withTagged tagNS tag p tv =
  case tv of
    EDN.Tagged tagNS' tag' v
      | tagNS' == tagNS && tag == tag' ->
          p v
      | otherwise ->
          DP.parserError . Text.unpack $ mconcat
            [ "unexpected tag. "
            , "expecting: #"
            , nsToText tagNS' tag'
            , "; got: #"
            , nsToText tagNS tag
            ]
    _ ->
      DP.parserError "expected tagged value"

withNoTag :: (EDN.Value -> DP.Parser a) -> EDN.TaggedValue -> DP.Parser a
withNoTag p tv =
  case tv of
    EDN.NoTag v ->
      p v
    EDN.Tagged tagNS tag _v ->
      DP.parserError $ "no tag expected, got #" <> Text.unpack (nsToText tagNS tag)

withNil :: DP.Parser a -> EDN.Value -> DP.Parser a
withNil p = \case
  EDN.Nil ->
    p
  got ->
    got `unexpected` "nil"

withBoolean :: (Bool -> DP.Parser a) -> EDN.Value -> DP.Parser a
withBoolean p = \case
  EDN.Boolean b ->
    p b
  got ->
    got `unexpected` "boolean"

withString :: (Text -> DP.Parser a) -> EDN.Value -> DP.Parser a
withString p = \case
  EDN.String t ->
    p t
  got ->
    got `unexpected` "string"

withCharacter :: (Char -> DP.Parser a) -> EDN.Value -> DP.Parser a
withCharacter p = \case
  EDN.Character c ->
    p c
  got ->
    got `unexpected` "char"

withSymbol :: (Text -> Text -> DP.Parser a) -> EDN.Value -> DP.Parser a
withSymbol p = \case
  EDN.Symbol ns n ->
    p ns n
  got ->
    got `unexpected` "symbol"

withKeyword :: (Text -> DP.Parser a) -> EDN.Value -> DP.Parser a
withKeyword p = \case
  EDN.Keyword t ->
    p t
  got ->
    got `unexpected` "keyword"

withTextual :: (Text -> DP.Parser a) -> EDN.Value -> DP.Parser a
withTextual p tv =
  withString p tv <|>
  withCharacter (p . Text.singleton) tv <|>
  withKeyword p tv <|>
  withSymbol (\ns n -> p $ nsToText ns n) tv

withInteger :: (Int -> DP.Parser a) -> EDN.Value -> DP.Parser a
withInteger p = \case
  EDN.Integer i ->
    p i
  got ->
    got `unexpected` "integer"

withIntegral :: Integral i => (i -> DP.Parser a) -> EDN.Value -> DP.Parser a
withIntegral p = \case
  EDN.Integer i ->
    p (fromIntegral i)
  got ->
    got `unexpected` "integer"

withFloating :: (Double -> DP.Parser a) -> EDN.Value -> DP.Parser a
withFloating p = \case
  EDN.Floating d ->
    p d
  got ->
    got `unexpected` "double"

withFractional :: Fractional f => (f -> DP.Parser a) -> EDN.Value -> DP.Parser a
withFractional p = \case
  EDN.Floating d ->
    p (realToFrac d)
  got ->
    got `unexpected` "double"

withList :: (EDN.EDNList -> DP.Parser a) -> EDN.Value -> DP.Parser a
withList p = \case
  EDN.List tvs ->
    p tvs
  got ->
    got `unexpected` "list"

withVec :: (EDN.EDNVec -> DP.Parser a) -> EDN.Value -> DP.Parser a
withVec p = \case
  EDN.Vec v ->
    p v
  got ->
    got `unexpected` "vector"

withMap :: (EDN.EDNMap -> DP.Parser a) -> EDN.Value -> DP.Parser a
withMap p = \case
  EDN.Map m ->
    p m
  got ->
    got `unexpected` "map"

withSet :: (EDN.EDNSet -> DP.Parser a) -> EDN.Value -> DP.Parser a
withSet p = \case
  EDN.Set m ->
    p m
  got ->
    got `unexpected` "set"

-- | Report an decoding error due to unexpected AST node given.
-- The 'DP.Parser' combines and reports alternatives expected.
unexpected :: EDN.Value -> DP.Label -> DP.Parser a
unexpected value label = DP.Parser $ \kf _ks ->
  kf (pure label) $ "Unexpected " <> label'
  where
    label' = case value of
      EDN.Nil         -> "nil"
      EDN.Boolean{}   -> "boolean"
      EDN.String{}    -> "string"
      EDN.Character{} -> "character"
      EDN.Symbol{}    -> "symbol"
      EDN.Keyword{}   -> "keyword"
      EDN.Integer{}   -> "integer"
      EDN.Floating{}  -> "floating"
      EDN.List{}      -> "list"
      EDN.Vec{}       -> "vector"
      EDN.Map{}       -> "map"
      EDN.Set{}       -> "set"

-- | A type that can be converted from EDN, with a possibility of failure.
--
-- When writing an instance, use 'unexpected' or 'fail' to make a
-- conversion fail, e.g. if an 'Map.Map' is missing a required key, or
-- the value is of the wrong type.
class FromEDN a where
  {-# MINIMAL parseEDN | parseEDNv #-}

  parseEDN :: EDN.TaggedValue -> DP.Parser a
  parseEDN = parseEDNv . EDN.stripTag
  {-# INLINE parseEDN #-}

  parseEDNv :: EDN.Value -> DP.Parser a
  parseEDNv = parseEDN . EDN.NoTag
  {-# INLINE parseEDNv #-}

-- | Apply appropriate parsers for a value to decode AST.
fromEDN :: (FromEDN a) => EDN.TaggedValue -> Either String a
fromEDN = DP.parseEither parseEDN

instance FromEDN EDN.TaggedValue where
  parseEDN = pure

instance FromEDN EDN.Value where
  parseEDNv = pure

instance FromEDN Void where
  parseEDN _ = DP.parserError "unable to construct Void value"

instance FromEDN () where
  parseEDNv = withNil $ pure ()

instance FromEDN Bool where
  parseEDNv = withBoolean pure

instance FromEDN Text where
  parseEDNv = withTextual pure

instance FromEDN LText.Text where
  parseEDNv = withTextual (pure . LText.fromStrict)

instance FromEDN Char where
  parseEDNv = withCharacter pure

instance FromEDN Int where
  parseEDNv = withInteger pure

instance FromEDN Double where
  parseEDNv = withFloating pure

instance FromEDN a => FromEDN (Maybe a) where
  parseEDN = \case
    EDN.NoTag      EDN.Nil -> pure Nothing
    EDN.Tagged _ _ EDN.Nil -> pure Nothing
    tv                     -> Just <$> parseEDN tv

  parseEDNv = \case
    EDN.Nil -> pure Nothing
    v       -> Just <$> parseEDNv v

instance FromEDN a => FromEDN [a] where
  parseEDNv = withList (traverse parseEDN)

instance FromEDN a => FromEDN (Vector a) where
  parseEDNv = withVec (traverse parseEDN)

-- | Get ix-th element of 'EDN.EDNVec' or fail with appropriate message.
vecGet
  :: FromEDN a
  => Int         -- ^ Element index
  -> EDN.EDNVec  -- ^ 'Vector.Vector' of EDN values
  -> DP.Parser a
vecGet ix v =
  case v Vector.!? ix of
    Nothing ->
      DP.parserError $ unwords
        [ "expected vector with at least"
        , show (succ ix)
        , "elements"
        ]
    Just x ->
      parseEDN x

instance (FromEDN a, Ord a) => FromEDN (Set a) where
  parseEDNv = withSet $ \s ->
    Set.fromList <$> traverse parseEDN (Set.toList s)

instance (FromEDN k, FromEDN v, Ord k) => FromEDN (Map k v) where
  parseEDNv = withMap $ \m ->
    Map.fromList <$> traverse parsePair (Map.toList m)
    where
      parsePair (k, v) = (,) <$> parseEDN k <*> parseEDN v

-- | Get a value from 'EDN.EDNMap' and apply a parser to it
mapGetP
  :: EDN.TaggedValue                  -- ^ Map key
  -> (EDN.TaggedValue -> DP.Parser a) -- ^ Parser to apply to a value
  -> EDN.EDNMap                       -- ^ Map with EDN keys and values
  -> DP.Parser a
mapGetP key inner m =
  case Map.lookup key m of
    Just tv ->
      inner tv
    Nothing ->
      DP.parserError . Text.unpack $ "key not found: " <> renderText key

-- | Get a value from 'EDN.EDNMap' for a 'EDN.Keyword' key.
mapGetKeyword :: FromEDN a => Text -> EDN.EDNMap -> DP.Parser a
mapGetKeyword key = mapGetP (EDN.NoTag $ EDN.Keyword key) parseEDN

-- | Get a value from 'EDN.EDNMap' for a 'EDN.String' key.
mapGetString :: FromEDN a => Text -> EDN.EDNMap -> DP.Parser a
mapGetString key = mapGetP (EDN.NoTag $ EDN.String key) parseEDN

-- | Get a value from 'EDN.EDNMap' for a 'EDN.Symbol' (empty namespace) key.
mapGetSymbol :: FromEDN a => Text -> EDN.EDNMap -> DP.Parser a
mapGetSymbol = mapGetSymbolNS ""

-- | Get a value from 'EDN.EDNMap' for a 'EDN.Symbol' (empty namespace) key.
mapGetSymbolNS
  :: FromEDN a
  => Text        -- ^ Symbol namespace
  -> Text        -- ^ Symbol name
  -> EDN.EDNMap
  -> DP.Parser a
mapGetSymbolNS ns name = mapGetP (EDN.NoTag $ EDN.Symbol ns name) parseEDN

instance (FromEDN a, FromEDN b) => FromEDN (a, b) where
  parseEDNv = withVec $ \case
    [a, b] ->
      (,) <$> parseEDN a <*> parseEDN b
    _ ->
      DP.parserError "vector of size 2 expected"

instance (FromEDN a, FromEDN b, FromEDN c) => FromEDN (a, b, c) where
  parseEDNv = withVec $ \case
    [a, b, c] ->
      (,,) <$> parseEDN a <*> parseEDN b <*> parseEDN c
    _ ->
      DP.parserError "vector of size 3 expected"

instance (FromEDN a, FromEDN b, FromEDN c, FromEDN d) => FromEDN (a, b, c, d) where
  parseEDNv = withVec $ \case
    [a, b, c, d] ->
      (,,,) <$> parseEDN a <*> parseEDN b <*> parseEDN c <*> parseEDN d
    _ ->
      DP.parserError "vector of size 3 expected"

instance FromEDN UTCTime where
  parseEDN tv = parseTaggedUTCTime tv <|> parseUntaggedUTCTime tv
    where
      parseTaggedUTCTime =
        withTagged "" "inst" $ withString parseUTCTime

      parseUntaggedUTCTime =
        withNoTag $ withString parseUTCTime

      parseUTCTime =
        parseTimeM False defaultTimeLocale "%Y-%m-%dT%H:%M:%S%Q%Z" . Text.unpack

instance FromEDN UUID where
  parseEDN tv = parseTaggedUUID tv <|> parseUntaggedUUID tv
    where
      parseTaggedUUID = withTagged "" "uuid" $ withString parseUUID

      parseUntaggedUUID = withNoTag $ withString parseUUID

      parseUUID t =
        case UUID.fromText t of
          Nothing ->
            DP.parserError "invalid UUID string"
          Just uuid ->
            pure uuid

nsToText
  :: Text -- ^ Namespace
  -> Text -- ^ Name
  -> Text -- ^ Resulting text
nsToText "" n = n
nsToText ns n = ns <> "/" <> n
