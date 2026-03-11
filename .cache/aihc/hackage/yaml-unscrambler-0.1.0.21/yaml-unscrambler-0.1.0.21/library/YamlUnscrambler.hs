module YamlUnscrambler
  ( -- * Execution
    parseText,
    parseByteString,
    getExpectations,

    -- * DSL

    -- ** Value
    Value,
    value,
    nullableValue,

    -- *** Helpers
    sequenceValue,
    mappingValue,
    scalarsValue,

    -- ** Scalar
    Scalar,
    stringScalar,
    nullScalar,
    boolScalar,
    scientificScalar,
    doubleScalar,
    rationalScalar,
    boundedIntegerScalar,
    unboundedIntegerScalar,
    timestampScalar,
    dayScalar,
    timeScalar,
    uuidScalar,
    binaryScalar,

    -- ** Mapping
    Mapping,
    foldMapping,
    byKeyMapping,

    -- ** Sequence
    Sequence,
    foldSequence,
    byOrderSequence,
    byKeySequence,

    -- ** String
    String,
    textString,
    enumString,
    formattedString,
    attoparsedString,

    -- ** ByKey
    ByKey,
    atByKey,
    atOneOfByKey,

    -- ** ByOrder
    ByOrder,
    fetchByOrder,

    -- * Value types
    MaxInputSize (..),
    Signed (..),
    NumeralSystem (..),
    CaseSensitive (..),
  )
where

import qualified Attoparsec.Time.ByteString as AsciiAtto
import qualified Control.Selective.Trans.Except as Selective.ExceptT
import qualified Data.Attoparsec.ByteString.Char8 as AsciiAtto
import qualified Data.Attoparsec.Text as TextAtto
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Base64 as Base64
import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import qualified Data.Yaml.Parser as Yaml
import qualified Text.Libyaml as Libyaml
import qualified YamlUnscrambler.AsciiAtto as AsciiAtto
import qualified YamlUnscrambler.CompactErrRendering as CompactErrRendering
import qualified YamlUnscrambler.Err as Err
import qualified YamlUnscrambler.Expectations as Ex
import YamlUnscrambler.Model
import YamlUnscrambler.Prelude hiding (String)
import qualified YamlUnscrambler.Util.ByteString as ByteString
import qualified YamlUnscrambler.Util.HashMap as HashMap
import qualified YamlUnscrambler.Util.Vector as Vector
import qualified YamlUnscrambler.Util.Yaml as Yaml

-- * Execution

-- |
-- Run a value parser on strict text.
parseText :: Value a -> Text -> Either Text a
parseText value =
  parseByteString value . Text.encodeUtf8

-- |
-- Run a value parser on strict bytestring.
parseByteString :: Value a -> ByteString -> Either Text a
parseByteString (Value {..}) input =
  do
    Yaml.RawDoc value map <- Yaml.parseByteStringToRawDoc input
    valueParser value map & first CompactErrRendering.renderErrAtPath

-- |
-- Get a tree of expectations, which can then be converted into
-- documentation for people working with the YAML document or
-- into one of the spec formats (e.g., YAML Spec, JSON Spec).
getExpectations :: Value a -> Ex.Value
getExpectations =
  valueExpectation

-- * --

-- | Parser of any kind of YAML value: scalar, mapping or sequence.
data Value a = Value
  { valueExpectation :: Ex.Value,
    valueParser :: Yaml.YamlValue -> Yaml.AnchorMap -> Either Err.ErrAtPath a
  }
  deriving (Functor)

-- | Specification of various alternative ways of parsing a non-nullable value.
value :: [Scalar a] -> Maybe (Mapping a) -> Maybe (Sequence a) -> Value a
value scalars mappings sequences =
  Value expectations parse
  where
    expectations =
      Ex.Value
        scalarExpectations
        (fmap mappingExpectation mappings)
        (fmap sequenceExpectation sequences)
    scalarExpectations =
      fmap scalarExpectation scalars
    parse input anchorMap =
      case input of
        Yaml.Scalar bytes tag style _ ->
          case scalars of
            [] ->
              Left (Err.ErrAtPath [] (Err.UnexpectedScalarErr expectations))
            _ ->
              runExcept (asum (fmap parse scalars))
                & first convErr
              where
                parse scalar =
                  except $ first (Last . Just) $ scalarParser scalar bytes tag style
                convErr (Last msg) =
                  Err.ErrAtPath [] (Err.ScalarErr scalarExpectations bytes tag style msg)
        Yaml.Mapping input _ ->
          case mappings of
            Just mapping ->
              mappingParser mapping input anchorMap
            Nothing ->
              Left (Err.ErrAtPath [] (Err.UnexpectedMappingErr expectations))
        Yaml.Sequence input _ ->
          case sequences of
            Just sequence ->
              sequenceParser sequence input anchorMap
            Nothing ->
              Left (Err.ErrAtPath [] (Err.UnexpectedSequenceErr expectations))
        Yaml.Alias anchorName ->
          case Map.lookup anchorName anchorMap of
            Just value ->
              parse value anchorMap
            Nothing ->
              Left (Err.ErrAtPath [] (Err.UnknownAnchorErr (fromString anchorName)))

-- | Specification of various alternative ways of parsing a nullable value.
nullableValue :: [Scalar a] -> Maybe (Mapping a) -> Maybe (Sequence a) -> Value (Maybe a)
nullableValue scalars mappings sequences =
  value
    ((nullScalar Nothing) : fmap (fmap Just) scalars)
    (fmap (fmap Just) mappings)
    (fmap (fmap Just) sequences)

-- ** Helpers

-- | Value parser, which only expects sequence values.
sequenceValue :: Sequence a -> Value a
sequenceValue sequence =
  value [] Nothing (Just sequence)

-- | Value parser, which only expects mapping values.
mappingValue :: Mapping a -> Value a
mappingValue mapping =
  value [] (Just mapping) Nothing

-- | Value parser, which only expects scalar values.
scalarsValue :: [Scalar a] -> Value a
scalarsValue scalars =
  value scalars Nothing Nothing

-- * --

-- | Scalar value parser.
data Scalar a = Scalar
  { scalarExpectation :: Ex.Scalar,
    scalarParser :: ByteString -> Libyaml.Tag -> Libyaml.Style -> Either Text a
  }
  deriving (Functor)

-- | Custom parser function of a strict bytestring as a scalar value.
bytesParsingScalar :: Ex.Scalar -> (ByteString -> Either Text a) -> Scalar a
bytesParsingScalar expectation parser =
  Scalar expectation (\bytes _ _ -> parser bytes)

-- | Custom ASCII attoparsec parser of a scalar value.
attoparsedScalar :: Ex.Scalar -> AsciiAtto.Parser a -> Scalar a
attoparsedScalar expectation parser =
  bytesParsingScalar expectation
    $ first (const "")
    . AsciiAtto.parseOnly (parser <* AsciiAtto.endOfInput)

-- | Add protection on the maximum allowed input size over a scalar parser.
sizedScalar :: MaxInputSize -> Scalar a -> Scalar a
sizedScalar (MaxInputSize maxInputSize) (Scalar {..}) =
  Scalar scalarExpectation $ \bytes tag style ->
    if ByteString.length bytes <= maxInputSize
      then scalarParser bytes tag style
      else Left ("Input is longer then the expected maximum of " <> showAsText maxInputSize <> " bytes")

-- | String scalar parser.
stringScalar :: String a -> Scalar a
stringScalar (String exp parse) =
  bytesParsingScalar
    (Ex.StringScalar exp)
    (\bytes -> first showAsText (Text.decodeUtf8' bytes) >>= parse)

-- | A parser expecting a null value and resulting in the provided constant value when successful.
nullScalar :: a -> Scalar a
nullScalar a =
  Scalar Ex.NullScalar $ \bytes tag _ ->
    if tag
      == Libyaml.NullTag
      || ByteString.null bytes
      || bytes
      == "~"
      || ByteString.saysNullInCiAscii bytes
      then Right a
      else Left "Not null"

-- | Boolean scalar parser.
boolScalar :: Scalar Bool
boolScalar =
  bytesParsingScalar Ex.BoolScalar $ \bytes ->
    if ByteString.length bytes <= 5
      then
        let lowercased =
              ByteString.lowercaseInAscii bytes
         in if elem lowercased ["y", "yes", "on", "true", "t", "1"]
              then return True
              else
                if elem lowercased ["n", "no", "off", "false", "f", "0"]
                  then return False
                  else Left "Not a boolean"
      else Left "Not a boolean"

-- | Numeric scalar as scientific parser.
scientificScalar :: Scalar Scientific
scientificScalar =
  attoparsedScalar Ex.ScientificScalar AsciiAtto.scientific

-- | Numeric scalar as double parser.
doubleScalar :: Scalar Double
doubleScalar =
  attoparsedScalar Ex.DoubleScalar AsciiAtto.double

-- | Numeric scalar as rational parser protected with maximum allowed input size.
rationalScalar :: MaxInputSize -> Scalar Rational
rationalScalar a =
  sizedScalar a
    $ attoparsedScalar (Ex.RationalScalar a) AsciiAtto.rational

-- |
-- Numeric scalar parser into a bounded integer value.
-- E.g., 'Int', 'Int64', 'Word', but not 'Integer'.
boundedIntegerScalar :: (Integral a, FiniteBits a) => Signed -> NumeralSystem -> Scalar a
boundedIntegerScalar a b =
  attoparsedScalar (Ex.BoundedIntegerScalar a b) (AsciiAtto.integralScalar a b)

-- |
-- Numeric scalar parser into any integer value.
unboundedIntegerScalar :: MaxInputSize -> Signed -> NumeralSystem -> Scalar Integer
unboundedIntegerScalar a b c =
  sizedScalar a
    $ attoparsedScalar (Ex.UnboundedIntegerScalar a b c) (AsciiAtto.integralScalar b c)

-- | String scalar parser as 'UTCTime' in ISO-8601.
timestampScalar :: Scalar UTCTime
timestampScalar =
  attoparsedScalar Ex.Iso8601TimestampScalar AsciiAtto.utcTimeInISO8601

-- | String scalar parser as 'Day' in ISO-8601.
dayScalar :: Scalar Day
dayScalar =
  attoparsedScalar Ex.Iso8601DayScalar AsciiAtto.dayInISO8601

-- | String scalar parser as 'TimeOfDay' in ISO-8601.
timeScalar :: Scalar TimeOfDay
timeScalar =
  attoparsedScalar Ex.Iso8601TimeScalar AsciiAtto.timeOfDayInISO8601

-- | String scalar parser as 'UUID'.
uuidScalar :: Scalar UUID
uuidScalar =
  bytesParsingScalar Ex.UuidScalar $ \bytes ->
    case UUID.fromASCIIBytes bytes of
      Just uuid ->
        return uuid
      Nothing ->
        Left "Invalid UUID"

-- | String scalar parser as binary data encoded in Base-64.
binaryScalar :: Scalar ByteString
binaryScalar =
  bytesParsingScalar Ex.Base64BinaryScalar $ \bytes ->
    let bytesWithoutNewlines =
          ByteString.filter (/= 10) bytes
     in case Base64.decode bytesWithoutNewlines of
          Right res ->
            return res
          Left err ->
            Left $ fromString err

-- * --

-- | Mapping value parser.
data Mapping a = Mapping
  { mappingExpectation :: Ex.Mapping,
    mappingParser :: [(Text, Yaml.YamlValue)] -> Yaml.AnchorMap -> Either Err.ErrAtPath a
  }
  deriving (Functor)

-- | Mapping parser which folds pairs into some final data-structure.
foldMapping :: (key -> val -> assoc) -> Fold assoc a -> String key -> Value val -> Mapping a
foldMapping zip (Fold foldStep foldInit foldExtract) key val =
  Mapping
    (Ex.MonomorphicMapping (stringExpectation key) (valueExpectation val))
    parser
  where
    parser input anchorMap =
      foldM step foldInit input
        & fmap foldExtract
      where
        step state (keyInput, valInput) =
          do
            parsedKey <- first keyErr (stringParser key keyInput)
            parsedVal <- first (Err.atSegment keyInput) (valueParser val valInput anchorMap)
            return $! foldStep state (zip parsedKey parsedVal)
          where
            keyErr =
              Err.ErrAtPath []
                . Err.KeyErr (stringExpectation key) keyInput

-- | Mapping parser which allows the user to look up fields and process them with individual parsers.
byKeyMapping :: CaseSensitive -> ByKey Text a -> Mapping a
byKeyMapping caseSensitive byKey =
  Mapping expectation parser
  where
    expectation =
      Ex.ByKeyMapping caseSensitive (byKeyExpectation byKey)
    parser input =
      either Left (first keysErr) . runExceptT . parser
      where
        parser =
          if coerce caseSensitive
            then
              let map =
                    HashMap.fromList input
                  lookup k =
                    HashMap.lookup k map
                  lookupFirst kl =
                    HashMap.lookupFirst kl map
               in byKeyParser byKey id lookup lookupFirst
            else
              let map =
                    HashMap.fromList (fmap (first Text.toLower) input)
                  lookup k =
                    HashMap.lookup (Text.toLower k) map
                  lookupFirst kl =
                    HashMap.lookupFirst (fmap Text.toLower kl) map
               in byKeyParser byKey id lookup lookupFirst
        keysErr keys =
          Err.ErrAtPath []
            $ Err.NoneOfMappingKeysFoundErr (byKeyExpectation byKey) caseSensitive keysAvail (toList keys)
          where
            keysAvail =
              fmap fst input

-- * --

-- | Sequence value parser.
data Sequence a = Sequence
  { sequenceExpectation :: Ex.Sequence,
    sequenceParser :: [Yaml.YamlValue] -> Yaml.AnchorMap -> Either Err.ErrAtPath a
  }
  deriving (Functor)

-- | Homogenous sequence parser which folds into a final data-structure.
foldSequence :: Fold a b -> Value a -> Sequence b
foldSequence (Fold foldStep foldInit foldExtract) value =
  Sequence
    (Ex.MonomorphicSequence (valueExpectation value))
    parser
  where
    parser input anchorMap =
      foldM step (0 :: Int, foldInit) input
        & fmap (foldExtract . snd)
      where
        step (!index, !state) input =
          valueParser value input anchorMap
            & first (Err.atSegment (showAsText index))
            & fmap (\a -> (succ index, foldStep state a))

-- | Heterogenous sequence parser by order in the sequence, which lets you apply individual parsers to elements.
byOrderSequence :: ByOrder a -> Sequence a
byOrderSequence (ByOrder {..}) =
  Sequence
    (Ex.ByOrderSequence byOrderExpectation)
    parser
  where
    parser input anchorMap =
      runExceptT (runReaderT (evalStateT byOrderParser (0, input)) anchorMap)
        & either Left (first mapErr)
      where
        mapErr =
          \case
            NotEnoughElementsByOrderErr a ->
              Err.ErrAtPath []
                $ Err.NotEnoughElementsErr byOrderExpectation a

-- | Heterogenous sequence parser by index in the sequence, which lets you apply individual parsers to elements.
byKeySequence :: ByKey Int a -> Sequence a
byKeySequence (ByKey {..}) =
  Sequence expectation parser
  where
    expectation =
      Ex.ByKeySequence byKeyExpectation
    parser input =
      let vector =
            Vector.fromList input
          lookup k =
            vector Vector.!? k
          lookupFirst kl =
            Vector.lookupFirst kl vector
       in \anchorMap ->
            runExceptT (byKeyParser showAsText lookup lookupFirst anchorMap)
              & either Left (first keysErr)
      where
        keysErr keys =
          Err.ErrAtPath []
            $ Err.NoneOfSequenceKeysFoundErr byKeyExpectation (toList keys)

-- * --

-- | String value parser applicable to string scalars and mapping keys.
data String a = String
  { stringExpectation :: Ex.String,
    stringParser :: Text -> Either Text a
  }
  deriving (Functor)

-- | String as is.
textString :: String Text
textString =
  String Ex.AnyString return

-- | Look the string up as a key in the provided dictionary.
enumString :: CaseSensitive -> [(Text, a)] -> String a
enumString (CaseSensitive caseSensitive) assocList =
  String expectation parser
  where
    expectation =
      Ex.OneOfString (CaseSensitive caseSensitive) (fmap fst assocList)
    {-# NOINLINE lookup #-}
    lookup =
      if length assocList > 512
        then
          if caseSensitive
            then
              let hashMap =
                    HashMap.fromList assocList
               in flip HashMap.lookup hashMap
            else
              let hashMap =
                    HashMap.fromList (fmap (first Text.toLower) assocList)
               in flip HashMap.lookup hashMap . Text.toLower
        else
          if caseSensitive
            then flip List.lookup assocList
            else flip List.lookup (fmap (first Text.toLower) assocList) . Text.toLower
    parser text =
      case lookup text of
        Just a -> return a
        _ -> Left "Unexpected value"

-- | String parsed using the provided function.
formattedString ::
  -- | Format name for documentation and expectations.
  Text ->
  (Text -> Either Text a) ->
  String a
formattedString format parser =
  String
    (Ex.FormattedString format)
    parser

-- | String parsed using the provided textual attoparsec parser.
attoparsedString ::
  -- | Format name for documentation and expectations.
  Text ->
  TextAtto.Parser a ->
  String a
attoparsedString format parser =
  String
    (Ex.FormattedString format)
    (first fromString . TextAtto.parseOnly parser)

-- * --

-- | General abstraction for specification of parsers performing lookups by keys.
data ByKey key a = ByKey
  { byKeyExpectation :: Ex.ByKey key,
    byKeyParser ::
      (key -> Text) ->
      (key -> Maybe Yaml.YamlValue) ->
      ([key] -> Maybe (key, Yaml.YamlValue)) ->
      Yaml.AnchorMap ->
      ExceptT (Acc key) (Either Err.ErrAtPath) a
  }
  deriving (Functor)

instance Applicative (ByKey key) where
  pure =
    ByKey Ex.AnyByKey . const . const . const . const . pure
  (<*>) (ByKey le lp) (ByKey re rp) =
    ByKey
      (Ex.BothByKey le re)
      (\a b c d -> lp a b c d <*> rp a b c d)

instance Selective (ByKey key) where
  select (ByKey le lp) (ByKey re rp) =
    ByKey
      (Ex.BothByKey le re)
      (\a b c d -> Selective.ExceptT.unwrap (select (Selective.ExceptT.ExceptT (lp a b c d)) (Selective.ExceptT.ExceptT (rp a b c d))))

instance Alternative (ByKey key) where
  empty =
    ByKey
      Ex.NoByKey
      (const (const (const (const empty))))
  (<|>) (ByKey le lp) (ByKey re rp) =
    ByKey
      (Ex.EitherByKey le re)
      (\a b c d -> lp a b c d <|> rp a b c d)

-- | Parse a value at a key using the provided parser.
atByKey :: key -> Value a -> ByKey key a
atByKey key valueSpec =
  ByKey
    (Ex.LookupByKey [key] (valueExpectation valueSpec))
    parser
  where
    parser renderKey lookup _ env =
      case lookup key of
        Just val ->
          lift
            $ first (Err.atSegment (renderKey key))
            $ valueParser valueSpec val env
        Nothing ->
          throwE (pure key)

-- | Parse a value at one of keys (whichever exists) using the provided parser.
atOneOfByKey :: [key] -> Value a -> ByKey key a
atOneOfByKey keys valueSpec =
  ByKey
    (Ex.LookupByKey keys (valueExpectation valueSpec))
    parser
  where
    parser renderKey _ lookup env =
      case lookup keys of
        Just (key, val) ->
          lift
            $ first (Err.atSegment (renderKey key))
            $ valueParser valueSpec val env
        Nothing ->
          throwE (fromList keys)

-- * --

data ByOrderErr
  = NotEnoughElementsByOrderErr
      Int

-- | Parser which fetches elements by the order in which it is composed.
data ByOrder a = ByOrder
  { byOrderExpectation :: Ex.ByOrder,
    byOrderParser :: StateT (Int, [Yaml.YamlValue]) (ReaderT Yaml.AnchorMap (ExceptT ByOrderErr (Either Err.ErrAtPath))) a
  }
  deriving (Functor)

instance Applicative ByOrder where
  pure =
    ByOrder Ex.AnyByOrder . pure
  (<*>) (ByOrder le lp) (ByOrder re rp) =
    ByOrder
      (Ex.BothByOrder le re)
      (lp <*> rp)

instance Selective ByOrder where
  select (ByOrder le lp) (ByOrder re rp) =
    ByOrder
      (Ex.BothByOrder le re)
      (select lp rp)

-- | Parse the next value using the provided parser.
fetchByOrder :: Value a -> ByOrder a
fetchByOrder value =
  ByOrder
    (Ex.FetchByOrder (valueExpectation value))
    parser
  where
    parser =
      do
        (!offset, list) <- get
        case list of
          h : t ->
            do
              put (succ offset, t)
              anchorMap <- ask
              lift $ lift $ lift $ first (Err.atSegment (showAsText offset)) $ valueParser value h anchorMap
          _ ->
            throwError $ NotEnoughElementsByOrderErr offset
