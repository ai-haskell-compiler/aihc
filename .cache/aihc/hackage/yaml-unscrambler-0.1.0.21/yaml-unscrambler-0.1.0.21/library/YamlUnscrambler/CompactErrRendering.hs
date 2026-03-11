module YamlUnscrambler.CompactErrRendering
  ( renderErrAtPath,
  )
where

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import TextBuilder
import qualified YamlUnscrambler.Err as Err
import qualified YamlUnscrambler.Expectations as Ex
import YamlUnscrambler.Model
import YamlUnscrambler.Prelude hiding (intercalate)

renderErrAtPath :: Err.ErrAtPath -> Text
renderErrAtPath =
  toText . errAtPath

path :: [Text] -> TextBuilder
path a =
  "/" <> intercalate "/" (fmap text a)

errAtPath :: Err.ErrAtPath -> TextBuilder
errAtPath (Err.ErrAtPath a b) =
  "Error at path " <> path a <> ". " <> reason b

reason :: Err.Err -> TextBuilder
reason =
  \case
    Err.KeyErr a b c ->
      text c
        <> ". On input: "
        <> string (show b)
        <> ". "
        <> "Expecting: "
        <> stringExpectation a
    Err.NoneOfMappingKeysFoundErr a b c d ->
      "None of keys found "
        <> caseSensitively b
        <> ": "
        <> string (show d)
        <> ". "
        <> "Keys available: "
        <> string (show c)
    Err.NoneOfSequenceKeysFoundErr a b ->
      "None of indices found: " <> string (show b)
    Err.ScalarErr a b c d e ->
      foldMap (\a -> text a <> ". ") (mfilter (not . Text.null) e)
        <> "Expecting one of the following formats: "
        <> intercalate ", " (fmap scalarExpectation a)
        <> foldMap (\a -> ". Got input: " <> string (show a)) (Text.decodeUtf8' b)
    Err.UnexpectedScalarErr a ->
      "Unexpected scalar value"
    Err.UnexpectedMappingErr a ->
      "Unexpected mapping value"
    Err.UnexpectedSequenceErr a ->
      "Unexpected sequence value"
    Err.UnknownAnchorErr a ->
      "Unknown anchor: " <> text a
    Err.NotEnoughElementsErr a b ->
      "Not enough elements: "
        <> decimal b
        <> ". "
        <> "Expecting: "
        <> byOrderExpectation a

scalarExpectation :: Ex.Scalar -> TextBuilder
scalarExpectation =
  \case
    Ex.StringScalar a ->
      stringExpectation a
    Ex.NullScalar ->
      "null"
    Ex.BoolScalar ->
      "boolean"
    Ex.ScientificScalar ->
      "scientific"
    Ex.DoubleScalar ->
      "double"
    Ex.RationalScalar a ->
      "rational of maximum length of " <> maxInputSize a <> " chars"
    Ex.BoundedIntegerScalar a b ->
      signed a <> " " <> numeralSystem b
    Ex.UnboundedIntegerScalar a b c ->
      signed b <> " " <> numeralSystem c <> " of maximum length of " <> maxInputSize a <> " chars"
    Ex.Iso8601TimestampScalar ->
      "timestamp in ISO-8601"
    Ex.Iso8601DayScalar ->
      "date in ISO-8601"
    Ex.Iso8601TimeScalar ->
      "time in ISO-8601"
    Ex.UuidScalar ->
      "UUID"
    Ex.Base64BinaryScalar ->
      "binary data in Base-64"

stringExpectation :: Ex.String -> TextBuilder
stringExpectation =
  \case
    Ex.AnyString ->
      "any string"
    Ex.OneOfString a b ->
      "one of " <> string (show b) <> "(" <> caseSensitive a <> ")"
    Ex.FormattedString a ->
      text a

byOrderExpectation :: Ex.ByOrder -> TextBuilder
byOrderExpectation =
  decimal @Integer . count 0
  where
    count !a =
      \case
        Ex.AnyByOrder ->
          a
        Ex.BothByOrder b c ->
          countBoth a b c
        Ex.FetchByOrder _ ->
          succ a
    countBoth a b c =
      case b of
        Ex.BothByOrder d e ->
          countBoth a d (Ex.BothByOrder e c)
        Ex.AnyByOrder ->
          count a c
        Ex.FetchByOrder _ ->
          count (succ a) c

caseSensitive :: CaseSensitive -> TextBuilder
caseSensitive (CaseSensitive a) =
  "case-" <> bool "insensitive" "sensitive" a

caseSensitively :: CaseSensitive -> TextBuilder
caseSensitively (CaseSensitive a) =
  "case-" <> bool "insensitively" "sensitively" a

signed :: Signed -> TextBuilder
signed (Signed a) =
  bool "unsigned" "signed" a

numeralSystem :: NumeralSystem -> TextBuilder
numeralSystem =
  \case
    DecimalNumeralSystem ->
      "decimal"
    HexadecimalNumeralSystem ->
      "hexadecimal"

maxInputSize :: MaxInputSize -> TextBuilder
maxInputSize (MaxInputSize a) =
  decimal a
