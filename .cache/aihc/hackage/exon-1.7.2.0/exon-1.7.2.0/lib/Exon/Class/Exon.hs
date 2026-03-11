-- | Description: Internal
module Exon.Class.Exon where

import qualified Data.ByteString.Builder as ByteString
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.Text.Lazy.Builder as Text
import Data.Text.Lazy.Builder (toLazyText)

import Exon.Class.Newtype (OverNewtypes, overNewtypes)
import Exon.Data.Result (Result (Empty, Result))
import qualified Exon.Data.Segment as Segment
import Exon.Data.Segment (Segment)

-- | Wrapping a quote type with this causes @a@ to be used irrespective of whether it is an unwrappable newtype.
--
-- @since 1.0.0.0
newtype ExonUse a =
  ExonUse a
  deriving stock (Eq, Show)
  deriving newtype (IsString)

exonUse :: ExonUse a -> a
exonUse = coerce

-- | This class converts a segment into a builder.
--
-- A builder is an auxiliary data type that may improve performance when concatenating segments, like 'Text.Builder'.
-- The default instance uses no builder and is implemented as 'id'.
--
-- @since 1.0.0.0
class ExonBuilder (inner :: Type) (builder :: Type) | inner -> builder where
  -- | Construct a builder from the newtype-unwrapped result type.
  exonBuilder :: inner -> builder

  default exonBuilder :: inner ~ builder => inner -> builder
  exonBuilder =
    id
  {-# inline exonBuilder #-}

  -- | Convert the result of the builder concatenation back to the newtype-unwrapped result type.
  exonBuilderExtract :: Result builder -> inner

  default exonBuilderExtract ::
    Monoid builder =>
    inner ~ builder =>
    Result builder ->
    inner
  exonBuilderExtract =
    fold
  {-# inline exonBuilderExtract #-}

instance {-# overlappable #-} (
    Monoid builder,
    inner ~ builder
  ) => ExonBuilder inner builder where

instance (
    ExonBuilder a builder
  ) => ExonBuilder (ExonUse a) builder where
  exonBuilder =
    exonBuilder @a . exonUse
  exonBuilderExtract =
    ExonUse . exonBuilderExtract

instance ExonBuilder Text Text.Builder where
  exonBuilder =
    Text.fromText
  {-# inline exonBuilder #-}
  exonBuilderExtract =
    foldMap (toStrict . toLazyText)
  {-# inline exonBuilderExtract #-}

instance ExonBuilder LText Text.Builder where
  exonBuilder =
    Text.fromLazyText
  {-# inline exonBuilder #-}
  exonBuilderExtract =
    foldMap toLazyText
  {-# inline exonBuilderExtract #-}

instance ExonBuilder ByteString ByteString.Builder where
  exonBuilder =
    ByteString.byteString
  {-# inline exonBuilder #-}
  exonBuilderExtract =
    foldMap (toStrict . toLazyByteString)
  {-# inline exonBuilderExtract #-}

instance ExonBuilder LByteString ByteString.Builder where
  exonBuilder =
    ByteString.lazyByteString
  {-# inline exonBuilder #-}
  exonBuilderExtract =
    foldMap toLazyByteString
  {-# inline exonBuilderExtract #-}

-- | This class generalizes 'IsString' for use in 'ExonSegment'.
--
-- When a plain text segment (not interpolated) is processed, it is converted to the result type, which usually happens
-- via 'fromString'.
--
-- For the type of 'Text.Show.showsPrec' (@'String' -> 'String'@), there is no instance of 'IsString', so this class
-- provides an instance that works around that by calling 'showString'.
--
-- @since 1.0.0.0
class ExonString (result :: Type) (builder :: Type) where
  -- | Convert a 'String' to the builder type.
  exonString :: String -> Result builder

  default exonString :: IsString builder => String -> Result builder
  exonString =
    Result . fromString
  {-# inline exonString #-}

  -- | Convert a 'String' containing whitespace to the builder type.
  -- This is only used by whitespace-aware quoters, like 'Exon.exonws' or 'Exon.intron'.
  exonWhitespace :: String -> Result builder

  default exonWhitespace :: String -> Result builder
  exonWhitespace =
    exonString @result @builder
  {-# inline exonWhitespace #-}

instance {-# overlappable #-} IsString a => ExonString result a where

-- | The instance for the type used by 'Text.Show.showsPrec'.
instance ExonString result (String -> String) where
  exonString =
    Result . showString
  {-# inline exonString #-}

-- | This class allows manipulation of interpolated expressions before they are processed, for example to replace empty
-- strings with 'Empty' for the purpose of collapsing multiple whitespaces.
--
-- The default instance does nothing.
class ExonExpression (result :: Type) (inner :: Type) (builder :: Type) where
  -- | Process a builder value constructed from an expression before concatenation.
  exonExpression :: (inner -> builder) -> inner -> Result builder
  exonExpression builder =
    Result . builder
  {-# inline exonExpression #-}

instance {-# overlappable #-} ExonExpression result inner builder where

-- | This class converts a 'Segment' to a builder.
--
-- The default implementation performs the following conversions for the different segment variants:
--
-- - [Segment.String]('Segment.String') and [Segment.Whitespace]('Segment.Whitespace') are plain 'String's parsed
-- literally from the quasiquote.
-- They are converted to the builder type by 'fromString' (handled by 'ExonString').
--
-- - [Segment.Whitespace]('Segment.Whitespace') is ignored when the quoter 'Exon.intron' was used (default behaviour of
-- 'ExonString').
--
-- - [Segment.Expression]('Segment.Expression') contains a value of the unwrapped type and is converted to a builder
-- using the function in the first argument, which is usually 'exonBuilder', supplied by 'exonBuild'.
--
-- @since 1.0.0.0
class ExonSegment (result :: Type) (inner :: Type) (builder :: Type) where
  -- | Convert literal string segments to the result type.
  exonSegment :: (inner -> builder) -> Segment inner -> Result builder

instance {-# overlappable #-} (
    ExonString result builder,
    ExonExpression result inner builder
  ) => ExonSegment result inner builder where
    exonSegment builder = \case
      Segment.String a ->
        exonString @result a
      Segment.Expression a ->
        exonExpression @result builder a
      Segment.Whitespace a ->
        exonWhitespace @result a
    {-# inline exonSegment #-}

-- | This class handles concatenation of segments, which might be a builder or the result type.
--
-- The default instance simply uses '(<>)', and there is only one special instance for @'String' -> 'String'@, the type
-- used by 'Text.Show.showsPrec'.
--
-- @since 1.0.0.0
class ExonAppend (result :: Type) (builder :: Type) where
  -- | Concatenate two segments of the builder type.
  exonAppend :: builder -> builder -> Result builder

  default exonAppend :: Semigroup builder => builder -> builder -> Result builder
  exonAppend z a =
    Result (z <> a)
  {-# inline exonAppend #-}

  -- | Concatenate a list of segments of the result type.
  --
  -- Folds the list over 'exonAppend', skipping over 'Empty' segments.
  --
  -- A possible overload may implement lookahead to skip whitespace.
  --
  -- @since 1.1.0.0
  exonConcat :: NonEmpty (Result builder) -> Result builder
  exonConcat (h :| t) =
    foldl folder h t
    where
      folder = \case
        Empty -> id
        Result z -> \case
          Empty -> Result z
          Result a -> exonAppend @result @builder z a
  {-# inline exonConcat #-}

instance {-# overlappable #-} (
    Semigroup builder
  ) => ExonAppend result builder where

instance ExonAppend result (String -> String) where
  exonAppend z a =
    Result (z . a)
  {-# inline exonAppend #-}

-- | This class implements the 'Segment' concatenation logic.
--
-- 1. Each 'Segment' is converted to the builder type by 'ExonSegment' using 'exonBuilder' to construct the builder from
--    expressions.
-- 2. The segments are folded over 'ExonAppend'.
-- 3. The result is converted from the builder type to the original type by 'ExonBuilder'.
--
-- Each step may be overridden individually by writing overlapping instances for the involved classes.
--
-- @since 1.0.0.0
class ExonBuild (result :: Type) (inner :: Type) where
  -- | Concatenate a list of 'Segment's.
  exonBuild :: NonEmpty (Segment inner) -> inner

instance {-# overlappable #-} (
    ExonAppend result builder,
    ExonSegment result inner builder,
    ExonBuilder inner builder
  ) => ExonBuild result inner where
  exonBuild =
    exonBuilderExtract .
    exonConcat @result .
    fmap (exonSegment @result exonBuilder)
  {-# inline exonBuild #-}

-- | This class is the main entry point for Exon.
--
-- The default instance unwraps all newtypes that are 'Generic' and passes the innermost type to 'ExonBuild'.
--
-- The original type is also used as a parameter to 'ExonBuild', so customizations can be based on it.
class Exon (result :: Type) where
  -- | Concatenate a list of 'Segment's.
  --
  -- @since 1.0.0.0
  exonProcess :: NonEmpty (Segment result) -> result

instance {-# overlappable #-} (
    OverNewtypes result inner,
    ExonBuild result inner
  ) => Exon result where
    exonProcess =
      overNewtypes @result (exonBuild @result)
    {-# inline exonProcess #-}

-- | Call 'exonProcess', but unwrap the arguments and rewrap the result using the supplied functions.
--
-- @since 1.0.0.0
exonProcessWith ::
  ∀ wrapped result .
  Exon wrapped =>
  (result -> wrapped) ->
  (wrapped -> result) ->
  NonEmpty (Segment result) ->
  result
exonProcessWith unwrap wrap =
  wrap . exonProcess @wrapped . fmap (fmap unwrap)
{-# inline exonProcessWith #-}
