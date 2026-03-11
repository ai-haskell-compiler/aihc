----------------------------------------------------------------------------
-- |
-- Module      :  Data.Toml.Parse
-- Copyright   :  (c) Sergey Vinokurov 2018
-- License     :  Apache-2.0 (see LICENSE)
-- Maintainer  :  serg.foo@gmail.com
----------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module TOML.Parse
  ( Value(..)
  , Parser
  , runParser
  , mkTomlError
  , AtomicTomlError(..)
  , TomlError
  , (<?>)
  , L
  , extract
  , TomlParse(..)
  , FromToml(..)
  , Index(..)
  , (.!=)
  , pTable
  , pTableL
  , pKey
  , pKeyMaybe
  , pStr
  , pStrL
  , pBool
  , pInt
  , pIntL
  , pDouble
  , pDoubleL
  , pArray
  , TomlDateTime(..)
  , pDatetime
  , pDatetimeL
  , pCases
  , ppToml
  ) where

import Control.Applicative
import Control.Comonad
import Control.DeepSeq
import Control.Monad

import Data.Bifunctor
import Data.DList (DList)
import Data.DList qualified as DL
import Data.Foldable
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time qualified as Time
import Data.Time.Format.ISO8601 qualified as Time
import Data.Traversable
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Void (Void, vacuous)
import Prettyprinter
import Prettyprinter.Combinators
import Prettyprinter.Generics

import TOML

import Unsafe.Coerce

data TomlType
  = TTable
  | TArray
  | TString
  | TInteger
  | TFloat
  | TBoolean
  | TDatetime
  deriving (Eq, Ord, Show, Generic)

getType :: Value -> TomlType
getType = \case
  Table{}          -> TTable
  Array{}          -> TArray
  String{}         -> TString
  Integer{}        -> TInteger
  Float{}          -> TFloat
  Boolean{}        -> TBoolean
  OffsetDateTime{} -> TDatetime
  LocalDateTime{}  -> TDatetime
  LocalDate{}      -> TDatetime
  LocalTime{}      -> TDatetime

ppTomlType :: TomlType -> (Doc ann, Doc ann)
ppTomlType = \case
  TTable    -> ("a",  "table")
  TString   -> ("a",  "string")
  TInteger  -> ("an", "integer")
  TFloat    -> ("a",  "float")
  TBoolean  -> ("a",  "boolean")
  TDatetime -> ("a",  "datetime")
  TArray    -> ("an", "array")

data TomlPath
  = PathIndex !Int
  | PathKey !Text
  | PathOther !Text
  deriving (Eq, Ord, Show, Generic)

instance Pretty TomlPath where
  pretty = \case
    PathIndex n     -> "In array element" <+> pretty n
    PathKey str     -> "In table key" <+> squotes (pretty str)
    PathOther thing -> "While parsing" <+> pretty thing

data AtomicTomlError
  = UnexpectedType
      !TomlType -- ^ Expected
      Value     -- ^ Got
  | MissingKey !Text Table
  | IndexOutOfBounds !Int Value
  | OtherError (Doc Void)
  deriving (Show, Generic)

-- | Prettyprint toml value.
ppToml :: Value -> Doc ann
ppToml = \case
  Table    x       -> ppMapWith pretty ppToml x
  String   x       -> pretty x
  Integer  x       -> pretty x
  Float    x       -> pretty x
  Boolean  x       -> pretty x
  LocalDateTime x  -> pretty $ TomlLocalDateTime x
  OffsetDateTime x -> pretty $ TomlOffsetDateTime x
  LocalDate x      -> pretty $ TomlLocalDate x
  LocalTime x      -> pretty $ TomlLocalTime x
  Array    xs      -> ppListWith ppToml xs

instance Pretty AtomicTomlError where
  pretty = \case
    UnexpectedType expected got ->
      "Expected to find" <+> article <+> typ <+> "but found" <+> article' <+> typ' <> "." <+>
      "Value:" ## ppToml got
      where
        (article,  typ)  = ppTomlType expected
        (article', typ') = ppTomlType $ getType got
    MissingKey key tab          -> "Missing key" <+> squotes (pretty key) <+> "in table:" ## ppMapWith pretty ppToml tab
    IndexOutOfBounds ix node    -> "Index" <+> pretty ix <+> "is out of bounds in array:" ## ppToml node
    OtherError err              -> "Other error:" ## vacuous err

data TomlError
  = ErrorEmpty
  | ErrorAtomic !AtomicTomlError
  -- | Invariant: children of ErrorAnd never share common prefix.
  | ErrorAnd TomlError TomlError
  -- | Invariant: children of ErrorOr never share common prefix.
  | ErrorOr TomlError TomlError
  | ErrorPrefix (NonEmpty TomlPath) TomlError
  deriving (Show, Generic)

instance Pretty TomlError where
  pretty = \case
    ErrorEmpty       -> "Control.Applicative.empty"
    ErrorAtomic err  -> pretty err
    ErrorAnd x y     -> "AND" ## align (vsep $ map pretty $ toList $ collectConjuctions x y)
    ErrorOr  x y     -> "OR"  ## align (vsep $ map pretty $ toList $ collectDisjunctions x y)
    ErrorPrefix ps e -> foldr (\p acc -> pretty p ## acc) (pretty e) ps
    where
      collectConjuctions :: TomlError -> TomlError -> DList TomlError
      collectConjuctions (ErrorAnd a b) (ErrorAnd c d) = collectConjuctions a b <> collectConjuctions c d
      collectConjuctions (ErrorAnd a b) c              = collectConjuctions a b <> DL.singleton c
      collectConjuctions a              (ErrorAnd c d) = DL.singleton a <> collectConjuctions c d
      collectConjuctions a              c              = DL.fromList [a, c]

      collectDisjunctions :: TomlError -> TomlError -> DList TomlError
      collectDisjunctions (ErrorOr a b) (ErrorOr c d) = collectDisjunctions a b <> collectDisjunctions c d
      collectDisjunctions (ErrorOr a b) c             = collectDisjunctions a b <> DL.singleton c
      collectDisjunctions a             (ErrorOr c d) = DL.singleton a <> collectDisjunctions c d
      collectDisjunctions a             c             = DL.fromList [a, c]

-- NB order of constructors is important
data IsCommitted = Uncommitted | Committed
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Semigroup IsCommitted where
  {-# INLINE (<>) #-}
  (<>) = max

newtype Validation a = Validation
  { unValidation :: Either (IsCommitted, TomlError) a }
  deriving (Functor)

zipErrors
  :: (TomlError -> TomlError -> TomlError)
  -> TomlError
  -> TomlError
  -> TomlError
zipErrors f x y = case commonPrefix x y of
  Nothing               -> f x y
  Just (common, x', y') ->
    ErrorPrefix common (f x' y')

commonPrefix
  :: TomlError
  -> TomlError
  -> Maybe (NonEmpty TomlPath, TomlError, TomlError)
commonPrefix x y = case (x, y) of
  (ErrorPrefix px x', ErrorPrefix py y') ->
    flip fmap (go px py) $ \(common, px', py') ->
      let prefix []       err = err
          prefix (p : ps) err = ErrorPrefix (p :| ps) err
      in (common, prefix px' x', prefix py' y')
  _ -> Nothing
  where
    go :: NonEmpty TomlPath -> NonEmpty TomlPath -> Maybe (NonEmpty TomlPath, [TomlPath], [TomlPath])
    go xs ys =
      case go' [] (toList xs) (toList ys) of
        (c : cs, xs', ys') -> Just (c :| cs, xs', ys')
        _                  -> Nothing
    go' :: Eq a => [a] -> [a] -> [a] -> ([a], [a], [a])
    go' common (a : as) (b : bs)
      | a == b = go' (a : common) as bs
    go' common as bs = (reverse common, as, bs)

instance Applicative Validation where
  {-# INLINE pure #-}
  pure = Validation . pure
  {-# NOINLINE (<*>) #-}
  (<*>) vf'@(Validation vf) vx'@(Validation vx) =
    case (vf, vx) of
      (Left (cf, ef), Left (cx, ex)) -> Validation $ Left (cf <> cx, zipErrors ErrorAnd ef ex)
      (Left _,        _)             -> unsafeCoerce vf'
      (_,             Left _)        -> unsafeCoerce vx'
      (Right f,       Right x)       -> Validation $ Right $ f x

instance Alternative Validation where
  {-# INLINE empty #-}
  empty = Validation $ Left (Uncommitted, ErrorEmpty)
  {-# NOINLINE (<|>) #-}
  (<|>) x'@(Validation x) y'@(Validation y) =
    case (x, y) of
      (Right _,       _)             -> x'
      (_,             Right _)       -> y'
      (Left (cf, ef), Left (cx, ex)) ->
        case (cf, cx) of
          (Committed,   Uncommitted) -> x'
          (Uncommitted, Committed)   -> y'
          _                          -> Validation $ Left (cf <> cx, zipErrors ErrorOr ef ex)

instance Monad Validation where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>)  #-}
  (>>=) x'@(Validation x) f =
    case x of
      Left  _ -> unsafeCoerce x'
      Right y -> commit $ f y
    where
      commit (Validation (Left (_, err))) = Validation $ Left (Committed, err)
      commit z@(Validation (Right _))     = z
  (>>) = (*>)

instance MonadPlus Validation

newtype ParseEnv = ParseEnv { unParseEnv :: [TomlPath] }
  deriving (Eq, Ord, Show, Generic, Pretty)

newtype Parser a = Parser
  { unParser :: Validation a }
  deriving (Functor, Applicative, Alternative, MonadPlus)

instance Monad Parser where
  {-# INLINE (>>=) #-}
  {-# INLINE (>>)  #-}
  (>>=) (Parser x) f = Parser $ do
    x' <- x
    unParser $ f x'
  (>>) = (*>)

infixl 9 <?>

-- | Add textual annotation to the provided located thing. The annotation will
-- be shows as part of error message if the location ultimately gets passed to
-- 'throwParseError'.
(<?>) :: L a -> Text -> L a
(<?>) (L env x) y = L (inside (PathOther y) env) x

instance TomlParse Parser where
  throwParseError loc err = Parser $ Validation $ Left (Uncommitted, mkTomlError' loc err)

runParser :: a -> (L a -> Parser b) -> Either (Doc Void) b
runParser x f
  = first (("Error while parsing:" ##) . pretty . snd)
  $ unValidation
  $ unParser
  $ f
  $ L (ParseEnv []) x

mkTomlError :: L a -> Doc Void -> TomlError
mkTomlError loc = mkTomlError' loc . OtherError

mkTomlError' :: L a -> AtomicTomlError -> TomlError
mkTomlError' (L env _) err = case reverse $ unParseEnv env of
  []     -> ErrorAtomic err
  p : ps -> ErrorPrefix (p :| ps) $ ErrorAtomic err

-- | Adds to 'a' its provenance in the toml file.
data L a = L ParseEnv a
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance Pretty a => Pretty (L a) where pretty = ppGeneric

instance Comonad L where
  {-# INLINE extract   #-}
  {-# INLINE duplicate #-}
  extract (L _ x) = x
  duplicate orig@(L env _) = L env orig

{-# INLINE inside #-}
inside :: TomlPath -> ParseEnv -> ParseEnv
inside x (ParseEnv xs) = ParseEnv (x : xs)

class (Applicative m, Alternative m) => TomlParse m where
  throwParseError :: L b -> AtomicTomlError -> m a

class FromToml a b where
  fromToml :: L a -> Parser b

instance FromToml b a => FromToml b (L a) where
  {-# INLINE fromToml #-}
  fromToml b@(L env _) = L env <$> fromToml b

instance FromToml a a where
  {-# INLINE fromToml #-}
  fromToml = pure . extract

instance {-# OVERLAPPING #-} FromToml Value String where
  {-# INLINE fromToml #-}
  fromToml = fmap T.unpack . pStr

instance FromToml Value Text where
  {-# INLINE fromToml #-}
  fromToml = pStr

instance FromToml Value Bool where
  {-# INLINE fromToml #-}
  fromToml = pBool

instance FromToml Value Int where
  {-# INLINE fromToml #-}
  fromToml = pInt

instance FromToml Value Double where
  {-# INLINE fromToml #-}
  fromToml = pDouble

instance (Ord k, FromToml Text k, FromToml Value v) => FromToml Value (Map k v) where
  fromToml = pTableL >=> fromToml

instance (Ord k, FromToml Text k, FromToml Value v) => FromToml Table (Map k v) where
  fromToml (L env y) = do
    ys <- for (M.toList y) $ \(k, v) ->
      (,)
        <$> fromToml (L env k)
        <*> fromToml (L (inside (PathKey k) env) v)
    pure $ M.fromList ys

instance FromToml Value a => FromToml Value (Vector a) where
  fromToml = pArray >=> traverse fromToml . V.fromList

instance FromToml Value a => FromToml Value [a] where
  fromToml = pArray >=> traverse fromToml

instance FromToml Value a => FromToml Value (NonEmpty a) where
  fromToml x = do
    ys <- pArray x
    case toList ys of
      []     -> throwParseError x $ OtherError "Expected a non-empty list"
      z : zs -> (:|) <$> fromToml z <*> traverse fromToml zs

infixl 5 .:, .:?, .!=

class Index a where
  (.:)  :: FromToml Value b => a -> Text -> Parser b
  (.:?) :: FromToml Value b => a -> Text -> Parser (Maybe b)

instance Index (L Table) where
  {-# INLINE (.:)  #-}
  {-# INLINE (.:?) #-}
  (.:)  x key = pKey key x >>= fromToml
  (.:?) x key = traverse fromToml $ liftMaybe $ pKeyMaybe key x

instance Index (L Value) where
  {-# INLINE (.:)  #-}
  {-# INLINE (.:?) #-}
  (.:)  x key = pTableL x >>= pKey key >>= fromToml
  (.:?) x key = pTableL x >>= traverse fromToml . liftMaybe . pKeyMaybe key

instance a ~ L Value => Index (Parser a) where
  {-# INLINE (.:)  #-}
  {-# INLINE (.:?) #-}
  (.:)  x key = x >>= pTableL >>= pKey key >>= fromToml
  (.:?) x key = x >>= pTableL >>= traverse fromToml . liftMaybe . pKeyMaybe key

-- | Assign default value to a parser that produces 'Maybe'. Typically used together with '.:?':
--
-- > foo .:? "bar" .!= 10
{-# INLINE (.!=) #-}
(.!=) :: Functor m => m (Maybe a) -> a -> m a
(.!=) action def = fromMaybe def <$> action

pTable :: TomlParse m => L Value -> m Table
pTable = fmap extract . pTableL

pTableL :: TomlParse m => L Value -> m (L Table)
pTableL = \case
  L env (Table x)    -> pure $ L env x
  other@(L _ other') -> throwParseError other $ UnexpectedType TTable other'

pKey :: TomlParse m => Text -> L Table -> m (L Value)
pKey key tab'@(L _ tab) = case liftMaybe $ pKeyMaybe key tab' of
  Just x  -> pure x
  Nothing -> throwParseError tab' $ MissingKey key tab

pKeyMaybe :: Text -> L Table -> L (Maybe Value)
pKeyMaybe key (L env tab) = L (inside (PathKey key) env) $ M.lookup key tab

pStr :: TomlParse m => L Value -> m Text
pStr = fmap extract . pStrL

pStrL :: TomlParse m => L Value -> m (L Text)
pStrL = \case
  L env (String x)   -> pure $ L env x
  other@(L _ other') -> throwParseError other $ UnexpectedType TString other'

pBool :: TomlParse m => L Value -> m Bool
pBool = \case
  L _ (Boolean x)    -> pure x
  other@(L _ other') -> throwParseError other $ UnexpectedType TBoolean other'

pInt :: TomlParse m => L Value -> m Int
pInt = fmap extract . pIntL

pIntL :: TomlParse m => L Value -> m (L Int)
pIntL = \case
  L env (Integer x)  -> pure $ L env $ fromIntegral x
  other@(L _ other') -> throwParseError other $ UnexpectedType TInteger other'

pDouble :: TomlParse m => L Value -> m Double
pDouble = fmap extract . pDoubleL

pDoubleL :: TomlParse m => L Value -> m (L Double)
pDoubleL = \case
  L env (Float x)    -> pure $ L env x
  other@(L _ other') -> throwParseError other $ UnexpectedType TFloat other'

data TomlDateTime
  = TomlLocalDateTime Time.LocalTime
  | TomlOffsetDateTime (Time.LocalTime, Time.TimeZone)
  | TomlLocalDate Time.Day
  | TomlLocalTime Time.TimeOfDay
  deriving (Eq, Ord, Show, Generic)

instance NFData TomlDateTime

instance Pretty TomlDateTime where
  pretty (TomlLocalDateTime t)        = pretty $ Time.iso8601Show t
  pretty (TomlOffsetDateTime (t, tz)) = pretty $ Time.iso8601Show $ Time.localTimeToUTC tz t
  pretty (TomlLocalDate t)            = pretty $ Time.iso8601Show t
  pretty (TomlLocalTime t)            = pretty $ Time.iso8601Show t

pDatetime :: TomlParse m => L Value -> m TomlDateTime
pDatetime = fmap extract . pDatetimeL

pDatetimeL :: TomlParse m => L Value -> m (L TomlDateTime)
pDatetimeL = \case
  L env (LocalDateTime x)  -> pure $ L env $ TomlLocalDateTime x
  L env (OffsetDateTime x) -> pure $ L env $ TomlOffsetDateTime x
  L env (LocalDate x)      -> pure $ L env $ TomlLocalDate x
  L env (LocalTime x)      -> pure $ L env $ TomlLocalTime x
  other@(L _ other')       -> throwParseError other $ UnexpectedType TDatetime other'

pArray :: TomlParse m => L Value -> m [L Value]
pArray = \case
  L env (Array x)    -> pure $ (\(n, x') -> L (inside (PathIndex n) env) x') <$> zip [0..] x
  other@(L _ other') -> throwParseError other $ UnexpectedType TArray other'

{-# INLINE pCases #-}
pCases :: (Ord k, FromToml Value k, Pretty k) => Map k v -> L Value -> Parser v
pCases env = \x -> do
  k <- fromToml x
  case M.lookup k env of
    Just v  -> pure v
    Nothing -> throwParseError x $ OtherError $
      "Unexpected value" <+> squotes (pretty k) <> "." <+>
      "Expected one of" <+> vsep (punctuate "," (map pretty (M.keys env)))

liftMaybe :: L (Maybe a) -> Maybe (L a)
liftMaybe (L env x) = L env <$> x
