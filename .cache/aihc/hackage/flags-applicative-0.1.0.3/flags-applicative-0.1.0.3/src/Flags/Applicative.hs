{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module implements a lightweight flags parser, inspired by @optparse-applicative@.
--
-- Sample usage (note the default log level and optional context):
--
-- @
-- module Main where
--
-- import Control.Applicative ((\<|\>), optional)
-- import Data.Text (Text)
-- import Flags.Applicative
--
-- -- Custom flags for our example.
-- data Flags = Flags
--   { rootPath :: Text
--   , logLevel :: Int
--   , context :: Maybe Text
--   } deriving Show
--
-- -- Returns a parser from CLI arguments to our custom flags.
-- flagsParser :: FlagsParser Flags
-- flagsParser = Flags
--   \<$\> flag textVal "root" "path to the root"
--   \<*\> (flag autoVal "log_level" "" \<|\> pure 0)
--   \<*\> (optional $ flag textVal "context" "")
--
-- main :: IO ()
-- main = do
--   (flags, args) <- parseSystemFlagsOrDie flagsParser
--   print flags
-- @
module Flags.Applicative (
  -- * Declaring flags
  Name, Description,
  -- ** Nullary flags
  switch, boolFlag,
  -- ** Unary flags
  flag, Reader,
  -- *** Common readers
  autoVal, textVal, stringVal, fracVal, intVal, enumVal, hostVal,
  -- *** Reader combinators
  listOf, mapOf,
  -- * Running parsers
  FlagsParser, FlagsError(..),
  parseFlags, parseSystemFlagsOrDie
) where

import Control.Applicative ((<|>), Alternative, empty)
import Control.Monad (when)
import Control.Monad.Except (Except, catchError, runExcept, throwError)
import Control.Monad.RWS.Strict (RWST, runRWST)
import Control.Monad.Reader (asks)
import Control.Monad.State.Strict (get, modify, put)
import Data.Bifunctor (second)
import Data.Foldable (foldl', toList)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
#if !MIN_VERSION_base(4, 11, 0)
import Data.Semigroup ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Network.Socket (HostName, PortNumber)
import System.Exit (die)
import System.Environment (getArgs)
import Text.Casing (fromHumps, toScreamingSnake)
import Text.Read (readEither)

-- The prefix used to identify all flags.
prefix :: String
prefix = "--"

-- | The name of a flag (without the @--@ prefix). Names can use all valid utf-8 characters except
-- @=@ (the value delimiter). In general, it's good practice for flag names to be lowercase ASCII
-- with underscores.
--
-- The following names are reserved and attempting to define a flag with the same name will cause an
-- error:
--
-- * @help@, displays usage when set.
-- * @swallowed_flags@, flags in this list which are set but undeclared will be ignored rather than
-- cause an error during parsing.
-- * @swallowed_switches@, similar to @swallowed_flags@ but for switches (nullary flags).
type Name = Text

-- Add the flag prefix to a name.
qualify :: Name -> Text
qualify name = T.pack prefix <> name

-- | An human-readable explanation of what the flag does. It is displayed when the parser is invoked
-- with the @--help@ flag.
type Description = Text

data Arity = Nullary | Unary deriving Eq

data Flag = Flag Arity Description

-- The errors which can happen during flag parsing.
data ValueError
  = InvalidValue Name Text String
  | MissingValues (NonEmpty Name)

missingValue :: Name -> ValueError
missingValue name = MissingValues $ name :| []

type Action a = RWST
  (Map Name Text) -- Flag values (or empty for nullary flags).
  () -- Unused.
  (Set Name) -- Used flags.
  (Except ValueError) -- Eventual parsing error.
  a

data Usage
  = Exactly Name
  | AllOf (Set Usage)
  | OneOf (Set Usage)
  deriving (Eq, Ord)

emptyUsage :: Usage
emptyUsage = AllOf Set.empty

andAlso :: Usage -> Usage -> Usage
andAlso (AllOf s1) (AllOf s2) = AllOf $ s1 <> s2
andAlso (AllOf s) u = AllOf $ Set.insert u s
andAlso u (AllOf s) = AllOf $ Set.insert u s
andAlso u1 u2 = AllOf $ Set.fromList [u1, u2]

orElse :: Usage -> Usage -> Usage
orElse (OneOf s1) (OneOf s2) = OneOf $ s1 <> s2
orElse (OneOf s) u = OneOf $ Set.insert u s
orElse u (OneOf s) = OneOf $ Set.insert u s
orElse u1 u2 = OneOf $ Set.fromList [u1, u2]

displayUsage :: Map Name Flag -> Usage -> Text
displayUsage flags usage = "usage: " <> go usage <> "\n" <> details where
  go (Exactly name) = case Map.lookup name flags of
    Just (Flag Unary _) -> qualify name <> "=*"
    _ -> qualify name
  go (AllOf s) =
    T.intercalate " " $ fmap go $ filter (/= emptyUsage) $ toList s
  go (OneOf s) =
    let contents s' = T.intercalate "|" $ fmap go $ toList $ s'
    in if Set.member emptyUsage s
      then "[" <> contents (Set.delete emptyUsage s) <> "]"
      else "(" <> contents s <> ")"
  describe (name, Flag _ desc) = if T.null desc then "" else "\n" <> qualify name <> "\t" <> desc
  details = T.concat $ fmap describe $ Map.toList flags

-- Parser definition errors.
data ParserError
  = Duplicate Name
  | Empty
  deriving (Eq, Show)

-- | Flags parser.
--
-- There are two types of flags:
--
-- * Nullary flags created with 'switch' and 'boolFlag', which do not accept a value.
-- * Unary flags created with 'flag'. These expect a value to be passed in either after an equal
-- sign (@--foo=value@) or as the following input value (@--foo value@). If the value starts with
-- @--@, only the first form is accepted.
--
-- You can run a parser using 'parseFlags' or 'parseSystemFlagsOrDie'.
data FlagsParser a
  = Actionable (Action a) (Map Name Flag) Usage
  | Invalid ParserError
  deriving Functor

-- Returns the combined map of flags if there are no duplicates, otherwise the name of one of the
-- duplicate flags.
mergeFlags :: Map Name Flag -> Map Name Flag -> Either Name (Map Name Flag)
mergeFlags flags1 flags2 = case Map.minViewWithKey $ flags1 `Map.intersection` flags2 of
  Just ((name, _), _) -> Left name
  Nothing -> Right $ flags1 `Map.union` flags2

instance Applicative FlagsParser where
  pure res = Actionable (pure res) Map.empty emptyUsage

  Invalid err <*> _ = Invalid err
  _ <*> Invalid err = Invalid err
  Actionable action1 flags1 usage1 <*> Actionable action2 flags2 usage2 =
    case mergeFlags flags1 flags2 of
      Left name -> Invalid $ Duplicate name
      Right flags -> Actionable (action1 <*> action2) flags (usage1 `andAlso` usage2)

instance Alternative FlagsParser where
  empty = Invalid Empty

  Invalid Empty <|> parser = parser
  parser <|> Invalid Empty = parser
  Invalid err <|> _ = Invalid err
  _ <|> Invalid err = Invalid err
  Actionable action1 flags1 usage1 <|> Actionable action2 flags2 usage2 =
    case mergeFlags flags1 flags2 of
      Left name -> Invalid $ Duplicate name
      Right flags -> Actionable action flags (usage1 `orElse` usage2) where
        wrap a = catchError (Right <$> a) $ \case
          (MissingValues names) -> pure $ Left names
          err -> throwError err
        action = do
          used <- get
          wrap action1 >>= \case
            Left names -> do
              put used
              wrap action2 >>= \case
                Left names' -> throwError $ MissingValues $ names <> names'
                Right res -> pure res
            Right res -> do
              used' <- get
              _ <- wrap action2
              put used'
              pure res

-- | The possible parsing errors.
data FlagsError
  = DuplicateFlag Name
  -- ^ A flag was declared multiple times.
  | EmptyParser
  -- ^ The parser was empty.
  | Help Text
  -- ^ The input included the @--help@ flag.
  | InconsistentFlagValues Name
  -- ^ At least one unary flag was specified multiple times with different values.
  | InvalidFlagValue Name Text String
  -- ^ A unary flag's value failed to parse.
  | MissingFlags (NonEmpty Name)
  -- ^ A required flag was missing; at least one of the returned flags should be set.
  | MissingFlagValue Name
  -- ^ A unary flag was missing a value. This can happen either if a value-less unary flag was the
  -- last token or was followed by a value which is also a flag name (in which case you should use
  -- the single-token form: @--flag=--value@).
  | ReservedFlag Name
  -- ^ A flag with a reserved name was declared.
  | UnexpectedFlagValue Name
  -- ^ A nullary flag was given a value.
  | UnexpectedFlags (NonEmpty Name)
  -- ^ At least one flag was set but unused. This can happen when optional flags are set but their
  -- branch is not selected.
  | UnknownFlag Name
  -- ^ An unknown flag was set.
  deriving (Eq, Show)

displayFlags :: Foldable f => f Name -> Text
displayFlags = T.intercalate " " . fmap qualify . toList

-- Pretty-print a 'FlagError'.
displayFlagError :: FlagsError -> Text
displayFlagError (DuplicateFlag name) = qualify name <> " was declared multiple times"
displayFlagError EmptyParser = "empty parser"
displayFlagError (Help usage) = usage
displayFlagError (InconsistentFlagValues name) = "inconsistent values for " <> qualify name
displayFlagError (InvalidFlagValue name val msg) =
  "invalid value \"" <> val <> "\" for " <> qualify name <> " (" <> T.pack msg <> ")"
displayFlagError (MissingFlags names) =
  "at least one of the following required flags must be set: " <> displayFlags names
displayFlagError (MissingFlagValue name) = "missing value for " <> qualify name
displayFlagError (ReservedFlag name) = qualify name <> " was declared but is reserved"
displayFlagError (UnexpectedFlagValue name) = "unexpected value for " <> qualify name
displayFlagError (UnexpectedFlags names) = "unexpected " <> displayFlags names
displayFlagError (UnknownFlag name) = "undeclared " <> qualify name

-- Marks a flag as used. This is useful to check for unexpected flags after parsing is complete.
useFlag :: Name -> Action ()
useFlag name = modify (Set.insert name)

-- | Returns a parser with the given name and description for a flag with no value, failing if the
-- flag is not present. See also 'boolFlag' for a variant which doesn't fail when the flag is
-- missing.
switch :: Name -> Description -> FlagsParser ()
switch name desc = Actionable action flags usage where
  action = asks (Map.member name) >>= \case
    True -> useFlag name
    False -> throwError $ missingValue name
  flags = Map.singleton name (Flag Nullary desc)
  usage = Exactly name

-- | Returns a parser with the given name and description for a flag with no value, returning
-- whether the flag was present.
boolFlag :: Name -> Description -> FlagsParser Bool
boolFlag name desc = (True <$ switch name desc) <|> pure False

-- | The type used to read flag values.
type Reader a = Text -> Either String a

-- | Returns a parser using the given value reader, name, and description for a flag with an
-- associated value.
flag :: Reader a -> Name -> Description -> FlagsParser a
flag convert name desc = Actionable action flags usage where
  action = do
    useFlag name
    asks (Map.lookup name) >>= \case
      Nothing -> throwError $ missingValue name
      Just val -> case convert val of
        Left err -> throwError $ InvalidValue name val err
        Right res -> pure res
  flags = Map.singleton name (Flag Unary desc)
  usage = Exactly name

-- | Returns a reader for any value with a 'Read' instance. Prefer 'textVal' for textual values
-- since 'autoVal'  will expect its values to be double-quoted and might not work as expected.
autoVal :: Read a => Reader a
autoVal = readEither . T.unpack

-- | Returns a reader for a single string value. This can useful when interfacing with non-text APIs
-- (e.g. 'FilePath') but in general prefer 'textVal'.
stringVal :: Reader String
stringVal = Right . T.unpack

-- | Returns a reader for a single text value.
textVal :: Reader Text
textVal = Right

-- Fully executes a reader. This function is useful for interacting with "Data.Text.Read".
readingFully :: (Text -> Either String (a, Text)) -> Reader a
readingFully f t = case f t of
  Left e -> Left e
  Right (v, t') -> if T.null t' then Right v else Left $ T.unpack $ "trailing chars: " <> t'

-- | Returns a reader for any number with a 'Fractional' instance (e.g. 'Double', 'Float').
fracVal :: Fractional a => Reader a
fracVal = readingFully T.rational

-- | Returns a reader for any number with an 'Integral instance (e.g. 'Int', 'Integer').
intVal :: Integral a => Reader a
intVal = readingFully $ T.signed T.decimal

-- | Returns a reader for 'Enum' instances. This reader assumes that enum (Haskell) constructors are
-- written in PascalCase and expects UPPER_SNAKE_CASE as command-line flag values. For example:
--
-- > data Mode = Flexible | Strict deriving (Bounded, Enum, Show)
-- > modeFlag = flag enumVal "mode" "the mode" :: FlagsParser Mode
--
-- The above flag will accept values @--mode=FLEXIBLE@ and @--mode=STRICT@.
enumVal :: (Bounded a, Enum a, Show a) => Reader a
enumVal = parse where
  write = T.pack . toScreamingSnake . fromHumps . show -- Serializes an enum value.
  m = Map.fromList $ fmap (\v -> (write v, v)) [minBound .. maxBound]
  parse t = case Map.lookup t m of
    Nothing ->
      let e = t <> " is not in " <> T.intercalate "," (Map.keys m)
      in Left $ T.unpack e
    Just v -> Right v

-- | Returns a reader for network hosts of the form @hostname:port@. The port part is optional.
hostVal :: Reader (HostName, Maybe PortNumber)
hostVal txt = do
  let (hostname, suffix) = T.breakOn ":" txt
  mbPort <- case T.stripPrefix ":" suffix of
      Nothing -> Right Nothing
      Just portStr -> Just <$> readEither (T.unpack portStr)
  pure (T.unpack hostname, mbPort)

-- | Transforms a single-valued unary flag into one which accepts multiple comma-separated values.
-- For example, to parse a comma-separated list of integers:
--
-- > countsFlag = flag (listOf intVal) "counts" "the counts"
--
-- Empty text values are ignored, which means both that trailing commas are supported and that an
-- empty list can be specified simply by specifying an empty value on the command line. Note that
-- escapes are not supported, so values should not contain any commas.
listOf :: Reader a -> Reader [a]
listOf f = traverse f . filter (not . T.null) . T.splitOn ","

-- | Transforms a single-valued unary flag into one which accepts a comma-separated list of
-- colon-delimited key-value pairs. The syntax is @key:value[,key:value...]@. Note that escapes are
-- not supported, so neither keys not values should contain colons or commas.
mapOf :: Ord a => Reader a -> Reader b -> Reader (Map a b)
mapOf f g = fmap Map.fromList <$> listOf (h . T.breakOn ":") where
  h (k, v) = case T.uncons v of
    Nothing -> Left $ T.unpack $ "empty value for key " <> k
    Just (_, v') -> (,) <$> f k <*> g v'

-- Tries to gather all raw flag values into a map. When @ignoreUnknown@ is true, this function will
-- pass through any unknown flags into the returned argument list( and pass through any @--@ value),
-- otherwise it will throw a 'FlagError'.
gatherValues :: Bool -> Map Name Flag -> [String] -> Either FlagsError ((Map Name Text), [String])
gatherValues ignoreUnknown flags = go where
  go [] = Right (Map.empty, [])
  go (token:tokens) = if not (prefix `isPrefixOf` token)
    then second (token:) <$> go tokens
    else
      let entry = drop 2 token :: String
      in if null entry
        then Right (Map.empty, if ignoreUnknown then "--":tokens else tokens)
        else
          let
            (name, pval) = T.breakOn "=" (T.pack entry)
            missing = Left $ MissingFlagValue name
            insert val tokens' = do
              (vals', args') <- go tokens'
              case Map.lookup name vals' of
                Nothing -> Right (Map.insert name val vals', args')
                Just val' -> if val == val'
                  then Right (vals', args')
                  else Left $ InconsistentFlagValues name
          in case Map.lookup name flags of
            Nothing -> if ignoreUnknown
              then second (token:) <$> go tokens
              else Left (UnknownFlag name)
            Just (Flag Nullary _) -> if T.null pval
              then insert "" tokens
              else Left $ UnexpectedFlagValue name
            Just (Flag Unary _) -> case T.uncons pval of
              Nothing -> case tokens of
                (token':tokens') -> if prefix `isPrefixOf` token'
                  then missing
                  else insert (T.pack token') tokens'
                _ -> missing
              Just (_, val) -> insert val tokens

-- Runs a single parsing pass.
runAction :: Bool -> Action a -> Map Name Flag -> [String] -> Either FlagsError (a, Set Name, [String])
runAction ignoreUnknown action flags tokens = case gatherValues ignoreUnknown flags tokens of
  Left err -> Left err
  Right (values, args) -> case runExcept $ runRWST action values Set.empty of
    Right (rv, usedNames, _) ->
      let unused = Set.difference (Map.keysSet values) usedNames
      in Right (rv, unused, args)
    Left (MissingValues names) -> Left $ MissingFlags names
    Left (InvalidValue name val msg) -> Left $ InvalidFlagValue name val msg

-- Preprocessing parser.
reservedParser :: FlagsParser (Bool, Set Name, Set Name)
reservedParser =
  let textSetFlag name = Set.fromList <$> (flag (Right . T.splitOn ",") name "" <|> pure [])
  in (,,)
    <$> boolFlag "help" ""
    <*> textSetFlag "swallowed_flags"
    <*> textSetFlag "swallowed_switches"

-- | Runs a parser on a list of tokens, returning the parsed flags alongside other non-flag
-- arguments (i.e. which don't start with @--@). If the special @--@ token is found, all following
-- tokens will be considered arguments even if they look like flags.
parseFlags :: FlagsParser a -> [String] -> Either FlagsError (a, [String])
parseFlags parser tokens = case reservedParser of
  Invalid _ -> error "unreachable"
  Actionable action0 flags0 _ -> do
    ((showHelp, sflags, sswitches), _, tokens') <- runAction True action0 flags0 tokens
    (action, flags) <- case parser of
      Invalid (Duplicate name) -> Left $ DuplicateFlag name
      Invalid Empty -> Left EmptyParser
      Actionable action flags usage -> do
        case Set.lookupMin (Map.keysSet $ Map.intersection flags0 flags) of
          Nothing -> Right ()
          Just name -> Left $ ReservedFlag name
        when showHelp $  Left (Help $ displayUsage flags usage)
        let
          flags' = foldl' (\m name -> Map.insert name (Flag Unary "") m) flags sflags
          flags'' = foldl' (\m name -> Map.insert name (Flag Nullary "") m) flags' sswitches
        Right (action, flags'')
    (rv, unused, tokens'') <- runAction False action flags tokens'
    case Set.minView $ Set.difference unused (sflags <> sswitches) of
      Nothing -> Right (rv, tokens'')
      Just (name, names) -> Left $ UnexpectedFlags $ name :| toList names

-- | Runs a parser on the system's arguments, or exits with code 1 and prints the relevant error
-- message in case of failure.
parseSystemFlagsOrDie :: FlagsParser a -> IO (a, [String])
parseSystemFlagsOrDie parser = parseFlags parser <$> getArgs >>= \case
  Left err -> die $ T.unpack $ displayFlagError err
  Right res -> pure res
