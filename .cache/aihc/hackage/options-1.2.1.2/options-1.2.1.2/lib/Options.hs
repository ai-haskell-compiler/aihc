-- |
-- Module: Options
-- License: MIT
module Options
  ( -- * Defining options
    Options (..),
    defaultOptions,
    simpleOption,
    DefineOptions,
    SimpleOptionType (..),

    -- * Defining subcommands
    Subcommand,
    subcommand,

    -- * Running main with options
    runCommand,
    runSubcommand,

    -- * Parsing argument lists
    Parsed,
    parsedError,
    parsedHelp,

    -- ** Parsing options
    ParsedOptions,
    parsedOptions,
    parsedArguments,
    parseOptions,

    -- ** Parsing sub-commands
    ParsedSubcommand,
    parsedSubcommand,
    parseSubcommand,

    -- * Advanced option definitions
    OptionType,
    defineOption,
    Option,
    optionShortFlags,
    optionLongFlags,
    optionDefault,
    optionDescription,
    optionGroup,

    -- ** Option groups
    Group,
    group,
    groupName,
    groupTitle,
    groupDescription,

    -- * Option types
    optionType_bool,
    optionType_string,
    optionType_int,
    optionType_int8,
    optionType_int16,
    optionType_int32,
    optionType_int64,
    optionType_word,
    optionType_word8,
    optionType_word16,
    optionType_word32,
    optionType_word64,
    optionType_integer,
    optionType_float,
    optionType_double,
    optionType_maybe,
    optionType_list,
    optionType_set,
    optionType_map,
    optionType_enum,

    -- ** Custom option types
    optionType,
    optionTypeName,
    optionTypeDefault,
    optionTypeParse,
    optionTypeShow,
    optionTypeUnary,
    optionTypeMerge,
  )
where

import Control.Monad (forM_)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Functor.Identity
import Data.Int
import Data.List (intercalate)
import Data.Map qualified as Map
import Data.Maybe (isJust)
import Data.Set qualified as Set
import Data.Word
import Options.Help
import Options.Tokenize
import Options.Types
import Options.Util (mapEither)
import System.Environment qualified
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStr, hPutStrLn, stderr, stdout)

-- | Options are defined together in a single data type, which will be an
--   instance of 'Options'
--
-- See 'defineOptions' for details on defining instances of 'Options'.
class Options opts where
  -- | Defines the structure and metadata of the options in this type,
  -- including their types, flag names, and documentation.
  --
  -- Options with a basic type and a single flag name may be defined
  -- with 'simpleOption'. Options with more complex requirements may
  -- be defined with 'defineOption'.
  --
  -- Non-option fields in the type may be set using applicative functions
  -- such as 'pure'.
  --
  -- Options may be included from another type by using a nested call to
  -- 'defineOptions'.
  --
  -- Library authors are encouraged to aggregate their options into a
  -- few top-level types, so application authors can include it
  -- easily in their own option definitions.
  defineOptions :: DefineOptions opts

data DefineOptions a
  = DefineOptions
      a
      (Integer -> (Integer, [OptionInfo]))
      (Integer -> Map.Map OptionKey [Token] -> Either String (Integer, a))

instance Functor DefineOptions where
  fmap fn (DefineOptions defaultValue getInfo parse) =
    DefineOptions
      (fn defaultValue)
      getInfo
      ( \key tokens -> case parse key tokens of
          Left err -> Left err
          Right (key', a) -> Right (key', fn a)
      )

instance Applicative DefineOptions where
  pure a = DefineOptions a (\key -> (key, [])) (\key _ -> Right (key, a))
  (DefineOptions acc_default acc_getInfo acc_parse) <*> (DefineOptions defaultValue getInfo parse) =
    DefineOptions
      (acc_default defaultValue)
      ( \key -> case acc_getInfo key of
          (key', infos) -> case getInfo key' of
            (key'', infos') -> (key'', infos ++ infos')
      )
      ( \key tokens -> case acc_parse key tokens of
          Left err -> Left err
          Right (key', fn) -> case parse key' tokens of
            Left err -> Left err
            Right (key'', a) -> Right (key'', fn a)
      )

-- | An options value containing only the default values for each option
--
-- This is equivalent to the options value when parsing an empty argument list.
defaultOptions :: Options opts => opts
defaultOptions = case defineOptions of
  (DefineOptions def _ _) -> def

-- | An option's type determines how the option will be parsed, and which
--   Haskell type the parsed value will be stored as
--
-- There are many types available, covering most basic types and a few more advanced types.
data OptionType val = OptionType
  { -- | The name of this option type; used in @--help@ output.
    optionTypeName :: String,
    -- | The default value for options of this type. This will be used
    -- if 'optionDefault' is not set when defining the option.
    optionTypeDefault :: val,
    -- | Try to parse the given string to an option value. If parsing
    -- fails, an error message will be returned.
    optionTypeParse :: String -> Either String val,
    -- | Format the value for display; used in @--help@ output.
    optionTypeShow :: val -> String,
    -- | If not Nothing, then options of this type may be set by a unary
    -- flag. The option will be parsed as if the given value were set.
    optionTypeUnary :: Maybe val,
    -- | If not Nothing, then options of this type may be set with repeated
    -- flags. Each flag will be parsed with 'optionTypeParse', and the
    -- resulting parsed values will be passed to this function for merger
    -- into the final value.
    optionTypeMerge :: Maybe ([val] -> val)
  }

-- | Define an option group with the given name and title
--
-- Use 'groupDescription' to add additional descriptive text, if needed.
group ::
  -- | Name
  String ->
  -- | Title; see 'groupTitle'.
  String ->
  -- | Description; see 'groupDescription'.
  String ->
  Group
group = Group

-- | Define a new option type with the given name, default, and behavior
optionType ::
  -- | Name
  String ->
  -- | Default value
  val ->
  -- | Parser
  (String -> Either String val) ->
  -- | Formatter
  (val -> String) ->
  OptionType val
optionType name def parse show' = OptionType name def parse show' Nothing Nothing

class SimpleOptionType a where
  simpleOptionType :: OptionType a

instance SimpleOptionType Bool where
  simpleOptionType = optionType_bool

-- | Store an option as a @'Bool'@
--
-- The option's value must be either @\"true\"@ or @\"false\"@.
-- Boolean options are unary, which means that their value is
-- optional when specified on the command line.
optionType_bool :: OptionType Bool
optionType_bool =
  (optionType "bool" False parseBool (\x -> if x then "true" else "false"))
    { optionTypeUnary = Just True
    }

parseBool :: String -> Either String Bool
parseBool s = case s of
  "true" -> Right True
  "false" -> Right False
  _ -> Left (show s ++ " is not in {\"true\", \"false\"}.")

instance SimpleOptionType String where
  simpleOptionType = optionType_string

-- | Store an option value as a @'String'@
--
-- The value is decoded to Unicode first, if needed.
optionType_string :: OptionType String
optionType_string = optionType "text" "" Right show

instance SimpleOptionType Integer where
  simpleOptionType = optionType_integer

-- | Store an option as an @'Integer'@
--
-- The option value must be an integer.
-- There is no minimum or maximum value.
optionType_integer :: OptionType Integer
optionType_integer = optionType "integer" 0 parseInteger show

parseInteger :: String -> Either String Integer
parseInteger s = parsed
  where
    parsed =
      if valid
        then Right (read s)
        else Left (show s ++ " is not an integer.")
    valid = case s of
      [] -> False
      '-' : s' -> allDigits s'
      _ -> allDigits s
    allDigits = all (\c -> c >= '0' && c <= '9')

parseBoundedIntegral :: (Bounded a, Integral a) => String -> String -> Either String a
parseBoundedIntegral label = parse
  where
    getBounds ::
      (Bounded a, Integral a) =>
      (String -> Either String a) ->
      a ->
      a ->
      (Integer, Integer)
    getBounds _ min' max' = (toInteger min', toInteger max')

    (minInt, maxInt) = getBounds parse minBound maxBound

    parse s = case parseInteger s of
      Left err -> Left err
      Right int ->
        if int < minInt || int > maxInt
          then Left (show int ++ " is not within bounds [" ++ show minInt ++ ":" ++ show maxInt ++ "] of type " ++ label ++ ".")
          else Right (fromInteger int)

optionTypeBoundedInt :: (Bounded a, Integral a, Show a) => String -> OptionType a
optionTypeBoundedInt tName = optionType tName 0 (parseBoundedIntegral tName) show

instance SimpleOptionType Int where
  simpleOptionType = optionType_int

-- | Store an option as an @'Int'@
--
-- The option value must be an integer /n/ such that @'minBound' <= n <= 'maxBound'@.
optionType_int :: OptionType Int
optionType_int = optionTypeBoundedInt "int"

instance SimpleOptionType Int8 where
  simpleOptionType = optionType_int8

-- | Store an option as an @'Int8'@
--
-- The option value must be an integer /n/ such that @'minBound' <= n <= 'maxBound'@.
optionType_int8 :: OptionType Int8
optionType_int8 = optionTypeBoundedInt "int8"

instance SimpleOptionType Int16 where
  simpleOptionType = optionType_int16

-- | Store an option as an @'Int16'@
--
-- The option value must be an integer /n/ such that @'minBound' <= n <= 'maxBound'@.
optionType_int16 :: OptionType Int16
optionType_int16 = optionTypeBoundedInt "int16"

instance SimpleOptionType Int32 where
  simpleOptionType = optionType_int32

-- | Store an option as an @'Int32'@
--
-- The option value must be an integer /n/ such that @'minBound' <= n <= 'maxBound'@.
optionType_int32 :: OptionType Int32
optionType_int32 = optionTypeBoundedInt "int32"

instance SimpleOptionType Int64 where
  simpleOptionType = optionType_int64

-- | Store an option as an @'Int64'@
--
-- The option value must be an integer /n/ such that @'minBound' <= n <= 'maxBound'@.
optionType_int64 :: OptionType Int64
optionType_int64 = optionTypeBoundedInt "int64"

instance SimpleOptionType Word where
  simpleOptionType = optionType_word

-- | Store an option as a @'Word'@
--
-- The option value must be a positive integer /n/ such that @0 <= n <= 'maxBound'@.
optionType_word :: OptionType Word
optionType_word = optionTypeBoundedInt "uint"

instance SimpleOptionType Word8 where
  simpleOptionType = optionType_word8

-- | Store an option as a @'Word8'@
--
-- The option value must be a positive integer /n/ such that @0 <= n <= 'maxBound'@.
optionType_word8 :: OptionType Word8
optionType_word8 = optionTypeBoundedInt "uint8"

instance SimpleOptionType Word16 where
  simpleOptionType = optionType_word16

-- | Store an option as a @'Word16'@
--
-- The option value must be a positive integer /n/ such that @0 <= n <= 'maxBound'@.
optionType_word16 :: OptionType Word16
optionType_word16 = optionTypeBoundedInt "uint16"

instance SimpleOptionType Word32 where
  simpleOptionType = optionType_word32

-- | Store an option as a @'Word32'@
--
-- The option value must be a positive integer /n/ such that @0 <= n <= 'maxBound'@.
optionType_word32 :: OptionType Word32
optionType_word32 = optionTypeBoundedInt "uint32"

instance SimpleOptionType Word64 where
  simpleOptionType = optionType_word64

-- | Store an option as a @'Word64'@
--
-- The option value must be a positive integer /n/ such that @0 <= n <= 'maxBound'@.
optionType_word64 :: OptionType Word64
optionType_word64 = optionTypeBoundedInt "uint64"

instance SimpleOptionType Float where
  simpleOptionType = optionType_float

-- | Store an option as a @'Float'@
--
-- The option value must be a number.
-- Due to the imprecision of floating-point math, the stored value might not
-- exactly match the user's input.
-- If the user's input is out of range for the @'Float'@ type, it will be
-- stored as @Infinity@ or @-Infinity@.
optionType_float :: OptionType Float
optionType_float = optionType "float32" 0 parseFloat show

instance SimpleOptionType Double where
  simpleOptionType = optionType_double

-- | Store an option as a @'Double'@
--
-- The option value must be a number.
-- Due to the imprecision of floating-point math, the stored value might
-- not exactly match the user's input.
-- If the user's input is out of range for the @'Double'@ type, it will
-- be stored as @Infinity@ or @-Infinity@.
optionType_double :: OptionType Double
optionType_double = optionType "float64" 0 parseFloat show

parseFloat :: Read a => String -> Either String a
parseFloat s = case reads s of
  [(x, "")] -> Right x
  _ -> Left (show s ++ " is not a number.")

instance SimpleOptionType a => SimpleOptionType (Maybe a) where
  simpleOptionType = optionType_maybe simpleOptionType

-- | Store an option as a @'Maybe'@ of another type
--
-- The value will be @Nothing@ if the option is set to an empty string.
optionType_maybe :: OptionType a -> OptionType (Maybe a)
optionType_maybe t = maybeT {optionTypeUnary = unary}
  where
    maybeT = optionType name Nothing (parseMaybe t) (showMaybe t)
    name = "maybe<" ++ optionTypeName t ++ ">"
    unary = case optionTypeUnary t of
      Nothing -> Nothing
      Just val -> Just (Just val)

parseMaybe :: OptionType val -> String -> Either String (Maybe val)
parseMaybe t s = case s of
  "" -> Right Nothing
  _ -> case optionTypeParse t s of
    Left err -> Left err
    Right a -> Right (Just a)

showMaybe :: OptionType val -> Maybe val -> String
showMaybe _ Nothing = ""
showMaybe t (Just x) = optionTypeShow t x

-- | Store an option as a @'Set.Set'@, using another option type for the elements
--
-- The separator should be a character that will not occur within the values,
-- such as a comma or semicolon.
--
-- Duplicate elements in the input are permitted.
optionType_set ::
  Ord a =>
  -- | Element separator
  Char ->
  -- | Element type
  OptionType a ->
  OptionType (Set.Set a)
optionType_set sep t = optionType name Set.empty parseSet showSet
  where
    name = "set<" ++ optionTypeName t ++ ">"
    parseSet s = case parseList (optionTypeParse t) (split sep s) of
      Left err -> Left err
      Right xs -> Right (Set.fromList xs)
    showSet xs = intercalate [sep] (map (optionTypeShow t) (Set.toList xs))

-- | Store an option as a 'Map.Map', using other option types for the keys and values
--
-- The item separator is used to separate key/value pairs from each other.
-- It should be a character that will not occur within either the keys or values.
--
-- The value separator is used to separate the key from the value.
-- It should be a character that will not occur within the keys.
-- It may occur within the values.
--
-- Duplicate keys in the input are permitted.
-- The final value for each key is stored.
optionType_map ::
  Ord k =>
  -- | Item separator
  Char ->
  -- | Key/Value separator
  Char ->
  -- | Key type
  OptionType k ->
  -- | Value type
  OptionType v ->
  OptionType (Map.Map k v)
optionType_map itemSep keySep kt vt = optionType name Map.empty parser showMap
  where
    name = "map<" ++ optionTypeName kt ++ "," ++ optionTypeName vt ++ ">"
    parser s = parseMap keySep (optionTypeParse kt) (optionTypeParse vt) (split itemSep s)
    showMap m = intercalate [itemSep] (map showItem (Map.toList m))
    showItem (k, v) = optionTypeShow kt k ++ [keySep] ++ optionTypeShow vt v

parseList :: (String -> Either String a) -> [String] -> Either String [a]
parseList p = loop
  where
    loop [] = Right []
    loop (x : xs) = case p x of
      Left err -> Left err
      Right v -> case loop xs of
        Left err -> Left err
        Right vs -> Right (v : vs)

parseMap ::
  Ord k =>
  Char ->
  (String -> Either String k) ->
  (String -> Either String v) ->
  [String] ->
  Either String (Map.Map k v)
parseMap keySep pKey pVal = parsed
  where
    parsed strs = case parseList pItem strs of
      Left err -> Left err
      Right xs -> Right (Map.fromList xs)
    pItem s = case break (== keySep) s of
      (sKey, valAndSep) -> case valAndSep of
        [] -> Left ("Map item " ++ show s ++ " has no value.")
        _ : sVal -> case pKey sKey of
          Left err -> Left err
          Right key -> case pVal sVal of
            Left err -> Left err
            Right val -> Right (key, val)

split :: Char -> String -> [String]
split _ [] = []
split sep s0 = loop s0
  where
    loop s =
      let (chunk, rest) = break (== sep) s
          cont = chunk : loop (tail rest)
       in if null rest then [chunk] else cont

-- | Store an option as a list, using another option type for the elements
--
-- The separator should be a character that will not occur within the values,
-- such as a comma or semicolon.
optionType_list ::
  -- | Element separator
  Char ->
  -- | Element type
  OptionType a ->
  OptionType [a]
optionType_list sep t = optionType name [] parser shower
  where
    name = "list<" ++ optionTypeName t ++ ">"
    parser s = parseList (optionTypeParse t) (split sep s)
    shower xs = intercalate [sep] (map (optionTypeShow t) xs)

-- | Store an option as one of a set of possible values
--
-- This is a simplistic implementation, useful for quick scripts.
-- For more possibilities, see 'optionType'.
optionType_enum ::
  (Bounded a, Enum a, Show a) =>
  -- | Option type name
  String ->
  OptionType a
optionType_enum tName = optionType tName minBound parseEnum show
  where
    values = Map.fromList [(show x, x) | x <- enumFrom minBound]
    setString = "{" ++ intercalate ", " (map show (Map.keys values)) ++ "}"
    parseEnum s = case Map.lookup s values of
      Nothing -> Left (show s ++ " is not in " ++ setString ++ ".")
      Just x -> Right x

-- | Defines a new option in the current options type
simpleOption ::
  SimpleOptionType a =>
  String -> -- long flag
  a -> -- default value
  String -> -- description
  DefineOptions a
simpleOption flag def desc =
  defineOption
    simpleOptionType
    ( \o ->
        o
          { optionLongFlags = [flag],
            optionDefault = def,
            optionDescription = desc
          }
    )

-- | Defines a new option in the current options type
--
-- All options must have one or more /flags/.
-- Options may also have a default value, a description, and a group.
--
-- The /flags/ are how the user specifies an option on the command line.
-- Flags may be /short/ or /long/.
-- See 'optionShortFlags' and 'optionLongFlags' for details.
--
-- @
-- 'defineOption' 'optionType_word16' (\\o -> o
--    { 'optionLongFlags' = [\"port\"]
--    , 'optionDefault' = 80
--    })
-- @
defineOption :: OptionType a -> (Option a -> Option a) -> DefineOptions a
defineOption t fn = DefineOptions (optionDefault opt) getInfo parser
  where
    opt =
      fn
        ( Option
            { optionShortFlags = [],
              optionLongFlags = [],
              optionDefault = optionTypeDefault t,
              optionDescription = "",
              optionGroup = Nothing,
              optionLocation = Nothing
            }
        )

    getInfo key =
      ( key + 1,
        [ OptionInfo
            { optionInfoKey = OptionKeyGenerated key,
              optionInfoShortFlags = optionShortFlags opt,
              optionInfoLongFlags = optionLongFlags opt,
              optionInfoDefault = optionTypeShow t (optionDefault opt),
              optionInfoDescription = optionDescription opt,
              optionInfoGroup = optionGroup opt,
              optionInfoLocation = optionLocation opt,
              optionInfoTypeName = optionTypeName t,
              optionInfoUnary = isJust (optionTypeUnary t),
              optionInfoUnaryOnly = False
            }
        ]
      )

    -- parseToken :: Token -> Either String val
    parseToken tok = case tok of
      TokenUnary flagName -> case optionTypeUnary t of
        Nothing -> Left ("The flag " ++ flagName ++ " requires an argument.")
        Just val -> Right val
      Token flagName rawValue -> case optionTypeParse t rawValue of
        Left err -> Left ("Value for flag " ++ flagName ++ " is invalid: " ++ err)
        Right val -> Right val

    parser key tokens = case Map.lookup (OptionKeyGenerated key) tokens of
      Nothing -> Right (key + 1, optionDefault opt)
      Just toks -> case toks of
        -- shouldn't happen, but lets do something graceful anyway.
        [] -> Right (key + 1, optionDefault opt)
        [tok] -> case parseToken tok of
          Left err -> Left err
          Right val -> Right (key + 1, val)
        _ -> case optionTypeMerge t of
          Nothing -> Left ("Multiple values for flag: " ++ showMultipleFlagValues toks)
          Just appendFn -> case mapEither parseToken toks of
            Left err -> Left err
            Right vals -> Right (key + 1, appendFn vals)

showMultipleFlagValues :: [Token] -> String
showMultipleFlagValues = intercalate " " . map showToken
  where
    showToken (TokenUnary flagName) = flagName
    showToken (Token flagName rawValue) = show (flagName ++ "=" ++ rawValue)

data Option a = Option
  { -- | Short flags are a single character. When entered by a user,
    -- they are preceded by a dash and possibly other short flags.
    --
    -- Short flags must be a letter or a number.
    --
    -- Example: An option with @optionShortFlags = [\'p\']@ may be set using:
    --
    -- >$ ./app -p 443
    -- >$ ./app -p443
    optionShortFlags :: [Char],
    -- | Long flags are multiple characters. When entered by a user, they
    -- are preceded by two dashes.
    --
    -- Long flags may contain letters, numbers, @\'-\'@, and @\'_\'@.
    --
    -- Example: An option with @optionLongFlags = [\"port\"]@ may be set using:
    --
    -- >$ ./app --port 443
    -- >$ ./app --port=443
    optionLongFlags :: [String],
    -- | Options may have a default value. This will be parsed as if the
    -- user had entered it on the command line.
    optionDefault :: a,
    -- | An option's description is used with the default implementation
    -- of @--help@. It should be a short string describing what the option
    -- does.
    optionDescription :: String,
    -- | Which group the option is in. See the \"Option groups\" section
    -- for details.
    optionGroup :: Maybe Group,
    optionLocation :: Maybe Location
  }

validateOptionDefs :: [OptionInfo] -> [(String, [OptionInfo])] -> Either String OptionDefinitions
validateOptionDefs cmdInfos subInfos = runIdentity $ runExceptT do
  -- All subcommands have unique names.
  let subcmdNames = map fst subInfos
  if Set.size (Set.fromList subcmdNames) /= length subcmdNames
    then -- TODO: the error should mention which subcommand names are duplicated
      throwError "Multiple subcommands exist with the same name."
    else return ()

  -- Each option defines at least one short or long flag.
  let allOptInfos = cmdInfos ++ concat [infos | (_, infos) <- subInfos]
  case mapEither optValidFlags allOptInfos of
    Left err -> throwError err
    Right _ -> return ()

  -- There are no duplicate short or long flags, unless:
  -- The flags are defined in separate subcommands.
  -- The flags have identical OptionInfos (aside from keys)
  cmdDeDupedFlags <- checkNoDuplicateFlags Map.empty cmdInfos
  forM_ subInfos (\subInfo -> checkNoDuplicateFlags cmdDeDupedFlags (snd subInfo))

  return (addHelpFlags (OptionDefinitions cmdInfos subInfos))

optValidFlags :: OptionInfo -> Either String ()
optValidFlags info =
  if null (optionInfoShortFlags info) && null (optionInfoLongFlags info)
    then case optionInfoLocation info of
      Nothing -> Left ("Option with description " ++ show (optionInfoDescription info) ++ " has no flags.")
      Just loc -> Left ("Option with description " ++ show (optionInfoDescription info) ++ " at " ++ locationFilename loc ++ ":" ++ show (locationLine loc) ++ " has no flags.")
    else -- TODO: All short or long flags have a reasonable name.
      Right ()

data DeDupFlag = DeDupShort Char | DeDupLong String
  deriving (Eq, Ord, Show)

checkNoDuplicateFlags :: Map.Map DeDupFlag OptionInfo -> [OptionInfo] -> ExceptT String Identity (Map.Map DeDupFlag OptionInfo)
checkNoDuplicateFlags checked [] = return checked
checkNoDuplicateFlags checked (info : infos) = do
  let mappedShort = map DeDupShort (optionInfoShortFlags info)
  let mappedLong = map DeDupLong (optionInfoLongFlags info)
  let mappedFlags = mappedShort ++ mappedLong
  forM_ mappedFlags \mapKey -> case Map.lookup mapKey checked of
    Nothing -> return ()
    Just prevInfo ->
      if eqIgnoringKey info prevInfo
        then return ()
        else
          let flagName = case mapKey of
                DeDupShort flag -> '-' : flag : []
                DeDupLong long -> "--" ++ long
           in throwError ("Duplicate option flag " ++ show flagName ++ ".")

  let infoMap = Map.fromList [(f, info) | f <- mappedFlags]
  checkNoDuplicateFlags (Map.union checked infoMap) infos

eqIgnoringKey :: OptionInfo -> OptionInfo -> Bool
eqIgnoringKey x y = normKey x == normKey y
  where
    normKey info = info {optionInfoKey = OptionKeyIgnored}

-- | See 'parseOptions' and 'parseSubcommand'
class Parsed a where
  parsedError_ :: a -> Maybe String
  parsedHelp_ :: a -> String

-- | See 'parseOptions'
data ParsedOptions opts = ParsedOptions (Maybe opts) (Maybe String) String [String]

-- | See 'parseSubcommand'
data ParsedSubcommand action = ParsedSubcommand (Maybe action) (Maybe String) String

instance Parsed (ParsedOptions a) where
  parsedError_ (ParsedOptions _ x _ _) = x
  parsedHelp_ (ParsedOptions _ _ x _) = x

instance Parsed (ParsedSubcommand a) where
  parsedError_ (ParsedSubcommand _ x _) = x
  parsedHelp_ (ParsedSubcommand _ _ x) = x

-- | Get the options value that was parsed from argv, or @Nothing@ if the
--   arguments could not be converted into options
--
-- Note: This function return @Nothing@ if the user provided a help flag.
-- To check whether an error occurred during parsing, check the value of 'parsedError'.
parsedOptions :: ParsedOptions opts -> Maybe opts
parsedOptions (ParsedOptions x _ _ _) = x

-- | Get command-line arguments remaining after parsing options
--
-- The arguments are unchanged from the original argument list, and
-- have not been decoded or otherwise transformed.
parsedArguments :: ParsedOptions opts -> [String]
parsedArguments (ParsedOptions _ _ _ x) = x

-- | Get the subcommand action that was parsed from argv, or @Nothing@ if the
--   arguments could not be converted into a valid action
--
-- Note: This function return @Nothing@ if the user provided a help flag.
-- To check whether an error occurred during parsing, check the value of 'parsedError'.
parsedSubcommand :: ParsedSubcommand action -> Maybe action
parsedSubcommand (ParsedSubcommand x _ _) = x

-- | Get the error that prevented options from being parsed from argv,
--   or @Nothing@ if no error was detected
parsedError :: Parsed a => a -> Maybe String
parsedError = parsedError_

-- | Get a help message to show the user
--
-- If the arguments included a help flag, this will be a message
-- appropriate to that flag. Otherwise, it is a summary (equivalent to @--help@).
--
-- This is always a non-empty string, regardless of whether the parse
-- succeeded or failed. If you need to perform additional validation
-- on the options value, this message can be displayed if validation fails.
parsedHelp :: Parsed a => a -> String
parsedHelp = parsedHelp_

-- | Attempt to convert a list of command-line arguments into an options value
parseOptions :: Options opts => [String] -> ParsedOptions opts
parseOptions argv = parsed
  where
    (DefineOptions _ getInfos parser) = defineOptions
    (_, optionInfos) = getInfos 0
    parseTokens = parser 0

    parsed = case validateOptionDefs optionInfos [] of
      Left err -> ParsedOptions Nothing (Just err) "" []
      Right optionDefs -> case tokenize (addHelpFlags optionDefs) argv of
        (_, Left err) -> ParsedOptions Nothing (Just err) (helpFor HelpSummary optionDefs Nothing) []
        (_, Right tokens) -> case checkHelpFlag tokens of
          Just helpFlag -> ParsedOptions Nothing Nothing (helpFor helpFlag optionDefs Nothing) []
          Nothing -> case parseTokens (tokensMap tokens) of
            Left err -> ParsedOptions Nothing (Just err) (helpFor HelpSummary optionDefs Nothing) []
            Right (_, opts) -> ParsedOptions (Just opts) Nothing (helpFor HelpSummary optionDefs Nothing) (tokensArgv tokens)

-- | Either calls the given continuation, prints help text and calls 'exitSuccess',
--   or prints an error and calls 'exitFailure'.
--
-- See 'runSubcommand' for details on subcommand support.
runCommand :: (MonadIO m, Options opts) => (opts -> [String] -> m a) -> m a
runCommand io = do
  argv <- liftIO System.Environment.getArgs
  let parsed = parseOptions argv
  case parsedOptions parsed of
    Just opts -> io opts (parsedArguments parsed)
    Nothing -> liftIO case parsedError parsed of
      Just err -> do
        hPutStrLn stderr (parsedHelp parsed)
        hPutStrLn stderr err
        exitFailure
      Nothing -> do
        hPutStr stdout (parsedHelp parsed)
        exitSuccess

data Subcommand cmdOpts action
  = Subcommand
      String
      (Integer -> ([OptionInfo], (cmdOpts -> Tokens -> Either String action), Integer))

subcommand ::
  (Options cmdOpts, Options subcmdOpts) =>
  -- | The subcommand name
  String ->
  -- | The action to run
  (cmdOpts -> subcmdOpts -> [String] -> action) ->
  Subcommand cmdOpts action
subcommand name fn =
  Subcommand
    name
    ( \initialKey ->
        let (DefineOptions _ getInfos parser) = defineOptions
            (nextKey, optionInfos) = getInfos initialKey
            parseTokens = parser initialKey

            runAction cmdOpts tokens = case parseTokens (tokensMap tokens) of
              Left err -> Left err
              Right (_, subOpts) -> Right (fn cmdOpts subOpts (tokensArgv tokens))
         in (optionInfos, runAction, nextKey)
    )

-- | Attempt to convert a list of command-line arguments into a subcommand action
parseSubcommand :: Options cmdOpts => [Subcommand cmdOpts action] -> [String] -> ParsedSubcommand action
parseSubcommand subcommands argv = parsed
  where
    (DefineOptions _ getInfos parser) = defineOptions
    (cmdNextKey, cmdInfos) = getInfos 0
    cmdParseTokens = parser 0

    subcmdInfos = do
      Subcommand name fn <- subcommands
      let (infos, _, _) = fn cmdNextKey
      return (name, infos)

    subcmdRunners = Map.fromList do
      Subcommand name fn <- subcommands
      let (_, runner, _) = fn cmdNextKey
      return (name, runner)

    parsed = case validateOptionDefs cmdInfos subcmdInfos of
      Left err -> ParsedSubcommand Nothing (Just err) ""
      Right optionDefs -> case tokenize (addHelpFlags optionDefs) argv of
        (subcmd, Left err) -> ParsedSubcommand Nothing (Just err) (helpFor HelpSummary optionDefs subcmd)
        (subcmd, Right tokens) -> case checkHelpFlag tokens of
          Just helpFlag -> ParsedSubcommand Nothing Nothing (helpFor helpFlag optionDefs subcmd)
          Nothing -> case findAction tokens subcmd of
            Left err -> ParsedSubcommand Nothing (Just err) (helpFor HelpSummary optionDefs subcmd)
            Right action -> ParsedSubcommand (Just action) Nothing (helpFor HelpSummary optionDefs subcmd)

    findAction _ Nothing = Left "No subcommand specified"
    findAction tokens (Just subcmdName) = case cmdParseTokens (tokensMap tokens) of
      Left err -> Left err
      Right (_, cmdOpts) -> case Map.lookup subcmdName subcmdRunners of
        Nothing -> Left ("Unknown subcommand " ++ show subcmdName ++ ".")
        Just getRunner -> case getRunner cmdOpts tokens of
          Left err -> Left err
          Right action -> Right action

-- | Used to run applications that are split into subcommands
runSubcommand :: (Options opts, MonadIO m) => [Subcommand opts (m a)] -> m a
runSubcommand subcommands = do
  argv <- liftIO System.Environment.getArgs
  let parsed = parseSubcommand subcommands argv
  case parsedSubcommand parsed of
    Just cmd -> cmd
    Nothing -> liftIO $ case parsedError parsed of
      Just err -> do
        hPutStrLn stderr (parsedHelp parsed)
        hPutStrLn stderr err
        exitFailure
      Nothing -> do
        hPutStr stdout (parsedHelp parsed)
        exitSuccess
