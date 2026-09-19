-- | The GNU @getopt@ command-line option parser.
--
-- Declare each option with 'Option'. Then give the declarations and the
-- command-line arguments to 'getOpt'. 'usageInfo' makes a help text from the
-- same declarations.
module System.Console.GetOpt
  ( getOpt,
    getOpt',
    usageInfo,
    ArgOrder (..),
    OptDescr (..),
    ArgDescr (..),
  )
where

import Data.List (isPrefixOf, zipWith3)
import Prelude

-- | One command-line option.
--
-- The fields are the short option characters, the long option names, the
-- argument that the option takes, and the help text of the option.
data OptDescr a = Option [Char] [String] (ArgDescr a) String

-- | The argument that an option takes.
data ArgDescr a
  = -- | The option takes no argument and gives this value.
    NoArg a
  | -- | The option must have an argument. The second field names the
    -- argument in the help text.
    ReqArg (String -> a) String
  | -- | The option can have an argument. The second field names the
    -- argument in the help text.
    OptArg (Maybe String -> a) String

-- | What 'getOpt' does with an argument that is not an option.
data ArgOrder a
  = -- | Stop at the first argument that is not an option.
    RequireOrder
  | -- | Move all options in front of the other arguments.
    Permute
  | -- | Keep the order and make a value from each other argument.
    ReturnInOrder (String -> a)

instance Functor OptDescr where
  fmap function (Option shorts longs argument text) = Option shorts longs (fmap function argument) text

instance Functor ArgDescr where
  fmap function (NoArg value) = NoArg (function value)
  fmap function (ReqArg make placeholder) = ReqArg (function . make) placeholder
  fmap function (OptArg make placeholder) = OptArg (function . make) placeholder

instance Functor ArgOrder where
  fmap _ RequireOrder = RequireOrder
  fmap _ Permute = Permute
  fmap function (ReturnInOrder make) = ReturnInOrder (function . make)

-- | What one command-line argument gave.
data OptKind a
  = -- | The value of a known option.
    Opt a
  | -- | An option that no declaration knows.
    UnreqOpt String
  | -- | An argument that is not an option.
    NonOpt String
  | -- | The @--@ argument, which ends the options.
    EndOfOpts
  | -- | An error message.
    OptErr String

-- | Parse the command line.
--
-- The result holds the option values, the arguments that are not options, and
-- the error messages.
getOpt :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String])
getOpt ordering descriptions arguments =
  joinUnrecognized (getOpt' ordering descriptions arguments)

joinUnrecognized :: ([a], [String], [String], [String]) -> ([a], [String], [String])
joinUnrecognized (values, others, unrecognized, errors) =
  (values, others, errors ++ map unrecognizedOption unrecognized)

-- | Parse the command line and keep the unknown options.
--
-- The result holds the option values, the arguments that are not options, the
-- unknown options, and the error messages.
getOpt' :: ArgOrder a -> [OptDescr a] -> [String] -> ([a], [String], [String], [String])
getOpt' _ _ [] = ([], [], [], [])
getOpt' ordering descriptions (argument : arguments) =
  continue (nextOption descriptions argument arguments)
  where
    continue (kind, rest) =
      case kind of
        Opt value -> addValue value (getOpt' ordering descriptions rest)
        UnreqOpt text -> addUnrecognized text (getOpt' ordering descriptions rest)
        OptErr message -> addError message (getOpt' ordering descriptions rest)
        EndOfOpts -> ([], rest, [], [])
        NonOpt text ->
          case ordering of
            RequireOrder -> ([], text : rest, [], [])
            Permute -> addOther text (getOpt' ordering descriptions rest)
            ReturnInOrder make -> addValue (make text) (getOpt' ordering descriptions rest)

addValue :: a -> ([a], [String], [String], [String]) -> ([a], [String], [String], [String])
addValue value (values, others, unrecognized, errors) = (value : values, others, unrecognized, errors)

addOther :: String -> ([a], [String], [String], [String]) -> ([a], [String], [String], [String])
addOther other (values, others, unrecognized, errors) = (values, other : others, unrecognized, errors)

addUnrecognized :: String -> ([a], [String], [String], [String]) -> ([a], [String], [String], [String])
addUnrecognized option (values, others, unrecognized, errors) = (values, others, option : unrecognized, errors)

addError :: String -> ([a], [String], [String], [String]) -> ([a], [String], [String], [String])
addError message (values, others, unrecognized, errors) = (values, others, unrecognized, message : errors)

-- | Read the first command-line argument, and give back the arguments that
-- the parser did not use.
nextOption :: [OptDescr a] -> String -> [String] -> (OptKind a, [String])
nextOption descriptions argument rest =
  case argument of
    ['-', '-'] -> (EndOfOpts, rest)
    '-' : '-' : text -> longOption descriptions text rest
    '-' : letter : more -> shortOption descriptions letter more rest
    _ -> (NonOpt argument, rest)

-- | Read one long option, such as @--width=10@.
longOption :: [OptDescr a] -> String -> [String] -> (OptKind a, [String])
longOption descriptions text rest =
  case arguments of
    _ : _ : _ -> (ambiguousOption options optionName, rest)
    [NoArg value] ->
      case remainder of
        [] -> (Opt value, rest)
        _ -> (unexpectedArgument optionName, rest)
    [ReqArg make placeholder] ->
      case remainder of
        '=' : value -> (Opt (make value), rest)
        _ ->
          case rest of
            [] -> (missingArgument placeholder optionName, [])
            value : remaining -> (Opt (make value), remaining)
    [OptArg make _] ->
      case remainder of
        '=' : value -> (Opt (make (Just value)), rest)
        _ -> (Opt (make Nothing), rest)
    _ -> (UnreqOpt ("--" ++ text), rest)
  where
    name = takeWhile (/= '=') text
    remainder = dropWhile (/= '=') text
    optionName = "--" ++ name
    exact = filter (hasLongName (name ==)) descriptions
    options = if null exact then filter (hasLongName (isPrefixOf name)) descriptions else exact
    arguments = map optionArgument options

-- | Read one short option, such as @-w10@.
shortOption :: [OptDescr a] -> Char -> String -> [String] -> (OptKind a, [String])
shortOption descriptions letter more rest =
  case arguments of
    _ : _ : _ -> (ambiguousOption options optionName, rest)
    NoArg value : _ ->
      case more of
        [] -> (Opt value, rest)
        _ -> (Opt value, ('-' : more) : rest)
    ReqArg make placeholder : _ ->
      case more of
        [] ->
          case rest of
            [] -> (missingArgument placeholder optionName, [])
            value : remaining -> (Opt (make value), remaining)
        _ -> (Opt (make more), rest)
    OptArg make _ : _ ->
      case more of
        [] -> (Opt (make Nothing), rest)
        _ -> (Opt (make (Just more)), rest)
    [] -> (UnreqOpt (optionName ++ more), rest)
  where
    optionName = '-' : [letter]
    options = filter (hasShortName letter) descriptions
    arguments = map optionArgument options

hasLongName :: (String -> Bool) -> OptDescr a -> Bool
hasLongName matches (Option _ longs _ _) = any matches longs

hasShortName :: Char -> OptDescr a -> Bool
hasShortName letter (Option shorts _ _ _) = letter `elem` shorts

optionArgument :: OptDescr a -> ArgDescr a
optionArgument (Option _ _ argument _) = argument

ambiguousOption :: [OptDescr a] -> String -> OptKind a
ambiguousOption options optionName =
  OptErr (usageInfo ("option `" ++ optionName ++ "' is ambiguous; could be one of:") options)

missingArgument :: String -> String -> OptKind a
missingArgument placeholder optionName =
  OptErr ("option `" ++ optionName ++ "' requires an argument " ++ placeholder ++ "\n")

unexpectedArgument :: String -> OptKind a
unexpectedArgument optionName =
  OptErr ("option `" ++ optionName ++ "' doesn't allow an argument\n")

unrecognizedOption :: String -> String
unrecognizedOption optionName = "unrecognized option `" ++ optionName ++ "'\n"

-- | One line of the help text.
data UsageRow = UsageRow
  { rowShort :: String,
    rowLong :: String,
    rowText :: String
  }

-- | Make a help text from a header and the option declarations.
usageInfo :: String -> [OptDescr a] -> String
usageInfo header descriptions = unlines (header : table)
  where
    rows = concatMap describeOption descriptions
    table = zipWith3 paste (padded (map rowShort rows)) (padded (map rowLong rows)) (map rowText rows)
    paste shorts longs text = "  " ++ shorts ++ "  " ++ longs ++ "  " ++ text

-- | Make each string as long as the longest string.
padded :: [String] -> [String]
padded texts = map (flushLeft (maximumLength texts)) texts

flushLeft :: Int -> String -> String
flushLeft width text = text ++ replicate (width - length text) ' '

maximumLength :: [String] -> Int
maximumLength = foldr (max . length) 0

describeOption :: OptDescr a -> [UsageRow]
describeOption (Option shorts longs argument text) =
  case lines text of
    [] -> [UsageRow shortText longText ""]
    first : more -> UsageRow shortText longText first : map (UsageRow "" "") more
  where
    shortText = commaSeparated (map (formatShort argument) shorts)
    longText = commaSeparated (map (formatLong argument) longs)

commaSeparated :: [String] -> String
commaSeparated [] = ""
commaSeparated [text] = text
commaSeparated (text : texts) = text ++ ", " ++ commaSeparated texts

formatShort :: ArgDescr a -> Char -> String
formatShort (NoArg _) letter = '-' : [letter]
formatShort (ReqArg _ placeholder) letter = '-' : letter : ' ' : placeholder
formatShort (OptArg _ placeholder) letter = '-' : letter : '[' : placeholder ++ "]"

formatLong :: ArgDescr a -> String -> String
formatLong (NoArg _) name = "--" ++ name
formatLong (ReqArg _ placeholder) name = "--" ++ name ++ "=" ++ placeholder
formatLong (OptArg _ placeholder) name = "--" ++ name ++ "[=" ++ placeholder ++ "]"
