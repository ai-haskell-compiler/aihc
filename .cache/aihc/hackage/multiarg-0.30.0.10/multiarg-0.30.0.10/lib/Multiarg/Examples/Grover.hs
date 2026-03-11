-- | Grover is a simple example program that shows how to write a
-- parser for commands with multiple modes.  You build such parsers
-- using "Multiarg.Mode".  It provides an example for the
-- documentation, and it also provides fodder for the QuickCheck
-- tests.  You will want to look at the source code.
--
-- Grover has three modes: @int@, @string@, and @maybe@.  Each of
-- these modes has three options: @-z@ or @--zero@, which takes no
-- arguments; @-s@ or @--single@, which takes one argument; @-d@ or
-- @--double@, which takes two arguments; and @-t@ or @--triple@,
-- which takes three arguments.  The type of the argument depends on
-- the mode.  For @int@, the argument or arguments must be an integer;
-- for @string@ the arguments can be any string; and for @maybe@ the
-- arguments must be a Maybe Int, such as @Nothing@ or @Just 5@.
--
-- Each mode also accepts any number of positional arguments, which
-- can be any string.
--
-- Grover handles simple errors right inside the parser by using the
-- @Either@ type as a return value.

module Multiarg.Examples.Grover where

import Control.Applicative
import Multiarg.Mode
import Text.Read (readMaybe)

-- | Grover's global options.
data Global
  = Help
  | Verbose Int
  -- ^ The Int would indicate, for example, the desired level of
  -- verbosity.
  | Version
  deriving (Eq, Ord, Show)

-- | Handles all options and positional arguments for any Grover mode.
data GroverOpt a
  = Zero
  | Single a
  | Double a a
  | Triple a a a
  | PosArg String
  deriving (Eq, Ord, Show)

instance Functor GroverOpt where
  fmap f g = case g of
    Zero -> Zero
    Single a -> Single (f a)
    Double a b -> Double (f a) (f b)
    Triple a b c -> Triple (f a) (f b) (f c)
    PosArg a -> PosArg a

-- | All of Grover's global options.  The 'OptSpec' is parameterized
-- on an 'Either' to allow for error handling.  If the user enters a
-- non-integer argument for the @--verbose@ option, a @Left@ with an
-- error message is returned.
globalOptSpecs :: [OptSpec (Either String Global)]
globalOptSpecs =
  [ optSpec "h" ["help"] . ZeroArg . return $ Help
  , optSpec "v" ["verbose"] . OneArg $ \s ->
    Verbose <$> readErr s
  , optSpec "" ["version"] . ZeroArg . return $ Version
  ]

-- | A list of 'OptSpec' that works for any 'Mode'.
modeOptSpecs :: Read a => [OptSpec (Either String (GroverOpt a))]
modeOptSpecs =
  [ optSpec "z" ["zero"] . ZeroArg . Right $ Zero
  , optSpec "s" ["single"] . OneArg $ \s -> Single <$> readErr s

  , optSpec "d" ["double"] . TwoArg $ \s1 s2 ->
      Double <$> readErr s1 <*> readErr s2

  , optSpec "t" ["triple"] . ThreeArg $ \s1 s2 s3 ->
      Triple <$> readErr s1 <*> readErr s2 <*> readErr s3
  ]

-- | Holds the results of parsing Grover's modes.
data Result
  = Ints [Either String (GroverOpt Int)]
  | Strings [Either String (GroverOpt String)]
  | Maybes [Either String (GroverOpt (Maybe Int))]
  deriving (Eq, Ord, Show)

-- | All Grover modes.
modes :: [Mode Result]
modes =
  [ mode "int" modeOptSpecs (return . PosArg) Ints
  , mode "string" modeOptSpecs (return . PosArg) Strings
  , mode "maybe" modeOptSpecs (return . PosArg) Maybes
  ]

-- | Reads a value.  If it cannot be read, returns an error message.
readErr :: Read a => String -> Either String a
readErr s = case readMaybe s of
  Nothing -> Left $ "could not read value: " ++ s
  Just a -> Right a

-- | Parses all of Grover's options and modes.
parseGrover
  :: [String]
  -- ^ Command line arguments, presumably from 'getArgs'
  -> Either (String, [String])
            (ModeResult (Either String Global) Result)
  -- ^ Returns a 'Left' if there are errors, or a 'Right' if there are
  -- no errors.  (In an actual application, further processing of a
  -- 'Right' would be necessary to determine whether all entered
  -- arguments were valid.)
parseGrover = parseModeLine globalOptSpecs modes
