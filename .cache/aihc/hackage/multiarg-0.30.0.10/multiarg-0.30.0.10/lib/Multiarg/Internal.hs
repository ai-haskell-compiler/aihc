-- | Functions and types used by the "Multiarg" module.  You don't
-- have to worry about \"breaking\" anything by using this module.
-- This module is separate from "Multiarg" only because it makes the
-- documentation in that module cleaner, as that module should satisfy
-- most use cases.  Use this module if you want more control over
-- error handling, or if you want to process arguments using pure
-- functions rather than IO functions.
module Multiarg.Internal where

import Multiarg.Maddash
import Multiarg.Limeline
import Multiarg.Types
import Multiarg.Util
import Data.Either (partitionEithers)
import System.Environment
import System.Exit
import qualified System.IO as IO

limelineOutputToParsedCommandLine
  :: ([Either [Output a] (PosArg a)], Maybe OptName)
  -> ParsedCommandLine a
limelineOutputToParsedCommandLine (ls, mayOpt) =
  ParsedCommandLine (concatMap f ls) mayOpt
  where
    f ei = case ei of
      Left os -> map g os
        where
          g o = case o of
            Good a -> Right a
            OptionError oe -> Left oe
      Right (PosArg pa) -> [Right pa]


-- | Indicates the result of parsing a command line.
data ParsedCommandLine a
  = ParsedCommandLine [Either OptionError a] (Maybe OptName)
  -- ^ @ParsedCommandLine a b@, where:
  --
  -- @a@ is a list of errors and results, in the original order in
  -- which they appeared on the command line.
  --
  -- @b@ is @Just p@ if the user included an /option/ at the end of
  -- the command line and there were not enough following /words/ to
  -- provide the /option/ with its necessary /option arguments/, where
  -- @p@ is the /name/ of the /option/ with insufficient
  -- /option arguments/;
  -- otherwise 'Nothing'.
  deriving (Eq, Ord, Show)

instance Functor ParsedCommandLine where
  fmap f (ParsedCommandLine ls m) = ParsedCommandLine
    (map (fmap f) ls) m

-- | Gets the results from a parsed command line.  If there were
-- errors, returns a 'Left' with an error message; otherwise, returns
-- a 'Right' with a list of the results.
parsedResults
  :: ParsedCommandLine a
  -> Either (String, [String]) [a]
parsedResults (ParsedCommandLine ls mayOpt) =
  let (ers, gds) = partitionEithers ls
  in case (ers, mayOpt) of
      ([], Nothing) -> Right gds
      ([], Just opt) -> Left (insufficientOptArgs opt, [])
      (x:xs, Just opt) -> Left
        (optError x, map optError xs ++ [insufficientOptArgs opt])
      (x:xs, Nothing) -> Left
        (optError x, map optError xs)

insufficientOptArgs :: OptName -> String
insufficientOptArgs n = "not enough arguments given for option: "
  ++ optNameToString n

optError :: OptionError -> String
optError oe = case oe of
  BadOption opt ->
    "unrecognized option: " ++ optNameToString opt
  LongArgumentForZeroArgumentOption lng arg ->
    "argument given for option that takes no arguments. "
    ++ "option: --" ++ longNameToString lng
    ++ " argument: " ++ optArgToString arg


-- | Parses a command line; a pure function (unlike
-- 'parseCommandLineIO').
parseCommandLinePure

  :: [OptSpec a]
  -- ^ All program options

  -> (String -> a)
  -- ^ Processes non-option positional arguments

  -> [String]
  -- ^ Input tokens from the command line, probably obtained from
  -- 'getArgs'

  -> ParsedCommandLine a

parseCommandLinePure os fPos inp =
  limelineOutputToParsedCommandLine limeOut
  where
    limeOut = interspersed shrts lngs fPos (map Word inp)
    (shrts, lngs) = splitOptSpecs os

-- | Parses a command line.  Runs in the IO monad so that it can do
-- some tedious things for you:
--
-- * fetches the /words/ on command line using 'getArgs' and the name
-- of the program with 'getProgName'
--
-- * prints help, if the user requested help, and exits
-- successfully
--
-- * prints an error message and exits unsuccessfully, if the user
-- entered a bad command line (such as an unknown option)
--
-- If you don't want this degree of automation or if you want a pure
-- function, see the 'parseCommandLinePure' function in the
-- "Multiarg.Internal" module.
parseCommandLine
  :: (String -> String)
  -- ^ Returns help for your command.  This function is applied to the
  -- name of the program being run, which is obtained from
  -- 'getProgName'.  The function should return a string that gives
  -- help for how to use your command; this string is printed as-is.

  -> [OptSpec a]
  -- ^ All program /options/.  An /option/ for @-h@ and for @--help@ is
  -- added for you, using the help function given above.  If the user
  -- asks for help, then it is printed and the program exits
  -- successfully.  If the user gives a command line with one or more
  -- errors in it, an error message is printed, along with something
  -- like @Enter program-name --help for help@.

  -> (String -> a)
  -- ^ Processes /positional arguments/.

  -> IO [a]
  -- ^ Fetches the /words/ from the command line arguments using
  -- 'getArgs' and parses them.  If there is an error, prints an error
  -- message and exits unsuccessfully.  Otherwise, returns the parsed
  -- result, where each item in the list corresponds to a parsed
  -- /option/ or /positional argument/ in the order in which it
  -- appeared on the command line.
parseCommandLine fHelp os fPos = do
  progName <- getProgName
  args <- getArgs
  case parsedResults $ parseCommandLineHelp os fPos args of
    Left (e1, es) -> do
      IO.hPutStrLn IO.stderr $ progName ++ ": error"
      _ <- mapM (IO.hPutStrLn IO.stderr) $ e1 : es
      IO.hPutStrLn IO.stderr $ "enter \"" ++ progName ++ " --help\" "
        ++ "for help."
      exitFailure
    Right mayResults -> case sequence mayResults of
      Nothing -> do
        putStr (fHelp progName)
        exitSuccess
      Just ls -> return ls


-- | Automatically adds a /short option/, @-h@, and a /long option/,
-- @--help@.  Intended primarily for use by the 'parseCommandLineIO'
-- function.
parseCommandLineHelp
  :: [OptSpec a]
  -> (String -> a)
  -> [String]
  -> ParsedCommandLine (Maybe a)
parseCommandLineHelp os fPos inp =
  limelineOutputToParsedCommandLine
  $ interspersed shrts lngs (fmap Just fPos) (map Word inp)
  where
    (shrts, lngs) = addHelpOption os

