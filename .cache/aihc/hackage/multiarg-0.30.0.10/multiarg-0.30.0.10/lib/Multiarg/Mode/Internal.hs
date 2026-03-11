-- | Internal functions used by "Multiarg.Mode".  You don't have to
-- worry about \"breaking\" anything by using this module; it is
-- separate from "Multiarg.Mode" primarily to tidy up the
-- documentation in that module.  The functions in "Multiarg.Mode"
-- should satisfy most use cases.  However, if you want more control
-- over error handling, you can use this module.
module Multiarg.Mode.Internal where

import Data.Either (partitionEithers)
import Multiarg.Maddash
import Multiarg.Internal
import Multiarg.Util
import Multiarg.Types
-- GHC 7.10 exports Word from the Prelude
import Prelude hiding (Word)

newtype ModeName = ModeName String
  deriving (Eq, Ord, Show)

data ParsedMode a
  = ModeGood a
  | ModeError [OptionError] (Either OptionError OptName)
  -- ^ There was an error.  There may be zero or more initial
  -- OptionError.  There must be at least one error, which is either
  -- an OptionError or the name of an option, if the error is that
  -- there were not enough words following the option to provide it
  -- with its necessary arguments.
  deriving (Eq, Ord, Show)

instance Functor ParsedMode where
  fmap f (ModeGood a) = ModeGood (f a)
  fmap _ (ModeError ls ei) = ModeError ls ei

-- | A 'Mode' represents a single command line mode, such as @check@
-- for @ghc-pkg check@.  It contains the name of the mode, as well as
-- a parser that handles all /options/ and /positional arguments/ for
-- the mode.  Ordinarily you will create a 'Mode' using the 'mode'
-- function rather than by using the constructor directly.
data Mode r = Mode ModeName ([Word] -> ParsedMode r)

instance Functor Mode where
  fmap f (Mode s p) = Mode s (fmap (fmap f) p)

parsedCommandLineToParsedMode
  :: ([a] -> r)
  -> ParsedCommandLine a
  -> ParsedMode r
parsedCommandLineToParsedMode fMode (ParsedCommandLine ls mayOpt)
  = case mayOpt of
      Nothing -> case mayLast errs of
        Nothing -> ModeGood (fMode goods)
        Just (errs1st, errsLst) -> ModeError errs1st (Left errsLst)
      Just opt -> ModeError errs (Right opt)
  where
    (errs, goods) = partitionEithers ls

-- | Creates a new 'Mode'.
mode
  :: String
  -- ^ Mode name.  For instance, for the @check@ mode of @ghc-pkg@,
  -- this would be @check@.
  -> [OptSpec a]
  -- ^ Mode /options/
  -> (String -> a)
  -- ^ Parses /positional arguments/
  -> ([a] -> r)
  -- ^ Processes the result of all mode /options/
  -> Mode r
mode name opts fPos fMode
  = Mode (ModeName name)
  $ parsedCommandLineToParsedMode fMode
  . parseCommandLinePure opts fPos
  . map (\(Word s) -> s)

data GlobalLocalEnd a
  = GlobalInsufficientOptArgs OptName
  | ModeNotFound String [String]
  | NoMode
  | ModeFound (ParsedMode a)
  deriving (Eq, Ord, Show)

data GlobalLocal g r
  = GlobalLocal [Either OptionError g] (GlobalLocalEnd r)
  deriving (Eq, Ord, Show)

-- | The result of parsing a mode command line.
data ModeResult g r
  = ModeResult [g] (Either [String] r)
  -- ^ @ModeResult a b@ is a successfully parsed mode command line,
  -- where:
  --
  -- @a@ is a list of all global options parsed; and
  --
  -- @b@ indicates the result of parsing the mode.  It is @Either c
  -- d@, where @Left c@ indicates that no mode was parsed.  This
  -- arises under two circumstances.  If the user did not include any
  -- /words/ after the global /options/, then @c@ will be the empty
  -- list, @[]@.  If the user did include /words/ after the global
  -- options, but the first /word/ was not recognized as a mode, then
  -- this list will contain the first /word/ and any subsequent /words/.
  -- Therefore, note that if the user attempted to use a mode that
  -- does not exist (e.g. she misspelled it), this is not treated as
  -- an error.  It's up to the client code to deal with this issue
  -- (for instance, your program might not view this situation as
  -- being an error.)
  --
  -- If @b@ is @Right d@, this indicates that the user entered a
  -- recognized mode, and the result is @d@.

  deriving (Eq, Ord, Show)

getModeResult
  :: GlobalLocal g r
  -> Either (String, [String]) (ModeResult g r)
getModeResult (GlobalLocal eis end)
  = combine global (endToModeResult end)
  where
    (glblErrs, glblGoods) = partitionEithers eis
    global = case glblErrs of
      [] -> Right glblGoods
      x:xs -> Left (x, xs)

combine
  :: Either (OptionError, [OptionError]) [g]
  -- ^ Global result.  Contains either one or more errors, or global
  -- /option/ results.

  -> Either (String, [String]) (Either [String] r)
  -- ^ Result of parsing mode /word/, and the mode /options/ and
  -- /positional arguments/.  May be @Left a@, where @a@ is one or
  -- more errors, or @Right b@, where @b@ is a good result.  A good
  -- result @b@ may be either @Left c@, where @c@ is a list of
  -- /positional arguments/, or @Right d@, where @d@ is the mode
  -- result.  @c@ indicates that no mode was recognized and may be
  -- either @[]@, which indicates that the user passed no /words/ at
  -- all after the global /options/, or @x:xs@, indicating that the
  -- user did pass /words/ after the global /options/, but the first
  -- /word/ was not recognized as a mode.

  -> Either (String, [String]) (ModeResult g r)
combine (Left (oe1, oes)) (Left (me1, mes)) =
  Left ( globalOptErrorToString oe1
       , map globalOptErrorToString oes ++ (me1 : mes) )
combine (Left (oe1, oes)) (Right _) =
  Left (globalOptErrorToString oe1, map globalOptErrorToString oes)
combine (Right _) (Left (me1, mes)) = Left (me1, mes)
combine (Right glbls) (Right r) =
  Right (ModeResult glbls r)

endToModeResult
  :: GlobalLocalEnd a
  -> Either (String, [String]) (Either [String] a)
endToModeResult end = case end of
  GlobalInsufficientOptArgs on -> Left
    (labeledInsufficientOptArgs "global" on, [])
  ModeNotFound s ss -> Right (Left $ s:ss)
  NoMode -> Right (Left [])
  ModeFound pm -> extractParsedMode pm

extractParsedMode
  :: ParsedMode a
  -> Either (String, [String]) (Either b a)
extractParsedMode (ModeGood g) = Right . Right $ g
extractParsedMode (ModeError es lst) = Left $ case es of
  [] -> (eiToError lst, [])
  (x:xs) ->
    ( modeOptErrorToString x
    , (map modeOptErrorToString xs) ++ [eiToError lst] )

globalOptErrorToString :: OptionError -> String
globalOptErrorToString = optErrorToString "global"

modeOptErrorToString :: OptionError -> String
modeOptErrorToString = optErrorToString "mode"

optErrorToString :: String -> OptionError -> String
optErrorToString lbl oe = case oe of
  BadOption opt ->
    "unrecognized " ++ lbl ++ "  option: " ++ optNameToString opt
  LongArgumentForZeroArgumentOption lng arg ->
    "argument given for " ++ lbl ++ " option that takes no arguments. "
    ++ "option: --" ++ longNameToString lng
    ++ " argument: " ++ optArgToString arg


eiToError :: Either OptionError OptName -> String
eiToError ei = case ei of
  Left oe -> modeOptErrorToString oe
  Right on -> labeledInsufficientOptArgs "mode" on


labeledInsufficientOptArgs :: String -> OptName -> String
labeledInsufficientOptArgs lbl on = "insufficient option arguments "
  ++ "given for " ++ lbl ++ " option: " ++ optNameToString on


-- | Parses a command line that may contain modes.
parseModeLine
  :: [OptSpec g]
  -- ^ Global /options/.  This might, for example, include a @--help@
  -- /option/.
  -> [Mode r]
  -- ^ All modes
  -> [String]
  -- ^ All command line /words/
  -> Either (String, [String]) (ModeResult g r)
  -- ^ Returns @Either a b@.  @Left a@ represents an error.  Each
  -- String represents a single error (this is returned as a pair
  -- because there must be at least one error; a simple list would not
  -- reflect this requirement.)
  --
  -- @Right b@ indicates that parsing proceeded successfully; consult
  -- 'ModeResult' to see what is returned.
parseModeLine glbl mds =
  getModeResult
  . parseModeLineWithErrors glbl mds

parseModeLineWithErrors
  :: [OptSpec g]
  -- ^ Global options
  -> [Mode r]
  -- ^ All modes
  -> [String]
  -- ^ All command line tokens
  -> GlobalLocal g r
parseModeLineWithErrors glbl mds tokStrings = GlobalLocal lsErrsGoods end
  where
    toks = map Word tokStrings
    (shorts, longs) = splitOptSpecs glbl
    (outs, eiOptTok) = processWords shorts longs toks
    lsErrsGoods = map f . concat $ outs
      where
        f (Good a) = Right a
        f (OptionError e) = Left e
    end = case eiOptTok of
      Left (opt, _) -> GlobalInsufficientOptArgs opt
      Right [] -> NoMode
      Right (x:xs) -> case findExactMode x mds of
        Nothing -> ModeNotFound (unWord x) (map unWord xs)
          where
            unWord (Word t) = t
        Just (Mode _ f) -> ModeFound (f xs)

-- | Takes a token and a list of all modes; returns the matching mode
-- if there is one, or Nothing if there is no match.
findExactMode
  :: Word
  -> [Mode a]
  -> Maybe (Mode a)
findExactMode _ [] = Nothing
findExactMode tok@(Word s) (m@(Mode (ModeName n) _) : ms)
  | s == n = Just m
  | otherwise = findExactMode tok ms

