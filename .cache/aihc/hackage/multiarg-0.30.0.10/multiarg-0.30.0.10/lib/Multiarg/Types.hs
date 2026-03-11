-- | Types used throughout Multiarg, and associated functions.
-- Ordinarily you should not need this module; "Multiarg" and
-- "Multiarg.Mode" export all the types and constructors you should
-- ordinarily need.  However, if you want more control than those
-- modules afford, you can import this one.
module Multiarg.Types
  ( ArgSpec(..)
  , OptSpec(..)
  , optSpec
  , ShortName
  , shortNameToChar
  , shortName
  , LongName
  , longNameToString
  , longName
  , Word(..)
  , OptName(..)
  , optNameToString
  , OptArg(..)
  , ShortTail(..)
  , isLong
  , isShort
  , wordToOptArg
  , splitShortTail
  ) where

-- GHC 7.10 incorporates 'Data.Word' into the Prelude, which clashes
-- with a binding below.
import Prelude hiding (Word)

-- | Specifies how many /option arguments/ an /option/ takes.
data ArgSpec a
  = ZeroArg a
  -- ^ This /option/ takes no /option arguments/
  | OneArg (String -> a)
  -- ^ This /option/ takes one /option argument/
  | TwoArg (String -> String -> a)
  -- ^ This /option/ takes two /option arguments/
  | ThreeArg (String -> String -> String -> a)
  -- ^ This /option/ takes three /option arguments/

instance Functor ArgSpec where
  fmap f (ZeroArg a) = ZeroArg (f a)
  fmap f (OneArg g) = OneArg $ \a -> f (g a)
  fmap f (TwoArg g) = TwoArg $ \a b -> f (g a b)
  fmap f (ThreeArg g) = ThreeArg $ \a b c -> f (g a b c)

instance Show (ArgSpec a) where
  show (ZeroArg _) = "ZeroArg"
  show (OneArg _) = "OneArg"
  show (TwoArg _) = "TwoArg"
  show (ThreeArg _) = "ThreeArg"

-- | Specifies an /option/.  Typically you will use 'optSpec' to
-- create an 'OptSpec' rather than using the constructor directly.
-- Each 'OptSpec' may contain mulitple /short option names/ and
-- /long option names/; but each 'OptSpec' contains only one 'ArgSpec'.
-- Therefore, all /short option names/ and /long option names/
-- specified in a single 'OptSpec' are synonymous.
data OptSpec a = OptSpec [ShortName] [LongName] (ArgSpec a)
  deriving Show

instance Functor OptSpec where
  fmap f (OptSpec s l p) = OptSpec s l (fmap f p)

-- | Creates an 'OptSpec'.
optSpec
  :: [Char]
  -- ^ There is one character for each desired /short option name/.
  -- Each of these characters may not be a hyphen; otherwise,
  -- 'optSpec' will apply 'error'.

  -> [String]
  -- ^ There is one string for each desired /long option name/.  Each
  -- string:
  --
  -- * cannot be empty;
  --
  -- * must not begin with a hyphen; and
  --
  -- * must not contain an equal sign.
  --
  -- Otherwise, 'optSpec' will apply 'error'.

  -> ArgSpec a
  -- ^ How many /option arguments/ this /option/ takes.  This also
  -- specifies what is returned when the /option/ is parsed on the
  -- command line.

  -> OptSpec a
optSpec ss ls = OptSpec (map mkShort ss) (map mkLong ls)
  where
    mkShort s = case shortName s of
      Nothing -> error $ "invalid short option name: " ++ [s]
      Just n -> n
    mkLong s = case longName s of
      Nothing -> error $ "invalid long option name: " ++ s
      Just n -> n


-- | A /short option name/.
newtype ShortName = ShortName { shortNameToChar ::  Char }
  deriving (Eq, Ord, Show)

-- | A /long option name/.
newtype LongName = LongName { longNameToString :: String }
  deriving (Eq, Ord, Show)

-- | Creates a /short option name/.  Any character other than a single
-- hyphen will succeed.
shortName :: Char -> Maybe ShortName
shortName '-' = Nothing
shortName x = Just $ ShortName x

-- | Creates a /long option name/.  The string may not be empty, and the
-- first character may not be a hyphen.  In addition, no character may
-- be an equal sign.
longName :: String -> Maybe LongName
longName s = case s of
  [] -> Nothing
  '-':_ -> Nothing
  xs | '=' `elem` xs -> Nothing
     | otherwise -> Just $ LongName xs

-- | The /name/ of an /option/ (either a /short option name/
-- or a /long option name/).
newtype OptName = OptName (Either ShortName LongName)
  deriving (Eq, Ord, Show)

optNameToString :: OptName -> String
optNameToString (OptName ei) = case ei of
  Left shrt -> '-' : shortNameToChar shrt : []
  Right lng -> "--" ++ longNameToString lng

-- | A /word/ supplied by the user on the command line.
newtype Word = Word String
  deriving (Eq, Ord, Show)

-- | An /option argument/.
newtype OptArg = OptArg { optArgToString :: String }
  deriving (Eq, Ord, Show)

-- | Is this /word/ an input for a /long option/?
isLong
  :: Word
  -> Maybe (LongName, Maybe OptArg)
  -- ^ Nothing if the option does not begin with a double dash and is
  -- not at least three characters long.  Otherwise, returns the
  -- characters following the double dash to the left of any equal
  -- sign.  The Maybe in the tuple is Nothing if there is no equal
  -- sign, or Just followed by characters following the equal sign if
  -- there is one.
isLong (Word ('-':'-':[])) = Nothing
isLong (Word ('-':'-':xs)) = Just (LongName optName, arg)
  where
    (optName, end) = span (/= '=') xs
    arg = case end of
      [] -> Nothing
      _:rs -> Just . OptArg $ rs
isLong _ = Nothing

-- | Characters after the first /short option name/ in a /flag/ that
-- specifies a /short option/ instance, if the user supplies
-- @-afoobar@, then this will be @foobar@.
newtype ShortTail = ShortTail String
  deriving (Eq, Ord, Show)

-- | Is this an input /word/ for a /short argument/?
isShort
  :: Word
  -> Maybe (ShortName, ShortTail)
isShort (Word ('-':'-':_)) = Nothing
isShort (Word ('-':[])) = Nothing
isShort (Word ('-':x:xs)) = Just (ShortName x, ShortTail xs)
isShort _ = Nothing

wordToOptArg :: Word -> OptArg
wordToOptArg (Word t) = OptArg t

-- | If possible, splits a ShortTail into a /short option name/ and a
-- remaining tail.
splitShortTail :: ShortTail -> Maybe (ShortName, ShortTail)
splitShortTail (ShortTail s) = case s of
  [] -> Nothing
  x:xs -> Just (ShortName x, ShortTail xs)
