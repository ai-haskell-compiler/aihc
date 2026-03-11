{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Text.Enum.Optparse
  (
  -- * A simple Whole Example
  -- $example

  -- * The Drivers
    parseIO
  , parseIOWithArgs
  , pureParse
  , testCLI
  -- * mkParserInfo
  , ParserDetails(..)
  , mkParserInfo
  -- * The Parser Generators
  , MetaVar
  , HelpText
  , FlagName
  , FlagChar
  , enumArgP
  , argP
  , argP'
  , enumOptP
  , optP
  , enumSwitchesP
  , shortEnumSwitchesP
  , module Text.Enum.Text
  ) where


import           Control.Applicative
import           Data.Char
import qualified Data.Text                      as T
import           Fmt
import           Options.Applicative
import           System.Environment
import           Text.Enum.Text

{- $example

A simple but complete example:

@
{\-\# LANGUAGE DeriveAnyClass    #-\}
{\-\# LANGUAGE DerivingVia       #-\}
{\-\# LANGUAGE OverloadedStrings #-\}

import Fmt
import Text.Enum.Optparse
import Paths_optparse_enum

data Choice
  = C_version
  | C_hello
  deriving (Bounded,Enum,EnumText,Eq,Ord,Show)
  deriving (Buildable,TextParsable) via UsingEnumText Choice

parserDetails ::   ParserDetails
parserDetails =
  ParserDetails
    { _pd_desc   = "optparse-enum example program"
    , _pd_header = "A simple optparse-enum illustrative program"
    , _pd_footer = "See the optparse-enum page on Hackage for details"
    }

main :: IO ()
main = do
  choice <- parseIO parserDetails enumSwitchesP
  case choice of
    C_version -> print    version
    C_hello   -> putStrLn "Hello!"
@
-}

--------------------------------------------------------------------------------
-- the drivers
--------------------------------------------------------------------------------

-- | making an IO parser
parseIO :: ParserDetails -> Parser a -> IO a
parseIO pd psr = getArgs >>= parseIOWithArgs pd psr

-- | making an IO parser, specifying the arguments
parseIOWithArgs :: ParserDetails -> Parser a -> [String] -> IO a
parseIOWithArgs pd psr as = handleParseResult $
    execParserPure (prefs idm) (mkParserInfo pd psr) as

-- | making a functional parser
pureParse :: ParserDetails -> Parser a -> [String] -> Maybe a
pureParse pd p =
    getParseResult . execParserPure (prefs idm) (mkParserInfo pd p)

-- | a testing helper
testCLI :: Show a => ParserDetails -> Parser a -> [String] -> IO ()
testCLI pd psr ss = do
    x <- handleParseResult $
              execParserPure (prefs idm) (mkParserInfo pd psr) ss
    print x


--------------------------------------------------------------------------------
-- mkParserInfo
--------------------------------------------------------------------------------

data ParserDetails =
  ParserDetails
    { _pd_desc   :: String
    , _pd_header :: String
    , _pd_footer :: String
    }
  deriving (Show)

-- | given a 'Parser' makes up a corresponding @ParserInfo@
mkParserInfo :: ParserDetails -> Parser a -> ParserInfo a
mkParserInfo ParserDetails{..} p =
    info (helper <*> p)
         $  fullDesc
         <> progDesc _pd_desc
         <> header   _pd_header
         <> footer   _pd_footer


--------------------------------------------------------------------------------
-- Parser generators
--------------------------------------------------------------------------------

type MetaVar  = String -- ^ name of a meta variable to be used in the docs
type HelpText = String -- ^ help text
type FlagName = String -- ^ name of a flag (will be forced to lower case)
type FlagChar = Char   -- ^ charcter used for a short flag

-- | parsing an 'EnumText' argument
enumArgP :: forall a . EnumText a => MetaVar -> Parser a
enumArgP var = argP var hlp
  where
    hlp = T.unpack $ T.intercalate "|" $
                map (fmt . build) [minBound..maxBound :: a]

-- | pasring a 'TextParsable' argument
argP :: TextParsable a => MetaVar -> HelpText -> Parser a
argP = argP' parseText

-- | pasring an 'TextParsable' argument, the parser being passed explicitly
argP' :: (T.Text->Either String a) -> MetaVar -> String -> Parser a
argP' prs var hlp = argument (eitherReader $ prs . T.pack)
      $  metavar var
      <> help    hlp

-- | parsing an 'EnumText' option
enumOptP :: forall a . EnumText a => FlagChar -> MetaVar -> Parser a
enumOptP c var = optP c var hlp
  where
    hlp = T.unpack $ T.intercalate "|" $
                map (fmt . build) [minBound..maxBound :: a]

-- | parsing a 'TextParsable' option
optP :: TextParsable a => FlagChar -> FlagName -> HelpText -> Parser a
optP ch nme hlp = option (eitherReader parse_string)
      $  metavar var
      <> short   ch
      <> long    lng
      <> help    hlp
  where
    var = map toUpper nme
    lng = map toLower nme

-- | generate mutually exclusive switches based on 'EnumText' @a@
enumSwitchesP :: EnumText a => Parser a
enumSwitchesP = shortEnumSwitchesP $ const Nothing

-- | generate mutually exclusive switches based on 'EnumText' @a@, with
-- some short swich options as specified by the argument function
shortEnumSwitchesP :: forall a . EnumText a => (a->Maybe FlagChar) -> Parser a
shortEnumSwitchesP sh_f = foldr (<|>) empty $ map mk [minBound..maxBound]
  where
    mk :: a -> Parser a
    mk x = flag' x $ (long $ fmt $ build x) <> shrt
      where
        shrt = case sh_f x of
          Nothing -> mempty
          Just c -> short c


--------------------------------------------------------------------------------
-- helpers
--------------------------------------------------------------------------------

parse_string :: TextParsable a => String -> Either String a
parse_string = parseText . T.pack
