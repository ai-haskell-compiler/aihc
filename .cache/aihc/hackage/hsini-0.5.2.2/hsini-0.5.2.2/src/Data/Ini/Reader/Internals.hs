{-# LANGUAGE FlexibleContexts #-}

{- |
Module    : Data.Ini.Reader.Internals
Copyright : 2011-2014 Magnus Therning
License   : BSD3

Internal functions used in 'Data.Ini.Reader'.
-}
module Data.Ini.Reader.Internals where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (evalState, get, put)
import Data.Functor (($>))
import Text.Parsec as P (
    anyChar,
    between,
    char,
    choice,
    many,
    many1,
    manyTill,
    newline,
    noneOf,
    oneOf,
 )
import Text.Parsec.String (Parser)

import Data.Char (isSpace)
import Data.Ini (emptyConfig, setOption)
import Data.Ini.Types (Config)
import Data.List (dropWhileEnd)

data IniReaderError
    = IniParserError String
    | IniSyntaxError String
    | IniOtherError String
    deriving (Eq, Show)

type IniParseResult = Either IniReaderError

-- | The type used to represent a line of a config file.
data IniFile
    = SectionL String
    | OptionL String String
    | OptionContL String
    | CommentL
    deriving (Show, Eq)

-- | Build a configuration from a list of 'IniFile' items.
buildConfig :: [IniFile] -> IniParseResult Config
buildConfig ifs =
    let
        isComment CommentL = True
        isComment _ = False

        fIfs = filter (not . isComment) ifs

        -- merge together OptionL and subsequent OptionContL items
        mergeOptions [] = return []
        mergeOptions (s@(SectionL _) : ifs') = (s :) `fmap` mergeOptions ifs'
        mergeOptions (CommentL : ifs') = (CommentL :) `fmap` mergeOptions ifs'
        mergeOptions (OptionL on ov : OptionContL ov2 : ifs') = mergeOptions $ OptionL on (ov ++ ov2) : ifs'
        mergeOptions (o@(OptionL _ _) : ifs') = (o :) `fmap` mergeOptions ifs'
        mergeOptions _ = throwError $ IniSyntaxError "Syntax error in INI file."

        -- build the configuration from a [IniFile]
        buildit a [] = return a
        buildit a (SectionL sn : is) = put sn >> buildit a is
        buildit a (OptionL on ov : is) = do
            sn <- get
            let na = setOption sn on ov a
            buildit na is
        buildit _ _ = undefined
     in
        mergeOptions fIfs >>= \is -> return $ evalState (buildit emptyConfig is) "default"

-- | Consumer of whitespace \"@ \t@\".
eatWhiteSpace :: Parser String
eatWhiteSpace = many $ oneOf " \t"

{- | Parser for the start-of-section line.  It expects the line to start with a
@[@ then find the section name, and finally a @]@.  The section name may be
surrounded by any number of white space characters (see 'eatWhiteSpace').
-}
secParser :: Parser IniFile
secParser = SectionL <$> between sectionNameOpen sectionNameClose sectionName
  where
    sectionNameOpen = char '[' *> eatWhiteSpace
    sectionNameClose = eatWhiteSpace *> char ']' *> manyTill anyChar newline
    sectionName = many1 $ oneOf validSecNameChrs
    validSecNameChrs = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "._-/@\" "

{- | Parser for a single line of an option.  The line must start with an option
name, then a @=@ must be found, and finally the rest of the line is taken as
the option value.  The equal sign may be surrounded by any number of white
space characters (see 'eatWhiteSpace').
-}
optLineParser :: Parser IniFile
optLineParser = OptionL <$> optionName <*> (optionEqual *> optionValue)
  where
    optionName = dropWhileEnd isSpace <$> (eatWhiteSpace *> many1 (oneOf validOptNameChrs))
    optionEqual = eatWhiteSpace *> char '=' *> eatWhiteSpace
    optionValue = manyTill anyChar newline
    validOptNameChrs = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_-/@ "

{- | Parser for an option-value continuation line.  The line must start with
either a space or a tab character (\"@ \t@\").  Everything else on the line,
until the newline character, is taken as the continuation of an option
value.
-}
optContParser :: Parser IniFile
optContParser = OptionContL <$> value
  where
    value = (:) <$> (oneOf " \t" *> eatWhiteSpace *> noneOf " \t") <*> manyTill anyChar newline

{- | Parser for "noise" in the configuration file, such as comments and empty
lines.  (Note that lines containing only space characters will be
successfully parsed by 'optContParser'.)
-}
noiseParser :: Parser IniFile
noiseParser =
    let
        commentP = oneOf "#;" *> manyTill anyChar newline
        emptyL = (newline $> "")
     in
        choice [commentP, emptyL] $> CommentL

iniParser :: Parser [IniFile]
iniParser =
    many $ choice [secParser, optLineParser, optContParser, noiseParser]
