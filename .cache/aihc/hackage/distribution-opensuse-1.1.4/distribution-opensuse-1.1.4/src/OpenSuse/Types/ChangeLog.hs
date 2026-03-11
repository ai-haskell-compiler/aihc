{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

module OpenSuse.Types.ChangeLog
  ( ChangeLog(..), Entry(..)
  , parseEntry, parseDashedLine, parseDateAddressLine, parseDescription
  )
  where

import OpenSuse.Prelude
import OpenSuse.Prelude.Parser as Parse
import qualified OpenSuse.Prelude.PrettyPrinting as Pretty ( )
import OpenSuse.Types.EMailAddress
import Data.Time.Format

newtype ChangeLog = ChangeLog [Entry]
  deriving (Show, Eq, Ord, Generic, NFData, Semigroup, Monoid)

instance HasParser ChangeLog where
  parser = ChangeLog <$> (parseDashedLine *> many parser)

data Entry = Entry
  { changedAt :: UTCTime
  , changedBy :: EMailAddress
  , changeDescription :: Text
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData Entry

instance HasParser Entry where
  parser = parseEntry

-- * Useful parsers for ChangeLog elements

{-# ANN parseEntry "HLint: ignore Use <$>" #-}
parseEntry :: CharParser st input m Entry
parseEntry = do
  (ts,author) <- parseDateAddressLine
  parseEmptyLine
  txt <- parseDescription
  return (Entry ts author (packText txt))

parseDashedLine :: CharParser st input m ()
parseDashedLine = do
  _ <- string "------------------------------------------------------------"
  skipMany1 (char '-')
  _ <- endOfLine
  return ()

-- | Note that the input must be terminated by a newline.
--
-- >>> parseTest parseDateAddressLine "Wed Jun 27 09:25:07 UTC 2018 - foo@example.org\n"
-- (2018-06-27 09:25:07 UTC,EMailAddress "foo@example.org")
parseDateAddressLine :: CharParser st input m (UTCTime, EMailAddress)
parseDateAddressLine = do
  ts <- many1 (alphaNum <|> oneOf ": ")
  _ <- char '-'
  addr <- parser
  _ <- endOfLine
  utct <- parseTimeM True defaultTimeLocale changeLogDateFormat ts
  return (utct,addr)

-- | Consume an empty line, i.e. a line that contains only whitespace.
parseEmptyLine :: CharParser st input m ()
parseEmptyLine = skipMany (oneOf " \t") <* endOfLine

-- | Consume all text until the end of the file or a dashed line is found. In
-- the latter case, the dashed line is consumed as well. This is unfortunate,
-- but it's how the 'notFollowedBy' combinator works, unfortunately,
parseDescription :: CharParser st input m String
parseDescription = manyTill anyChar (try parseDashedLine <|> eof)

-- | Appropriate format parameter for 'formatTime' and 'parseTimeM'.
changeLogDateFormat :: String
changeLogDateFormat = "%a %b %e %H:%M:%S %Z %Y"
