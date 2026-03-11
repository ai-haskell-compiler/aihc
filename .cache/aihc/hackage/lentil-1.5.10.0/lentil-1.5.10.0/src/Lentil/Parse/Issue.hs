-----------------------------------------------------------------------------
-- |
-- Module      :  Lentil.Parse.Issue
-- Copyright   :  © 2015-2016 Francesco Ariis, Michał Antkiewi
-- License     :  GPLv3 (see the LICENSE file)
--
-- Issues parsing from comments
-----------------------------------------------------------------------------

module Lentil.Parse.Issue where

import Lentil.Types
import Lentil.Helpers

import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Char           as C
import qualified Data.Maybe          as M
import qualified Control.Monad.State as S


-----------
-- TYPES --
-----------

type ParIssue a = StateParser [FlagWord] a

-- standard flagwords
stdFlagwords :: [FlagWord]
stdFlagwords = ["todo", "fixme", "xxx"]

-- top-level constant to evaluate once
flagWords :: ParIssue [FlagWord]
flagWords = fmap (stdFlagwords ++) (S.lift S.get)

---------------
-- PRIMITIVE --
---------------

-- a unix file is defined as many lines (each one ended by '\n')
-- this parser captures the definition and can be used as eof replacement too
eoft :: ParIssue ()
eoft = optional (char '\n') *> eof

-- case insensitive string, lifted from Text.ParserCombinators.Parsec.Rfc2234
ciString :: String -> ParIssue String
ciString s = mapM ciChar s <?> "case insensitive string"
    where ciChar :: Char -> ParIssue Char
          ciChar c = char (C.toLower c) <|> char (C.toUpper c)

-- i.e. remove unneeded whitespace
htmlify :: String -> String
htmlify cs = unwords . words $ cs

-- a blank line of text (even at eof)
blankline :: ParIssue ()
blankline = char '\n' *> (() <$ char '\n' <|> eof)


----------
-- TAGS --
----------

-- simple tags parsing

-- tag only
tag :: ParIssue Tag
tag = Tag <$> (openPar *> tagLabel <* closePar) <?> "tag"

-- anything goes, apart from ' '
tagLabel :: ParIssue String
tagLabel = some (satisfy sf) <?> "tag label"
    where sf :: Char -> Bool
          sf c | c == closeDel = False
               | C.isSpace c   = False
               | otherwise     = True

openPar, closePar :: ParIssue Char
openPar  = char openDel  <?> "open-tag delimiter"
closePar = char closeDel <?> "close-tag delimiter"


-------------
-- INCIPIT --
-------------

-- optional ws + flagwords (case unsensitive) + optional ':' ++ optional1 ws
incipit :: ParIssue FlagWord
incipit = fwpar                         >>= \fw ->
          optional (char ':')           >>
          notFollowedBy nonSpace        >> -- real todo, not todoodle
          return (normaliseFlagword fw)
        <?> "incipit"
    where
          fwpar = flagWords                        >>= \fw ->
                  choice (map (try . ciString) fw)

          nonSpace = satisfy (not . C.isSpace)

-- issues (bar the first one) should only appear at beginning of line
incipitv :: ParIssue FlagWord
incipitv = eol *> space *> incipit
         <?> "incipit stopper"

------------
-- ISSUES --
------------

-- simple issue parsing

-- an issue is a flagword, followed by : , followed by some description and
-- ended optionally by some tags (No t/f? End by whiteline or eof or
-- another TODO).
-- tags can be placed *before* description, too
issue :: ParIssue Issue
issue = (mkIssue <$> incipit
                 <*> fmap sourceName getSourcePos
                 <*> fmap (unPos . sourceLine) getSourcePos
                 <*> option [] (try tags)
                 <*> freeText
                 <*> option [] (try tags))
        <?> "issue"
    where
          mkIssue fw fp ln tg1 ds tg2 = Issue fp ln ds (addTag fw (tg1++tg2))

          addTag "todo" tgs = tgs
          addTag fw     tgs = Tag fw : tgs



-- any text. Since tags/fields at the end of the issue are optional, we need
-- a way to delimit this. Delimiters are: eof, tags/fields, blank line
freeText :: ParIssue (Maybe Description)
freeText = fmap htmlify (manyTill anySingle end) >>= \t ->
           case t of
             [] -> return Nothing
             _  -> return $ Just t

         <?> "free text"
    where vp p      = try . fmap (const ()) $ p
          end       = lookAhead $ choice [vp (space1 *> tag),
                                          vp blankline,  -- \n\n or \neof
                                          vp incipitv,   -- another issue
                                          vp (eof :: ParIssue ())]

tags :: ParIssue [Tag]
tags = some (try $ space *> tag) <?> "tags"


-- parses a number of issues from a given line-of-text
issues :: ParIssue [Issue]
issues = space *> -- first issue can have preceding ws
         (M.catMaybes <$> ifs)
       <?> "issues"
    where
          -- unissue text
          -- remember to consume what you find before a will-formed incipit
          t :: ParIssue (Maybe Issue)
          t = try (manyTill anySingle (lookAhead $ try incipitv)) *>
              char '\n' *> space *> pure Nothing

          i :: ParIssue (Maybe Issue)
          i =  Just <$> try issue

          ifs :: ParIssue [Maybe Issue]
          ifs = many (i <|> t)

