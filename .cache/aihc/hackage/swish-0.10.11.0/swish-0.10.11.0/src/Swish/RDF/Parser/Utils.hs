{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Utils
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013, 2014, 2018, 2022 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  Support for the RDF Parsing modules.
--
--------------------------------------------------------------------------------

module Swish.RDF.Parser.Utils
    ( SpecialMap
    -- , mapPrefix
              
    -- tables
    , prefixTable, specialTable

    -- parser
    , runParserWithError
    , ParseResult
    , ignore
    , char
    , ichar
    , string
    , stringT
    , symbol
    , isymbol
    , lexeme
    , notFollowedBy
    , whiteSpace
    , skipMany
    , skipMany1
    , endBy
    , sepEndBy
    , sepEndBy1
    , manyTill
    , noneOf
    , eoln
    , fullStop
    , hex4
    , hex8
    , appendURIs
    )
    where

import Swish.Namespace (Namespace, makeNamespace, ScopedName)

import Swish.RDF.Graph (RDFGraph)
import Swish.RDF.Vocabulary
    ( namespaceRDF
    , namespaceRDFS
    , namespaceRDFD
    , namespaceOWL
    , namespaceLOG
    , rdfType
    , rdfFirst, rdfRest, rdfNil
    , owlSameAs, logImplies
    , defaultBase
    )

import Data.Char (isSpace, isHexDigit, chr)

#if MIN_VERSION_base(4, 7, 0)
import Data.Functor (($>))
#endif

import Data.Maybe (fromMaybe, fromJust)

import Network.URI (URI(..), relativeTo, parseURIReference)

import Text.ParserCombinators.Poly.StateText

import qualified Data.Map       as M
import qualified Data.Text      as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.Read as R

#if !MIN_VERSION_base(4, 7, 0)
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
#endif

-- Code

-- | Append the two URIs. Given the change in signature of
--   `Network.URI.relativeTo` in version @2.4.0.0@ of @network@,
--   it is not clear that this function is necessary. At the
--   very least, it should be changed to just return a `URI`.
--
appendURIs ::
  URI     -- ^ The base URI
  -> URI  -- ^ The URI to append (it can be an absolute URI).
  -> Either String URI
appendURIs base uri =
  case uriScheme uri of
    "" -> Right $ uri `relativeTo` base
    _  -> Right uri

-- | Type for special name lookup table
type SpecialMap = M.Map String ScopedName

-- | Define default table of namespaces
prefixTable :: [Namespace]
prefixTable =   [ namespaceRDF
                , namespaceRDFS
                , namespaceRDFD     -- datatypes
                , namespaceOWL
                , namespaceLOG
                , makeNamespace Nothing $ fromJust (parseURIReference "#") -- is this correct?
                ]

-- | Define default special-URI table.
specialTable ::
    Maybe ScopedName  -- ^ initial base URI, otherwise uses 'defaultBase'
    -> [(String,ScopedName)]
specialTable mbase =
  [ ("a",         rdfType    ),
    ("equals",    owlSameAs  ),
    ("implies",   logImplies ),
    ("listfirst", rdfFirst   ),
    ("listrest",  rdfRest    ),
    ("listnull",  rdfNil     ),
    ("base",      fromMaybe defaultBase mbase ) 
  ]

-- Parser routines, heavily based on Parsec combinators

-- | Run the parser and return the successful parse or an error
-- message which consists of the standard Polyparse error plus
-- a fragment of the unparsed input to provide context.
--
runParserWithError :: 
  Parser a b -- ^ parser (carrying state) to apply
  -> a       -- ^ starting state for the parser
  -> L.Text       -- ^ input to be parsed
  -> Either String b
runParserWithError parser state0 input = 
  let (result, _, unparsed) = runParser parser state0 input
     
      -- TODO: work out how best to report error context; for now just take the
      -- next 40 characters and assume there is enough context.
      econtext = if L.null unparsed
                 then "\n(at end of the text)\n"
                 else "\nRemaining input:\n" ++ 
                      case L.compareLength unparsed 40 of
                        GT -> L.unpack (L.take 40 unparsed) ++ "..."
                        _ -> L.unpack unparsed

  in case result of
    Left emsg -> Left $ emsg ++ econtext
    _ -> result

-- | The result of a parse, which is either an error message or a graph.
type ParseResult = Either String RDFGraph


-- | Run the parser and ignore the result.
ignore :: (Applicative f) => f a -> f ()
ignore f = f $> ()

-- | Match the character.
char :: Char -> Parser s Char
char c = satisfy (== c)

-- | Match the character, ignoring the result.
ichar :: Char -> Parser s ()
ichar = ignore . char

-- TODO: is there a better way to do this?
-- | Match the text.
string :: String -> Parser s String
string = mapM char
  
-- | Match the text.
stringT :: T.Text -> Parser s T.Text
stringT s = string (T.unpack s) >> return s

-- | Run the parser 'many' times and ignore the result.
skipMany :: Parser s a -> Parser s ()
skipMany = ignore . many
  
-- | Run the parser 'many1' times and ignore the result.
skipMany1 :: Parser s a -> Parser s ()
skipMany1 = ignore . many1

-- | Match zero or more occurences of
-- parser followed by separator.
endBy :: 
    Parser s a    -- ^ parser
    -> Parser s b -- ^ separator
    -> Parser s [a]
endBy p sep = many (p <* sep)

-- | Match zero or more occurences of the parser followed
-- by the separator.
sepEndBy :: 
    Parser s a    -- ^ parser
    -> Parser s b -- ^ separator
    -> Parser s [a]
sepEndBy p sep = sepEndBy1 p sep <|> pure []

-- | Accept one or more occurences of the parser
-- separated by the separator. Unlike 'endBy' the
-- last separator is optional.
sepEndBy1 :: 
    Parser s a    -- ^ parser
    -> Parser s b -- ^ separator
    -> Parser s [a]
sepEndBy1 p sep = do
  x <- p
  (sep *> ((x:) <$> sepEndBy p sep)) <|> return [x]
  
-- | Accept zero or more runs of the parser
-- ending with the delimiter.
manyTill :: 
    Parser s a    -- ^ parser
    -> Parser s b -- ^ delimiter
    -> Parser s [a]
manyTill p end = go
  where
    go = (end $> [])
         <|>
         ((:) <$> p <*> go)

-- | Accept any character that is not a member of the given string.
noneOf :: String -> Parser s Char           
noneOf istr = satisfy (`notElem` istr)

-- | Matches '.'.           
fullStop :: Parser s ()
fullStop = ichar '.'

-- | Match the end-of-line sequence (@"\\n"@, @"\\r"@, or @"\\r\\n"@). 
eoln :: Parser s ()
-- eoln = ignore (newline <|> (lineFeed *> optional newline))
-- eoln = ignore (try (string "\r\n") <|> string "\r" <|> string "\n")
eoln = ignore (oneOf [string "\r\n", string "\r", string "\n"])

-- | Succeed if the next character does not match the given function.
notFollowedBy :: (Char -> Bool) -> Parser s ()
notFollowedBy p = do
  c <- next
  if p c 
    then fail $ "Unexpected character: " ++ show [c]
    else reparse $ L.singleton c

-- | Match the given string and any trailing 'whiteSpace'.
symbol :: String -> Parser s String
symbol = lexeme . string

-- | As 'symbol' but ignoring the result.
isymbol :: String -> Parser s ()
isymbol = ignore . symbol

-- | Convert a parser into one that also matches, and ignores,
-- trailing 'whiteSpace'.
lexeme :: Parser s a -> Parser s a
lexeme p = p <* whiteSpace

-- | Match white space: a space or a comment (@#@ character and anything following it
-- up to to a new line).
whiteSpace :: Parser s ()
whiteSpace = skipMany (simpleSpace <|> oneLineComment)

simpleSpace :: Parser s ()
simpleSpace = ignore $ many1Satisfy isSpace

-- TODO: this should use eoln rather than a check on \n
oneLineComment :: Parser s ()
oneLineComment = (ichar '#' *> manySatisfy (/= '\n')) $> ()

{-

Not sure we can get this with polyparse

-- | Annotate a Parsec error with the local context - i.e. the actual text
-- that caused the error and preceeding/succeeding lines (if available)
--
annotateParsecError :: 
    Int -- ^ the number of extra lines to include in the context (<=0 is ignored)
    -> [String] -- ^ text being parsed
    -> ParseError -- ^ the parse error
    -> String -- ^ Parsec error with additional context
annotateParsecError extraLines ls err = 
    -- the following is based on the show instance of ParseError
    let ePos = errorPos err
        lNum = sourceLine ePos
        cNum = sourceColumn ePos
        -- it is possible to be at the end of the input so need
        -- to check; should produce better output than this in this
        -- case
        nLines = length ls
        ln1 = lNum - 1
        eln = max 0 extraLines
        lNums = [max 0 (ln1 - eln) .. min (nLines-1) (ln1 + eln)]
        
        beforeLines = map (ls !!) $ filter (< ln1) lNums
        afterLines  = map (ls !!) $ filter (> ln1) lNums
        
        -- in testing was able to get a line number after the text so catch this
        -- case; is it still necessary?
        errorLine = if ln1 >= nLines then "" else ls !! ln1
        arrowLine = replicate (cNum-1) ' ' ++ "^"
        finalLine = "(line " ++ show lNum ++ ", column " ++ show cNum ++ " indicated by the '^' sign above):"
        
        eHdr = "" : beforeLines ++ errorLine : arrowLine : afterLines ++ [finalLine]
        eMsg = showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input"
               (errorMessages err)

    in unlines eHdr ++ eMsg

-}

{-
Handle hex encoding; the spec for N3 and NTriples suggest that
only upper-case A..F are valid but you can find lower-case values
out there so support these too.
-}

hexDigit :: Parser a Char
-- hexDigit = satisfy (`elem` ['0'..'9'] ++ ['A'..'F'])
hexDigit = satisfy isHexDigit

-- | A four-digit hex value (e.g. @1a34@ or @03F1@).
hex4 :: Parser a Char
hex4 = do
  digs <- exactly 4 hexDigit
  let mhex = R.hexadecimal (T.pack digs)
  case mhex of
    Left emsg     -> failBad $ "Internal error: unable to parse hex4: " ++ emsg
    Right (v, "") -> return $ chr v
    Right (_, vs) -> failBad $ "Internal error: hex4 remainder = " ++ T.unpack vs

-- | An eight-digit hex value that has a maximum of @0010FFFF@.
hex8 :: Parser a Char
hex8 = do
  digs <- exactly 8 hexDigit
  let mhex = R.hexadecimal (T.pack digs)
  case mhex of
    Left emsg     -> failBad $ "Internal error: unable to parse hex8: " ++ emsg
    Right (v, "") -> if v <= 0x10FFFF
                     then return $ chr v
                     else failBad "\\UHHHHHHHH format is limited to a maximum of \\U0010FFFF"
    Right (_, vs) -> failBad $ "Internal error: hex8 remainder = " ++ T.unpack vs
        
--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2014, 2018, 2022 Douglas Burke
--  All rights reserved.
--
--  This file is part of Swish.
--
--  Swish is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  Swish is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with Swish; if not, write to:
--    The Free Software Foundation, Inc.,
--    59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--------------------------------------------------------------------------------
