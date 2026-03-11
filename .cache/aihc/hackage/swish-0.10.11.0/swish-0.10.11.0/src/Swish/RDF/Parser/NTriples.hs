{-# LANGUAGE CPP #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------

-- |
--  Module      :  NTriples
--  Copyright   :  (c) 2011, 2012, 2013, 2018, 2021 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP
--
--  This Module implements a NTriples parser, returning a
--  new 'RDFGraph' consisting of triples and namespace information parsed from
--  the supplied NTriples input string, or an error indication.
--
-- REFERENCES:
--
--  - \"RDF Test Cases\",
--    W3C Recommendation 10 February 2004,
--    <http://www.w3.org/TR/rdf-testcases/#ntriples>
--
-- NOTES:
--
--  - If the URI is actually an IRI (Internationalized Resource Identifiers)
--    then the parser will fail since 'Network.URI.parseURI' fails.
--
--  - The case of language tags is retained.
--
--  - Update to the document \"N-Triples. A line-based syntax for an RDF graph\"
--    W3C Working Group Note 09 April 2013,
--    <http://www.w3.org/TR/2013/NOTE-n-triples-20130409/>
--
--------------------------------------------------------------------------------

module Swish.RDF.Parser.NTriples
    ( ParseResult
    , parseNT      
    )
where

import Swish.GraphClass (arc)
import Swish.Namespace (ScopedName, makeURIScopedName)

import Swish.RDF.Graph (RDFGraph, RDFLabel(..), addArc, emptyRDFGraph)
import Swish.RDF.Vocabulary (LanguageTag, toLangTag)

import Swish.RDF.Parser.Utils (ParseResult
    , runParserWithError
    , ignore
    , skipMany
    , noneOf
    , char
    , string
    , eoln
    , fullStop
    , hex4
    , hex8
    )
  
import Control.Applicative

import Network.URI (parseURI)

import qualified Data.Text as T
import qualified Data.Text.Lazy as L

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord)

#if MIN_VERSION_base(4, 7, 0)
import Data.Functor (($>))
#endif

import Data.Maybe (fromMaybe)

import Text.ParserCombinators.Poly.StateText

#if !MIN_VERSION_base(4, 7, 0)
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
#endif

----------------------------------------------------------------------
-- Define parser state and helper functions
----------------------------------------------------------------------

-- | NT parser state
data NTState = NTState
        { graphState :: RDFGraph            -- Graph under construction
        }

emptyState :: NTState
emptyState = NTState { graphState = emptyRDFGraph }
           
--  Return function to update graph in NT parser state,
--  using the supplied function of a graph. This is for use
--  with stUpdate.
--
updateGraph :: (RDFGraph -> RDFGraph) -> NTState -> NTState
updateGraph f s = s { graphState = f (graphState s) }

----------------------------------------------------------------------
--  Define top-level parser function:
--  accepts a string and returns a graph or error
----------------------------------------------------------------------

-- | Parser that carries around a NTState record.
type NTParser a = Parser NTState a

-- | Parse a string.
-- 
parseNT ::
  L.Text -- ^ input in NTriples format.
  -> ParseResult
parseNT = parsefromText ntripleDoc

{-
-- useful for testing
test :: String -> RDFGraph
test = either error id . parseNT
-}

-- | Function to supply initial context and parse supplied term.
--
--  Used for debugging.
parsefromText :: 
    NTParser a      -- ^ parser to apply
    -> L.Text       -- ^ input to be parsed
    -> Either String a
parsefromText parser = runParserWithError parser emptyState

-- helper routines

{-
lineFeed :: NTParser ()
lineFeed = ignore (char '\r')
-}

-- Add statement to graph in NT parser state

addStatement :: RDFLabel -> RDFLabel -> RDFLabel -> NTParser ()
addStatement s p o = stUpdate (updateGraph (addArc (arc s p o) ))

----------------------------------------------------------------------
--  Syntax productions
----------------------------------------------------------------------

{-

EBNF from the specification, using the notation from XML 1.0, second edition,
is included inline below.

We do not force ASCII 7-bit semantics here yet.

space	::=	#x20 /* US-ASCII space - decimal 32 */	
cr	::=	#xD /* US-ASCII carriage return - decimal 13 */	
lf	::=	#xA /* US-ASCII line feed - decimal 10 */	
tab	::=	#x9 /* US-ASCII horizontal tab - decimal 9 */	

The productions are kept as close as possible to the specification
for now.

-}

{-
ntripleDoc	::=	line*	
line	::=	ws* ( comment | triple )? eoln	

We relax the rule that the input must be empty or end with a new line.

ntripleDoc :: NTParser RDFGraph
ntripleDoc = graphState <$> (many line *> eof *> getState)

line :: NTParser ()
line = skipMany ws *> optional (comment <|> triple) *> eoln
-}

ntripleDoc :: NTParser RDFGraph
ntripleDoc = graphState <$> (sepBy line eoln *> optional eoln *> skipWS *> eof *> stGet)

line :: NTParser ()
line = skipWS *> ignore (optional (comment <|> triple))

{-
ws	::=	space | tab	

Could use whiteSpace rule here, but that would permit
constructs (e.g. comments) where we do not support them.
-}

isWS :: Char -> Bool
isWS = (`elem` (" \t" :: String))

{-
ws :: NTParser ()
-- ws = ignore (char ' ' <|> tab)
ws = ignore $ satisfy isWS
-}
           
skipWS :: NTParser ()
skipWS = ignore $ manySatisfy isWS

skip1WS :: NTParser ()
skip1WS = ignore $ many1Satisfy isWS

{-
comment	::=	'#' ( character - ( cr | lf ) )*	
-}

comment :: NTParser ()
comment = char '#' *> skipMany (noneOf "\r\n")

{-
eoln	::=	cr | lf | cr lf	
-}

{-
name	::=	[A-Za-z][A-Za-z0-9]*	
-}

isaz, isAZ, is09 :: Char -> Bool
isaz = isAsciiLower
isAZ = isAsciiUpper
is09 = isDigit

isaZ, isaZ09 :: Char -> Bool
isaZ c = isaz c || isAZ c
isaZ09 c = isaZ c || is09 c

isHeadChar, isBodyChar :: Char -> Bool
isHeadChar = isaZ
isBodyChar = isaZ09

name :: NTParser L.Text
name = L.cons <$> satisfy isHeadChar <*> manySatisfy isBodyChar

nameStr :: NTParser String
nameStr = L.unpack <$> name

{-
triple	::=	subject ws+ predicate ws+ object ws* '.' ws*	

-}

triple :: NTParser ()
triple = 
  do
    s <- subject <* skip1WS
    p <- predicate <* skip1WS
    o <- object <* (skipWS >> fullStop >> skipWS)
    addStatement s p o

{-
subject	::=	uriref | nodeID	
predicate	::=	uriref	
object	::=	uriref | nodeID | literal	
-}

subject :: NTParser RDFLabel
subject = urirefLbl <|> nodeID

predicate :: NTParser RDFLabel
predicate = urirefLbl

object :: NTParser RDFLabel
object = urirefLbl <|> nodeID <|> literal

{-
uriref	::=	'<' absoluteURI '>'	
absoluteURI	::=	character+ with escapes as defined below (from section 'URI References')	

The absoluteURI production encodes a Unicode string representing an RDF URI references as specified in
[RDF-CONCEPTS]. These are encoded in N-Triples using the escapes described in section Strings.

-}

uriref :: NTParser ScopedName
uriref = do
  ignore $ char '<'
  uri <- manyFinally' character (char '>')
  maybe (failBad ("Invalid URI: <" ++ uri ++ ">"))
    (return . makeURIScopedName)
    (parseURI uri)

urirefLbl :: NTParser RDFLabel
urirefLbl = Res <$> uriref

{-
nodeID	::=	'_:' name	
-}

nodeID :: NTParser RDFLabel
nodeID = Blank <$> (string "_:" *> nameStr)

{-  
literal	::=	langString | datatypeString	
langString	::=	'"' string '"' ( '@' language )?	
datatypeString	::=	'"' string '"' '^^' uriref	
language	::=	[a-z]+ ('-' [a-z0-9]+ )*
encoding a language tag.	
string	::=	character* with escapes as defined in section Strings	

-}

literal :: NTParser RDFLabel
literal = do
    lit <- T.pack <$> ntstring
    opt <- optional dtlang
    return $ case opt of
               Just (Left lcode)  -> LangLit lit lcode
               Just (Right dtype) -> TypedLit lit dtype
               _                  -> Lit lit

ntstring :: NTParser String
ntstring = bracket (char '"') (char '"') (many character)

dtlang :: NTParser (Either LanguageTag ScopedName)
dtlang = 
    (char '@' *> commit (Left <$> language))
    <|> (string "^^" *> commit (Right <$> uriref))

-- Note that toLangTag may fail since it does some extra
-- validation not done by the parser (mainly on the length of the
-- primary and secondary tags).
--
-- NOTE: This parser does not accept multiple secondary tags which RFC3066
-- does.
--
-- Although the EBNF only lists [a-z] we also support upper case values,
-- since the W3C Turtle test case includes a NTriples file with
-- "...@en-UK" in it.
--
language :: NTParser LanguageTag
language = do
    h <- many1Satisfy isaZ
    mt <- optional $ L.cons <$> char '-' <*> many1Satisfy isaZ09
    let lbl = L.toStrict $ L.append h $ fromMaybe L.empty mt
    case toLangTag lbl of
        Just lt -> return lt
        _ -> fail ("Invalid language tag: " ++ T.unpack lbl) -- should this be failBad?

{-
String handling: 

EBNF has:

character	::=	[#x20-#x7E] /* US-ASCII space to decimal 126 */	

Additional information from:

  http://www.w3.org/TR/rdf-testcases/#ntrip_strings

N-Triples strings are sequences of US-ASCII character productions encoding [UNICODE] character strings. The characters outside the US-ASCII range and some other specific characters are made available by \-escape sequences as follows:

 Unicode character
 (with code point u)	N-Triples encoding
 [#x0-#x8]	\uHHHH
 4 required hexadecimal digits HHHH encoding Unicode character u
 #x9	\t
 #xA	\n
 [#xB-#xC]	\uHHHH
 4 required hexadecimal digits HHHH encoding Unicode character u
 #xD	\r
 [#xE-#x1F]	\uHHHH
 4 required hexadecimal digits HHHH encoding Unicode character u
 [#x20-#x21]	the character u
 #x22	\"
 [#x23-#x5B]	the character u
 #x5C	\\
 [#x5D-#x7E]	the character u
 [#x7F-#xFFFF]	\uHHHH
 4 required hexadecimal digits HHHH encoding Unicode character u
 [#10000-#x10FFFF]	\UHHHHHHHH
 8 required hexadecimal digits HHHHHHHH encoding Unicode character u
 where H is a hexadecimal digit: [#x30-#x39],[#x41-#x46] (0-9, uppercase A-F).

This escaping satisfies the [CHARMOD] section Reference Processing Model on making the full Unicode character range U+0 to U+10FFFF available to applications and providing only one way to escape any character.

-}

-- 0x22 is " and 0x5c is \

isAsciiChar :: Char -> Bool
isAsciiChar c = let i = ord c
                in i >= 0x20 && i <= 0x21 ||
                   i >= 0x23 && i <= 0x5b ||
                   i >= 0x5d && i <= 0x7e

protectedChar :: NTParser Char
protectedChar =
  (char 't' $> '\t')
  <|> (char 'n' $> '\n')
  <|> (char 'r' $> '\r')
  <|> (char '"' $> '"')
  <|> (char '\\' $> '\\')
  <|> (char 'u' *> hex4)
  <|> (char 'U' *> hex8)

character :: NTParser Char
character = 
  (char '\\' *> protectedChar)
  <|> satisfy isAsciiChar

--------------------------------------------------------------------------------
--
--  Copyright (c) 2011, 2012, 2013, 2018 Douglas Burke
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
