{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if (__GLASGOW_HASKELL__ >= 802)
{-# LANGUAGE DerivingStrategies #-}
#endif

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  Turtle
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013, 2014, 2018, 2020, 2022 Douglas Burke
--  License     :  GPL V2
-- 
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, DerivingStrategies, OverloadedStrings
-- 
--  This Module implements a Turtle parser, returning a
--  new 'RDFGraph' consisting of triples and namespace information parsed from
--  the supplied input string, or an error indication.
-- 
-- REFERENCES:
-- 
--  - \"Turtle, Terse RDF Triple Language\",
--    W3C Candidate Recommendation 19 February 2013 (<http://www.w3.org/TR/2013/CR-turtle-20130219/L),
--    <http://www.w3.org/TR/turtle/>
-- 
-- NOTES:
-- 
--  - Prior to version @0.9.0.4@, the parser followed the
--    W3C Working Draft 09 August 2011 (<http://www.w3.org/TR/2011/WD-turtle-20110809/>)
-- 
--  - Strings with no language tag are converted to a 'LitTag' not a
--    'TypedLitTag' with a type of @xsd:string@ (e.g. see
--    <http://www.w3.org/TR/2011/WD-turtle-20110809/#terms>).
-- 
--  - If the URI is actually an IRI (Internationalized Resource Identifiers)
--    then the parser will fail since 'Network.URI.parseURI' fails.
-- 
--  - The current (August 2013) Turtle test suite from
--    <http://www.w3.org/2013/TurtleTests/> passes except for the four
--    tests with non-ASCII local names, namely:
--    @localName_with_assigned_nfc_bmp_PN_CHARS_BASE_character_boundaries@,
--    @localName_with_assigned_nfc_PN_CHARS_BASE_character_boundaries@,
--    @localName_with_nfc_PN_CHARS_BASE_character_boundaries@,
--    and
--    @localName_with_non_leading_extras@.
-- 
--------------------------------------------------------------------------------

-- TODO:
--   - should the productions moved to an Internal module for use by
--     others - e.g. Sparql or the N3 parser?

module Swish.RDF.Parser.Turtle
    ( ParseResult
    , parseTurtle      
    , parseTurtlefromText      
    )
where

import Swish.GraphClass (arc)
import Swish.Namespace (Namespace, ScopedName)
import Swish.Namespace (makeNamespace, getNamespaceTuple
                       , getScopeNamespace, getScopedNameURI
                       , getScopeNamespace, makeURIScopedName, makeNSScopedName)
import Swish.QName (newLName, emptyLName)

import Swish.RDF.Graph
    ( RDFGraph, RDFLabel(..)
    , NamespaceMap
    , addArc 
    , setNamespaces
    , emptyRDFGraph
    )

import Swish.RDF.Vocabulary
    ( LanguageTag
    , toLangTag
    , rdfType
    , rdfFirst, rdfRest, rdfNil
    , xsdBoolean, xsdInteger, xsdDecimal, xsdDouble
    , defaultBase
    )

import Swish.RDF.Datatype (makeDatatypedLiteral)

import Swish.RDF.Parser.Utils
    ( ParseResult
    , runParserWithError
    , ignore
    , noneOf
    , char
    , ichar
    , string
    , stringT
    , sepEndBy1
    , isymbol
    , lexeme
    , whiteSpace
    , hex4  
    , hex8  
    , appendURIs
    )

import Control.Applicative
import Control.Monad (foldM)

import Data.Char (chr, isAsciiLower, isAsciiUpper, isDigit, isHexDigit, ord, toLower)

#if MIN_VERSION_base(4, 7, 0)
import Data.Functor (($>))
#endif

import Data.Maybe (fromMaybe)
import Data.Word (Word32)

import Network.URI (URI(..), parseURIReference)

import Text.ParserCombinators.Poly.StateText

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L

#if !MIN_VERSION_base(4, 7, 0)
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
#endif

----------------------------------------------------------------------
-- Define parser state and helper functions
----------------------------------------------------------------------

-- | Turtle parser state
data TurtleState = TurtleState
        { graphState :: RDFGraph            -- Graph under construction
        , prefixUris :: NamespaceMap        -- namespace prefix mapping table
        , baseUri    :: URI                 -- base URI
        , nodeGen    :: Word32              -- blank node id generator
        } deriving
#if (__GLASGOW_HASKELL__ >= 802)
    stock
#endif
    Show

-- | Functions to update TurtleState vector (use with stUpdate)

setPrefix :: Maybe T.Text -> URI -> TurtleState -> TurtleState
setPrefix pre uri st =  st { prefixUris=p' }
    where
        p' = M.insert pre uri (prefixUris st)

-- | Change the base
setBase :: URI -> TurtleState -> TurtleState
setBase buri st = st { baseUri = buri }

--  Functions to access state:

-- | Return the default prefix
getDefaultPrefix :: TurtleParser Namespace
getDefaultPrefix = do
  s <- stGet
  case getPrefixURI s Nothing of
    Just uri -> return $ makeNamespace Nothing uri
    _ -> failBad "No default prefix defined; how unexpected (probably a programming error)!"

--  Map prefix to URI (naming needs a scrub here)
getPrefixURI :: TurtleState -> Maybe T.Text -> Maybe URI
getPrefixURI st pre = M.lookup pre (prefixUris st)

findPrefixNamespace :: Maybe L.Text -> TurtleParser Namespace
findPrefixNamespace (Just p) = findPrefix (L.toStrict p)
findPrefixNamespace Nothing  = getDefaultPrefix

--  Return function to update graph in Turtle parser state,
--  using the supplied function of a graph
--
updateGraph :: (RDFGraph -> RDFGraph) -> TurtleState -> TurtleState
updateGraph f s = s { graphState = f (graphState s) }

----------------------------------------------------------------------
--  Define top-level parser function:
--  accepts a string and returns a graph or error
----------------------------------------------------------------------

type TurtleParser a = Parser TurtleState a

-- | Parse as Turtle (with no real base URI).
-- 
-- See 'parseTurtle' if you need to provide a base URI.
--
parseTurtlefromText ::
  L.Text -- ^ input in N3 format.
  -> ParseResult
parseTurtlefromText = flip parseTurtle Nothing

-- | Parse a string with an optional base URI.
--            
-- Unlike 'parseN3' we treat the base URI as a URI and not
-- a QName.
--
parseTurtle ::
  L.Text -- ^ input in N3 format.
  -> Maybe URI -- ^ optional base URI
  -> ParseResult
parseTurtle txt mbase = parseAnyfromText turtleDoc mbase txt

{-
hashURI :: URI
hashURI = fromJust $ parseURIReference "#"
-}

-- | The W3C turtle tests - e.g. <http://www.w3.org/2013/TurtleTests/turtle-syntax-bad-prefix-01.ttl> -
-- point out there's no default prefix mapping.
--
emptyState :: 
  Maybe URI  -- ^ starting base for the graph
  -> TurtleState
emptyState mbase = 
  let pmap   = M.empty -- M.singleton Nothing hashURI
      buri   = fromMaybe (getScopedNameURI defaultBase) mbase
  in TurtleState
     { graphState = emptyRDFGraph
     , prefixUris = pmap
     , baseUri    = buri
     , nodeGen    = 0
     }
  
-- | Function to supply initial context and parse supplied term.
--
parseAnyfromText :: 
  TurtleParser a  -- ^ parser to apply
  -> Maybe URI    -- ^ base URI of the input, or @Nothing@ to use default base value
  -> L.Text       -- ^ input to be parsed
  -> Either String a
parseAnyfromText parser mbase = runParserWithError parser (emptyState mbase)

newBlankNode :: TurtleParser RDFLabel
newBlankNode = do
  n <- stQuery (succ . nodeGen)
  stUpdate $ \s -> s { nodeGen = n }
  return $ Blank (show n)
  
{-
This has been made tricky by the attempt to remove the default list
of prefixes from the starting point of a parse and the subsequent
attempt to add every new namespace we come across to the parser state.

So we add in the original default namespaces for testing, since
this routine is really for testing.

addTestPrefixes :: TurtleParser ()
addTestPrefixes = stUpdate $ \st -> st { prefixUris = LookupMap prefixTable } -- should append to existing map

-}

-- helper routines

comma, semiColon , fullStop :: TurtleParser ()
comma = isymbol ","
semiColon = isymbol ";"
fullStop = isymbol "."

sQuot, dQuot, sQuot3, dQuot3 :: TurtleParser ()
sQuot = ichar '\''
dQuot = ichar '"'
sQuot3 = ignore $ string "'''"
dQuot3 = ignore $ string "\"\"\""

match :: (Ord a) => a -> [(a,a)] -> Bool
match v = any (\(l,h) -> v >= l && v <= h)

-- a specialization of bracket that ensures white space after
-- the bracket symbol is parsed.
br :: Char -> Char -> TurtleParser a -> TurtleParser a
br lsym rsym =
  let f = lexeme . char
  in bracket (f lsym) (f rsym)

-- this is a lot simpler than N3
atWord :: T.Text -> TurtleParser ()
atWord s = (char '@' *> lexeme (stringT s)) $> ()

-- | Case insensitive match.
charI ::
  Char  -- ^ must be upper case
  -> TurtleParser Char
charI c = satisfy (`elem` c : [ toLower c ])

-- | Case insensitive match.
stringI ::
  String  -- ^ must be upper case
  -> TurtleParser String
stringI = mapM charI

{-
Add statement to graph in the parser state; there is a special case
for the special-case literals in the grammar since we need to ensure
the necessary namespaces (in other words xsd) are added to the
namespace store.
-}

addStatement :: RDFLabel -> RDFLabel -> RDFLabel -> TurtleParser ()
addStatement s p o@(TypedLit _ dtype) | dtype `elem` [xsdBoolean, xsdInteger, xsdDecimal, xsdDouble] = do 
  ost <- stGet
  let stmt = arc s p o
      oldp = prefixUris ost
      ogs = graphState ost
      (nspre, nsuri) = getNamespaceTuple $ getScopeNamespace dtype
      newp = M.insert nspre nsuri oldp
  stUpdate $ \st -> st { prefixUris = newp, graphState = addArc stmt ogs }
addStatement s p o = stUpdate (updateGraph (addArc (arc s p o) ))

isaz, isAZ, isaZ, is09, isaZ09 :: Char -> Bool
isaz = isAsciiLower
isAZ = isAsciiUpper
isaZ c = isaz c || isAZ c
is09 = isDigit
isaZ09 c = isaZ c || is09 c

{-
Since operatorLabel can be used to add a label with an 
unknown namespace, we need to ensure that the namespace
is added if not known. If the namespace prefix is already
in use then it is over-written (rather than add a new
prefix for the label).

TODO:
  - could we use the reverse lookupmap functionality to
    find if the given namespace URI is in the namespace
    list? If it is, use it's key otherwise do a
    mapReplace for the input namespace (updated to use the
    Data.Map.Map representation).
    
-}
operatorLabel :: ScopedName -> TurtleParser RDFLabel
operatorLabel snam = do
  st <- stGet
  let (pkey, pval) = getNamespaceTuple $ getScopeNamespace snam
      opmap = prefixUris st
      
      rval = Res snam
      
  -- TODO: the lookup and the replacement could be fused; it may not
  --       even make sense to separate now using a Map
  case M.lookup pkey opmap of
    Just val | val == pval -> return rval
             | otherwise   -> do
               stUpdate $ \s -> s { prefixUris = M.insert pkey pval opmap }
               return rval
    
    _ -> do
      stUpdate $ \s -> s { prefixUris = M.insert pkey pval opmap }
      return rval
        
findPrefix :: T.Text -> TurtleParser Namespace
findPrefix pre = do
  st <- stGet
  case M.lookup (Just pre) (prefixUris st) of
    Just uri -> return $ makeNamespace (Just pre) uri
    Nothing  -> failBad $ "Undefined prefix '" ++ T.unpack pre ++ ":'."

-- | Add the message to the start of the error message if the
--   parser fails (a minor specialization of 'adjustErr').

{-
addErr :: Parser s a -> String -> Parser s a
addErr p m = adjustErr p (m++)
-}

(<?) ::
  Parser s a
  -> String -- ^ Error message to add (a new line is added after the message)
  -> Parser s a
(<?) p m = adjustErr p ((m ++ "\n") ++)

-- Applicative's <* et al are infixl 4, with <|> infixl 3
infixl 4 <?

{-

Syntax productions; the Turtle ENBF grammar elements are from
http://www.w3.org/TR/2013/CR-turtle-20130219/#sec-grammar-grammar

The element names are converted to match Haskell syntax
and idioms where possible:

  - camel Case rather than underscores and all upper case

  - upper-case identifiers prepended by _ after above form

-}
{-
[1]	turtleDoc	::=	statement*
-}
turtleDoc :: TurtleParser RDFGraph
turtleDoc = mkGr <$> (whiteSpace *> many statement *> eof *> stGet)
  where
    mkGr s = setNamespaces (prefixUris s) (graphState s)

{-
[2]	statement	::=	directive | triples '.'
-}
statement :: TurtleParser ()
statement = directive <|> (triples *> commit fullStop <? "Missing '.' after a statement.")

{-
[3]	directive	::=	prefixID | base | sparqlPrefix | sparqlBase

With the addition of sparqlPrefix/sparqlBase (so '.' handling moved
into prefixID/base) may need to adjust use of lexeme.
-}
directive :: TurtleParser ()
directive =
  lexeme
  (prefixID <? "Unable to parse @prefix statement."
   <|> base <? "Unable to parse @base statement."
   <|> sparqlPrefix <? "Unable to parse Sparql PREFIX statement."
   <|> sparqlBase <? "Unable to parse Sparql BASE statement.")

{-
[4]	prefixID	::=	'@prefix' PNAME_NS IRIREF '.'
-}
prefixID :: TurtleParser ()
prefixID = do
  atWord "prefix"
  p <- commit $ lexeme _pnameNS
  u <- lexeme _iriRef
  fullStop
  stUpdate $ setPrefix (fmap L.toStrict p) u

{-
[5]	base	::=	'@base' IRIREF '.'
-}
base :: TurtleParser ()
base = do
  atWord "base"
  b <- commit $ lexeme _iriRef
  fullStop
  stUpdate $ setBase b

{-
[5s]	sparqlBase	::=	"BASE" IRIREF
-}
sparqlBase :: TurtleParser ()
sparqlBase = lexeme (stringI "BASE") >> commit _iriRef >>= stUpdate . setBase

{-
[6s]	sparqlPrefix	::=	"PREFIX" PNAME_NS IRIREF
-}
sparqlPrefix :: TurtleParser ()
sparqlPrefix = do
  ignore $ lexeme $ stringI "PREFIX"
  p <- commit $ lexeme _pnameNS
  u <- lexeme _iriRef
  stUpdate $ setPrefix (fmap L.toStrict p) u

{-
[6]	triples	::=	subject predicateObjectList | blankNodePropertyList predicateObjectList?
-}

triples :: TurtleParser ()
triples =
  (subject >>= predicateObjectList)
  <|>
  (blankNodePropertyList >>= ignore . optional . predicateObjectList)

{-
[7]	predicateObjectList	::=	verb objectList (';' (verb objectList)?)*
-}

predicateObjectList :: RDFLabel -> TurtleParser ()
predicateObjectList subj = 
  let term = verb >>= objectList subj
  in ignore $ sepEndBy1 term (many1 semiColon)

{-
[8]	objectList	::=	object (',' object)*
-}

objectList :: RDFLabel -> RDFLabel -> TurtleParser ()
objectList subj prd = sepBy1 object comma >>= mapM_ (addStatement subj prd)

{-
[9]	verb	::=	predicate | 'a'
-}

verb :: TurtleParser RDFLabel
verb = predicate <|> (lexeme (char 'a') *> operatorLabel rdfType)
   
{-       
[10]	subject	::=	iri | BlankNode | collection
-}

subject :: TurtleParser RDFLabel
subject = (Res <$> iri) <|> blankNode <|> collection

{-
[11]	predicate	::=	iri
-}

predicate :: TurtleParser RDFLabel
predicate = Res <$> iri

{-
[12]	object	::=	iri | BlankNode | collection | blankNodePropertyList | literal
-}

object :: TurtleParser RDFLabel
object = (Res <$> iri) <|> blankNode <|> collection <|>
         blankNodePropertyList <|> literal

{-
[13]	literal	::=	RDFLiteral | NumericLiteral | BooleanLiteral
-}

literal :: TurtleParser RDFLabel
literal = lexeme $ rdfLiteral <|> numericLiteral <|> booleanLiteral

{-
[14]	blankNodePropertyList	::=	'[' predicateObjectList ']'
-}

blankNodePropertyList :: TurtleParser RDFLabel
blankNodePropertyList = do
  bNode <- newBlankNode
  br '[' ']' $ lexeme (predicateObjectList bNode)
  return bNode

{-
[15]	collection	::=	'(' object* ')'
-}
collection :: TurtleParser RDFLabel
collection = do
  os <- br '(' ')' $ many object
  eNode <- operatorLabel rdfNil
  case os of
    [] -> return eNode
    
    (x:xs) -> do
      sNode <- newBlankNode
      first <- operatorLabel rdfFirst
      addStatement sNode first x
      lNode <- foldM addElem sNode xs
      rest <- operatorLabel rdfRest
      addStatement lNode rest eNode
      return sNode

    where      
      addElem prevNode curElem = do
        bNode <- newBlankNode
        first <- operatorLabel rdfFirst
        rest <- operatorLabel rdfRest
        addStatement prevNode rest bNode
        addStatement bNode first curElem
        return bNode

{-
[16]	NumericLiteral	::=	INTEGER | DECIMAL | DOUBLE

NOTE: We swap the order from this production

I have removed the conversion to a canonical form for
the double production, since it makes running the W3C
tests for Turtle harder (since it assumes that "1E0"
is passed through as is). It is also funny to
create a "canonical" form for only certain data types.
-}
numericLiteral :: TurtleParser RDFLabel
numericLiteral =
  let f t v = makeDatatypedLiteral t (L.toStrict v)
  in (f xsdDouble <$> _double)
     <|>
     (f xsdDecimal <$> _decimal)
     <|>
     (f xsdInteger <$> _integer)

{-
[128s]	RDFLiteral	::=	String (LANGTAG | '^^' iri)?

TODO: remove 'Lit lbl' form, since dtype=xsd:string in this case.
-}
rdfLiteral :: TurtleParser RDFLabel
rdfLiteral = do
  lbl <- L.toStrict <$> turtleString
  opt <- optional ((Left <$> (_langTag <? "Unable to parse the language tag"))
                   <|>
                   (string "^^" *> (Right <$> (commit iri <? "Unable to parse the datatype of the literal"))))
  ignore $ optional whiteSpace
  return $ case opt of
             Just (Left lcode)  -> LangLit lbl lcode
             Just (Right dtype) -> TypedLit lbl dtype
             _                  -> Lit lbl

{-
[133s]	BooleanLiteral	::=	'true' | 'false'
-}
booleanLiteral :: TurtleParser RDFLabel
booleanLiteral = makeDatatypedLiteral xsdBoolean . T.pack <$> lexeme (string "true" <|> string "false")

{-
[17]	String	::=	STRING_LITERAL_QUOTE | STRING_LITERAL_SINGLE_QUOTE | STRING_LITERAL_LONG_SINGLE_QUOTE | STRING_LITERAL_LONG_QUOTE
-}
turtleString :: TurtleParser L.Text
turtleString = 
  lexeme (
    _stringLiteralLongQuote <|> _stringLiteralQuote <|>
    _stringLiteralLongSingleQuote <|> _stringLiteralSingleQuote
    ) <? "Unable to parse a string literal"

{-
[135s]	iri	::=	IRIREF | PrefixedName
-}
iri :: TurtleParser ScopedName
iri = lexeme (
  (makeURIScopedName <$> _iriRef)
  <|>
  prefixedName)

{-
[136s]	PrefixedName	::=	PNAME_LN | PNAME_NS
-}
prefixedName :: TurtleParser ScopedName
prefixedName = 
  _pnameLN <|> 
  flip makeNSScopedName emptyLName <$> (_pnameNS >>= findPrefixNamespace)

{-
[137s]	BlankNode	::=	BLANK_NODE_LABEL | ANON
-}
blankNode :: TurtleParser RDFLabel
blankNode = lexeme (_blankNodeLabel <|> _anon)

{--- Productions for terminals ---}

{-
[18]	IRIREF	::=	'<' ([^#x00-#x20<>\"{}|^`\] | UCHAR)* '>'
-}
_iriRef :: TurtleParser URI
_iriRef = do
  -- ignore $ char '<'
  -- why a, I using manyFinally' here? '>' shouldn't overlap
  -- with iriRefChar.
  -- ustr <- manyFinally' iriRefChar (char '>')
  ustr <- bracket (char '<') (commit (char '>')) (many iriRefChar)
  case parseURIReference ustr of
    Nothing -> failBad $ "Invalid URI: <" ++ ustr ++ ">"
    Just uref -> do
      s <- stGet
      either fail return $ appendURIs (baseUri s) uref

iriRefChar :: TurtleParser Char
iriRefChar = satisfy isIRIChar <|> _uchar

isIRIChar :: Char -> Bool
isIRIChar c =
  c > chr 0x20
  && 
  c `notElem` ("<>\"{}|^`\\"::String)

{-
[139s]	PNAME_NS	::=	PN_PREFIX? ':'
-}
_pnameNS :: TurtleParser (Maybe L.Text)
_pnameNS = optional _pnPrefix <* char ':'

{-
[140s]	PNAME_LN	::=	PNAME_NS PN_LOCAL
-}
_pnameLN :: TurtleParser ScopedName
_pnameLN = do
  ns <- _pnameNS >>= findPrefixNamespace
  l <- fmap L.toStrict _pnLocal
  case newLName l of
    Just lname -> return $ makeNSScopedName ns lname
    _ -> fail $ "Invalid local name: '" ++ T.unpack l ++ "'"

{-
[141s]	BLANK_NODE_LABEL	::=	'_:' (PN_CHARS_U | [0-9]) ((PN_CHARS | '.')* PN_CHARS)?
-}
_blankNodeLabel :: TurtleParser RDFLabel
_blankNodeLabel = do
  ignore $ string "_:"
  fChar <- _pnCharsU <|> satisfy is09
  rest <- _pnRest
  return $ Blank $ fChar : L.unpack rest

{-
Extracted from BLANK_NODE_LABEL and PN_PREFIX

<PN_REST> :== ( ( PN_CHARS | '.' )* PN_CHARS )?

We assume below that the match is only ever done for small strings, so
the cost isn't likely to be large. Let's see how well this assumption
holds up.

-}

_pnRest :: TurtleParser L.Text
_pnRest = noTrailingDot _pnChars

{-
There are two productions which look like

  ( (parser | '.')* parser )?

Unfortunately one of them has parser returning a Char and the
other has the parser returning multiple characters, so separate
out for now; hopefully can combine

Have decided to try replacing this with sepEndBy1, treating the '.'
as a separator, since this is closer to the EBNF. However, this
then eats up multiple '.' characters.

noTrailingDot ::
  TurtleParser Char -- ^ This *should not* match '.'
  -> TurtleParser L.Text
noTrailingDot p = do
  terms <- sepEndBy1 (many p) (char '.')
  return $ L.pack $ intercalate "." terms

noTrailingDotM ::
  TurtleParser L.Text -- ^ This *should not* match '.'
  -> TurtleParser L.Text
noTrailingDotM p = do
  terms <- sepEndBy1 (many p) (char '.')
  return $ L.intercalate "." $ map L.concat terms

-}

noTrailing ::
  TurtleParser a      -- ^ parser for '.'
  -> ([a] -> String)  -- ^ Collect fragments into a string
  -> TurtleParser a   -- ^ This *should not* match '.'
  -> TurtleParser L.Text
noTrailing dotParser conv parser = do
  lbl <- many (parser <|> dotParser)
  let (nret, lclean) = clean $ conv lbl
      
      -- a simple difference list implementation
      edl = id
      snocdl x xs = xs . (x:)
      appenddl = (.)
      replicatedl n x = (replicate n x ++)
  
      -- this started out as a simple automaton/transducer from
      -- http://www.haskell.org/pipermail/haskell-cafe/2011-September/095347.html
      -- but then I decided to complicate it
      -- 
      clean :: String -> (Int, String)
      clean = go 0 edl
        where
          go n acc [] = (n, acc [])
          go n acc ('.':xs) = go (n + 1) acc xs 
          go 0 acc (x:xs) = go 0 (snocdl x acc) xs
          go n acc (x:xs) = go 0 (appenddl acc (snocdl x (replicatedl n '.'))) xs

  reparse $ L.replicate (fromIntegral nret) "."
  return $ L.pack lclean

noTrailingDot ::
  TurtleParser Char -- ^ This *should not* match '.'
  -> TurtleParser L.Text
noTrailingDot = noTrailing (char '.') id

noTrailingDotM ::
  TurtleParser L.Text -- ^ This *should not* match '.'
  -> TurtleParser L.Text
noTrailingDotM  = noTrailing (char '.' $> ".") (L.unpack . L.concat)

{-
[144s]	LANGTAG	::=	'@' [a-zA-Z]+ ('-' [a-zA-Z0-9]+)*

Note that toLangTag may fail since it does some extra
validation not done by the parser (mainly on the length of the
primary and secondary tags).

NOTE: This parser does not accept multiple secondary tags which RFC3066
does.

-}
_langTag :: TurtleParser LanguageTag
_langTag = do
    ichar '@'
    h <- commit $ many1Satisfy isaZ
    mt <- optional (L.cons <$> char '-' <*> many1Satisfy isaZ09)
    let lbl = L.toStrict $ L.append h $ fromMaybe L.empty mt
    case toLangTag lbl of
        Just lt -> return lt
        _ -> fail ("Invalid language tag: " ++ T.unpack lbl) -- should this be failBad?

-- Returns True for + and False for -.
_leadingSign :: TurtleParser (Maybe Bool)
_leadingSign = do
  ms <- optional (satisfy (`elem` ("+-"::String)))
  return $ (== '+') `fmap` ms

{-
For when we tried to create a canonical representation.
addSign :: Maybe Bool -> L.Text -> L.Text
addSign (Just False) t = L.cons '-' t
addSign _            t = t
-}

addSign :: Maybe Bool -> L.Text -> L.Text
addSign (Just True) t = L.cons '+' t
addSign (Just _)    t = L.cons '-' t
addSign _           t = t

{-
[19]	INTEGER	::=	[+-]? [0-9]+

We try to produce a canonical form for the
numbers.
-}

_integer :: TurtleParser L.Text
_integer = do
  ms <- _leadingSign
  rest <- many1Satisfy is09
  return $ addSign ms rest

{-
[20]	DECIMAL	::=	[+-]? [0-9]* '.' [0-9]+
-}

_decimal :: TurtleParser L.Text
_decimal = do
  ms <- _leadingSign
  leading <- manySatisfy is09
  ichar '.'
  trailing <- many1Satisfy is09
  let ans2 = L.cons '.' trailing
      ans = if L.null leading
            -- then L.cons '0' ans2 -- create a 'canonical' version
            then ans2
            else L.append leading ans2
  return $ addSign ms ans
  
{-
[21]	DOUBLE	::=	[+-]? ([0-9]+ '.' [0-9]* EXPONENT | '.' [0-9]+ EXPONENT | [0-9]+ EXPONENT)

-}
_d1 :: TurtleParser L.Text
_d1 = do
  a <- many1Satisfy is09
  ichar '.'
  b <- manySatisfy is09
  return $ a `L.append` ('.' `L.cons` b)

_d2 :: TurtleParser L.Text
_d2 = do
  ichar '.'
  b <- many1Satisfy is09
  return $ '.' `L.cons` b

_d3 :: TurtleParser L.Text
_d3 = many1Satisfy is09

_double :: TurtleParser L.Text
_double = do
  ms <- _leadingSign
  leading <- _d1 <|> _d2 <|> _d3
  e <- _exponent
  return $ addSign ms $ leading `L.append` e

{-
[154s]	EXPONENT	::=	[eE] [+-]? [0-9]+
-}
_exponent :: TurtleParser L.Text
_exponent = do
  e <- char 'e' <|> char 'E'
  ms <- _leadingSign
  L.cons e . addSign ms <$> _integer

{-
[22]	STRING_LITERAL_QUOTE	::=	'"' ([^#x22#x5C#xA#xD] | ECHAR | UCHAR)* '"'
[23]	STRING_LITERAL_SINGLE_QUOTE	::=	"'" ([^#x27#x5C#xA#xD] | ECHAR | UCHAR)* "'"
[24]	STRING_LITERAL_LONG_SINGLE_QUOTE	::=	"'''" (("'" | "''")? [^'\] | ECHAR | UCHAR)* "'''"
[25]	STRING_LITERAL_LONG_QUOTE	::=	'"""' (('"' | '""')? [^"\] | ECHAR | UCHAR)* '"""'

Since ECHAR | UCHAR is common to all these we pull it out to
create the _protChar parser.
-}

_protChar :: TurtleParser Char
_protChar = char '\\' *> (_echar' <|> _uchar')

_exclSLQ, _exclSLSQ :: String
_exclSLQ = map chr [0x22, 0x5c, 0x0a, 0x0d]
_exclSLSQ = map chr [0x27, 0x5c, 0x0a, 0x0d]

_stringLiteralQuote, _stringLiteralSingleQuote :: TurtleParser L.Text
_stringLiteralQuote = _stringIt dQuot (_tChars _exclSLQ)
_stringLiteralSingleQuote = _stringIt sQuot (_tChars _exclSLSQ)

_stringLiteralLongQuote, _stringLiteralLongSingleQuote :: TurtleParser L.Text
_stringLiteralLongQuote = _stringItLong dQuot3 (_tCharsLong '"')
_stringLiteralLongSingleQuote = _stringItLong sQuot3 (_tCharsLong '\'')

_stringIt :: TurtleParser a -> TurtleParser Char -> TurtleParser L.Text
_stringIt sep chars = L.pack <$> bracket sep sep (many chars)

_stringItLong :: TurtleParser a -> TurtleParser L.Text -> TurtleParser L.Text
_stringItLong sep chars = L.concat <$> bracket sep sep (many chars)

_tChars :: String -> TurtleParser Char
_tChars excl = _protChar <|> noneOf excl

oneOrTwo :: Char -> TurtleParser L.Text
oneOrTwo c = do
  ignore $ char c
  mb <- optional (char c)
  case mb of
    Just _ -> return $ L.pack [c,c]
    _      -> return $ L.singleton c

_multiQuote :: Char -> TurtleParser L.Text
_multiQuote c = do
  mq <- optional (oneOrTwo c)
  r <- noneOf (c : "\\")
  return $ fromMaybe L.empty mq `L.snoc` r
                
_tCharsLong :: Char -> TurtleParser L.Text
_tCharsLong c =
  L.singleton <$> _protChar
  <|> _multiQuote c

{-
[26]	UCHAR	::=	'\u' HEX HEX HEX HEX | '\U' HEX HEX HEX HEX HEX HEX HEX HEX
-}
_uchar :: TurtleParser Char
_uchar = char '\\' >> _uchar'

_uchar' :: TurtleParser Char
_uchar' =
  (char 'u' *> (commit hex4 <? "Expected 4 hex characters after \\u"))
  <|>
  (char 'U' *> (commit hex8 <? "Expected 8 hex characters after \\U"))

{-
[159s]	ECHAR	::=	'\' [tbnrf\"']

Since ECHAR is only used by the string productions
in the form ECHAR | UCHAR, the check for the leading
\ has been moved out (see _protChar)

_echar :: TurtleParser Char
_echar = char '\\' *> _echar'
-}

_echar' :: TurtleParser Char
_echar' = 
  (char 't' $> '\t') <|>
  (char 'b' $> '\b') <|>
  (char 'n' $> '\n') <|>
  (char 'r' $> '\r') <|>
  (char 'f' $> '\f') <|>
  (char '\\' $> '\\') <|>
  (char '"' $> '"') <|>
  (char '\'' $> '\'')

{-
[161s]	WS	::=	#x20 | #x9 | #xD | #xA
-}

_ws :: TurtleParser ()
_ws = ignore $ satisfy (`elem` _wsChars)

_wsChars :: String
_wsChars = map chr [0x20, 0x09, 0x0d, 0x0a]

{-
[162s]	ANON	::=	'[' WS* ']'
-}

_anon :: TurtleParser RDFLabel
_anon =
  br '[' ']' (many _ws) >> newBlankNode

{-
[163s]	PN_CHARS_BASE	::=	[A-Z] | [a-z] | [#x00C0-#x00D6] | [#x00D8-#x00F6] | [#x00F8-#x02FF] | [#x0370-#x037D] | [#x037F-#x1FFF] | [#x200C-#x200D] | [#x2070-#x218F] | [#x2C00-#x2FEF] | [#x3001-#xD7FF] | [#xF900-#xFDCF] | [#xFDF0-#xFFFD] | [#x10000-#xEFFFF]

TODO: may want to make this a Char -> Bool selector for
use with manySatisfy rather than a combinator.
-}

_pnCharsBase :: TurtleParser Char
_pnCharsBase = 
  let f c = let i = ord c
            in isaZ c || 
               match i [(0xc0, 0xd6), (0xd8, 0xf6), (0xf8, 0x2ff),
                        (0x370, 0x37d), (0x37f, 0x1fff), (0x200c, 0x200d),
                        (0x2070, 0x218f), (0x2c00, 0x2fef), (0x3001, 0xd7ff),
                        (0xf900, 0xfdcf), (0xfdf0, 0xfffd), (0x10000, 0xeffff)]
  in satisfy f

{-
[164s]	PN_CHARS_U	::=	PN_CHARS_BASE | '_'
[166s]	PN_CHARS	::=	PN_CHARS_U | '-' | [0-9] | #x00B7 | [#x0300-#x036F] | [#x203F-#x2040]
-}

_pnCharsU, _pnChars :: TurtleParser Char
_pnCharsU = _pnCharsBase <|> char '_'
_pnChars =
  let f c = let i = ord c
            in match i [(0x300, 0x36f), (0x203f, 0x2040)]
  in _pnCharsU <|> char '-' <|> satisfy is09 <|>
     char (chr 0xb7) <|> satisfy f

{-
[167s]	PN_PREFIX	::=	PN_CHARS_BASE ((PN_CHARS | '.')* PN_CHARS)?
[168s]	PN_LOCAL	::=	(PN_CHARS_U | ':' | [0-9] | PLX) ((PN_CHARS | '.' | ':' | PLX)* (PN_CHARS | ':' | PLX))?
-}

_pnPrefix :: TurtleParser L.Text
_pnPrefix = L.cons <$> _pnCharsBase <*> _pnRest

_pnLocal :: TurtleParser L.Text
_pnLocal = do
  s <- L.singleton <$> (_pnCharsU <|> char ':' <|> satisfy is09)
       <|> _plx
  e <- noTrailingDotM (L.singleton <$> (_pnChars <|> char ':') <|> _plx)
  return $ s `L.append` e

{-
[169s]	PLX	::=	PERCENT | PN_LOCAL_ESC
[170s]	PERCENT	::=	'%' HEX HEX
[171s]	HEX	::=	[0-9] | [A-F] | [a-f]
[172s]	PN_LOCAL_ESC	::=	'\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | "'" | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%')

We do not convert hex-encoded values into the characters, which
means we have to deal with Text rather than Char for these
parsers, which is annoying.
-}

_plx, _percent :: TurtleParser L.Text
_plx = _percent <|> (L.singleton <$> _pnLocalEsc)

_percent = do
  ichar '%'
  a <- _hex
  L.cons '%' . L.cons a . L.singleton <$> _hex

_hex, _pnLocalEsc :: TurtleParser Char
_hex = satisfy isHexDigit
_pnLocalEsc = char '\\' *> satisfy (`elem` _pnLocalEscChars)
  
_pnLocalEscChars :: String
_pnLocalEscChars = "_~.-!$&'()*+,;=/?#@%"

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2013, 2014, 2018, 2020, 2022 Douglas Burke
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
