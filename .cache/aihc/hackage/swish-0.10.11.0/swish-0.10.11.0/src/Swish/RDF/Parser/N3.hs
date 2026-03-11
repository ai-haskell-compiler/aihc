{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

--------------------------------------------------------------------------------
--  See end of this file for licence information.
--------------------------------------------------------------------------------
-- |
--  Module      :  N3
--  Copyright   :  (c) 2003, Graham Klyne, 2009 Vasili I Galchin, 2011, 2012, 2013, 2014, 2018, 2020 Douglas Burke
--  License     :  GPL V2
--
--  Maintainer  :  Douglas Burke
--  Stability   :  experimental
--  Portability :  CPP, OverloadedStrings
--
--  This Module implements a Notation 3 parser, returning a
--  new 'RDFGraph' consisting of triples and namespace information parsed from
--  the supplied N3 input string, or an error indication.
--
-- REFERENCES:
--
-- - \"Notation3 (N3): A readable RDF syntax\",
--      W3C Team Submission 14 January 2008,
--      <http://www.w3.org/TeamSubmission/2008/SUBM-n3-20080114/>
--
-- - Tim Berners-Lee's design issues series notes and description,
--      <http://www.w3.org/DesignIssues/Notation3.html>
--
-- - Notation 3 Primer by Sean Palmer,
--      <http://www.w3.org/2000/10/swap/Primer.html>
--    
-- NOTES:
--
--  - The parser needs to be updated to the latest version
--    (\"W3C Team Submission 28 March 2011\",
--    <http://www.w3.org/TeamSubmission/2011/SUBM-n3-20110328/>)
--
--  - UTF-8 handling is not really tested.
--
--  - No performance testing has been applied.
--
--  - Not all N3 grammar elements are supported, including:
--
--    - @\@forSome@ (we read it in but ignore the arguments)
--
--    - @\@forAll@  (this causes a parse error)
--
--    - formulae are lightly tested
--
--    - string support is incomplete (e.g. unrecognized escape characters
--      such as @\\q@ are probably handled incorrectly)
--
--------------------------------------------------------------------------------

module Swish.RDF.Parser.N3
    ( ParseResult
    , parseN3      
    , parseN3fromText      
    , parseAnyfromText
    , parseTextFromText, parseAltFromText
    , parseNameFromText -- , parsePrefixFromText
    , parseAbsURIrefFromText, parseLexURIrefFromText, parseURIref2FromText
    
    -- * Exports for parsers that embed Notation3 in a bigger syntax
    , N3Parser, N3State(..), SpecialMap
    
    , getPrefix -- a combination of the old defaultPrefix and namedPrefix productions
    , n3symbol -- replacement for uriRef2 -- TODO: check this is semantically correct      
    , quickVariable -- was varid      
    , lexUriRef       
    , document, subgraph                                                   
    , newBlankNode
    )
where

import Swish.GraphClass (arc)
import Swish.Namespace
    ( Namespace
    , ScopedName
    , makeNamespace
    , getNamespaceTuple
    , getScopeNamespace
    , getScopedNameURI
    , getScopeNamespace
    , makeURIScopedName
    , makeQNameScopedName
    , makeNSScopedName
    , nullScopedName
    )
import Swish.QName (QName, newLName)

import Swish.RDF.Graph
    ( RDFGraph, RDFLabel(..)
    , ToRDFLabel(..)
    , NamespaceMap
    , LookupFormula(..) 
    , addArc 
    , setFormula
    , setNamespaces
    , emptyRDFGraph
    )

import Swish.RDF.Datatype (makeDatatypedLiteral)

import Swish.RDF.Vocabulary
    ( LanguageTag
    , toLangTag
    , rdfType
    , rdfFirst, rdfRest, rdfNil
    , owlSameAs, logImplies
    , xsdBoolean, xsdInteger, xsdDecimal, xsdDouble
    )

import Swish.RDF.Parser.Utils
    ( SpecialMap
    , ParseResult
    , runParserWithError
    -- , mapPrefix
    , prefixTable
    , specialTable
    , ignore
    , notFollowedBy
    , endBy
    , sepEndBy
    -- , manyTill
    , noneOf
    , char
    , ichar
    , string
    , stringT
    , symbol
    , lexeme
    , whiteSpace
    , hex4  
    , hex8  
    , appendURIs
    )

import Control.Applicative
import Control.Monad (forM_, foldM)

import Data.Char (isSpace, isDigit, ord, isAsciiLower)

#if MIN_VERSION_base(4, 7, 0)
import Data.Functor (($>))
#endif

import Data.Maybe (fromMaybe, fromJust)
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

-- | N3 parser state
data N3State = N3State
        { graphState :: RDFGraph            -- Graph under construction
        , thisNode   :: RDFLabel            -- current context node (aka 'this')
        , prefixUris :: NamespaceMap        -- namespace prefix mapping table
        , syntaxUris :: SpecialMap          -- special name mapping table
        , nodeGen    :: Word32              -- blank node id generator
        , keywordsList :: [T.Text]          -- contents of the @keywords statement
        , allowLocalNames :: Bool           -- True if @keywords used so that bare names are QNames in default namespace
        }

-- | Functions to update N3State vector (use with stUpdate)

setPrefix :: Maybe T.Text -> URI -> N3State -> N3State
setPrefix pre uri st =  st { prefixUris=p' }
    where
        p' = M.insert pre uri (prefixUris st)

-- | Set name for special syntax element
setSName :: String -> ScopedName -> N3State -> N3State
setSName nam snam st =  st { syntaxUris=s' }
    where
        s' = M.insert nam snam (syntaxUris st)

setSUri :: String -> URI -> N3State -> N3State
setSUri nam = setSName nam . makeURIScopedName

-- | Set the list of tokens that can be used without needing the leading 
-- \@ symbol.
setKeywordsList :: [T.Text] -> N3State -> N3State
setKeywordsList ks st = st { keywordsList = ks, allowLocalNames = True }

--  Functions to access state:

-- | Get name for special syntax element, default null
getSName :: N3State -> String -> ScopedName
getSName st nam = M.findWithDefault nullScopedName nam $ syntaxUris st

getSUri :: N3State -> String -> URI
getSUri st nam = getScopedNameURI $ getSName st nam

--  Map prefix to URI
getPrefixURI :: N3State -> Maybe T.Text -> Maybe URI
getPrefixURI st pre = M.lookup pre (prefixUris st)

getKeywordsList :: N3State -> [T.Text]
getKeywordsList = keywordsList

getAllowLocalNames :: N3State -> Bool
getAllowLocalNames = allowLocalNames

--  Return function to update graph in N3 parser state,
--  using the supplied function of a graph
--
updateGraph :: (RDFGraph -> RDFGraph) -> N3State -> N3State
updateGraph f s = s { graphState = f (graphState s) }

----------------------------------------------------------------------
--  Define top-level parser function:
--  accepts a string and returns a graph or error
----------------------------------------------------------------------

-- | The N3 parser.
type N3Parser a = Parser N3State a

-- | Parse a string as N3 (with no real base URI).
-- 
-- See 'parseN3' if you need to provide a base URI.
--
parseN3fromText ::
  L.Text -- ^ input in N3 format.
  -> ParseResult
parseN3fromText = flip parseN3 Nothing

-- | Parse a string with an optional base URI.
--            
-- See also 'parseN3fromString'.            
--
parseN3 ::
  L.Text -- ^ input in N3 format.
  -> Maybe QName -- ^ optional base URI
  -> ParseResult
parseN3 txt mbase = parseAnyfromText document mbase txt

{-
-- useful for testing
test :: String -> RDFGraph
test = either error id . parseAnyfromString document Nothing
-}

hashURI :: URI
hashURI = fromJust $ parseURIReference "#"

emptyState :: 
  Maybe QName  -- ^ starting base for the graph
  -> N3State
emptyState mbase = 
  let pmap   = M.singleton Nothing hashURI
      muri   = fmap (makeQNameScopedName Nothing) mbase
      smap   = M.fromList $ specialTable muri
  in N3State
     { graphState = emptyRDFGraph
     , thisNode   = NoNode
     , prefixUris = pmap
     , syntaxUris = smap
     , nodeGen    = 0
     , keywordsList = ["a", "is", "of", "true", "false"] -- not 100% sure about true/false here
     , allowLocalNames = False
     }


-- TODO: change from QName to URI for the base?

-- | Function to supply initial context and parse supplied term.
--
parseAnyfromText :: N3Parser a      -- ^ parser to apply
                    -> Maybe QName  -- ^ base URI of the input, or @Nothing@ to use default base value
                    -> L.Text       -- ^ input to be parsed
                    -> Either String a
parseAnyfromText parser mbase = runParserWithError parser (emptyState mbase)

-- | Create a new blank node.
newBlankNode :: N3Parser RDFLabel
newBlankNode = do
  n <- stQuery (succ . nodeGen)
  stUpdate $ \s -> s { nodeGen = n }
  return $ Blank (show n)
  
--  Test functions for selected element parsing

-- TODO: remove these
  
-- | Used in testing.
parseTextFromText :: String -> L.Text -> Either String String
parseTextFromText s =
    parseAnyfromText (string s) Nothing

-- | Used in testing.
parseAltFromText :: String -> String -> L.Text -> Either String String
parseAltFromText s1 s2 =
    parseAnyfromText (string s1 <|> string s2) Nothing

-- | Used in testing.
parseNameFromText :: L.Text -> Either String String
parseNameFromText =
    parseAnyfromText n3NameStr Nothing

{-
This has been made tricky by the attempt to remove the default list
of prefixes from the starting point of a N3 parse and the subsequent
attempt to add every new namespace we come across to the parser state.

So we add in the original default namespaces for testing, since
this routine is really for testing.
-}

addTestPrefixes :: N3Parser ()
addTestPrefixes = stUpdate $ \st -> st { prefixUris = 
                                         M.fromList 
                                          $ map getNamespaceTuple prefixTable 
                                       } -- should append to existing map

{-
parsePrefixFromText :: L.Text -> Either String URI
parsePrefixFromText =
    parseAnyfromText p Nothing
      where
        p = do
          addTestPrefixes
          pref <- n3Name
          st   <- stGet
          case getPrefixURI st (Just pref) of
            Just uri -> return uri
            _ -> fail $ "Undefined prefix: '" ++ pref ++ "'"
-}

-- | Used in testing.
parseAbsURIrefFromText :: L.Text -> Either String URI
parseAbsURIrefFromText =
    parseAnyfromText explicitURI Nothing

-- | Used in testing.
parseLexURIrefFromText :: L.Text -> Either String URI
parseLexURIrefFromText =
    parseAnyfromText lexUriRef Nothing

-- | Used in testing.
parseURIref2FromText :: L.Text -> Either String ScopedName
parseURIref2FromText = 
    parseAnyfromText (addTestPrefixes *> n3symbol) Nothing

----------------------------------------------------------------------
--  Syntax productions
----------------------------------------------------------------------

-- helper routines

comma, semiColon , fullStop :: N3Parser ()
comma = ignore $ symbol ","
semiColon = ignore $ symbol ";"
fullStop = ignore $ symbol "."

-- a specialization of bracket/between 
br :: String -> String -> N3Parser a -> N3Parser a
br lsym rsym = bracket (symbol lsym) (symbol rsym)

-- to make porting from parsec to polyparse easier
between :: Parser s lbr -> Parser s rbr -> Parser s a -> Parser s a
between = bracket

-- The @ character is optional if the keyword is in the
-- keyword list
--
atSign :: T.Text -> N3Parser ()
atSign s = do
  st <- stGet
  
  let p = ichar '@'
  
  if s `elem` getKeywordsList st
    then ignore $ optional p
    else p
         
atWord :: T.Text -> N3Parser T.Text
atWord s = do
  atSign s
  
  -- TODO: does it really make sense to add the not-followed-by-a-colon rule here?
  -- apply to both cases even though should only really be necessary
  -- when the at sign is not given
  --
  lexeme $ stringT s *> notFollowedBy (== ':')
  return s

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
    mapReplace for the input namespace.
    
-}
operatorLabel :: ScopedName -> N3Parser RDFLabel
operatorLabel snam = do
  st <- stGet
  let (pkey, pval) = getNamespaceTuple $ getScopeNamespace snam
      opmap = prefixUris st
      
      rval = Res snam
      
  -- TODO: the lookup and the replacement could be fused
  case M.lookup pkey opmap of
    Just val | val == pval -> return rval
             | otherwise   -> do
               stUpdate $ \s -> s { prefixUris = M.insert pkey pval opmap }
               return rval
    
    _ -> do
      stUpdate $ \s -> s { prefixUris = M.insert pkey pval opmap }
      return rval
        
{-
Add statement to graph in N3 parser state.

To support literals that are written directly/implicitly - i.e.  as
true/false/1/1.0/1.0e23 - rather than a string with an explicit
datatype we need to special case handling of the object label for
literals. Is this actually needed? The N3 Formatter now doesn't
display the xsd: datatypes on output, but there may be issues with
other formats (e.g RDF/XML once it is supported).

-}

type AddStatement = RDFLabel -> N3Parser ()

addStatement :: RDFLabel -> RDFLabel -> AddStatement
addStatement s p o@(TypedLit _ dtype) | dtype `elem` [xsdBoolean, xsdInteger, xsdDecimal, xsdDouble] = do 
  ost <- stGet
  let stmt = arc s p o
      oldp = prefixUris ost
      ogs = graphState ost
      (ns, uri) = getNamespaceTuple $ getScopeNamespace dtype
      newp = M.insert ns uri oldp
  stUpdate $ \st -> st { prefixUris = newp, graphState = addArc stmt ogs }
addStatement s p o = stUpdate (updateGraph (addArc (arc s p o) ))

addStatementRev :: RDFLabel -> RDFLabel -> AddStatement
addStatementRev o p s = addStatement s p o

{-
A number of productions require a name, which starts with

[A-Z_a-z#x00c0-#x00d6#x00d8-#x00f6#x00f8-#x02ff#x0370-#x037d#x037f-#x1fff#x200c-#x200d#x2070-#x218f#x2c00-#x2fef#x3001-#xd7ff#xf900-#xfdcf#xfdf0-#xfffd#x00010000-#x000effff]

and then has

[\-0-9A-Z_a-z#x00b7#x00c0-#x00d6#x00d8-#x00f6#x00f8-#x037d#x037f-#x1fff#x200c-#x200d#x203f-#x2040#x2070-#x218f#x2c00-#x2fef#x3001-#xd7ff#xf900-#xfdcf#xfdf0-#xfffd#x00010000-#x000effff]*

we encode this as the n3Name production
-}

isaz, is09, isaz09 :: Char -> Bool
isaz = isAsciiLower
is09 = isDigit
isaz09 c = isaz c || is09 c

match :: (Ord a) => a -> [(a,a)] -> Bool
match v = any (\(l,h) -> v >= l && v <= h)

startChar :: Char -> Bool
startChar c = let i = ord c
              in c == '_' || 
                 match c [('A', 'Z'), ('a', 'z')] ||
                 match i [(0x00c0, 0x00d6), (0x00d8, 0x00f6), (0x00f8, 0x02ff), 
                          (0x0370, 0x037d), 
                          (0x037f, 0x1fff), (0x200c, 0x200d), 
                          (0x2070, 0x218f), (0x2c00, 0x2fef), (0x3001, 0xd7ff), 
                          (0xf900, 0xfdcf), (0xfdf0, 0xfffd), 
                          (0x00010000, 0x000effff)]           
  
inBody :: Char -> Bool
inBody c = let i = ord c
           in c `elem` ("-_"::String) || i == 0x007 ||
              match c [('0', '9'), ('A', 'Z'), ('a', 'z')] ||
              match i [(0x00c0, 0x00d6), (0x00d8, 0x00f6), (0x00f8, 0x037d), 
                       (0x037f, 0x1fff), (0x200c, 0x200d), (0x203f, 0x2040), 
                       (0x2070, 0x218f), (0x2c00, 0x2fef), (0x3001, 0xd7ff), 
                       (0xf900, 0xfdcf), (0xfdf0, 0xfffd), 
                       (0x00010000, 0x000effff)]           

-- should this be strict or lazy text?
n3Name :: N3Parser T.Text
n3Name = T.cons <$> n3Init <*> n3Body
  where
    n3Init = satisfy startChar
    n3Body = L.toStrict <$> manySatisfy inBody


n3NameStr :: N3Parser String
n3NameStr = T.unpack <$> n3Name

{-
quickvariable ::=	\?[A-Z_a-z#x00c0-#x00d6#x00d8-#x00f6#x00f8-#x02ff#x0370-#x037d#x037f-#x1fff#x200c-#x200d#x2070-#x218f#x2c00-#x2fef#x3001-#xd7ff#xf900-#xfdcf#xfdf0-#xfffd#x00010000-#x000effff][\-0-9A-Z_a-z#x00b7#x00c0-#x00d6#x00d8-#x00f6#x00f8-#x037d#x037f-#x1fff#x200c-#x200d#x203f-#x2040#x2070-#x218f#x2c00-#x2fef#x3001-#xd7ff#xf900-#xfdcf#xfdf0-#xfffd#x00010000-#x000effff]*
-}

-- TODO: is mapping to Var correct?
-- | Match @?<variable name>@.
quickVariable :: N3Parser RDFLabel
quickVariable = char '?' *> (Var <$> n3NameStr) 

{-
string ::=	("""[^"\\]*(?:(?:\\.|"(?!""))[^"\\]*)*""")|("[^"\\]*(?:\\.[^"\\]*)*")

or

string ::= tripleQuoted | singleQUoted

-}

n3string :: N3Parser T.Text
n3string = tripleQuoted <|> singleQuoted 

{-
singleQuoted ::=  "[^"\\]*(?:\\.[^"\\]*)*"

asciiChars :: String
asciiChars = map chr [0x20..0x7e]

asciiCharsN3 :: String
asciiCharsN3 = filter (`notElem` "\\\"") asciiChars

-}

digit :: N3Parser Char
digit = satisfy isDigit

{-
This is very similar to NTriples accept that also allow the escaping of '
even though it is not required.

The Python rules allow \N{name}, where name is the Unicode name. It's
not clear whether we need to support this too, so for now we do not.

-}
protectedChar :: N3Parser Char
protectedChar =
  (char 't' $> '\t')
  <|> (char 'n' $> '\n')
  <|> (char 'r' $> '\r')
  <|> (char '"' $> '"')
  <|> (char '\'' $> '\'')
  <|> (char '\\' $> '\\')
  <|> (char 'u' *> hex4)
  <|> (char 'U' *> hex8)

-- Accept an escape character or any character as long as it isn't
-- a new-line or quote. Unrecognized escape sequences should therefore
-- be left alone by this. 
--
n3Character :: N3Parser Char
n3Character = 
  (char '\\' *> (protectedChar <|> return '\\'))
  <|> noneOf "\"\n"
      
{-
      <|> (oneOf asciiCharsN3 <?> "ASCII character")
              -- TODO: bodyChar and asciiCharsN3 overlap
      <|> (oneOf bodyChar <?> "Unicode character")
-}              

sQuot :: N3Parser Char
sQuot = char '"'

{-
TODO: there must be a better way of building up the Text
-}

singleQuoted :: N3Parser T.Text
singleQuoted = T.pack <$> bracket sQuot sQuot (many n3Character)
    
{-
tripleQUoted ::=	"""[^"\\]*(?:(?:\\.|"(?!""))[^"\\]*)*"""

The following may not match the output format we now create (with the
move to the Turtle Candidate Recommendation), so re-writing as a test,
but this means pulling in a lot of Turtle productions, which should
be shared.

tripleQuoted = tQuot *> fmap T.pack (manyTill (n3Character <|> sQuot <|> char '\n') tQuot)
  where
    -- tQuot = try (count 3 sQuot)
    tQuot = exactly 3 sQuot
-}
tripleQuoted :: N3Parser T.Text
tripleQuoted =
  let sep = exactly 3 sQuot
  in T.concat <$> bracket sep sep (many _tCharsLong)

{-- Turtle productions: start --}
oneOrTwo :: N3Parser T.Text
oneOrTwo = do
  ignore $ char '"'
  mb <- optional (char '"')
  case mb of
    Just _ -> return "\"\""
    _      -> return "\""

_multiQuote :: N3Parser T.Text
_multiQuote = do
  mq <- optional oneOrTwo
  r <- noneOf "\"\\"
  return $ fromMaybe T.empty mq `T.snoc` r
                
_tCharsLong :: N3Parser T.Text
_tCharsLong =
  T.singleton <$> _protChar
  <|> _multiQuote

_protChar :: N3Parser Char
_protChar = char '\\' *> (_echar' <|> _uchar')

_echar' :: N3Parser Char
_echar' = 
  (char 't' $> '\t') <|>
  (char 'b' $> '\b') <|>
  (char 'n' $> '\n') <|>
  (char 'r' $> '\r') <|>
  (char 'f' $> '\f') <|>
  (char '\\' $> '\\') <|>
  (char '"' $> '"') <|>
  (char '\'' $> '\'')

_uchar' :: N3Parser Char
_uchar' =
  (char 'u' *> commit hex4)
  <|>
  (char 'U' *> commit hex8)

{-- Turtle productions: end --}

getDefaultPrefix :: N3Parser Namespace
getDefaultPrefix = do
  s <- stGet
  case getPrefixURI s Nothing of
    Just uri -> return $ makeNamespace Nothing uri
    _ -> fail "No default prefix defined; how unexpected!"

addBase :: URI -> N3Parser ()
addBase = stUpdate . setSUri "base" 

addPrefix :: Maybe T.Text -> URI -> N3Parser ()
addPrefix p = stUpdate . setPrefix p 

{-|
Update the set of keywords that can be given without
an \@ sign.
-}
updateKeywordsList :: [T.Text] -> N3Parser ()
updateKeywordsList = stUpdate . setKeywordsList

{-
document ::=		|	statements_optional EOF
-}

-- | Process a N3 document, returning a graph.
document :: N3Parser RDFGraph
document = mkGr <$> (whiteSpace *> statementsOptional *> eof *> stGet)
  where
    mkGr s = setNamespaces (prefixUris s) (graphState s)

{-
statements_optional ::=		|	statement  "."  statements_optional
		|	void

-}

statementsOptional :: N3Parser ()
statementsOptional = ignore $ endBy (lexeme statement) fullStop
    
{-
statement ::=		|	declaration
		|	existential
		|	simpleStatement
		|	universal

-}

statement :: N3Parser ()
statement =
  declaration
  <|> existential
  <|> universal
  <|> simpleStatement
  -- having an error here leads to less informative errors in general, it seems
  -- <?> "statement (existential or universal quantification or a simple statement)"
  
{-
declaration ::=		|	 "@base"  explicituri
		|	 "@keywords"  barename_csl
		|	 "@prefix"  prefix explicituri
-}

-- TODO: do we need the try statements here? atWord would need to have a try on '@'
-- (if applicable) which should mean being able to get rid of try
--
declaration :: N3Parser ()
declaration = oneOf [
  atWord "base" >> explicitURI >>= addBase,
  atWord "keywords" >> bareNameCsl >>= updateKeywordsList,
  atWord "prefix" *> getPrefix
  ]

  {-
  (try (atWord "base") >> explicitURI >>= addBase)
  <|>
  (try (atWord "keywords") >> bareNameCsl >>= updateKeywordsList)
  <|>
  (try (atWord "prefix") *> getPrefix)
  -}

-- | Process the remainder of an @\@prefix@ line (after this
-- has been processed). The prefix value and URI are added to the parser
-- state.
getPrefix :: N3Parser ()  
getPrefix = do
  p <- lexeme prefix
  u <- explicitURI
  addPrefix p u

{-
explicituri ::=	<[^>]*>

Note: white space is to be ignored within <>
-}

explicitURI :: N3Parser URI
explicitURI = do
  ignore $ char '<'
  ustr <- manyFinally' ((satisfy isSpace *> next) <|> next) (char '>')
  case parseURIReference ustr of
    Nothing -> failBad $ "Invalid URI: <" ++ ustr ++ ">"
    Just uref -> do
      s <- stGet
      let base = getSUri s "base"
      either fail return $ appendURIs base uref
      
-- production from the old parser; used in SwishScript
-- | An explicitly given URI followed by white space.
lexUriRef :: N3Parser URI
lexUriRef = lexeme explicitURI

{-
barename ::=	[A-Z_a-z#x00c0-#x00d6#x00d8-#x00f6#x00f8-#x02ff#x0370-#x037d#x037f-#x1fff#x200c-#x200d#x2070-#x218f#x2c00-#x2fef#x3001-#xd7ff#xf900-#xfdcf#xfdf0-#xfffd#x00010000-#x000effff][\-0-9A-Z_a-z#x00b7#x00c0-#x00d6#x00d8-#x00f6#x00f8-#x037d#x037f-#x1fff#x200c-#x200d#x203f-#x2040#x2070-#x218f#x2c00-#x2fef#x3001-#xd7ff#xf900-#xfdcf#xfdf0-#xfffd#x00010000-#x000effff]*
barename_csl ::=		|	barename barename_csl_tail
		|	void
barename_csl_tail ::=		|	 ","  barename barename_csl_tail
		|	void
-}

bareNameCsl :: N3Parser [T.Text]
bareNameCsl = sepBy (lexeme bareName) comma

bareName :: N3Parser T.Text
bareName = n3Name 

{-
prefix ::=	([A-Z_a-z#x00c0-#x00d6#x00d8-#x00f6#x00f8-#x02ff#x0370-#x037d#x037f-#x1fff#x200c-#x200d#x2070-#x218f#x2c00-#x2fef#x3001-#xd7ff#xf900-#xfdcf#xfdf0-#xfffd#x00010000-#x000effff][\-0-9A-Z_a-z#x00b7#x00c0-#x00d6#x00d8-#x00f6#x00f8-#x037d#x037f-#x1fff#x200c-#x200d#x203f-#x2040#x2070-#x218f#x2c00-#x2fef#x3001-#xd7ff#xf900-#xfdcf#xfdf0-#xfffd#x00010000-#x000effff]*)?:
-}

prefix :: N3Parser (Maybe T.Text)
prefix = optional (lexeme n3Name) <* char ':'
         

{-
symbol ::=		|	explicituri
		|	qname
symbol_csl ::=		|	symbol symbol_csl_tail
		|	void
symbol_csl_tail ::=		|	 ","  symbol symbol_csl_tail
		|	void

-}

-- | Match a N3 symbol (an explicit URI or a QName)
-- and convert it to a 'ScopedName'.
n3symbol :: N3Parser ScopedName
n3symbol = 
  (makeURIScopedName <$> explicitURI)
  <|> qname

symbolCsl :: N3Parser [ScopedName]
symbolCsl = sepBy (lexeme n3symbol) comma

{-
qname ::=	(([A-Z_a-z#x00c0-#x00d6#x00d8-#x00f6#x00f8-#x02ff#x0370-#x037d#x037f-#x1fff#x200c-#x200d#x2070-#x218f#x2c00-#x2fef#x3001-#xd7ff#xf900-#xfdcf#xfdf0-#xfffd#x00010000-#x000effff][\-0-9A-Z_a-z#x00b7#x00c0-#x00d6#x00d8-#x00f6#x00f8-#x037d#x037f-#x1fff#x200c-#x200d#x203f-#x2040#x2070-#x218f#x2c00-#x2fef#x3001-#xd7ff#xf900-#xfdcf#xfdf0-#xfffd#x00010000-#x000effff]*)?:)?[A-Z_a-z#x00c0-#x00d6#x00d8-#x00f6#x00f8-#x02ff#x0370-#x037d#x037f-#x1fff#x200c-#x200d#x2070-#x218f#x2c00-#x2fef#x3001-#xd7ff#xf900-#xfdcf#xfdf0-#xfffd#x00010000-#x000effff][\-0-9A-Z_a-z#x00b7#x00c0-#x00d6#x00d8-#x00f6#x00f8-#x037d#x037f-#x1fff#x200c-#x200d#x203f-#x2040#x2070-#x218f#x2c00-#x2fef#x3001-#xd7ff#xf900-#xfdcf#xfdf0-#xfffd#x00010000-#x000effff]*

TODO:
  Note that, for now, we explicitly handle blank nodes
  (of the form _:name) direcly in pathItem'.
  This is not a good idea since qname' is used elsewhere
  and so shouldn't we do the same thing there too?
-}

-- for now assume that the parsing rule for the local part
-- will not create an invalid LName.
toName :: Namespace -> T.Text -> ScopedName
toName ns l = 
    case newLName l of
      Just local -> makeNSScopedName ns local
      _ -> error $ "Invalid local name: " ++ T.unpack l

qname :: N3Parser ScopedName
qname = qname1 <|> qname2

qname1 :: N3Parser ScopedName
qname1 = fmap (uncurry toName) (char ':' >> g)
    where
      g = (,) <$> getDefaultPrefix <*> (n3Name <|> return "")
               
qname2 :: N3Parser ScopedName
qname2 = n3Name >>= fullOrLocalQName

fullOrLocalQName :: T.Text -> N3Parser ScopedName
fullOrLocalQName name = 
  (char ':' *> fullQName name)
  <|> localQName name
  
fullQName :: T.Text -> N3Parser ScopedName
fullQName name = toName <$> findPrefix name <*> (n3Name <|> pure "")
  
findPrefix :: T.Text -> N3Parser Namespace
findPrefix pre = do
  st <- stGet
  case M.lookup (Just pre) (prefixUris st) of
    Just uri -> return $ makeNamespace (Just pre) uri
    Nothing  -> failBad $ "Prefix '" ++ T.unpack pre ++ ":' not bound."
  
localQName :: T.Text -> N3Parser ScopedName
localQName name = do
  st <- stGet
  if getAllowLocalNames st
    then let g = (,) <$> getDefaultPrefix <*> pure name
         in uncurry toName <$> g
            
    else fail ("Invalid 'bare' word: " ++ T.unpack name)-- TODO: not ideal error message; can we handle this case differently?

{-
existential ::=		|	 "@forSome"  symbol_csl

For now we just read in the symbols and ignore them,
since we do not mark blank nodes as existentially quantified
(we assume this is the case).

TODO: fix this?
-}

existential :: N3Parser ()
-- existential = try (atWord "forSome") *> symbolCsl >> return ()
existential = (atWord "forSome" *> symbolCsl) $> ()

{-
simpleStatement ::=		|	subject propertylist
-}

simpleStatement :: N3Parser ()
simpleStatement = subject >>= propertyListWith
  
{-
subject ::=		|	expression
-}

subject :: N3Parser RDFLabel
subject = lexeme expression

{-
expression ::=		|	pathitem pathtail
pathtail ::=		|	 "!"  expression
		|	 "^"  expression
		|	void

-}

expression :: N3Parser RDFLabel
expression = do
  i <- pathItem
  
  let backwardExpr = char '!' $> addStatementRev
      forwardExpr  = char '^' $> addStatement
  
  mpt <- optional
        ( (,) <$> lexeme (forwardExpr <|> backwardExpr) <*> lexeme expression )
  case mpt of
    Nothing -> return i 
    Just (addFunc, pt) -> do
      bNode <- newBlankNode
      addFunc bNode pt i
      return bNode
  
{-
pathitem ::=		|	 "("  pathlist  ")" 
		|	 "["  propertylist  "]" 
		|	 "{"  formulacontent  "}" 
		|	boolean
		|	literal
		|	numericliteral
		|	quickvariable
		|	symbol

pathlist ::=		|	expression pathlist
		|	void

Need to think about how to handle formulae, since need to know the context
of the call to know where to add them.

TOOD: may include direct support for blank nodes here,
namely convert _:stringval -> Blank stringval since although
this should be done by symbol the types don't seem to easily match
up (at first blush anyway)
-}

pathItem :: N3Parser RDFLabel
pathItem = 
  br "(" ")" pathList
  <|> br "[" "]" propertyListBNode
  <|> br "{" "}" formulaContent
  -- <|> try boolean
  <|> boolean
  <|> literal
  <|> numericLiteral
  <|> quickVariable
  <|> Blank <$> (string "_:" *> n3NameStr) -- TODO a hack that needs fixing
  <|> Res <$> n3symbol
  
{-  
we create a blank node for the list and return it, whilst
adding the list contents to the graph
-}
pathList :: N3Parser RDFLabel
pathList = do
  cts <- many (lexeme expression)
  eNode <- operatorLabel rdfNil
  case cts of
    [] -> return eNode
      
    (c:cs) -> do
      sNode <- newBlankNode
      first <- operatorLabel rdfFirst
      addStatement sNode first c
      lNode <- foldM addElem sNode cs
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
formulacontent ::=		|	statementlist

statementlist ::=		|	statement statementtail
		|	void
statementtail ::=		|	 "."  statementlist
		|	void
-}

restoreState :: N3State -> N3Parser N3State
restoreState origState = do
  oldState <- stGet
  stUpdate $ const origState { nodeGen = nodeGen oldState }
  return oldState

{-
We create a subgraph and assign it to a blank node, returning the
blank node. At present it is a combination of the subgraph and formula
productions from the origial parser.

TODO: is it correct?
-}
formulaContent :: N3Parser RDFLabel
formulaContent = do
  bNode <- newBlankNode
  pstate <- stGet
  stUpdate $ \st -> st { graphState = emptyRDFGraph, thisNode = bNode }
  statementList
  oldState <- restoreState pstate
  stUpdate $ updateGraph $ setFormula (Formula bNode (graphState oldState))
  return bNode

-- | Process a sub graph and assign it to the given label.  
subgraph :: RDFLabel -> N3Parser RDFGraph
subgraph this = do
  pstate <- stGet
  stUpdate $ \st -> st { graphState = emptyRDFGraph, thisNode = this }
  statementsOptional    -- parse statements of formula
  oldState <- restoreState pstate  
  return $ graphState oldState
  
statementList :: N3Parser ()
statementList = ignore $ sepEndBy (lexeme statement) fullStop

{-
boolean ::=		|	 "@false" 
		|	 "@true" 
-}

boolean :: N3Parser RDFLabel
boolean = makeDatatypedLiteral xsdBoolean <$> 
          (atWord "false" <|> atWord "true")
          -- (try (atWord "false") <|> atWord "true")
           
{-
dtlang ::=		|	 "@"  langcode
		|	 "^^"  symbol
		|	void
literal ::=		|	string dtlang

langcode ::=	[a-z]+(-[a-z0-9]+)*

-}

literal :: N3Parser RDFLabel
literal = do
    lit <- n3string
    opt <- optional dtlang
    return $ case opt of
               Just (Left lcode)  -> LangLit lit lcode
               Just (Right dtype) -> TypedLit lit dtype
               _                  -> Lit lit
  
dtlang :: N3Parser (Either LanguageTag ScopedName)
dtlang = 
  (char '@' *> (Left <$> langcode))
  <|> string "^^" *> (Right <$> n3symbol)

-- Note that toLangTag may fail since it does some extra
-- validation not done by the parser (mainly on the length of the
-- primary and secondary tags).
--
-- NOTE: This parser does not accept multiple secondary tags which RFC3066
-- does.
--
langcode :: N3Parser LanguageTag
langcode = do
    h <- many1Satisfy isaz
    mt <- optional (L.cons <$> char '-' <*> many1Satisfy isaz09)
    let lbl = L.toStrict $ L.append h $ fromMaybe L.empty mt
    case toLangTag lbl of
        Just lt -> return lt
        _ -> fail ("Invalid language tag: " ++ T.unpack lbl) -- should this be failBad?
    
{-
decimal ::=	[-+]?[0-9]+(\.[0-9]+)?
double ::=	[-+]?[0-9]+(\.[0-9]+)?([eE][-+]?[0-9]+)
integer ::=	[-+]?[0-9]+
numericliteral ::=		|	decimal
		|	double
		|	integer

We actually support 1. for decimal values which isn't supported 
by the above production.

TODO: we could convert via something like

  maybeRead value :: Double >>= Just . toRDFLabel

which would mean we store the canonical XSD value in the
label, but it is not useful for the xsd:decimal case
since we currently don't have a Haskell type that
goes with it.
-}

numericLiteral :: N3Parser RDFLabel
numericLiteral =
  -- -- try (makeDatatypedLiteral xsdDouble <$> n3double)
  -- try (d2s <$> n3double)
  -- <|> try (makeDatatypedLiteral xsdDecimal <$> n3decimal)
  d2s <$> n3double
  <|> makeDatatypedLiteral xsdDecimal . T.pack <$> n3decimal
  <|> makeDatatypedLiteral xsdInteger . T.pack <$> n3integer

n3sign :: N3Parser Char
n3sign = char '+' <|> char '-'

n3integer :: N3Parser String
n3integer = do
  ms <- optional n3sign
  ds <- many1 digit
  case ms of
    Just s -> return $ s : ds
    _ -> return ds

n3decimal :: N3Parser String
-- n3decimal = (++) <$> n3integer <*> ( (:) <$> char '.' <*> many1 digit )
n3decimal = (++) <$> n3integer <*> ( (:) <$> char '.' <*> many digit )
           
n3double :: N3Parser String
n3double = (++) 
           <$> n3decimal 
           <*> ( (:) <$> satisfy (`elem` ("eE"::String)) <*> n3integer )

-- Convert a double, as returned by n3double, into it's
-- canonical XSD form. We assume that n3double returns
-- a syntactivally valid Double, so do not bother with reads here
--
d2s :: String -> RDFLabel
d2s s = toRDFLabel (read s :: Double)

{-
propertylist ::=		|	verb object objecttail propertylisttail
		|	void
propertylisttail ::=		|	 ";"  propertylist
		|	void

-}

-- it's probably important that bNode is created *after*
-- processing the plist (mainly for the assumptions made by
-- formatting the output as N3; e.g. list/sequence ordering)
--
propertyListBNode :: N3Parser RDFLabel
propertyListBNode = do
  plist <- sepEndBy ((,) <$> lexeme verb <*> objectList) semiColon
  bNode <- newBlankNode
  let addList ((addFunc,vrb),items) = mapM_ (addFunc bNode vrb) items
  forM_ plist addList
  return bNode

propertyListWith :: RDFLabel -> N3Parser ()
propertyListWith subj = 
  let -- term = lexeme verb >>= objectListWith subj
      term = lexeme verb >>= \(addFunc, vrb) -> objectListWith (addFunc subj vrb)
  in ignore $ sepEndBy term semiColon
  
{-
object ::=		|	expression
objecttail ::=		|	 ","  object objecttail
		|	void

We change the production rule from objecttail to objectlist for lists of
objects (may change back).

-}

object :: N3Parser RDFLabel
object = lexeme expression

objectList :: N3Parser [RDFLabel]
objectList = sepBy1 object comma

objectWith :: AddStatement -> N3Parser ()
objectWith addFunc = object >>= addFunc 

objectListWith :: AddStatement -> N3Parser ()
objectListWith addFunc =
  ignore $ sepBy1 (objectWith addFunc) comma

{-
objectList1 :: N3Parser [RDFLabel]
objectList1 = sepBy1 object comma
-}

{-
verb ::=		|	 "<=" 
		|	 "=" 
		|	 "=>" 
		|	 "@a" 
		|	 "@has"  expression
		|	 "@is"  expression  "@of" 
		|	expression

-}

verb :: N3Parser (RDFLabel -> RDFLabel -> AddStatement, RDFLabel)
verb = 
  -- we check reverse first so that <= is tried before looking for a URI via expression rule
  (,) addStatementRev <$> verbReverse
  <|> (,) addStatement <$> verbForward

-- those verbs for which subject is on the right and object on the left
verbReverse :: N3Parser RDFLabel
verbReverse =
  string "<=" *> operatorLabel logImplies
  <|> between (atWord "is") (atWord "of") (lexeme expression)

{-
  try (string "<=") *> operatorLabel logImplies
  <|> between (try (atWord "is")) (atWord "of") (lexeme expression)
-}

-- those verbs with subject on the left and object on the right
verbForward :: N3Parser RDFLabel
verbForward =  
  -- (try (string "=>") *> operatorLabel logImplies)
  (string "=>" *> operatorLabel logImplies)
  <|> (string "=" *> operatorLabel owlSameAs)
  -- <|> (try (atWord "a") *> operatorLabel rdfType)
  <|> (atWord "a" *> operatorLabel rdfType)
  <|> (atWord "has" *> lexeme expression)
  <|> lexeme expression

{-
universal ::=		|	 "@forAll"  symbol_csl

TODO: what needs to be done to support universal quantification
-}
universal :: N3Parser ()
universal = 
  -- try (atWord "forAll") *> 
  atWord "forAll" *> 
  failBad "universal (@forAll) currently unsupported." 
  -- will be something like: *> symbolCsl

--------------------------------------------------------------------------------
--
--  Copyright (c) 2003, Graham Klyne, 2009 Vasili I Galchin,
--    2011, 2012, 2013, 2014, 2018, 2020 Douglas Burke
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
