{-# LANGUAGE CPP #-}

module Language.Dot.Parser
  ( parseDot
#ifdef TEST
  , parseGraph
  , parsePort
  , parseCompass
  , parseAttribute
  , parseId
#endif
  )
  where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad       (when)
import Data.Char           (digitToInt, toLower)
import Data.List           (foldl')
import Data.Maybe          (fromJust, fromMaybe, isJust)
import Numeric             (readFloat)

import Text.Parsec
import Text.Parsec.Language
import Text.Parsec.String
import Text.Parsec.Token

import Language.Dot.Syntax

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseDot
  :: String  -- ^ origin of the data, e.g., the name of a file
  -> String  -- ^ DOT source code
  -> Either ParseError Graph
parseDot origin =
    parse (whiteSpace' >> parseGraph) origin . preprocess

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

preprocess :: String -> String
preprocess =
    unlines . map commentPoundLines . lines
  where
    commentPoundLines []         = []
    commentPoundLines line@(c:_) = if c == '#' then "// " ++ line else line

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseGraph :: Parser Graph
parseGraph =
    ( Graph <$>
          parseGraphStrictness
      <*> parseGraphDirectedness
      <*> optionMaybe parseId
      <*> parseStatementList
    )
    <?> "graph"

parseGraphStrictness :: Parser GraphStrictness
parseGraphStrictness =
    ((reserved' "strict" >> return StrictGraph) <|> return UnstrictGraph)
    <?> "graph strictness"

parseGraphDirectedness :: Parser GraphDirectedness
parseGraphDirectedness =
    (   (reserved' "graph"   >> return UndirectedGraph)
    <|> (reserved' "digraph" >> return DirectedGraph)
    )
    <?> "graph directedness"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseStatementList :: Parser [Statement]
parseStatementList =
    braces' (parseStatement `endBy` optional semi')
    <?> "statement list"

parseStatement :: Parser Statement
parseStatement =
    (   try parseEdgeStatement
    <|> try parseAttributeStatement
    <|> try parseAssignmentStatement
    <|> try parseSubgraphStatement
    <|>     parseNodeStatement
    )
    <?> "statement"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseNodeStatement :: Parser Statement
parseNodeStatement =
    ( NodeStatement <$>
      parseNodeId <*> parseAttributeList
    )
    <?> "node statement"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseEdgeStatement :: Parser Statement
parseEdgeStatement =
    ( EdgeStatement <$>
      parseEntityList <*> parseAttributeList
    )
    <?> "edge statement"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseAttributeStatement :: Parser Statement
parseAttributeStatement =
    ( AttributeStatement <$>
      parseAttributeStatementType <*> parseAttributeList
    )
    <?> "attribute statement"

parseAttributeStatementType :: Parser AttributeStatementType
parseAttributeStatementType =
    (   (reserved' "graph" >> return GraphAttributeStatement)
    <|> (reserved' "node"  >> return NodeAttributeStatement)
    <|> (reserved' "edge"  >> return EdgeAttributeStatement)
    )
    <?> "attribute statement type"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseAssignmentStatement :: Parser Statement
parseAssignmentStatement =
    ( AssignmentStatement <$>
      parseId <*> (reservedOp' "=" *> parseId)
    )
    <?> "assignment statement"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseSubgraphStatement :: Parser Statement
parseSubgraphStatement =
    ( SubgraphStatement <$>
       parseSubgraph
    )
    <?> "subgraph statement"

parseSubgraph :: Parser Subgraph
parseSubgraph =
    (   try parseNewSubgraph
    <|>     parseSubgraphRef
    )
    <?> "subgraph"

parseNewSubgraph :: Parser Subgraph
parseNewSubgraph =
    ( NewSubgraph <$>
      (optional (reserved' "subgraph") *> optionMaybe parseId) <*> parseStatementList
    )
    <?> "new subgraph"

parseSubgraphRef :: Parser Subgraph
parseSubgraphRef =
    ( SubgraphRef <$>
      (reserved' "subgraph" *> parseId)
    )
    <?> "subgraph ref"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseEntityList :: Parser [Entity]
parseEntityList =
    ( (:) <$>
      parseEntity True <*> many1 (parseEntity False)
    )
    <?> "entity list"

parseEntity :: Bool -> Parser Entity
parseEntity first =
    (   try (parseENodeId first)
    <|>     parseESubgraph first
    )
    <?> "entity"

parseENodeId :: Bool -> Parser Entity
parseENodeId first =
    ( ENodeId <$>
      (if first then return NoEdge else parseEdgeType) <*> parseNodeId
    )
    <?> "entity node id"

parseESubgraph :: Bool -> Parser Entity
parseESubgraph first =
    ( ESubgraph <$>
      (if first then return NoEdge else parseEdgeType) <*> parseSubgraph
    )
    <?> "entity subgraph"

parseEdgeType :: Parser EdgeType
parseEdgeType =
    (   try (reservedOp' "->" >> return DirectedEdge)
    <|>     (reservedOp' "--" >> return UndirectedEdge)
    )
    <?> "edge operator"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseNodeId :: Parser NodeId
parseNodeId =
    ( NodeId <$>
      parseId <*> optionMaybe parsePort
    )
    <?> "node id"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parsePort :: Parser Port
parsePort =
    (   try parsePortC
    <|>     parsePortI
    )
    <?> "port"

parsePortC :: Parser Port
parsePortC =
    ( PortC <$>
      (colon' *> parseCompass)
    )
    <?> "port (compass variant)"

parsePortI :: Parser Port
parsePortI =
    ( PortI <$>
      (colon' *> parseId) <*> optionMaybe (colon' *> parseCompass)
    )
    <?> "port (id variant)"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseCompass :: Parser Compass
parseCompass =
    (fmap convert identifier' >>= maybe err return)
    <?> "compass"
  where
    err = parserFail "invalid compass value"
    convert =
        flip lookup table . stringToLower
      where
        table =
          [ ("n",  CompassN),  ("e",  CompassE),  ("s",  CompassS),  ("w",  CompassW)
          , ("ne", CompassNE), ("nw", CompassNW), ("se", CompassSE), ("sw", CompassSW)
          ]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseAttributeList :: Parser [Attribute]
parseAttributeList =
    (brackets' (parseAttribute `sepBy` optional comma') <|> return [])
    <?> "attribute list"

parseAttribute :: Parser Attribute
parseAttribute =
    ( do
      id0 <- parseId
      id1 <- optionMaybe (reservedOp' "=" >> parseId)
      return $ maybe (AttributeSetTrue id0) (AttributeSetValue id0) id1
    )
    <?> "attribute"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseId :: Parser Id
parseId =
    (   try parseNameId
    <|> try parseStringId
    <|> try parseFloatId
    <|> try parseIntegerId
    <|>     parseXmlId
    )
    <?> "id"

parseNameId :: Parser Id
parseNameId =
    ( NameId <$>
      identifier'
    )
    <?> "name"

parseStringId :: Parser Id
parseStringId =
    ( StringId <$>
      lexeme' (char '"' *> manyTill stringChar (char '"'))
    )
    <?> "string literal"
  where
    stringChar =
        (try (string "\\\"" >> return '"') <|> noneOf "\"")
        <?> "string character"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

-- | DOT allows floating point numbers having no whole part like @.123@, but
--   Parsec 'float' does not accept them.
parseFloatId :: Parser Id
parseFloatId =
    lexeme'
      ( do s <- parseSign
           l <- fmap (fromMaybe 0) (optionMaybe parseNatural)
           _ <- char '.'
           r <- many1 digit
           maybe err return (make s (show l ++ "." ++ r))
      )
    <?> "float"
  where
    err = parserFail "invalid float value"
    make s f =
        case readFloat f of
          [(v,"")] -> (Just . FloatId . s) v
          _        -> Nothing

parseSign :: (Num a) => Parser (a -> a)
parseSign =
    (   (char '-' >> return negate)
    <|> (char '+' >> return id)
    <|> return id
    )
    <?> "sign"

-- | Non-'lexeme' variant of 'natural' for parsing the natural part of a float.
parseNatural :: Parser Integer
parseNatural =
    (   (char '0' >> return 0)
    <|> (convert <$> many1 digit)
    )
    <?> "natural"
  where
    convert = foldl' (\acc d -> 10 * acc + fromIntegral (digitToInt d)) 0

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseIntegerId :: Parser Id
parseIntegerId =
    ( IntegerId <$>
      integer'
    )
    <?> "integer"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

parseXmlId :: Parser Id
parseXmlId =
    ( XmlId <$>
      angles' parseXml
    )
    <?> "XML id"

parseXml :: Parser Xml
parseXml =
    (   try parseXmlEmptyTag
    <|> try parseXmlTag
    <|>     parseXmlText
    )
    <?> "XML"

parseXmlEmptyTag :: Parser Xml
parseXmlEmptyTag =
    ( XmlEmptyTag <$>
      (char '<' *> parseXmlName) <*> (parseXmlAttributes <* (char '/' >> char '>'))
    )
    <?> "XML empty tag"

parseXmlTag :: Parser Xml
parseXmlTag =
    ( do (name, attributes) <- parseXmlTagOpen
         elements           <- manyTill parseXml (lookAhead (try (parseXmlTagClose (Just name))))
         parseXmlTagClose (Just name)
         return $ XmlTag name attributes elements
    )
    <?> "XML tag"

parseXmlTagOpen :: Parser (XmlName, [XmlAttribute])
parseXmlTagOpen =
    ( (,) <$>
      (char '<' *> parseXmlName) <*> (parseXmlAttributes <* char '>')
    )
    <?> "XML opening tag"

parseXmlTagClose :: Maybe XmlName -> Parser ()
parseXmlTagClose mn0 =
    ( do _  <- char '<'
         _  <- char '/'
         n1 <- parseXmlName
         _  <- char '>'
         when (isJust mn0 && fromJust mn0 /= n1) parserZero
    )
    <?> "XML closing tag " ++ "(" ++ which ++ ")"
  where
    which =
        case mn0 of
          Just (XmlName n) -> "for " ++ show n
          Nothing          -> "any"

parseXmlText :: Parser Xml
parseXmlText =
    ( XmlText <$>
      anyChar `manyTill` lookAhead (   try (parseXmlEmptyTag >> return ())
                                   <|> try (parseXmlTag      >> return ())
                                   <|>      parseXmlTagClose Nothing
                                   )
    )
    <?> "XML text"

parseXmlAttributes :: Parser [XmlAttribute]
parseXmlAttributes =
    many parseXmlAttribute
    <?> "XML attribute list"

parseXmlAttribute :: Parser XmlAttribute
parseXmlAttribute =
    ( XmlAttribute <$>
      (parseXmlName <* reservedOp' "=") <*> parseXmlAttributeValue
    )
    <?> "XML attribute"

parseXmlAttributeValue :: Parser XmlAttributeValue
parseXmlAttributeValue =
    ( XmlAttributeValue <$>
      stringLiteral'
    )
    <?> "XML attribute value"

parseXmlName :: Parser XmlName
parseXmlName =
    ( XmlName <$>
      ((:) <$> c0 <*> (many c1 <* whiteSpace'))
    )
    <?> "XML name"
  where
    c0 = letter   <|> cs
    c1 = alphaNum <|> cs
    cs = oneOf "-.:_"

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

angles'        :: Parser a -> Parser a
braces'        :: Parser a -> Parser a
brackets'      :: Parser a -> Parser a
colon'         :: Parser String
comma'         :: Parser String
identifier'    :: Parser String
integer'       :: Parser Integer
lexeme'        :: Parser a -> Parser a
reserved'      :: String -> Parser ()
reservedOp'    :: String -> Parser ()
semi'          :: Parser String
stringLiteral' :: Parser String
whiteSpace'    :: Parser ()

angles'        = angles        lexer
braces'        = braces        lexer
brackets'      = brackets      lexer
colon'         = colon         lexer
comma'         = comma         lexer
identifier'    = identifier    lexer
integer'       = integer       lexer
lexeme'        = lexeme        lexer
reserved'      = reserved      lexer
reservedOp'    = reservedOp    lexer
semi'          = semi          lexer
stringLiteral' = stringLiteral lexer
whiteSpace'    = whiteSpace    lexer

lexer :: TokenParser ()
lexer =
    makeTokenParser dotDef
  where
    dotDef = emptyDef
      { commentStart    = "/*"
      , commentEnd      = "*/"
      , commentLine     = "//"
      , nestedComments  = True
      , identStart      = letter   <|> char '_'
      , identLetter     = alphaNum <|> char '_'
      , opStart         = oneOf "-="
      , opLetter        = oneOf ""
      , reservedOpNames = ["->", "--", "="]
      , reservedNames   = ["digraph", "edge", "graph", "node", "strict", "subgraph"]
      , caseSensitive   = False
      }

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

stringToLower :: String -> String
stringToLower = map toLower
