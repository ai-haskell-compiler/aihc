module XmlParser
  ( -- * Execution
    parseByteString,
    parseLazyByteString,
    parseFile,
    parseElementAst,

    -- * Parsers by context

    -- ** Element
    AstParser.Element,
    AstParser.elementName,
    AstParser.elementNameIs,
    AstParser.children,
    AstParser.childrenByName,
    AstParser.attributesByName,
    AstParser.astElement,

    -- ** Nodes
    AstParser.Nodes,
    AstParser.elementNode,
    AstParser.contentNode,

    -- ** ByName
    AstParser.ByName,
    AstParser.byName,

    -- ** Content
    AstParser.Content,
    AstParser.textContent,
    AstParser.narrowedContent,
    AstParser.refinedContent,
    AstParser.enumContent,
    AstParser.attoparsedContent,
    AstParser.qNameContent,
  )
where

import qualified Data.ByteString.Lazy as Lbs
import qualified Text.XML as XmlConduit
import qualified XmlParser.AstParser as AstParser
import XmlParser.Prelude
import qualified XmlParser.XmlConduitWrapper as XmlConduitWrapper

-- |
-- Parse XML bytestring.
parseByteString :: AstParser.Element a -> ByteString -> Either Text a
parseByteString astParser input =
  XmlConduitWrapper.parseByteString input >>= parseDocumentAst astParser

-- |
-- Parse XML lazy bytestring.
parseLazyByteString :: AstParser.Element a -> Lbs.ByteString -> Either Text a
parseLazyByteString astParser input =
  XmlConduitWrapper.parseLazyByteString input >>= parseDocumentAst astParser

-- |
-- Parse XML file.
parseFile :: AstParser.Element a -> FilePath -> IO (Either Text a)
parseFile astParser path =
  fmap (>>= parseDocumentAst astParser)
    $ XmlConduitWrapper.parseFile path

parseDocumentAst :: AstParser.Element a -> XmlConduit.Document -> Either Text a
parseDocumentAst astParser =
  parseElementAst astParser . XmlConduit.documentRoot

-- |
-- Parse an \"xml-conduit\" element AST.
parseElementAst :: AstParser.Element a -> XmlConduit.Element -> Either Text a
parseElementAst astParser =
  first AstParser.renderElementError . AstParser.parseElement astParser
