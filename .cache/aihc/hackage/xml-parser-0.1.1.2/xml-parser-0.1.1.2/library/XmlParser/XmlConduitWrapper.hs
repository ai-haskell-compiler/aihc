-- |
-- A minimal wrapper over xml-conduit parsing API bringing it to our standards.
module XmlParser.XmlConduitWrapper
  ( parseByteString,
    parseLazyByteString,
    parseFile,
  )
where

import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Text.XML as XmlConduit
import qualified Text.XML.Unresolved as XmlConduit (InvalidEventStream (..))
import XmlParser.Prelude

parseByteString :: ByteString -> Either Text XmlConduit.Document
parseByteString =
  parseLazyByteString . LazyByteString.fromStrict

parseLazyByteString :: LazyByteString.ByteString -> Either Text XmlConduit.Document
parseLazyByteString input =
  first renderError (XmlConduit.parseLBS settings input)

parseFile :: FilePath -> IO (Either Text XmlConduit.Document)
parseFile path =
  tryMapping renderError (XmlConduit.readFile settings path)

settings :: XmlConduit.ParseSettings
settings =
  XmlConduit.def
    { XmlConduit.psRetainNamespaces = True
    }

renderError :: SomeException -> Text
renderError e
  | Just e <- fromException @XmlConduit.XMLException e =
      fromString (show e)
  | Just e <- fromException @XmlConduit.InvalidEventStream e =
      fromString (show e)
  | Just (XmlConduit.UnresolvedEntityException e) <- fromException @XmlConduit.UnresolvedEntityException e =
      "Unresolved entities: " <> Text.intercalate "," (toList e)
  | otherwise =
      -- FIXME: Find other cases and do something more user-friendly about them
      fromString (show e)
