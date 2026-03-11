module Iri.Optics.Defs where

import qualified Data.Text.Encoding as Text
import Iri.Data
import Iri.Optics.Basics
import qualified Iri.Parsing.ByteString as B
import qualified Iri.Parsing.Text as D
import Iri.Prelude
import qualified Iri.Rendering.ByteString as A
import qualified Iri.Rendering.Text as C

-- * Definitions by source

-------------------------

-- ** Text

-------------------------

textIriIri :: Prism' Text Iri
textIriIri =
  prism C.iri (\text -> either (const (Left text)) Right (D.iri text))

textIriHttpIri :: Prism' Text HttpIri
textIriHttpIri =
  textIriIri . iriHttpIri

-- ** ByteString

-------------------------

byteStringIri :: Prism' ByteString Iri
byteStringIri =
  prism A.uri (\bytes -> either (const (Left bytes)) Right (B.uri bytes))

byteStringHttpIri :: Prism' ByteString HttpIri
byteStringHttpIri =
  byteStringIri . iriHttpIri

byteStringTextInUtf8 :: Prism' ByteString Text
byteStringTextInUtf8 = prism' Text.encodeUtf8 (either (const Nothing) Just . Text.decodeUtf8')

-- ** IRI

-------------------------

iriHttpIri :: Prism' Iri HttpIri
iriHttpIri = prism' iriFromHttpIri (either (const Nothing) Just . httpIriFromIri)

iriScheme :: Lens' Iri Scheme
iriScheme = lens (\(Iri x _ _ _) -> x) (\(Iri _ hierarchy query fragment) x -> Iri x hierarchy query fragment)

iriHierarchy :: Lens' Iri Hierarchy
iriHierarchy = lens (\(Iri _ x _ _) -> x) (\(Iri scheme _ query fragment) x -> Iri scheme x query fragment)

iriQuery :: Lens' Iri Query
iriQuery = lens (\(Iri _ _ x _) -> x) (\(Iri scheme hierarchy _ fragment) x -> Iri scheme hierarchy x fragment)

iriFragment :: Lens' Iri Fragment
iriFragment = lens (\(Iri _ _ _ x) -> x) (\(Iri scheme hierarchy query _) x -> Iri scheme hierarchy query x)

-- ** Scheme

-------------------------

schemeByteString :: Lens' Scheme ByteString
schemeByteString = lens (\(Scheme x) -> x) (const Scheme)

-- ** Fragment

-------------------------

fragmentByteString :: Lens' Fragment ByteString
fragmentByteString = lens (\(Fragment x) -> x) (const Fragment)
