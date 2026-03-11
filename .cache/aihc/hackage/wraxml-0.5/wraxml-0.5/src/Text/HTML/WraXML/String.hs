module Text.HTML.WraXML.String where

import qualified Text.HTML.Basic.Character as HtmlChar
import qualified Text.XML.WraXML.String as XmlString
import Text.XML.WraXML.Utility (compose)

import qualified Data.String.Unicode as Unicode
import qualified Data.Char as Char

import qualified Control.Monad.Exception.Synchronous as Exc


type T = [Atom]

type Atom = HtmlChar.T


{- |
Literal translation from pure strings.
This can only work, if the string does not contain special characters.
-}
fromString :: String -> T
fromString = map HtmlChar.fromUnicode

{- |
default routine
-}
fromUnicodeString :: String -> T
fromUnicodeString = map HtmlChar.asciiFromUnicode

fromUnicodeStringInternetExploder :: String -> T
fromUnicodeStringInternetExploder =
   map HtmlChar.asciiFromUnicodeInternetExploder


toUnicodeStringOrFormat :: T -> String
toUnicodeStringOrFormat =
   flip compose "" .
   map HtmlChar.toUnicodeOrFormat

toUnicodeString :: T -> String
toUnicodeString =
   map (Exc.resolve error . HtmlChar.toUnicode)


{- |
Decode plain characters using the given decoder,
and decode entities by HXT's XML entity table.
Decoding errors for both conversions are embedded where they occur.
-}
toUnicodeStringDecodingEmbedError ::
   Unicode.DecodingFctEmbedErrors -> T -> XmlString.EmbeddedExceptions
toUnicodeStringDecodingEmbedError f =
   concat .
   HtmlChar.switchUnicodeRuns
      (map Exc.fromEither . f)
      ((:[]) . HtmlChar.toUnicode . HtmlChar.fromCharRef)
      ((:[]) . HtmlChar.toUnicode . HtmlChar.fromEntityRef)



{- |
Convert characters to lower case.
This uses ISO latin encoding and may fail for exotic characters.
-}
toLower :: T -> T
toLower = map HtmlChar.toLower

toUpper :: T -> T
toUpper = map HtmlChar.toUpper


toCanonicalUnicodeString :: T -> String
toCanonicalUnicodeString = map Char.toLower . toUnicodeString

equalIgnoreCase :: T -> T -> Bool
equalIgnoreCase x y =
   toCanonicalUnicodeString x == toCanonicalUnicodeString y

elemIgnoreCase :: T -> [T] -> Bool
elemIgnoreCase x ys =
   elem (toCanonicalUnicodeString x) (map toCanonicalUnicodeString ys)
