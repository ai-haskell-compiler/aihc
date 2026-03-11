module Text.XML.WraXML.String where

import qualified Text.XML.Basic.Character as XmlChar
import Text.XML.WraXML.Utility (compose)

import qualified Data.String.Unicode as Unicode
import qualified Data.Char as Char
import Data.Tuple.HT (mapFst)

import qualified Numeric

import qualified Control.Monad.Exception.Synchronous as Exc
import Control.Monad.Trans.State (State, evalState)
import Control.Applicative (Applicative, liftA)


type T = [Atom]
type EmbeddedExceptions = [Exc.Exceptional String Char]

type Atom = XmlChar.T


-- | should be an abstract type
type Encoding = String
-- | should be [Word8]
type Encoded  = String


{- |
Literal translation from pure strings.
This can only work, if the string does not contain special characters.
-}
fromString :: String -> T
fromString = map XmlChar.fromUnicode

diffFromString :: String -> T -> T
diffFromString =
   flip (foldr (\c -> (XmlChar.fromUnicode c :)))

{- |
default routine
-}
fromUnicodeString :: String -> T
fromUnicodeString = map XmlChar.asciiFromUnicode


toUnicodeStringOrFormat :: T -> String
toUnicodeStringOrFormat =
   flip compose "" .
   map XmlChar.toUnicodeOrFormat

toUnicodeStringEmbedMessage :: T -> String
toUnicodeStringEmbedMessage =
   flip compose "" .
   map
      (Exc.switch (\err ->
         showString "(decoding error: " . showString err . showString ")")
         (:)) .
   toUnicodeStringEmbedException

{- |
Errors in on resolution of references yield undefined elements.
-}
toUnicodeString :: T -> String
toUnicodeString =
   map (Exc.resolve error) . toUnicodeStringEmbedException

toUnicodeStringEmbedException :: T -> EmbeddedExceptions
toUnicodeStringEmbedException =
   map XmlChar.toUnicode


{-# DEPRECATED utf8ToUnicodeString, isoLatin1ToUnicodeString, replaceUTF8ByUnicode, uStringWithErrorsMergePlainChars "XmlChar.Unicode constructors must contain unicode characters and not encoded ones. Decode characters before parsing!" #-}
{- |
Interpret the XML string as mixture of ISO-Latin-1 characters and XML entities
and convert that to a Unicode string.
-}
isoLatin1ToUnicodeString :: T -> String
isoLatin1ToUnicodeString = toUnicodeString

{- |
Interpret the XML string as mixture of UTF-8 characters and XML entities
and convert that to a Unicode string.
-}
utf8ToUnicodeString :: T -> String
utf8ToUnicodeString = toUnicodeString . replaceUTF8ByUnicode


readHex :: (Eq a, Num a) => String -> a
readHex str =
   case Numeric.readHex str of
      [(n,"")] -> n
      _ -> error "readHex: no parse"

{- |
Caution: There is a memory leak for the case that entity references are huge.
-}
parse :: String -> T
parse ('&':'#':'x':xs) =
   parseAux Char.isHexDigit (XmlChar.fromCharRef . readHex) "&#x" xs
parse ('&':'#':xs) =
   parseAux Char.isDigit (XmlChar.fromCharRef . read) "&#" xs
parse ('&':xs) =
   parseAux Char.isAlphaNum XmlChar.fromEntityRef "&" xs
parse (x:xs) = XmlChar.fromUnicode x : parse xs
parse [] = []
-- use unfoldr?

parseAux ::
   (Char -> Bool)         ->
   (String -> XmlChar.T)  ->
   String                 ->
   String                 ->
   T
parseAux check ref prefix xs =
   let (name,rest0) = span check xs
   in  case rest0 of
          ';':rest1 -> ref name : parse rest1
          _ -> map XmlChar.fromUnicode (prefix++name) ++ parse rest0


reduceRefs :: T -> T
reduceRefs = map XmlChar.reduceRef

{- |
Consider the XmlString as a mixture of XML entities and UTF-8 characters.
Replace UTF-8 characters by Unicode representations.
-}
replaceUTF8ByUnicode :: T -> T
replaceUTF8ByUnicode =
   mapUnicodeRuns (fst . Unicode.utf8ToUnicode)

mapUnicodeRuns :: (String -> String) -> T -> T
mapUnicodeRuns f =
   flip compose [] .
   XmlChar.switchUnicodeRuns
--      (\s -> fromString (f s) ++)
      (diffFromString . f)
      ((:) . XmlChar.fromCharRef)
      ((:) . XmlChar.fromEntityRef)



uStringWithErrorsMergePlainChars ::
   EmbeddedExceptions -> [Exc.Exceptional String String]
uStringWithErrorsMergePlainChars =
   foldr (\x ys ->
      case x of
         Exc.Exception err -> Exc.Exception err : ys
         Exc.Success c  ->
            uncurry (:) $
            mapFst Exc.Success $
            mapFst (c:) $
               case ys of
                  Exc.Success cs : ys0 -> (cs, ys0)
                  _ -> ([], ys))
      []


evalDecodeAdaptive ::
   State (Encoded -> String) a -> a
evalDecodeAdaptive =
   flip evalState id


liftFromUnicode :: (String -> String) -> (T -> T)
liftFromUnicode f =
   fromUnicodeString . f . toUnicodeString

liftFromUnicodeA :: Applicative m => (String -> m String) -> (T -> m T)
liftFromUnicodeA f =
   liftA fromUnicodeString . f . toUnicodeString
