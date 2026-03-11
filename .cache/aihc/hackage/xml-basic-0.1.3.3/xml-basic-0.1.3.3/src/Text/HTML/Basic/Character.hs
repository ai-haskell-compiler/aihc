module Text.HTML.Basic.Character (
   T(..), toUnicode, toUnicodeOrFormat,
   fromUnicode, fromCharRef, fromEntityRef,
   maybeUnicode, maybeCharRef, maybeEntityRef,
   isUnicode, isCharRef, isEntityRef, isRef,
   unicode, refC, refE,
   asciiFromUnicode,
   asciiFromUnicodeInternetExploder,
   minimalRefFromUnicode,
   reduceRef,
   validCharRef, switchUnicodeRuns,
   isLower, isUpper, toLower, toUpper,
   ) where

import Text.XML.Basic.Character
   hiding (toUnicode, toUnicodeOrFormat, asciiFromUnicode, reduceRef, )
import qualified Text.HTML.Basic.Entity as Ent
import qualified Data.Char as Char
import qualified Data.Map  as Map
import Data.Maybe (fromMaybe, )

import qualified Control.Monad.Exception.Synchronous as Exc


toUnicode :: T -> Exc.Exceptional String Char
toUnicode = toUnicodeGen Ent.mapNameToChar

toUnicodeOrFormat :: T -> ShowS
toUnicodeOrFormat =
   toUnicodeOrFormatGen Ent.mapNameToChar

{-|
Convert unicode character to XML Char.
If there is a named reference, use this.
If it is ASCII, represent it as Char.
Otherwise use a numeric reference.
-}
asciiFromUnicode :: Char -> T
asciiFromUnicode =
   asciiFromUnicodeGen Ent.mapCharToName

asciiFromUnicodeInternetExploder :: Char -> T
asciiFromUnicodeInternetExploder =
   asciiFromUnicodeGen Ent.mapCharToNameInternetExploder

reduceRef :: T -> T
reduceRef = reduceRefGen Ent.mapNameToChar


isLower :: T -> Bool
isLower =
   Exc.switch (const False) Char.isLower . toUnicode

isUpper :: T -> Bool
isUpper =
   Exc.switch (const False) Char.isUpper . toUnicode


toLower :: T -> T
toLower = lift Char.toLower

toUpper :: T -> T
toUpper = lift Char.toUpper

lift :: (Char -> Char) -> T -> T
lift f x =
   case x of
      Unicode c -> Unicode $ f c
      CharRef n -> CharRef $
         if validCharRef n then Char.ord $ f $ Char.chr n else n
      EntityRef n -> EntityRef $
         fromMaybe n $
         flip Map.lookup Ent.mapCharToName . f
            =<< flip Map.lookup Ent.mapNameToChar n
