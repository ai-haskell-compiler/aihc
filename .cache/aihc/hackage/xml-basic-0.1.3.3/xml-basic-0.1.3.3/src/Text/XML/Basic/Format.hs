module Text.XML.Basic.Format where

import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.Entity as Ent
import qualified Data.Map as Map

import Prelude hiding (quot)


class C object where
   run :: object -> ShowS

instance C Char where
   run c =
      maybe
         (showChar c)
         (\n -> amp . showString n . semicolon) $
      Map.lookup c Ent.mapCharToName

{- that one causes cyclic module dependencies
   run = run . XMLChar.minimalRefFromUnicode
-}
{- that one is too simple - we always have to emit valid XML code
   run = showChar
-}

instance C object => C [object] where
   run = many run


{-
adapted from HXT

import Text.XML.HXT.DOM.ShowXml
   (showBlank, showQuoteString,
    showEq, showLt, showGt, showSlash)
-}

nl, blank,
  eq, lt, gt, slash, amp, sharp, colon, semicolon,
  apos, quot, lpar, rpar, exclam, quest :: ShowS

nl          = showChar '\n'
blank       = showChar ' '
eq          = showChar '='
lt          = showChar '<'
gt          = showChar '>'
slash       = showChar '/'
amp         = showChar '&'
sharp       = showChar '#'
colon       = showChar ':'
semicolon   = showChar ';'
apos        = showChar '\''
quot        = showChar '\"'
lpar        = showChar '('
rpar        = showChar ')'
exclam      = showChar '!'
quest       = showChar '?'


angle :: ShowS -> ShowS
angle s = lt . s . gt

{- |
Internet Explorer does not recognize &apos;
and thus we have to format it literally.
-}
stringQuoted :: String -> ShowS
stringQuoted s =
   if elem '\'' s
     then quot . showString s . quot
     else apos . showString s . apos

name :: Name.C name => name -> ShowS
name n =
   showString (Name.toString n)

many :: (a -> ShowS) -> [a] -> ShowS
many s = foldr (.) id . map s
