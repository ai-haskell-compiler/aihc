{- |
This name uses only lowercase characters as canonical representation,
except for @DOCTYPE@ and @CDATA@.
This is optimal for processing HTML which is case-insensitiv.
-}
module Text.XML.Basic.Name.LowerCase where

import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.Tag  as Tag
import Data.Char (toLower, toUpper, )


newtype T = Cons String
   deriving (Eq, Ord)


instance Show T where
   showsPrec p (Cons s) = showsPrec p s

instance Name.Tag T where
   tagFromString x = Cons $
      let xu = map toUpper x
      in  if elem xu $ [Tag.doctypeString, Tag.cdataString]
            then xu
            else map toLower x
   tagToString (Cons s) = s

instance Name.Attribute T where
   attributeFromString = Cons . map toLower
   attributeToString (Cons s) = s
