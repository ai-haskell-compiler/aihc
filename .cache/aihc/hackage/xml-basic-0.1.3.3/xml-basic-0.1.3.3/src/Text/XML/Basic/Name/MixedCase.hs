{- |
This name type preserves the characters case of its input.
This is the right choice for case-sensitive names (XML)
or if you like to preserve case of HTML tags.
In the latter case it is however more difficult to match tag names.
-}
module Text.XML.Basic.Name.MixedCase where

import qualified Text.XML.Basic.Name as Name


newtype T = Cons String
   deriving (Eq, Ord)

instance Show T where
   showsPrec p (Cons s) = showsPrec p s

instance Name.Tag T where
   tagFromString = Cons
   tagToString (Cons s) = s

instance Name.Attribute T where
   attributeFromString = Cons
   attributeToString (Cons s) = s
