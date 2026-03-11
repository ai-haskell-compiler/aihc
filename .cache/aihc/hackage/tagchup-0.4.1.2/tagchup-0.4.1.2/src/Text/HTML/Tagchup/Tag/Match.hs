module Text.HTML.Tagchup.Tag.Match where

import qualified Text.HTML.Tagchup.Tag as Tag
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name


ignore :: a -> Bool
ignore _ = True


-- | match an opening tag
open ::
   (Tag.Name name -> Bool) -> ([Attr.T name string] -> Bool) ->
   Tag.T name string -> Bool
open pName pAttrs (Tag.Open name attrs) = pName name && pAttrs attrs
open _ _ _ = False

-- | match a closing tag
close :: (Tag.Name name -> Bool) -> Tag.T name string -> Bool
close pName (Tag.Close name) = pName name
close _ _ = False

-- | match a text
text :: (string -> Bool) -> Tag.T name string -> Bool
text p (Tag.Text str) = p str
text _ _ = False

comment :: (String -> Bool) -> Tag.T name string -> Bool
comment p (Tag.Comment str) = p str
comment _ _ = False

special :: (Tag.Name name -> Bool) -> (String -> Bool) -> Tag.T name string -> Bool
special pType pInfo (Tag.Special typ info) = pType typ && pInfo info
special _ _ _ = False


-- | match a opening tag's name literally
openLit ::
   (Name.Tag name) =>
   String -> ([Attr.T name string] -> Bool) -> Tag.T name string -> Bool
openLit name = open (Name.match name)

-- | match a closing tag's name literally
closeLit ::
   (Name.Tag name) =>
   String -> Tag.T name string -> Bool
closeLit name = close (Name.match name)

openAttrLit ::
   (Name.Attribute name, Name.Tag name, Eq string) =>
   String -> String -> string -> Tag.T name string -> Bool
openAttrLit name attrName attrValue =
   openLit name (Attr.anyLit attrName attrValue)

{- |
Match a tag with given name, that contains an attribute
with given name, that satisfies a predicate.
If an attribute occurs multiple times,
all occurrences are checked.
-}
openAttrNameLit ::
   (Name.Attribute name, Name.Tag name) =>
   String -> String -> (string -> Bool) -> Tag.T name string -> Bool
openAttrNameLit tagName attrName pAttrValue =
   openLit tagName
      (Attr.any (\(Attr.Cons name value) ->
          Name.match attrName name && pAttrValue value))


-- | Check whether the 'Tag.T' is 'Tag.Open' and matches the given name
openNameLit ::
   (Name.Tag name) =>
   String -> Tag.T name string -> Bool
openNameLit name = openLit name ignore

-- | Check whether the 'Tag.T' is 'Tag.Close' and matches the given name
closeNameLit ::
   (Name.Tag name) =>
   String -> Tag.T name string -> Bool
closeNameLit name = closeLit name



{-
getTagContent :: String -> ([Attr.T name string] -> Bool) -> [Tag.T name string] -> [Tag.T name string]
getTagContent name pAttrs =
   takeWhile (not . tagCloseLit name) . drop 1 .
   head . sections (tagOpenLit name pAttrs)
-}
