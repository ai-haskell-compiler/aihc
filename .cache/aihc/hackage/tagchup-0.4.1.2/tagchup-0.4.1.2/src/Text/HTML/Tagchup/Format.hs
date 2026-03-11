{-|
Convert a tag soup to its text representation
respecting various conventions for merging open and close tags.
-}
module Text.HTML.Tagchup.Format (
   xml, xmlCondensed, html, xhtml, htmlOrXhtml,
   ) where

import qualified Text.HTML.Tagchup.Tag as Tag
import qualified Text.HTML.Basic.Tag      as TagH
import qualified Text.XML.Basic.Name      as Name
import qualified Text.XML.Basic.Format    as Fmt

import Data.List.HT (viewL, )
import Data.Maybe (fromMaybe, )
import Control.Monad (guard, )


{-
*Text.HTML.Tagchup.Format> flip xml "" $ (Text.HTML.TagSoup.HT.Parser.runSoup "<?xml version=1.0 ?>" :: [Tag.T Text.XML.Basic.Name.LowerCase.T String])
-}


{- |
All tags are formatted as they are.
-}
xml :: (Name.Tag name, Name.Attribute name, Fmt.C string) =>
   [Tag.T name string] -> ShowS
xml = Fmt.many Fmt.run


{- |
Adjacent corresponding open and close tags are merged to a self-closing tag.
E.g. @\<a\>\<\/a\>@ becomes @\<a\/\>@.
-}
xmlCondensed :: (Name.Tag name, Name.Attribute name, Fmt.C string) =>
   [Tag.T name string] -> ShowS
xmlCondensed = xmlCondensedGen (==)

{- |
All tags that are defined being self-closing by the HTML standard
are formatted only as open tag.
E.g. @\<br\>@.
-}
html :: (Name.Tag name, Name.Attribute name, Fmt.C string) =>
   [Tag.T name string] -> ShowS
html =
   Fmt.many Fmt.run .
   filter (maybe True (not . TagH.isEmpty) . Tag.maybeClose)

{- |
All tags that are defined being self-closing by the XHTML standard
are formatted as self-closing open tag.
E.g. @\<br\/\>@.
-}
xhtml :: (Name.Tag name, Name.Attribute name, Fmt.C string) =>
   [Tag.T name string] -> ShowS
xhtml =
   xmlCondensedGen
      -- e.g. <div></div> must not be merged to <div/>
      (\nameOpen nameClose ->
          nameOpen==nameClose && TagH.isEmpty nameOpen)

{- |
If the first tag is @\<?xml ...?\>@ then format in XHTML style,
else in HTML style.
-}
htmlOrXhtml :: (Name.Tag name, Name.Attribute name, Fmt.C string) =>
   [Tag.T name string] -> ShowS
htmlOrXhtml tags =
   fromMaybe (html tags) $
      do (tag,_) <- viewL tags
         (name,_) <- Tag.maybeProcessing tag
         guard (Name.match "xml" name)
         return (xhtml tags)


xmlCondensedGen :: (Name.Tag name, Name.Attribute name, Fmt.C string) =>
   (Tag.Name name -> Tag.Name name -> Bool) ->
   [Tag.T name string] -> ShowS
xmlCondensedGen check =
   let recourse (Tag.Open nameOpen attrs : Tag.Close nameClose : ts) =
          (if check nameOpen nameClose
             then Tag.formatOpen True  nameOpen attrs
             else Tag.formatOpen False nameOpen attrs .
                  Tag.formatClose nameClose)
          . recourse ts
       recourse (t : ts) = Fmt.run t . recourse ts
       recourse [] = id
   in  recourse
