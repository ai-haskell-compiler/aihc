module Text.HTML.WraXML.Tree.HXT
   (tidy, format, fromHTMLString, fromHTMLStringMetaEncoding,
    errorAnnFromHTMLStringMetaEncoding,
    errorAnnFromHTMLStringOpt, ErrorMsg,
    getMetaHTTPHeaders, ) where


import qualified Text.XML.HXT.DOM.XmlNode as XmlNode
import qualified Text.XML.HXT.Arrow.XmlState.ErrorHandling as HXTError
import qualified Text.XML.HXT.Core as HXT
import Text.XML.HXT.Core (withInputEncoding, isoLatin1, yes)
import Text.XML.HXT.Arrow.XmlState (runX)

import Text.XML.HXT.DOM.QualifiedName (QName, qualifiedName)
import Text.XML.HXT.DOM.TypeDefs (XNode(..))
import Text.XML.HXT.DOM.ShowXml (xshow)

import qualified Data.Tree.Class as HXTTree
import Data.Tree.NTree.TypeDefs (NTree(NTree))

import qualified Text.XML.WraXML.Tree.HXT as WraHXT

import qualified Text.HTML.WraXML.Tree   as HtmlTree
import qualified Text.XML.WraXML.Tree    as XmlTree

import qualified Text.XML.Basic.Name.LowerCase as NameLC
import qualified Text.XML.Basic.Name as Name

import qualified Text.XML.Basic.Format    as Format

import Text.XML.HXT.Parser.HtmlParsec (isEmptyHtmlTag)

import Control.Category ((>>>))
import Control.Monad (msum)
import Data.Maybe (fromMaybe)



{- |
Tidy a piece of HTML code.
&   ->   &amp;
<   ->   &lt;
unquoted tag attribute values: size=-1   ->   size="-1"
insert omitted closing tags
-}
tidy :: String -> IO String
tidy input =
   fmap (\[str] -> str) $
   runX $
      ioSLAFromIO (fromHTMLString input)
      >>>
      HXT.writeDocumentToString [HXT.withOutputHTML]

ioSLAFromIO :: IO b -> HXT.IOSLA s a b
ioSLAFromIO act = HXT.IOSLA $ \s _ -> fmap (\b -> (s,[b])) act


-- * formatting

{- |
Like 'Text.XML.HXT.DOM.XmlTreeFunctions.xshow'
but it shows empty tags the HTML way.
E.g. it emits @<br>@ instead of @<br\/>@,
@<noscript><\/noscript>@ instead of @<noscript\/>@.
Many browsers prefer that.
-}
format :: HXT.XmlTree -> String
format leaf = formatTrees (HXTTree.getChildren leaf) ""

formatTrees :: HXT.XmlTrees -> ShowS
formatTrees = foldr (.) id . map formatTree


-- cf. src/Text/XML/HXT/DOM/XmlTreeFunctions.hs
formatTree :: HXT.XmlTree -> ShowS
formatTree leaf =
   case leaf of
      (NTree (XPi n al) _) ->
         showString "<?"
         .
         formatQName n
         .
         (foldr (.) id . map showPiAttr) al
         .
         showString "?>"
           where
             showPiAttr :: HXT.XmlTree -> String -> String
             showPiAttr a@(NTree (XAttr an) cs) =
                 if qualifiedName an == HXT.a_value
                   then Format.blank . formatTrees cs
                   else formatTree a
             showPiAttr _ = id
      (NTree (XTag t al) cs) ->
         if null cs && isEmptyHtmlTag (qualifiedName t)
           then Format.lt . formatQName t . formatTrees al . Format.gt
           else Format.lt . formatQName t . formatTrees al . Format.gt
                 . formatTrees cs
                 . Format.lt . Format.slash . formatQName t . Format.gt
      (NTree (XAttr an) cs) ->
         Format.blank . formatQName an . Format.eq .
         Format.stringQuoted (formatTrees cs "")
      (NTree (XError l e) _) ->
         showString "<!-- ERROR (" . showString (show l) . showString "):\n"
          . showString e . showString "\n-->"
      _ -> (xshow [leaf] ++) -- showXmlTree leaf


formatQName :: QName -> ShowS
formatQName = showString . qualifiedName


-- * parsing and encoding

{- |
Search for a META tag for the encoding of the HTML text.
-}
findMetaEncoding :: String -> IO (Maybe String)
findMetaEncoding str =
   do htmlTrees <- xmlTreesFromHTMLString str
      return (msum (map HtmlTree.findMetaEncoding
                           (htmlTrees :: [XmlTree.T () NameLC.T String])))

getMetaHTTPHeaders :: String -> IO [(String, String)]
getMetaHTTPHeaders str =
   do htmlTrees <- xmlTreesFromHTMLString str
      return (concatMap HtmlTree.getMetaHTTPHeaders
                           (htmlTrees :: [XmlTree.T () NameLC.T String]))

xmlTreesFromHTMLString ::
   (Name.Tag name, Name.Attribute name) =>
   String -> IO [XmlTree.T () name String]
xmlTreesFromHTMLString str =
   do hxtTree <- fromHTMLString str
      -- it will hopefully be only one HTML tree
      return $
         map (XmlTree.unescape . WraHXT.toXmlTree)
             (filter (WraHXT.checkTagName "html" . HXTTree.getNode)
                     (HXTTree.getChildren hxtTree))


{- |
Guess the encoding from the META-HTTP-EQUIV attribute, if available.
Otherwise fall back to ISO-Latin-1.
-}
fromHTMLStringMetaEncoding :: String -> IO HXT.XmlTree
fromHTMLStringMetaEncoding str =
   do enc <- findMetaEncoding str
      fromHTMLStringOpt [withInputEncoding $ fromMaybe isoLatin1 enc] str

{-
With no encoding option given,
utf8ToUnicode fails when trying
to interpret ISO-Latin characters as UTF-8 characters.
-}
fromHTMLString :: String -> IO HXT.XmlTree
fromHTMLString = fromHTMLStringOpt [withInputEncoding isoLatin1]

fromHTMLStringOpt :: HXT.SysConfigList -> String -> IO HXT.XmlTree
fromHTMLStringOpt options input =
   do (tree,_,_) <- errorAnnFromHTMLStringOpt options input
      return tree

type ErrorMsg = (Int,String)

{-# WARNING errorAnnFromHTMLStringMetaEncoding
      "error collection does not work currently" #-}
errorAnnFromHTMLStringMetaEncoding ::
   String -> IO (HXT.XmlTree, [ErrorMsg], Maybe Int)
errorAnnFromHTMLStringMetaEncoding str = do
   enc <- findMetaEncoding str
   errorAnnFromHTMLStringOpt [withInputEncoding $ fromMaybe isoLatin1 enc] str


{- |
In earlier version I managed to obtain the error messages.
This does not work anymore and do not know how to achieve this.
-}
{-# WARNING errorAnnFromHTMLStringOpt
      "error collection does not work currently" #-}
errorAnnFromHTMLStringOpt ::
   HXT.SysConfigList -> String -> IO (HXT.XmlTree, [ErrorMsg], Maybe Int)
errorAnnFromHTMLStringOpt = errorAnnFromHTMLStringInternal

{- |
Adaption of Text.XML.HXT.Parser.MainFunctions.getXmlDocument
-}
errorAnnFromHTMLStringInternal ::
   HXT.SysConfigList -> String -> IO (HXT.XmlTree, [ErrorMsg], Maybe Int)
errorAnnFromHTMLStringInternal options contents = do
   (root_:errs) <-
      runX $
         HXT.errorMsgCollect
         >>>
         HXT.readString (HXT.withParseHTML yes : options) contents
         >>>
         HXTError.filterErrorMsg
   let elvl = XmlNode.getErrorLevel root_
   let errMsgs =
         map ((\(XError level msg) -> (level, msg)) . HXTTree.getNode) errs
   return (root_, errMsgs, elvl)


{-
putStr . xshow =<< run' (Text.XML.HXT.Parser.MainFunctions.parseDocument [(a_source,"lousy.html"), (a_parse_html,v_1)] emptyRoot)

readFile "lousy.html" >>= tidy >>= putStr
-}
