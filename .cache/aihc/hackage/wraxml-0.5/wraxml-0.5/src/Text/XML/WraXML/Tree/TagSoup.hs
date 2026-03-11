{- |
Very lazy HTML tree parser using TagSoup as lexer.
-}
module Text.XML.WraXML.Tree.TagSoup where

import qualified Text.HTML.TagSoup as Tag
import qualified Text.HTML.TagSoup as Match -- TagSoup.Match became hidden in tagsoup-0.8
import Text.HTML.TagSoup (Tag(..), )

import qualified Text.XML.WraXML.Tree.Literal as XmlTree

import qualified Text.HTML.Basic.Tag as TagH
import qualified Text.XML.Basic.Position as Position
import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.ProcessingInstruction as PI

-- import qualified Text.XML.WraXML.String         as XmlString
-- import qualified Text.XML.WraXML.String.TagSoup as XmlStringTagSoup

import qualified Text.XML.Basic.Name.LowerCase as NameLC
import qualified Text.XML.Basic.Name           as Name

import qualified Text.XML.WraXML.Tree.LazyParser as ParserU
import qualified Text.ParserCombinators.Poly.Lazy as Parser

import Data.Bool.HT (select, )
import Control.Monad (liftM, )
import qualified Data.List as List


type PosTag = (Position.T, Tag String)
-- data PosTag = PosTag !Position.T Tag


defaultFilename :: String
defaultFilename = "input"

attachPos :: [Tag String] -> [PosTag]
attachPos =
   snd .
   List.mapAccumL
      (\pos t ->
          let newPos =
                  case t of
                     Tag.TagPosition setRow setColumn ->
                          Position.new defaultFilename setRow setColumn
                     _ -> pos
          in  (newPos, (newPos, t)))
--          in  (newPos, PosTag newPos t))
      (Position.initialize defaultFilename)

removeMetaPos :: [PosTag] -> [PosTag]
removeMetaPos =
   filter (\(_pos,tag) -> case tag of
               TagPosition _ _ -> False
               _ -> True)


type XmlTree name string = XmlTree.T Position.T name string

{- |
A TagSoup could represent multiple HTML trees,
e.g. with some introducing comments.
-}
{-
toXmlTrees ::
   [Tag XmlChar] -> [XmlTree name String]
toXmlTrees =
   toXmlTreesAux .
   Tag.canonicalizeTags
-}

toXmlTreesString ::
   (Name.Tag name, Name.Attribute name) =>
   [Tag String] -> [XmlTree name String]
toXmlTreesString =
   toXmlTreesAux .
   removeMetaPos .
   attachPos .
   Tag.canonicalizeTags  -- must remain for Match.tagCloseLit
--   toXmlTrees


toXmlTreesAux ::
   (Name.Tag name, Name.Attribute name) =>
   [PosTag] -> [XmlTree name String]
toXmlTreesAux =
   fst . Parser.runParser (ParserU.manyLazy parseBranch)



type Parser i a = Parser.Parser (i, Tag String) a


parseBranch ::
   (Name.Tag name, Name.Attribute name) =>
   Parser i (XmlTree.T i name String)
parseBranch =
   do (ti,t) <- Parser.next
      case t of
         TagText text -> return (XmlTree.literalIndex ti text)
         TagOpen ('?':target) attrs ->
            return $
               XmlTree.processingIndex ti target $
               case attrs of
                  [("",str)] ->
                     PI.Unknown str
                  _ ->
                     PI.Known $
                     map (uncurry Attr.new) attrs
         TagOpen "![CDATA[" [("",str)] ->
            return (XmlTree.cdataIndex ti str)
         TagOpen name attrs ->
            liftM
               (XmlTree.tagIndexAttr ti name attrs)
               (parseSubTrees
                  (XmlTree.warningIndex ti
                      "unexpected end of file") (Name.fromString name))
         TagClose name ->
            let makeWarning ::
                   (Name.Tag name) =>
                   i -> TagH.Name name -> XmlTree.T i name String
                makeWarning i n =
                   XmlTree.warningIndex i $
                   if TagH.isSloppy n
                     then "misplaced sloppy closing tag </" ++ name ++ ">"
                     else "isolated closing tag </" ++ name ++ ">"
            in  return $ makeWarning ti $ Name.fromString name
         TagComment cmt ->
            return (XmlTree.commentIndex ti cmt)
         TagWarning msg ->
            return (XmlTree.warningIndex ti msg)
         TagPosition _ _ ->
            error "Unexpected position information. Please filter it out!"
{-
         TagSpecial name _ ->
            return $ XmlTree.warningIndex ti $
               "Special tag " ++ name ++ " not allowed within a HTML document"
-}


parseSubTrees ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.T i name String -> TagH.Name name -> Parser i [XmlTree.T i name String]
parseSubTrees warn name =
   ParserU.force $
   if TagH.isEmpty name
     then
       (Parser.satisfy (Match.isTagCloseName (Name.toString name) . snd) >> return [])
         `Parser.onFail` return []
     else
       ParserU.manyFinallyAppend
          (parseTerminator name)
          warn parseBranch


parseTerminator ::
   (Name.Tag name, Name.Attribute name) =>
   TagH.Name name -> Parser i [XmlTree.T i name String]
parseTerminator name =
   do c@(ci,ct) <- Parser.next
      let retry warns = Parser.reparse [c] >> return warns
      case ct of
         TagClose closeName_ ->
            let closeName = Name.fromString closeName_
            in  flip select
                   ((TagH.isEmpty closeName,
                        fail $ "the tag <" ++ closeName_ ++ "> closes nothing.") :
                    (name `TagH.isInnerOf` closeName, retry []) :
                    (name==closeName, return []) :
                    (TagH.isSloppy name,
                     retry [XmlTree.warningIndex ci $
                               "sloppy tag <"
                               ++ Name.toString name ++ "> closed by </"
                               ++ closeName_ ++ ">"]) :
                    (TagH.isSloppy closeName,
                     fail $ "ignore sloppy closing tag </" ++ Name.toString name ++ ">") :
                    [])
                   (return [XmlTree.warningIndex ci $
                                "open tag <" ++ Name.toString name ++
                                "> and close tag </" ++ Name.toString closeName ++
                                "> do not match"])
         TagOpen openName _ ->
            if Name.fromString openName `TagH.closes` name
              then retry []
              else fail $
                      "open tag <" ++ openName ++
                      "> does not close tag <" ++ Name.toString name ++ ">"
         _ -> fail $ "not a termination of a tag"


parseOptions :: Tag.ParseOptions String
parseOptions =
   Tag.parseOptions
      {Tag.optTagPosition = True,
       Tag.optTagWarning = True}

example :: [XmlTree NameLC.T String]
example =
   toXmlTreesString $
   Tag.parseTagsOptions parseOptions $
--   "<html><head>blub<meta/></head><br><body>bla</body></html>"
--   "<html><head>blub<meta/></head><br><body>bla<UL><li>1.<li>2.</OL><TABLE border=1></TABLE></body></html>"
--   "<html><head><meta>too much</meta>blub<meta/></head><br><body>bla<UL><li>1.<li>2.</UL><TABLE border=1></TABLE></bo"++undefined++"dy></html>"
--    "<b><font>test</b></font>"
    "<html><b><font>test</b></font></html>"
