{- |
Very lazy HTML tree parser using TagSoup as lexer.
-}
module Text.XML.WraXML.Tree.Tagchup (
   toXmlTrees,
   fromXmlTree,
   fromElement, fromLeaf,
   liftElementFilter, liftElementCheck,
   example,
   ) where

import qualified Text.HTML.Tagchup.Parser      as TagParser
import qualified Text.HTML.Tagchup.Tag         as Tag
import qualified Text.HTML.Tagchup.PositionTag as PosTag
import qualified Text.HTML.Tagchup.Tag.Match   as MatchTag

import qualified Text.XML.WraXML.Element          as Elem
import qualified Text.XML.WraXML.Tree             as XmlTree
import qualified Data.Tree.BranchLeafLabel        as Tree

import qualified Text.HTML.Basic.Tag as TagH

import qualified Text.XML.Basic.Position    as Position
import qualified Text.XML.Basic.Name.LowerCase as NameLC
import qualified Text.XML.Basic.Name           as Name

-- import Control.Monad.Trans.State (evalState, put, get, gets, )
import Control.Monad.Trans.Writer (runWriter, writer, )

import Data.Monoid (Last(Last, getLast), )
import Data.Maybe (fromMaybe, )
import Data.Bool.HT (select, )

import qualified Text.XML.WraXML.Tree.LazyParser as ParserU
import qualified Text.ParserCombinators.Poly.Lazy as Parser

import Control.Monad (liftM, )


-- we don't use XmlTree synonym in the exported functions for documentation purposes
type XmlTree name string = XmlTree.T Position.T name string


{-
-- FIXME: how to get rid of 'error' ?
fromXmlTree' ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.T Position.T name string -> [PosTag.T name string]
fromXmlTree' =
   flip evalState (error "position not initialised, yet") .
   Tree.fold
      (\pos x -> put pos >> x)
      (\branch subTreesM ->
          do pos <- get
             subTrees <- sequence subTreesM
             lastPos <- get
             
             let elm = XmlTree.getElement branch
                 openTag =
                    PosTag.cons pos $ fromElement elm
                 closeTag =
                    PosTag.cons lastPos $
                    Tag.Close $ Elem.name_ elm
             return $ openTag : concat subTrees ++ [closeTag])

      (\leaf ->
          gets (\pos -> [PosTag.cons pos $ fromLeaf leaf]))
   .
   XmlTree.unwrap
-}

fromXmlTree ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.T Position.T name string -> [PosTag.T name string]
fromXmlTree =
   fst . runWriter .
   Tree.foldLabel
      (\pos branch subTreesM ->
          let (subTrees, lastSubPos) = runWriter $ sequence subTreesM
              lastPos = fromMaybe pos $ getLast lastSubPos
              elm = XmlTree.getElement branch
              openTag =
                 PosTag.cons pos $ fromElement elm
              closeTag =
                 PosTag.cons lastPos $
                 Tag.Close $ Elem.name_ elm
          in  writer (openTag : concat subTrees ++ [closeTag],
                      Last $ Just lastPos))
      (\pos leaf ->
          writer $ flip (,) (Last $ Just pos) $
          [PosTag.cons pos $ fromLeaf leaf])
   .
   XmlTree.unwrap

fromElement ::
   (Name.Tag name, Name.Attribute name) =>
   Elem.T name string -> Tag.T name string
fromElement elm =
   Tag.Open (Elem.name_ elm) (Elem.attributes_ elm)

fromLeaf ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.Leaf name string -> Tag.T name string
fromLeaf leaf =
   case leaf of
      XmlTree.Text  _ text  -> Tag.text text
      XmlTree.Comment text  -> Tag.comment text
      XmlTree.CData   text  -> Tag.cdata text
      XmlTree.PI name instr -> Tag.processing name instr
      XmlTree.Warning text  -> Tag.warning text


liftElementFilter ::
   (Name.Tag name, Name.Attribute name) =>
   (Elem.T name str -> Elem.T name str) ->
   Tag.T name str -> Tag.T name str
liftElementFilter f tag =
   flip (maybe tag) (Tag.maybeOpen tag) $
   fromElement . f . uncurry Elem.Cons

liftElementCheck :: (Elem.T name string -> Bool) -> (Tag.T name string -> Bool)
liftElementCheck f tag =
   flip (maybe False) (Tag.maybeOpen tag) $
   f . uncurry Elem.Cons



{- |
A TagSoup could represent multiple HTML trees,
e.g. with some introducing comments.
-}
toXmlTrees ::
   (Name.Tag name, Name.Attribute name) =>
   [PosTag.T name string] -> [XmlTree.T Position.T name string]
toXmlTrees =
   fst . Parser.runParser (ParserU.manyLazy parseBranch)


type Parser name string a = Parser.Parser (PosTag.T name string) a


parseBranch ::
   (Name.Tag name, Name.Attribute name) =>
   Parser name string (XmlTree name string)
parseBranch =
   do nt <- Parser.next
      let ti = PosTag.position_ nt
      case PosTag.tag_ nt of
         Tag.Text text -> return (XmlTree.literalIndex ti text)
         Tag.Open name attrs ->
            liftM
               (XmlTree.tagIndexAttr ti name attrs)
               (parseSubTrees
                  (XmlTree.warningIndex ti
                      "unexpected end of file") name)
         Tag.Close name ->
            return $ XmlTree.warningIndex ti $
            if TagH.isSloppy name
              then "misplaced sloppy closing tag </" ++ Name.toString name ++ ">"
              else "isolated closing tag </" ++ Name.toString name ++ ">"
         Tag.Comment cmt ->
            return (XmlTree.commentIndex ti cmt)
         Tag.Warning msg ->
            return (XmlTree.warningIndex ti msg)
         Tag.Processing target p ->
            return (XmlTree.processingIndex ti target p)
         Tag.Special name str ->
            return $
            if Name.match TagH.cdataString name
              then XmlTree.cdataIndex ti str
              else XmlTree.warningIndex ti $
                      "Special tag " ++ Name.toString name ++
                      " not allowed within a HTML document"


parseSubTrees ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree name string -> TagH.Name name ->
   Parser name string [XmlTree name string]
parseSubTrees warn name =
   ParserU.force $
   if TagH.isEmpty name
     then
       (Parser.satisfy (MatchTag.close (name==) . PosTag.tag_) >> return [])
         `Parser.onFail` return []
     else
       ParserU.manyFinallyAppend
          (parseTerminator name)
          warn parseBranch


parseTerminator ::
   (Name.Tag name, Name.Attribute name) =>
   TagH.Name name -> Parser name string [XmlTree name string]
parseTerminator name =
   do c <- Parser.next
      let ci = PosTag.position_ c
          retry warns = Parser.reparse [c] >> return warns
      case PosTag.tag_ c of
         Tag.Close closeName ->
            flip select
               ((TagH.isEmpty closeName,
                    fail $ "the tag <" ++ Name.toString closeName ++ "> closes nothing.") :
                (name `TagH.isInnerOf` closeName, retry []) :
                (name==closeName, return []) :
                (TagH.isSloppy name,
                 retry [XmlTree.warningIndex ci $
                           "sloppy tag <"
                           ++ Name.toString name ++ "> closed by </"
                           ++ Name.toString closeName ++ ">"]) :
                (TagH.isSloppy closeName,
                 fail $ "ignore sloppy closing tag </" ++ Name.toString name ++ ">") :
                [])
               (return [XmlTree.warningIndex ci $
                            "open tag <" ++ Name.toString name ++
                            "> and close tag </" ++ Name.toString closeName ++
                            "> do not match"])
         Tag.Open openName _ ->
            if openName `TagH.closes` name
              then retry []
              else fail $
                      "open tag <" ++ Name.toString openName ++
                      "> does not close tag <" ++ Name.toString name ++ ">"
         _ -> fail $ "not a termination of a tag"


example ::
   [XmlTree NameLC.T String]
example =
   toXmlTrees $
   TagParser.runSoupWithPositions $
--   "<html><head>blub<meta/></head><br><body>bla</body></html>"
--   "<html><head>blub<meta/></head><br><body>bla<UL><li>1.<li>2.</OL><TABLE border=1></TABLE></body></html>"
--   "<html><head><meta>too much</meta>blub<meta/></head><br><body>bla<UL><li>1.<li>2.</UL><TABLE border=1></TABLE></bo"++undefined++"dy></html>"
--    "<b><font>test</b></font>"
    "<html><b><font>test</b></font></html>"
