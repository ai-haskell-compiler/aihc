module Text.XML.WraXML.Document.Tagchup where

import qualified Text.XML.WraXML.Tree.Tagchup as TreeTagchup
import qualified Text.XML.WraXML.Document as XmlDoc
-- import qualified Text.XML.WraXML.Tree     as XmlTree

import qualified Text.HTML.Tagchup.Parser      as TagParser
import qualified Text.HTML.Tagchup.Tag         as Tag
import qualified Text.HTML.Tagchup.PositionTag as PosTag
import qualified Text.XML.Basic.Position    as Position
import qualified Text.HTML.Basic.Tag as TagH
import qualified Text.HTML.Basic.Character    as HtmlChar

import qualified Text.XML.Basic.Name.LowerCase as NameLC
import qualified Text.XML.Basic.Name           as Name
import qualified Text.XML.Basic.ProcessingInstruction as PI

import Data.List.HT (viewL, )
import Data.Maybe (fromMaybe, )
import Control.Monad (guard, )

import Control.Monad.Trans.State (State, state, evalState, modify, gets, )

import qualified Data.Char as Char



type XmlDoc = XmlDoc.T Position.T


class TagParser.CharType char => CharSpace char where
   isSpace :: char -> Bool

instance CharSpace Char where
   isSpace = Char.isSpace

instance CharSpace HtmlChar.T where
   isSpace c =
      case c of
         HtmlChar.Unicode chr -> Char.isSpace chr
         HtmlChar.EntityRef "nbsp" -> True
         _ -> False

class StringSpace string where
   isAllSpace :: string -> Bool

instance CharSpace char => StringSpace [char] where
   isAllSpace = all isSpace


dropSpace :: StringSpace string =>
   [PosTag.T name string] -> [PosTag.T name string]
dropSpace =
   dropWhile
      (\tag ->
          case PosTag.tag_ tag of
             Tag.Text text -> isAllSpace text
             _ -> False)

withoutLeadingSpace :: (StringSpace string) =>
   ([PosTag.T name string] -> (a, [PosTag.T name string])) ->
   State [PosTag.T name string] a
withoutLeadingSpace f =
   modify dropSpace >> state f

toXmlDocument ::
   (Name.Tag name, Name.Attribute name, StringSpace string) =>
   [PosTag.T name string] -> XmlDoc name string
toXmlDocument ts =
   flip evalState ts $
   do xml <- withoutLeadingSpace $ \ts0 ->
         fromMaybe (Nothing, ts0) $
         do (t,ts1) <- viewL ts0
            (name, PI.Known attrs) <- Tag.maybeProcessing (PosTag.tag_ t)
            guard (Name.match "xml" name)
            return (Just attrs, ts1)
      docType <- withoutLeadingSpace $ \ts0 ->
         fromMaybe (Nothing, ts0) $
         do (t,ts1) <- viewL ts0
            (name, dtd) <- Tag.maybeSpecial (PosTag.tag_ t)
            guard (Name.match TagH.doctypeString name)
            return (Just dtd, ts1)
      gets (XmlDoc.Cons xml docType . TreeTagchup.toXmlTrees)


example :: IO ()
example =
  print .
  (toXmlDocument :: [PosTag.T NameLC.T String] -> XmlDoc NameLC.T String) .
  TagParser.runSoupWithPositions
    =<< readFile "/home/thielema/public_html/index.html"
