module Text.XML.WraXML.Tree.HaXml (
   fromXmlTree,
   multiFromXmlTree,
   toXmlTree, toXmlTree',
   lift,
   onContent,
   liftFilterToDocument,
   processWith,
   ) where

import qualified Text.XML.HaXml as HaXml
import           Text.XML.HaXml.Posn(Posn, posInNewCxt)

import Text.XML.WraXML.Tree
   (Branch(Tag), Leaf(Text, PI, Comment, CData, Warning), )
import qualified Text.XML.WraXML.Element as Elem

import qualified Text.XML.WraXML.Tree         as XmlTree
import qualified Text.XML.WraXML.Tree.Literal as XmlTreeL

import qualified Data.Tree.BranchLeafLabel as Tree

import qualified Text.XML.WraXML.String       as XmlString
import qualified Text.XML.WraXML.String.HaXml as HaXmlString
import qualified Text.XML.Basic.Character as XmlChar

import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.ProcessingInstruction as PI

import Data.Tuple.HT (mapPair, )


{- * conversion from our XML tree to HaXml tree -}

fromXmlTree ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.T i name XmlString.T -> HaXml.Content i
fromXmlTree x =
   case multiFromXmlTree x of
      [y] -> y
      _   -> error "top branch can't be a string"

haxmlName :: (Name.C name) => name -> HaXml.QName
haxmlName = HaXml.N . Name.toString

fromAttribute ::
   (Name.Attribute name) => Attr.T name XmlString.T -> HaXml.Attribute
fromAttribute =
   mapPair (HaXml.N, HaXmlString.fromXmlString) . Attr.toPair

multiFromXmlTree ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.T i name XmlString.T -> [HaXml.Content i]
multiFromXmlTree =
   Tree.fold
      (\i f -> f i)
      (\x subs i ->
          case x of
             Tag (Elem.Cons name attrs) ->
               [HaXml.CElem (HaXml.Elem (haxmlName name)
                  (map fromAttribute attrs)
                  (concat subs)) i])
      (\x i ->
          case x of
             Text whitespace str0 ->
                XmlChar.switchUnicodeRuns
                   (flip (HaXml.CString whitespace) i)
                   (flip HaXml.CRef i . HaXml.RefChar)
                   (flip HaXml.CRef i . HaXml.RefEntity)
                   str0
             Comment str ->
                [HaXml.CMisc (HaXml.Comment str) i]
             CData str ->
                [HaXml.CString True str i]
             PI target p ->
                [HaXml.CMisc (HaXml.PI (Name.toString target,
                   case p of
                      PI.Known attrs -> Attr.formatListBlankHead attrs ""
                      PI.Unknown str -> str)) i]
             Warning str ->
                [HaXml.CMisc (HaXml.Comment ("Warning: " ++ str)) i]) .
    XmlTree.unwrap


{- * conversion from HaXml tree to our XML tree -}

toXmlTree, toXmlTree' ::
   (Name.Tag name, Name.Attribute name) =>
   HaXml.Content i -> XmlTree.T i name XmlString.T
toXmlTree = XmlTree.mergeStrings . toXmlTree'


toAttribute :: HaXml.Attribute -> (String, XmlString.T)
toAttribute =
   mapPair (HaXml.qname, HaXmlString.toXmlString)

toXmlTree' x =
   case x of
      HaXml.CElem (HaXml.Elem name attrs subTrees) i ->
         XmlTreeL.tagIndexAttr i
            (HaXml.qname name)
            (map toAttribute attrs)
            (map toXmlTree' subTrees)
      HaXml.CString whitespace str i ->
         XmlTree.wrap2 i $
         Tree.Leaf (Text whitespace (XmlString.fromString str))
      HaXml.CRef ref i ->
         XmlTree.literalIndex i [HaXmlString.refToXmlAtom ref]
      HaXml.CMisc misc i ->
         XmlTree.wrap2 i $
         Tree.Leaf (case misc of
            HaXml.Comment str -> Comment str
            HaXml.PI (target, p) ->
               PI (Name.fromString target) $ PI.Unknown p)


{- * lift our XML filters to HaXml filters -}

lift ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.Filter i name XmlString.T -> (HaXml.Content i -> HaXml.Content i)
lift f = fromXmlTree . f . toXmlTree


{- |
Lift a filter of HaXml trees to a processor of a HaXml document.

cf. 'Text.XML.HaXml.Wrappers.onContent'
-}
onContent ::
   FilePath ->
   (HaXml.Content  Posn -> HaXml.Content  Posn) ->
   (HaXml.Document Posn -> HaXml.Document Posn)
onContent file f (HaXml.Document p s e m) =
    case f (HaXml.CElem e (posInNewCxt file Nothing)) of
        HaXml.CElem e' _ -> HaXml.Document p s e' m
        _                -> error "produced wrong output"

liftFilterToDocument ::
   (Name.Tag name, Name.Attribute name) =>
   FilePath -> XmlTree.Filter Posn name XmlString.T ->
      (HaXml.Document Posn -> HaXml.Document Posn)
liftFilterToDocument file =
   onContent file . lift



processWith ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.Filter Posn name XmlString.T -> IO ()
processWith = HaXml.processXmlWith . XmlTree.liftTrans . lift
