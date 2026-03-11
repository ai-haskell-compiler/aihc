module Text.XML.WraXML.Tree.HXT
   (fromXmlTree, toXmlTree, lift, liftFilterToDocument,
    checkTagName, ) where

import qualified Text.XML.HXT.DOM.TypeDefs as HXT
import qualified Data.Tree.Class as HXTTree

import Text.XML.WraXML.Tree
   (Branch(Tag), Leaf(Text, PI, Comment, Warning, CData), )
import qualified Text.XML.WraXML.Element as Elem

import qualified Text.XML.WraXML.Tree    as XmlTree
import qualified Data.Tree.BranchLeafLabel as Tree

import qualified Text.XML.WraXML.String     as XmlString
import qualified Text.XML.WraXML.String.HXT as HXTString

import qualified Text.XML.Basic.Attribute as Attr
import qualified Text.XML.Basic.Name as Name
import qualified Text.XML.Basic.ProcessingInstruction as PI

import qualified Text.XML.Basic.Character as XmlChar

-- needed for code copies from HXT at the of the file
import Text.XML.HXT.DOM.QualifiedName (QName)
import Text.XML.HXT.DOM.TypeDefs (XNode)
import Data.Tree.NTree.TypeDefs (NTree(NTree))


{- * conversion from our XML tree to HXT tree -}

fromXmlTree ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.T i name XmlString.T -> HXT.XmlTree
fromXmlTree x =
   case multiFromXmlTree x of
      [y] -> y
      _   -> error "top branch can't be a string"


mkHXTName :: (Name.C name) => name -> QName
mkHXTName = HXT.mkName . Name.toString

mkHXTAttrs :: (Name.Attribute name) =>
   [Attr.T name XmlString.T] -> [NTree XNode]
mkHXTAttrs =
   map (\a ->
            HXTTree.mkTree
               (HXT.XAttr (mkHXTName $ Attr.name_ a))
               (HXTString.fromXmlString $ Attr.value_ a))

multiFromXmlTree ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.T i name XmlString.T -> [HXT.XmlTree]
multiFromXmlTree =
   Tree.fold
      (\i f -> f i)
      (\x subs _ ->
          case x of
             Tag (Elem.Cons name attrs) ->
                [HXTTree.mkTree
                    (HXT.XTag (mkHXTName name) (mkHXTAttrs attrs))
                    (concat subs)])
      (\x _ ->
          case x of
             Text _ {- whitespace -} str -> HXTString.fromXmlString str
             Comment str ->
                [HXTTree.mkTree (HXT.XCmt str) []]
             CData str ->
                [HXTTree.mkTree (HXT.XCdata str) []]
             PI target proc ->
                let (attrTree,subTrees) =
                       case proc of
                          PI.Known attrs -> (mkHXTAttrs attrs, [])
                          PI.Unknown str -> ([], [HXTTree.mkTree (HXT.XText str) []])
                in  [HXTTree.mkTree
                        (HXT.XPi (mkHXTName target) attrTree)
                        subTrees]
             Warning str ->
                [HXTTree.mkTree (HXT.XError 0 str) []])
    . XmlTree.unwrap


{- * conversion from HXT tree to our XML tree -}

toXmlTree, toXmlTree' ::
   (Name.Tag name, Name.Attribute name) =>
   HXT.XmlTree -> XmlTree.T () name XmlString.T
toXmlTree = XmlTree.mergeStrings . toXmlTree'


toXmlTree' (NTree label subTrees) = XmlTree.wrap $ (,) () $
   let leaf x =
          if null subTrees
            then Tree.Leaf x
            else error "HXT to WraXML: Leaf must not contain sub trees."

       fromHXTName = Name.fromString . HXT.qualifiedName

       convAttr (NTree x value) =
          case x of
             HXT.XAttr name ->
                Attr.new (HXT.qualifiedName name) (HXTString.toXmlString value)
             _ -> error "HXT.XAttr expected"

   in  case label of
          HXT.XTag name attrs ->
             Tree.Branch
                (Tag (Elem.Cons
                          (fromHXTName name)
                          (map convAttr attrs)))
                (map (XmlTree.unwrap . toXmlTree') subTrees)
          HXT.XText str ->
             leaf (Text True (XmlString.fromString str))
          HXT.XCharRef ref ->
             leaf (Text False [XmlChar.fromCharRef ref])
          HXT.XEntityRef ref ->
             leaf (Text False [XmlChar.fromEntityRef ref])
          HXT.XCmt cmt ->
             leaf (Comment cmt)
          HXT.XPi target proc ->
             Tree.Leaf $ PI (fromHXTName target) $
             case subTrees of
                [] -> PI.Known $ map convAttr proc
                [NTree (HXT.XText str) []] ->
                      PI.Unknown str
                _ ->  error "from HXT: processing instruction - there must be no children or a single text child"
          HXT.XAttr _    -> error "from HXT: attribute not allowed in normal text"
          HXT.XDTD _ _   -> error "from HXT: document type descriptor not allowed in normal text"
          HXT.XCdata x   -> leaf (CData x)
          HXT.XError lev x -> leaf (Warning ("Level: " ++ show lev ++ ": " ++ x))



{- * lift our XML filters to HXT filters -}

lift ::
   (Name.Tag name, Name.Attribute name) =>
   XmlTree.Filter () name XmlString.T -> (HXT.XmlTree -> HXT.XmlTree)
lift f = fromXmlTree . f . toXmlTree


{- |
Lift our XML filters to HXT document processors.
-}
liftFilterToDocument ::
   (Name.Tag name, Name.Attribute name) =>
      String  {- ^ Name of root tag for processing, e.g. "html".
                   That tag must be in the first level.
                   It is an unchecked run-time error
                   if it is missing or occurs more than once. -}
   -> XmlTree.Filter () name XmlString.T
   -> (HXT.XmlTree -> HXT.XmlTree)
liftFilterToDocument tagName f =
   HXTTree.changeChildren $ \subTrees ->
      let (pre,x:post) =
              break (checkTagName tagName . HXTTree.getNode) subTrees
      in  pre ++ lift f x : post

checkTagName :: String -> HXT.XNode -> Bool
checkTagName tagName tree =
   case tree of
      (HXT.XTag name _)  ->  HXT.qualifiedName name == tagName
      _  ->  False


{-
The HTML parser of the Haskell XML toolbox HXT
is great in parsing lousy HTML.
However its API is extremely weakly typed.
Everything is a monadic function, even simple XML trees.
This design was done in order to reduce the combinators essentially to .>>

That's why I decided to use HXT as a parser and pretty printer,
but not using its XML tree structure for manipulations.
This way I hope I can keep in sync with the main development path
while not interfering with HXT's typing style.
-}
