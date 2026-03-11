{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
        FlexibleContexts, EmptyDataDecls #-}
-- | An extended variant of /Node/ intended to implement the entire XML
-- specification.  DTDs are not yet supported, however.
--
-- The names conflict with those in /Tree/ so you must use qualified import
-- if you want to use both modules.
module Text.XML.Expat.Extended (
  -- * Tree structure
  Document,
  DocumentG(..),
  Node,
  NodeG(..),
  UDocument,
  LDocument,
  ULDocument,
  UNode,
  LNode,
  ULNode,

  -- * Generic document/node manipulation
  module Text.XML.Expat.Internal.DocumentClass,
  module Text.XML.Expat.Internal.NodeClass,

  -- * Annotation-specific
  modifyAnnotation,
  mapAnnotation,
  mapDocumentAnnotation,

  -- * Qualified nodes
  QDocument,
  QLDocument,
  QNode,
  QLNode,
  module Text.XML.Expat.Internal.Qualified,

  -- * Namespaced nodes
  NDocument,
  NLDocument,
  NNode,
  NLNode,
  module Text.XML.Expat.Internal.Namespaced,

  -- * Parse to tree
  ParseOptions(..),
  defaultParseOptions,
  Encoding(..),
  parse,
  parse',
  XMLParseError(..),
  XMLParseLocation(..),

  -- * Variant that throws exceptions
  parseThrowing,
  XMLParseException(..),

  -- * Convert from SAX
  saxToTree,

  -- * Abstraction of string types
  GenericXMLString(..)
  ) where

import Control.Arrow
import Text.XML.Expat.SAX ( Encoding(..)
                          , GenericXMLString(..)
                          , ParseOptions(..)
                          , defaultParseOptions
                          , SAXEvent
                          , XMLParseError(..)
                          , XMLParseException(..)
                          , XMLParseLocation(..) )
import qualified Text.XML.Expat.SAX as SAX
import Text.XML.Expat.Internal.DocumentClass
import Text.XML.Expat.Internal.Namespaced
import Text.XML.Expat.Internal.NodeClass
import Text.XML.Expat.Internal.Qualified

import Control.Monad (mplus, mzero)
import Control.DeepSeq
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List.Class (List, foldlL, joinM)
import Data.Maybe
import Data.Monoid


-- | Document representation of the XML document, intended to support the entire
-- XML specification.  DTDs are not yet supported, however.
data DocumentG a c tag text = Document {
        dXMLDeclaration          :: Maybe (XMLDeclaration text),
        dDocumentTypeDeclaration :: Maybe (DocumentTypeDeclaration c tag text),
        dTopLevelMiscs           :: c (Misc text),
        dRoot                    :: NodeG a c tag text
    }

instance (Show tag, Show text, Show a) => Show (DocumentG a [] tag text) where
    showsPrec d (Document xd dtd m r) = showParen (d > 10) $
        ("Document "++) . showsPrec 11 xd . (" "++) .
                          showsPrec 11 dtd . (" "++) .
                          showsPrec 11 m . (" "++) .
                          showsPrec 11 r

instance (Eq tag, Eq text, Eq a) => Eq (DocumentG a [] tag text) where
    Document xd1 dtd1 m1 r1 == Document xd2 dtd2 m2 r2 =
        xd1 == xd2 &&
        dtd1 == dtd2 &&
        m1 == m2 &&
        r1 == r2

-- | A pure representation of an XML document that uses a list as its container type.
type Document a tag text = DocumentG a [] tag text

type instance NodeType (DocumentG ann) = NodeG ann

instance (Functor c, List c) => DocumentClass (DocumentG ann) c where
    getXMLDeclaration          = dXMLDeclaration
    getDocumentTypeDeclaration = dDocumentTypeDeclaration
    getTopLevelMiscs           = dTopLevelMiscs
    getRoot                    = dRoot
    mkDocument                 = Document

-- | Extended variant of the tree representation of the XML document, intended
-- to support the entire XML specification.  DTDs are not yet supported, however.
--
-- @c@ is the container type for the element's children, which is [] in the
-- @hexpat@ package, and a monadic list type for @hexpat-iteratee@.
--
-- @tag@ is the tag type, which can either be one of several string types,
-- or a special type from the @Text.XML.Expat.Namespaced@ or
-- @Text.XML.Expat.Qualified@ modules.
--
-- @text@ is the string type for text content.
--
-- @a@ is the type of the annotation.  One of the things this can be used for
-- is to store the XML parse location, which is useful for error handling.
--
-- Note that some functions in the @Text.XML.Expat.Cursor@ module need to create
-- new nodes through the 'MkElementClass' type class. Normally this can only be done
-- if @a@ is a Maybe type or () (so it can provide the Nothing value for the annotation
-- on newly created nodes).  Or, you can write your own 'MkElementClass' instance.
-- Apart from that, there is no requirement for @a@ to be a Maybe type.
data NodeG a c tag text =
    Element {
        eName       :: !tag,
        eAttributes :: ![(tag,text)],
        eChildren   :: c (NodeG a c tag text),
        eAnn        :: a
    } |
    Text !text |
    CData !text |
    Misc (Misc text)

type instance ListOf (NodeG a c tag text) = c (NodeG a c tag text)

-- | A pure tree representation that uses a list as its container type,
-- extended variant.
--
-- In the @hexpat@ package, a list of nodes has the type @[Node tag text]@, but note
-- that you can also use the more general type function 'ListOf' to give a list of
-- any node type, using that node's associated list type, e.g.
-- @ListOf (UNode Text)@.
type Node a tag text = NodeG a [] tag text

instance (Show tag, Show text, Show a) => Show (NodeG a [] tag text) where
    showsPrec d (Element na at ch an) = showParen (d > 10) $
        ("Element "++) . showsPrec 11 na . (" "++) .
                         showsPrec 11 at . (" "++) .
                         showsPrec 11 ch . (" "++) .
                         showsPrec 11 an
    showsPrec d (Text t) = showParen (d > 10) $ ("Text "++) . showsPrec 11 t
    showsPrec d (CData t) = showParen (d > 10) $ ("CData "++) . showsPrec 11 t
    showsPrec d (Misc m)  = showParen (d > 10) $ ("Misc "++) . showsPrec 11 m

instance (Eq tag, Eq text, Eq a) => Eq (NodeG a [] tag text) where
    Element na1 at1 ch1 an1 == Element na2 at2 ch2 an2 =
        na1 == na2 &&
        at1 == at2 &&
        ch1 == ch2 &&
        an1 == an2
    Text t1 == Text t2 = t1 == t2
    CData t1 == CData t2 = t1 == t2
    Misc t1 == Misc t2 = t1 == t2
    _ == _ = False

instance (NFData tag, NFData text, NFData a) => NFData (NodeG a [] tag text) where
    rnf (Element nam att chi ann) = rnf (nam, att, chi, ann)
    rnf (Text txt) = rnf txt
    rnf (CData txt) = rnf txt
    rnf (Misc m) = rnf m

instance (Functor c, List c) => NodeClass (NodeG a) c where
    textContentM (Element _ _ children _) = foldlL mappend mempty $ joinM $ fmap textContentM children
    textContentM (Text txt) = return txt
    textContentM (CData txt) = return txt
    textContentM (Misc (ProcessingInstruction _ _)) = return mempty
    textContentM (Misc (Comment _)) = return mempty

    isElement (Element _ _ _ _) = True
    isElement _                 = False
    
    isText (Text _) = True
    isText (CData _) = True
    isText _        = False
    
    isCData (CData _) = True
    isCData _        = False
    
    isProcessingInstruction (Misc (ProcessingInstruction _ _)) = True
    isProcessingInstruction _        = False
    
    isComment (Misc (Comment _)) = True
    isComment _                  = False
    
    isNamed nm (Element nm' _ _ _) = nm == nm'
    isNamed _  _ = False
    
    getName (Element name _ _ _) = name
    getName _             = mempty
    
    hasTarget t (Misc (ProcessingInstruction t' _ )) = t == t'
    hasTarget _  _ = False
    
    getTarget (Misc (ProcessingInstruction target _)) = target
    getTarget _                                       = mempty

    getAttributes (Element _ attrs _ _) = attrs
    getAttributes _              = []

    getChildren (Element _ _ ch _) = ch
    getChildren _           = mzero

    getText (Text txt) = txt
    getText (CData txt) = txt
    getText (Misc (ProcessingInstruction _ txt)) = txt
    getText (Misc (Comment txt)) = txt
    getText (Element _ _ _ _) = mempty

    modifyName f (Element n a c ann) = Element (f n) a c ann
    modifyName _ node = node

    modifyAttributes f (Element n a c ann) = Element n (f a) c ann
    modifyAttributes _ node = node

    modifyChildren f (Element n a c ann) = Element n a (f c) ann
    modifyChildren _ node = node

    mapAllTags f (Element n a c ann) = Element (f n) (map (first f) a) (fmap (mapAllTags f) c) ann
    mapAllTags _ (Text txt) = Text txt
    mapAllTags _ (CData txt) = CData txt
    mapAllTags _ (Misc (ProcessingInstruction n txt)) = Misc (ProcessingInstruction n txt)
    mapAllTags _ (Misc (Comment txt)) = Misc (Comment txt)

    modifyElement f (Element n a c ann) =
        let (n', a', c') = f (n, a, c)
        in  Element n' a' c' ann
    modifyElement _ (Text txt) = Text txt
    modifyElement _ (CData txt) = CData txt
    modifyElement _ (Misc (ProcessingInstruction n txt)) = Misc (ProcessingInstruction n txt)
    modifyElement _ (Misc (Comment txt)) = Misc (Comment txt)

    mapNodeContainer f (Element n a ch an) = do
        ch' <- mapNodeListContainer f ch
        return $ Element n a ch' an
    mapNodeContainer _ (Text txt) = return $ (Text txt)
    mapNodeContainer _ (CData txt) = return $ (CData txt)
    mapNodeContainer _ (Misc (ProcessingInstruction n txt)) = return $ Misc (ProcessingInstruction n txt)
    mapNodeContainer _ (Misc (Comment txt)) = return $ Misc (Comment txt)

    mkText = Text

instance (Functor c, List c) => MkElementClass (NodeG (Maybe a)) c where
    mkElement name attrs children = Element name attrs children Nothing

instance (Functor c, List c) => MkElementClass (NodeG ()) c where
    mkElement name attrs children = Element name attrs children ()

-- | Type alias for an extended document with unqualified tag names where
-- tag and text are the same string type
type UDocument a text = Document a text text

-- | Type alias for an extended document, annotated with parse location
type LDocument tag text = Document XMLParseLocation tag text

-- | Type alias for an extended document with unqualified tag names where
-- tag and text are the same string type, annotated with parse location
type ULDocument text = Document XMLParseLocation text text

-- | Type alias for an extended document where qualified names are used for tags
type QDocument a text = Document a (QName text) text

-- | Type alias for an extended document where qualified names are used for tags, annotated with parse location
type QLDocument text = Document XMLParseLocation (QName text) text

-- | Type alias for an extended document where namespaced names are used for tags
type NDocument a text = Document a (NName text) text

-- | Type alias for an extended document where namespaced names are used for tags, annotated with parse location
type NLDocument text = Document XMLParseLocation (NName text) text

-- | Type alias for an extended node with unqualified tag names where
-- tag and text are the same string type
type UNode a text = Node a text text

-- | Type alias for an extended node, annotated with parse location
type LNode tag text = Node XMLParseLocation tag text

-- | Type alias for an extended node with unqualified tag names where
-- tag and text are the same string type, annotated with parse location
type ULNode text = LNode text text 

-- | Type alias for an extended node where qualified names are used for tags
type QNode a text = Node a (QName text) text

-- | Type alias for an extended node where qualified names are used for tags, annotated with parse location
type QLNode text = LNode (QName text) text

-- | Type alias for an extended node where namespaced names are used for tags
type NNode a text = Node a (NName text) text

-- | Type alias for an extended node where namespaced names are used for tags, annotated with parse location
type NLNode text = LNode (NName text) text

-- | Modify this node's annotation (non-recursively) if it's an element, otherwise no-op.
modifyAnnotation :: (a -> a) -> Node a tag text -> Node a tag text
f `modifyAnnotation` Element na at ch an = Element na at ch (f an)
_ `modifyAnnotation` Text t = Text t
_ `modifyAnnotation` CData t = CData t
_ `modifyAnnotation` Misc (ProcessingInstruction n t) = Misc (ProcessingInstruction n t)
_ `modifyAnnotation` Misc (Comment t) = Misc (Comment t)

-- | Modify this node's annotation and all its children recursively if it's an element, otherwise no-op.
mapAnnotation :: (a -> b) -> Node a tag text -> Node b tag text
f `mapAnnotation` Element na at ch an = Element na at (map (f `mapAnnotation`) ch) (f an)
_ `mapAnnotation` Text t = Text t
_ `mapAnnotation` CData t = CData t
_ `mapAnnotation` Misc (ProcessingInstruction n t) = Misc (ProcessingInstruction n t)
_ `mapAnnotation` Misc (Comment t) = Misc (Comment t)

-- | Modify the annotation of every node in the document recursively.
mapDocumentAnnotation :: (a -> b) -> Document a tag text -> Document b tag text
mapDocumentAnnotation f doc = Document {
        dXMLDeclaration          = dXMLDeclaration doc,
        dDocumentTypeDeclaration = dDocumentTypeDeclaration doc,
        dTopLevelMiscs           = dTopLevelMiscs doc,
        dRoot                    = mapAnnotation f (dRoot doc)
    }

-- | A lower level function that lazily converts a SAX stream into a tree structure.
-- Variant that takes annotations for start tags.
saxToTree :: (GenericXMLString tag, Monoid text) =>
             [(SAXEvent tag text, a)]
          -> (Document a tag text, Maybe XMLParseError)
saxToTree ((SAX.XMLDeclaration ver mEnc mSD, _):events) =
    let (doc, mErr) = saxToTree events
    in  (doc {
            dXMLDeclaration = Just $ XMLDeclaration ver mEnc mSD
        }, mErr)
saxToTree events =
    let (nodes, mError, _) = ptl events False []
        doc = Document {
                dXMLDeclaration          = Nothing,
                dDocumentTypeDeclaration = Nothing,
                dTopLevelMiscs           = findTopLevelMiscs nodes,
                dRoot                    = findRoot nodes
            }
    in  (doc, mError)
  where
    findRoot (elt@(Element _ _ _ _):_) = elt
    findRoot (_:nodes) = findRoot nodes
    findRoot [] = Element (gxFromString "") [] [] (error "saxToTree null annotation")
    findTopLevelMiscs = mapMaybe $ \node -> case node of
        Misc m -> Just m
        _      -> Nothing
    ptl ((SAX.StartElement name attrs,ann):rema) isCD cd =
        let (children, err1, rema') = ptl rema isCD cd
            elt = Element name attrs children ann
            (out, err2, rema'') = ptl rema' isCD cd
        in  (elt:out, err1 `mplus` err2, rema'')
    ptl ((SAX.EndElement _, _):rema) _ _ = ([], Nothing, rema)
    ptl ((SAX.CharacterData txt, _):rema) isCD cd =
        if isCD then
            ptl rema isCD (txt:cd)
        else
            let (out, err, rema') = ptl rema isCD cd
            in  (Text txt:out, err, rema')
    ptl ((SAX.StartCData,_) :rema) _ _ =
        ptl rema True mzero
    ptl ((SAX.EndCData, _) :rema) _ cd =
        let (out, err, rema') = ptl rema False mzero
        in  (CData (mconcat $ reverse cd):out, err, rema')
    ptl ((SAX.Comment txt, _):rema) isCD cd =
        let (out, err, rema') = ptl rema isCD cd
        in  (Misc (Comment txt):out, err, rema')
    ptl ((SAX.ProcessingInstruction target txt, _):rema) isCD cd =
        let (out, err, rema') = ptl rema isCD cd
        in  (Misc (ProcessingInstruction target txt):out, err, rema')
    ptl ((SAX.FailDocument err, _):_) _ _ = ([], Just err, [])
    ptl ((SAX.XMLDeclaration _ _ _, _):rema) isCD cd = ptl rema isCD cd  -- doesn't appear in the middle of a document
    ptl [] _ _ = ([], Nothing, [])

-- | Lazily parse XML to tree. Note that forcing the XMLParseError return value
-- will force the entire parse.  Therefore, to ensure lazy operation, don't
-- check the error status until you have processed the tree.
parse :: (GenericXMLString tag, GenericXMLString text) =>
         ParseOptions tag text    -- ^ Parse options
      -> L.ByteString             -- ^ Input text (a lazy ByteString)
      -> (LDocument tag text, Maybe XMLParseError)
parse opts bs = saxToTree $ SAX.parseLocations opts bs

-- | Lazily parse XML to tree. In the event of an error, throw 'XMLParseException'.
--
-- @parseThrowing@ can throw an exception from pure code, which is generally a bad
-- way to handle errors, because Haskell\'s lazy evaluation means it\'s hard to
-- predict where it will be thrown from.  However, it may be acceptable in
-- situations where it's not expected during normal operation, depending on the
-- design of your program.
parseThrowing :: (GenericXMLString tag, GenericXMLString text) =>
                 ParseOptions tag text    -- ^ Parse options
              -> L.ByteString             -- ^ Input text (a lazy ByteString)
              -> LDocument tag text
parseThrowing opts bs = fst $ saxToTree $ SAX.parseLocationsThrowing opts bs

-- | Strictly parse XML to tree. Returns error message or valid parsed tree.
parse' :: (GenericXMLString tag, GenericXMLString text) =>
          ParseOptions tag text   -- ^ Parse options
       -> B.ByteString            -- ^ Input text (a strict ByteString)
       -> Either XMLParseError (LDocument tag text)
parse' opts bs = case parse opts (L.fromChunks [bs]) of
    (_, Just err)   -> Left err
    (root, Nothing) -> Right root 

