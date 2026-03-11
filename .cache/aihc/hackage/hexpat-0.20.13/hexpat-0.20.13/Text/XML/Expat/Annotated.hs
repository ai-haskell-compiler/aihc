{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies,
        FlexibleContexts, ScopedTypeVariables #-}
-- | A variant of /Node/ in which Element nodes have an annotation of any type,
-- and some concrete functions that annotate with the XML parse location.
--
-- The names conflict with those in /Tree/ so you must use qualified import
-- if you want to use both modules.
module Text.XML.Expat.Annotated (
  -- * Tree structure
  Node,
  NodeG(..),
  UNode,
  LNode,
  ULNode,

  -- * Generic node manipulation
  module Text.XML.Expat.Internal.NodeClass,

  -- * Annotation-specific
  modifyAnnotation,
  mapAnnotation,

  -- * Qualified nodes
  QNode,
  QLNode,
  module Text.XML.Expat.Internal.Qualified,

  -- * Namespaced nodes
  NNode,
  NLNode,
  module Text.XML.Expat.Internal.Namespaced,

  -- * Parse to tree
  ParseOptions(..),
  defaultParseOptions,
  Encoding(..),
  parse,
  parse',
  parseG,
  XMLParseError(..),
  XMLParseLocation(..),

  -- * Variant that throws exceptions
  parseThrowing,
  XMLParseException(..),

  -- * Convert from SAX
  saxToTree,
  saxToTreeG,

  -- * Abstraction of string types
  GenericXMLString(..)
  ) where

import Control.Arrow (first)
import Text.XML.Expat.SAX ( Encoding(..)
                          , GenericXMLString(..)
                          , ParseOptions(..)
                          , defaultParseOptions
                          , SAXEvent(..)
                          , XMLParseError(..)
                          , XMLParseException(..)
                          , XMLParseLocation(..) )
import qualified Text.XML.Expat.SAX as SAX
import Text.XML.Expat.Internal.Namespaced
import Text.XML.Expat.Internal.NodeClass
import Text.XML.Expat.Internal.Qualified

import Control.Monad (mplus, mzero)
import Control.DeepSeq
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.List.Class (List(..), ListItem(..), cons, foldlL, joinM)
import Data.Monoid


-- | Annotated variant of the tree representation of the XML document, meaning
-- that it has an extra piece of information of your choice attached to each
-- Element.
--
-- @c@ is the container type for the element's children, which would normally be [],
-- but could potentially be a monadic list type to allow for chunked I/O.
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
    Text !text

type instance ListOf (NodeG a c tag text) = c (NodeG a c tag text)

-- | A pure tree representation that uses a list as its container type,
-- annotated variant.
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

instance (Eq tag, Eq text, Eq a) => Eq (NodeG a [] tag text) where
    Element na1 at1 ch1 an1 == Element na2 at2 ch2 an2 =
        na1 == na2 &&
        at1 == at2 &&
        ch1 == ch2 &&
        an1 == an2
    Text t1 == Text t2 = t1 == t2
    _ == _ = False

instance (NFData tag, NFData text, NFData a) => NFData (NodeG a [] tag text) where
    rnf (Element nam att chi ann) = rnf (nam, att, chi, ann)
    rnf (Text txt) = rnf txt

instance (Functor c, List c) => NodeClass (NodeG a) c where
    textContentM (Element _ _ children _) = foldlL mappend mempty $ joinM $ fmap textContentM children
    textContentM (Text txt) = return txt
    
    isElement (Element _ _ _ _) = True
    isElement _                 = False
    
    isText (Text _) = True
    isText _        = False

    isCData _ = False
    isProcessingInstruction _ = False
    isComment _ = False

    isNamed _  (Text _) = False
    isNamed nm (Element nm' _ _ _) = nm == nm'

    getName (Text _)             = mempty
    getName (Element name _ _ _) = name

    hasTarget _ _ = False
    getTarget _ = mempty

    getAttributes (Text _)              = []
    getAttributes (Element _ attrs _ _) = attrs

    getChildren (Text _)           = mzero
    getChildren (Element _ _ ch _) = ch

    getText (Text txt) = txt
    getText (Element _ _ _ _) = mempty

    modifyName _ node@(Text _) = node
    modifyName f (Element n a c ann) = Element (f n) a c ann

    modifyAttributes _ node@(Text _) = node
    modifyAttributes f (Element n a c ann) = Element n (f a) c ann

    modifyChildren _ node@(Text _) = node
    modifyChildren f (Element n a c ann) = Element n a (f c) ann

    mapAllTags _ (Text t) = Text t
    mapAllTags f (Element n a c ann) = Element (f n) (map (first f) a) (fmap (mapAllTags f) c) ann

    modifyElement _ (Text t) = Text t
    modifyElement f (Element n a c ann) =
        let (n', a', c') = f (n, a, c)
        in  Element n' a' c' ann

    mapNodeContainer f (Element n a ch an) = do
        ch' <- mapNodeListContainer f ch
        return $ Element n a ch' an
    mapNodeContainer _ (Text t) = return $ Text t

    mkText = Text

instance (Functor c, List c) => MkElementClass (NodeG (Maybe a)) c where
    mkElement name attrs children = Element name attrs children Nothing

instance (Functor c, List c) => MkElementClass (NodeG ()) c where
    mkElement name attrs children = Element name attrs children ()

-- | Type alias for an annotated node with unqualified tag names where
-- tag and text are the same string type
type UNode a text = Node a text text

-- | Type alias for an annotated node, annotated with parse location
type LNode tag text = Node XMLParseLocation tag text

-- | Type alias for an annotated node with unqualified tag names where
-- tag and text are the same string type, annotated with parse location
type ULNode text = LNode text text 

-- | Type alias for an annotated node where qualified names are used for tags
type QNode a text = Node a (QName text) text

-- | Type alias for an annotated node where qualified names are used for tags, annotated with parse location
type QLNode text = LNode (QName text) text

-- | Type alias for an annotated node where namespaced names are used for tags
type NNode a text = Node a (NName text) text

-- | Type alias for an annotated node where namespaced names are used for tags, annotated with parse location
type NLNode text = LNode (NName text) text

-- | Modify this node's annotation (non-recursively) if it's an element, otherwise no-op.
modifyAnnotation :: (a -> a) -> Node a tag text -> Node a tag text
f `modifyAnnotation` Element na at ch an = Element na at ch (f an)
_ `modifyAnnotation` Text t = Text t

-- | Modify this node's annotation and all its children recursively if it's an element, otherwise no-op.
mapAnnotation :: (a -> b) -> Node a tag text -> Node b tag text
f `mapAnnotation` Element na at ch an = Element na at (map (f `mapAnnotation`) ch) (f an)
_ `mapAnnotation` Text t = Text t

-- | A lower level function that lazily converts a SAX stream into a tree structure.
-- Variant that takes annotations for start tags.
saxToTree :: GenericXMLString tag =>
             [(SAXEvent tag text, a)]
          -> (Node a tag text, Maybe XMLParseError)
saxToTree events =
    let (nodes, mError, _) = ptl events
    in  (findRoot nodes, mError)
  where
    findRoot (elt@(Element _ _ _ _):_) = elt
    findRoot (_:nodes) = findRoot nodes
    findRoot [] = Element (gxFromString "") [] [] (error "saxToTree null annotation")
    ptl ((StartElement name attrs, ann):rema) =
        let (children, err1, rema') = ptl rema
            elt = Element name attrs children ann
            (out, err2, rema'') = ptl rema'
        in  (elt:out, err1 `mplus` err2, rema'')
    ptl ((EndElement _, _):rema) = ([], Nothing, rema)
    ptl ((CharacterData txt, _):rema) =
        let (out, err, rema') = ptl rema
        in  (Text txt:out, err, rema')
    ptl ((FailDocument err, _):_) = ([], Just err, [])
    ptl (_:rema) = ptl rema  -- extended node types not supported in this tree type
    ptl [] = ([], Nothing, [])

-- | A lower level function that converts a generalized SAX stream into a tree structure.
-- Ignores parse errors.
saxToTreeG :: forall l a tag text . (GenericXMLString tag, List l, Monad (ItemM l)) =>
              l (SAXEvent tag text, a)
           -> ItemM l (NodeG a l tag text)
saxToTreeG events = do
    (elts, _) <- process events
    findRoot elts
  where
    findRoot :: l (NodeG a l tag text) -> ItemM l (NodeG a l tag text)
    findRoot elts = do
        li <- runList elts
        case li of
            Cons elt@(Element _ _ _ _) _ -> return elt
            Cons _ rema -> findRoot rema
            Nil -> return $ Element (gxFromString "") mzero mzero (error "saxToTree null annotation")
    process :: l (SAXEvent tag text, a)
            -> ItemM l (l (NodeG a l tag text), l (SAXEvent tag text, a))
    process events = do
        li <- runList events
        case li of
            Nil -> return (mzero, mzero)
            Cons (StartElement name attrs, ann) rema -> do
                (children, rema') <- process rema
                (out, rema'') <- process rema'
                return (Element name attrs children ann `cons` out, rema'')
            Cons (EndElement _, _) rema -> return (mzero, rema)
            Cons (CharacterData txt, _) rema -> do
                (out, rema') <- process rema
                return (Text txt `cons` out, rema')
            --Cons (FailDocument err) rema = (mzero, mzero)
            Cons _ rema -> process rema

-- | Lazily parse XML to tree. Note that forcing the XMLParseError return value
-- will force the entire parse.  Therefore, to ensure lazy operation, don't
-- check the error status until you have processed the tree.
parse :: (GenericXMLString tag, GenericXMLString text) =>
         ParseOptions tag text    -- ^ Parse options
      -> L.ByteString             -- ^ Input text (a lazy ByteString)
      -> (LNode tag text, Maybe XMLParseError)
parse opts bs = saxToTree $ SAX.parseLocations opts bs

-- | Parse a generalized list to a tree, ignoring parse errors.
-- This function allows for a parse from an enumerator/iteratee to a "lazy"
-- tree structure using the @List-enumerator@ package.
parseG :: (GenericXMLString tag, GenericXMLString text, List l) =>
          ParseOptions tag text  -- ^ Parse options
       -> l ByteString           -- ^ Input text as a generalized list of blocks
       -> ItemM l (NodeG XMLParseLocation l tag text)
parseG opts = saxToTreeG . SAX.parseLocationsG opts

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
              -> LNode tag text
parseThrowing opts bs = fst $ saxToTree $ SAX.parseLocationsThrowing opts bs

-- | Strictly parse XML to tree. Returns error message or valid parsed tree.
parse' :: (GenericXMLString tag, GenericXMLString text) =>
          ParseOptions tag text   -- ^ Parse options
       -> B.ByteString            -- ^ Input text (a strict ByteString)
       -> Either XMLParseError (LNode tag text)
parse' opts doc = case parse opts (L.fromChunks [doc]) of
    (xml, Nothing) -> Right xml
    (_, Just err)  -> Left err

