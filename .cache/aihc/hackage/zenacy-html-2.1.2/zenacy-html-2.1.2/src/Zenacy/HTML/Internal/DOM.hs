{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

-- | Defines the HTML DOM types and functions.
module Zenacy.HTML.Internal.DOM
  ( DOM(..)
  , DOMNode(..)
  , DOMAttr(..)
  , DOMType(..)
  , DOMQuirks(..)
  , DOMPos(..)
  , DOMID
  , DOMMap
  , domAttrMake
  , domDefaultDocument
  , domDefaultDoctype
  , domDefaultFragment
  , domDefaultElement
  , domDefaultTemplate
  , domDefaultText
  , domDefaultComment
  , domDefaultType
  , domMakeTypeHTML
  , domMakeTypeMathML
  , domMakeTypeSVG
  , domPos
  , domNull
  , domRoot
  , domRootPos
  , domDocument
  , domQuirksSet
  , domQuirksGet
  , domNewID
  , domGetNode
  , domPutNode
  , domInsert
  , domInsertNew
  , domAppend
  , domAppendNew
  , domElementHasAttr
  , domElementFindAttr
  , domElementAttrValue
  , domAttrEq
  , domAttrMerge
  , domAttrSet
  , domMatch
  , domLastChild
  , domMapID
  , domFindParent
  , domSetParent
  , domMapChild
  , domRemoveChild
  , domRemoveChildren
  , domMove
  , domMoveChildren
  , domChildren
  , domHasChild
  , domNodeID
  , domNodeParent
  , domNodeIsHTML
  , domNodeIsSVG
  , domNodeIsMathML
  , domNodeIsDocument
  , domNodeIsFragment
  , domNodeIsElement
  , domNodeIsTemplate
  , domNodeIsHtmlElement
  , domNodeIsText
  , domNodeElementName
  , domNodeElementNamespace
  , domNodeType
  , domTypesHTML
  , domTypesMathML
  , domTypesSVG
  , domRender
  ) where

import Zenacy.HTML.Internal.BS
import Zenacy.HTML.Internal.Core
import Zenacy.HTML.Internal.Types
import Data.Foldable
  ( toList
  )
import Data.List
  ( find
  )
import Data.Maybe
  ( fromJust
  , listToMaybe
  , isJust
  , mapMaybe
  )
import Data.Monoid
  ( (<>)
  )
import Data.Word
  ( Word8
  )
import Data.Default
  ( Default(..)
  )
import Data.IntMap
  ( IntMap
  )
import qualified Data.IntMap as IntMap
  ( singleton
  , lookup
  , insert
  , keys
  )
import Data.Set
  ( Set
  )
import qualified Data.Set as Set
  ( empty
  , insert
  )
import Data.Sequence
  ( Seq(..)
  , ViewL(..)
  , ViewR(..)
  , (<|)
  , (|>)
  , (><)
  )
import qualified Data.Sequence as Seq
  ( breakl
  , empty
  , filter
  , viewl
  , viewr
  )

-- | DOM represents an HTML document while being parsed.
data DOM = DOM
  { domNodes  :: !DOMMap
  , domNextID :: !DOMID
  } deriving (Eq, Ord, Show)

-- | Defines an ID for a node in a document.
type DOMID = Int

-- | Defines a mapping between node references and nodes.
type DOMMap = IntMap DOMNode

-- | Node is the model type for an HTML document.
data DOMNode
  = DOMDocument
    { domDocumentID          :: DOMID
    , domDocumentParent      :: DOMID
    , domDocumentName        :: BS
    , domDocumentChildren    :: Seq DOMID
    , domDocumentQuirksMode  :: DOMQuirks
    }
  | DOMDoctype
    { domDoctypeID           :: DOMID
    , domDoctypeParent       :: DOMID
    , domDoctypeName         :: BS
    , domDoctypePublicID     :: Maybe BS
    , domDoctypeSystemID     :: Maybe BS
    }
  | DOMFragment
    { domFragmentID          :: DOMID
    , domFragmentParent      :: DOMID
    , domFragmentName        :: BS
    , domFragmentChildren    :: Seq DOMID
    }
  | DOMElement
    { domElementID           :: DOMID
    , domElementParent       :: DOMID
    , domElementName         :: BS
    , domElementNamespace    :: HTMLNamespace
    , domElementAttributes   :: Seq DOMAttr
    , domElementChildren     :: Seq DOMID
    }
  | DOMTemplate
    { domTemplateID          :: DOMID
    , domTemplateParent      :: DOMID
    , domTemplateNamespace   :: HTMLNamespace
    , domTemplateAttributes  :: Seq DOMAttr
    , domTemplateContents    :: DOMID
    }
  | DOMText
    { domTextID              :: DOMID
    , domTextParent          :: DOMID
    , domTextData            :: BS
    }
  | DOMComment
    { domCommentID           :: DOMID
    , domCommentParent       :: DOMID
    , domCommentData         :: BS
    }
    deriving (Eq, Ord, Show)

-- | An HTML element attribute type.
data DOMAttr = DOMAttr
  { domAttrName      :: BS
  , domAttrVal       :: BS
  , domAttrNamespace :: HTMLAttrNamespace
  } deriving (Eq, Ord, Show)


-- | Identifies the type of an element.
data DOMType = DOMType
  { domTypeName      :: BS
  , domTypeNamespace :: HTMLNamespace
  } deriving (Eq, Ord, Show)

-- | Indentifies the quirks mode.
data DOMQuirks
  = DOMQuirksNone
  | DOMQuirksMode
  | DOMQuirksLimited
    deriving (Eq, Ord, Show)

-- | Defines a position in the DOM.
data DOMPos = DOMPos
  { domPosParent :: DOMID
  , domPosChild  :: Maybe DOMID
  } deriving (Eq, Ord, Show)

-- | Defines a default DOM.
instance Default DOM where
  def = DOM
    { domNodes = IntMap.singleton domRoot domDefaultDocument
    , domNextID = domRoot + 1
    }

-- | Defines a default attribute.
instance Default DOMAttr where
  def = DOMAttr
    { domAttrName      = bsEmpty
    , domAttrVal       = bsEmpty
    , domAttrNamespace = def
    }

-- | Makes an attribute.
domAttrMake :: BS -> BS -> DOMAttr
domAttrMake n v = DOMAttr n v def

-- | Defines a default document.
domDefaultDocument :: DOMNode
domDefaultDocument = DOMDocument
  { domDocumentID         = domNull
  , domDocumentName       = bsEmpty
  , domDocumentChildren   = Seq.empty
  , domDocumentQuirksMode = DOMQuirksNone
  , domDocumentParent     = domNull
  }

-- | Defines a default document type.
domDefaultDoctype :: DOMNode
domDefaultDoctype = DOMDoctype
  { domDoctypeID       = domNull
  , domDoctypeName     = bsEmpty
  , domDoctypePublicID = Nothing
  , domDoctypeSystemID = Nothing
  , domDoctypeParent   = domNull
  }

-- | Defines a default document fragment.
domDefaultFragment :: DOMNode
domDefaultFragment = DOMFragment
  { domFragmentID       = domNull
  , domFragmentName     = bsEmpty
  , domFragmentChildren = Seq.empty
  , domFragmentParent   = domNull
  }

-- | Defines a default element.
domDefaultElement :: DOMNode
domDefaultElement = DOMElement
  { domElementID         = domNull
  , domElementName       = bsEmpty
  , domElementNamespace  = HTMLNamespaceHTML
  , domElementAttributes = Seq.empty
  , domElementChildren   = Seq.empty
  , domElementParent     = domNull
  }

-- | Defines a default template.
domDefaultTemplate :: DOMNode
domDefaultTemplate = DOMTemplate
  { domTemplateID         = domNull
  , domTemplateNamespace  = HTMLNamespaceHTML
  , domTemplateAttributes = Seq.empty
  , domTemplateContents   = domNull
  , domTemplateParent     = domNull
  }

-- | Defines a default text.
domDefaultText :: DOMNode
domDefaultText = DOMText
  { domTextID     = domNull
  , domTextData   = bsEmpty
  , domTextParent = domNull
  }

-- | Defines a default comment.
domDefaultComment :: DOMNode
domDefaultComment = DOMComment
  { domCommentID     = domNull
  , domCommentData   = bsEmpty
  , domCommentParent = domNull
  }

-- | Defines a default type.
domDefaultType :: DOMType
domDefaultType = domMakeTypeHTML bsEmpty

-- | Makes a new HTML element type.
domMakeTypeHTML :: BS -> DOMType
domMakeTypeHTML = flip DOMType HTMLNamespaceHTML

-- | Makes a new MathML element type.
domMakeTypeMathML :: BS -> DOMType
domMakeTypeMathML = flip DOMType HTMLNamespaceMathML

-- | Makes a new SVG element type.
domMakeTypeSVG :: BS -> DOMType
domMakeTypeSVG = flip DOMType HTMLNamespaceSVG

-- | Makes a new position.
domPos :: DOMID -> DOMPos
domPos x = DOMPos x Nothing

-- | The null node ID.
domNull :: DOMID
domNull = 0

-- | The root document node ID.
domRoot :: DOMID
domRoot = 1

-- | Defines an appending position in a document node.
domRootPos :: DOMPos
domRootPos = domPos domRoot

-- | Gets the document node for a DOM.
domDocument :: DOM -> DOMNode
domDocument d = fromJust $ IntMap.lookup domRoot $ domNodes d

-- | Sets the quirks mode for a document.
domQuirksSet :: DOMQuirks -> DOM -> DOM
domQuirksSet x d =
  case (domDocument d) of
    y@DOMDocument {} ->
      let
        y' = y { domDocumentQuirksMode = x }
      in
        domPutNode domRoot y' d
    _otherwise ->
      d

-- | Gets the quirks mode for a document.
domQuirksGet :: DOM -> DOMQuirks
domQuirksGet = domDocumentQuirksMode . domDocument

-- | Adds a new node to a DOM.
domNewID :: DOM -> DOMNode -> (DOM, DOMID)
domNewID d n = (d', i)
  where
    i = domNextID d
    n' = domSetID n i
    d' = d { domNodes = IntMap.insert i n' $ domNodes d
           , domNextID = i + 1
           }

-- | Sets the ID for a node.
domSetID :: DOMNode -> DOMID -> DOMNode
domSetID x y =
  case x of
    DOMDocument{} -> x { domDocumentID = y }
    DOMDoctype{}  -> x { domDoctypeID = y }
    DOMFragment{} -> x { domFragmentID = y }
    DOMElement{}  -> x { domElementID = y }
    DOMTemplate{} -> x { domTemplateID = y }
    DOMText{}     -> x { domTextID = y }
    DOMComment{}  -> x { domCommentID = y }

-- | Gets a node for a node ID.
domGetNode :: DOM -> DOMID -> Maybe DOMNode
domGetNode d x = IntMap.lookup x $ domNodes d

-- | Updates a node in the DOM.
domPutNode :: DOMID -> DOMNode -> DOM -> DOM
domPutNode x n d = d { domNodes = IntMap.insert x n $ domNodes d }

-- | Inserts a node at a position.
domInsert :: DOMPos -> DOMID -> DOM -> DOM
domInsert p@(DOMPos r c) x d =
  case domGetNode d r of
    Just n@(DOMDocument { domDocumentChildren = a }) ->
      f $ n { domDocumentChildren = g a }
    Just n@(DOMElement { domElementChildren = a }) ->
      f $ n { domElementChildren = g a }
    Just n@(DOMFragment { domFragmentChildren = a }) ->
      f $ n { domFragmentChildren = g a }
    Just n@(DOMTemplate { domTemplateContents = a }) ->
      domInsert (DOMPos a c) x d
    _otherwise -> d
  where
    f a = domSetParent x r (domPutNode r a d)
    g = domInsertChild p x

-- | Inserts a node at a position.
domInsertNew :: DOMPos -> DOMNode -> DOM -> (DOM, DOMID)
domInsertNew p x d =
  (domInsert p i d', i)
  where
    (d', i) = domNewID d x

-- | Inserts a child in a list of children.
domInsertChild :: DOMPos -> DOMID -> Seq DOMID -> Seq DOMID
domInsertChild (DOMPos _ Nothing) x = (|> x)
domInsertChild (DOMPos _ (Just a)) x = seqInsertBefore (==a) x

-- | Appends a node ID to a node.
domAppend :: DOMID -> DOMID -> DOM -> DOM
domAppend x y d =
  case domGetNode d x of
    Just (DOMDocument i p n c q) ->
      f $ DOMDocument i p n (c |> y) q
    Just (DOMElement i p n s a c) ->
      f $ DOMElement i p n s a (c |> y)
    Just (DOMFragment i p n c) ->
      f $ DOMFragment i p n (c |> y)
    Just (DOMTemplate _ _ _ _ c) ->
      domAppend c y d
    _otherwise -> d
  where
    f a = domSetParent y x (domPutNode x a d)

-- | Appends a node to a node.
domAppendNew :: DOMID -> DOMNode -> DOM -> DOM
domAppendNew x y d = domAppend x i d'
  where (d', i) = domNewID d y

-- | Finds an attribute for an element.
domElementFindAttr :: DOMNode -> BS -> Maybe DOMAttr
domElementFindAttr node name = case node of
  DOMElement{..} -> f domElementAttributes
  DOMTemplate{..} -> f domTemplateAttributes
  _otherwise -> Nothing
  where
    f = seqFind (\DOMAttr{..} -> domAttrName == name)

-- | Gets the last element in a sequence if it exists.
seqLast :: Seq a -> Maybe a
seqLast (Seq.viewr -> EmptyR) = Nothing
seqLast (Seq.viewr -> _ :> a) = Just a

-- | Finds an element in a sequence.
seqFind :: (a -> Bool) -> Seq a -> Maybe a
seqFind f x = go x
  where
    go (Seq.viewl -> EmptyL) = Nothing
    go (Seq.viewl -> a :< b) = if f a then Just a else go b

-- | Inserts an element into a sequence before the element satisfying a predicate.
seqInsertBefore :: (a -> Bool) -> a -> Seq a -> Seq a
seqInsertBefore f x y =
  (a |> x) <> b
  where
    (a, b) = Seq.breakl f y

-- | Finds an attribute value for an element.
domElementAttrValue :: DOMNode -> BS -> Maybe BS
domElementAttrValue x n = domAttrVal <$> domElementFindAttr x n

-- | Determines if a node has a named attribute.
domElementHasAttr  :: DOMNode -> BS -> Bool
domElementHasAttr x = isJust . domElementFindAttr x

-- | Merges attributes into an existing node.
domAttrMerge :: DOMID -> Seq DOMAttr -> DOM -> DOM
domAttrMerge x y d =
  case domGetNode d x of
    Just n@(DOMElement { domElementAttributes = a }) ->
      domPutNode x (n { domElementAttributes = a <> f n y }) d
    Just n@(DOMTemplate { domTemplateAttributes = a }) ->
      domPutNode x (n { domTemplateAttributes = a <> f n y }) d
    _otherwise -> d
  where
    f n = Seq.filter (not . domElementHasAttr n . domAttrName)

-- | Detmermines if two elements match.
domMatch :: DOM -> DOMID -> DOMID -> Bool
domMatch d i j =
  case (domGetNode d i, domGetNode d j) of
    (Just (DOMElement _ _ n1 s1 a1 _), Just (DOMElement _ _ n2 s2 a2 _)) ->
      n1 == n2 && s1 == s2 && domAttrEq a1 a2
    (Just (DOMTemplate _ _ s1 a1 _ ), Just (DOMTemplate _ _ s2 a2 _)) ->
      s1 == s2 && domAttrEq a1 a2
    _otherwise ->
      False

-- | Determine if two sequences of attributes are equivalent.
domAttrEq :: Seq DOMAttr -> Seq DOMAttr -> Bool
domAttrEq a b = domAttrSet a == domAttrSet b

-- | Converta an attribute sequence to an attribute set in order
-- to be insensitive to order.
domAttrSet :: Seq DOMAttr -> Set DOMAttr
domAttrSet = foldr Set.insert Set.empty

-- | Returns the last child of a node if it exists.
domLastChild :: DOM -> DOMID -> Maybe DOMID
domLastChild d x =
  domGetNode d x >>= \case
    DOMDocument{..} -> seqLast domDocumentChildren
    DOMFragment{..} -> seqLast domFragmentChildren
    DOMElement{..}  -> seqLast domElementChildren
    DOMTemplate{..} -> domLastChild d domTemplateContents
    _otherwise -> Nothing

-- | Converts a list of node IDs to nodes.
domMapID :: DOM -> [DOMID] -> [DOMNode]
domMapID d = mapMaybe $ domGetNode d

-- | Finds the parent node for a node.
domFindParent :: DOM -> DOMID -> Maybe DOMID
domFindParent d x = find (domHasChild d x) $ IntMap.keys $ domNodes d

-- | Sets the parent for a node.
domSetParent :: DOMID -> DOMID -> DOM -> DOM
domSetParent x y d =
  case domGetNode d x of
    Just a -> case a of
      DOMDocument{} -> f a { domDocumentParent = y }
      DOMDoctype{}  -> f a { domDoctypeParent = y }
      DOMFragment{} -> f a { domFragmentParent = y }
      DOMElement{}  -> f a { domElementParent = y }
      DOMTemplate{} -> f a { domTemplateParent = y }
      DOMText{}     -> f a { domTextParent = y }
      DOMComment{}  -> f a { domCommentParent = y }
    Nothing -> d
  where
    f z = domPutNode x z d

-- | Maps a function over children of a node.
domMapChild :: DOMID -> (Seq DOMID -> Seq DOMID)-> DOM -> DOM
domMapChild x f d =
  case domGetNode d x of
    Just a -> case a of
      DOMDocument { domDocumentChildren = c } ->
        domPutNode x a { domDocumentChildren = f c } d
      DOMFragment { domFragmentChildren = c } ->
        domPutNode x a { domFragmentChildren = f c } d
      DOMElement { domElementChildren = c } ->
        domPutNode x a { domElementChildren = f c } d
      DOMTemplate { domTemplateContents = c } ->
        domMapChild c f d
      _otherwise -> d
    Nothing -> d

-- | Removes a child from a node.
domRemoveChild :: DOMID -> DOMID -> DOM -> DOM
domRemoveChild parent child = domMapChild parent $ Seq.filter (/=child)

-- | Removes all the children from a node.
domRemoveChildren :: DOMID -> DOM -> DOM
domRemoveChildren x = domMapChild x $ const Seq.empty

-- | Moves a node to another parent.
domMove :: DOMID -> DOMID -> DOM -> DOM
domMove x newParent d =
  case domGetNode d x of
    Just a ->
      let d' = domRemoveChild (domNodeParent a) x d
      in domAppend newParent x d'
    Nothing -> d

-- | Moves the children of a node to another node.
domMoveChildren :: DOMID -> DOMID -> DOM -> DOM
domMoveChildren x y d =
  foldl (\d' c -> domAppend y c d') (domRemoveChildren x d) $ domChildren d x

-- | Gets the children of a node.
domChildren :: DOM -> DOMID -> Seq DOMID
domChildren d x =
  case domGetNode d x of
    Just (DOMDocument{..}) -> domDocumentChildren
    Just (DOMFragment{..}) -> domFragmentChildren
    Just (DOMElement{..})  -> domElementChildren
    Just (DOMTemplate{..}) -> domChildren d domTemplateContents
    _otherwise             -> Seq.empty

-- | Determines if a node has a specific child.
domHasChild :: DOM -> DOMID -> DOMID -> Bool
domHasChild d x z = z `elem` domChildren d x

-- | Gets the id for a node.
domNodeID :: DOMNode -> DOMID
domNodeID = \case
  DOMDocument{..} -> domDocumentID
  DOMDoctype{..}  -> domDoctypeID
  DOMFragment{..} -> domFragmentID
  DOMElement{..}  -> domElementID
  DOMTemplate{..} -> domTemplateID
  DOMText{..}     -> domTextID
  DOMComment{..}  -> domCommentID

-- | Gets the parent for a node.
domNodeParent :: DOMNode -> DOMID
domNodeParent = \case
  DOMDocument{..} -> domDocumentParent
  DOMDoctype{..}  -> domDoctypeParent
  DOMFragment{..} -> domFragmentParent
  DOMElement{..}  -> domElementParent
  DOMTemplate{..} -> domTemplateParent
  DOMText{..}     -> domTextParent
  DOMComment{..}  -> domCommentParent

-- | Detmermines if a node is in the HTML namespace.
domNodeIsHTML :: DOMNode -> Bool
domNodeIsHTML = \case
  DOMElement{..}  -> domElementNamespace == HTMLNamespaceHTML
  DOMTemplate{..} -> domTemplateNamespace == HTMLNamespaceHTML
  _otherwise      -> False

-- | Detmermines if a node is in the SVG namespace.
domNodeIsSVG :: DOMNode -> Bool
domNodeIsSVG = \case
  DOMElement{..}  -> domElementNamespace == HTMLNamespaceSVG
  DOMTemplate{..} -> domTemplateNamespace == HTMLNamespaceSVG
  _otherwise      -> False

-- | Detmermines if a node is in the MathML namespace.
domNodeIsMathML :: DOMNode -> Bool
domNodeIsMathML = \case
  DOMElement{..}  -> domElementNamespace == HTMLNamespaceMathML
  DOMTemplate{..} -> domTemplateNamespace == HTMLNamespaceMathML
  _otherwise      -> False

-- | Detmermines if a node is a document node.
domNodeIsDocument :: DOMNode -> Bool
domNodeIsDocument DOMDocument{} = True
domNodeIsDocument _ = False

-- | Detmermines if a node is a document fragment node.
domNodeIsFragment :: DOMNode -> Bool
domNodeIsFragment DOMFragment{} = True
domNodeIsFragment _ = False

-- | Detmermines if a node is an element node.
domNodeIsElement :: DOMNode -> Bool
domNodeIsElement DOMElement{} = True
domNodeIsElement _ = False

-- | Detmermines if a node is a template node.
domNodeIsTemplate :: DOMNode -> Bool
domNodeIsTemplate DOMTemplate{} = True
domNodeIsTemplate _ = False

-- | Detmermines if a node is an HTML element node.
domNodeIsHtmlElement :: DOMNode -> Bool
domNodeIsHtmlElement x = domNodeIsElement x && domNodeIsHTML x

-- | Detmermines if a node is a text node.
domNodeIsText :: DOMNode -> Bool
domNodeIsText DOMText{} = True
domNodeIsText _ = False

-- | Gets the name for an element node.
domNodeElementName :: DOMNode -> BS
domNodeElementName DOMElement{..} = domElementName
domNodeElementName DOMTemplate{} = "template"
domNodeElementName _ = ""

-- | Gets the name for an element node.
domNodeElementNamespace :: DOMNode -> HTMLNamespace
domNodeElementNamespace DOMElement{..} = domElementNamespace
domNodeElementNamespace DOMTemplate{..} = domTemplateNamespace
domNodeElementNamespace _ = HTMLNamespaceHTML

-- | Gets the type for an element node.
domNodeType :: DOMNode -> DOMType
domNodeType x = DOMType (domNodeElementName x) (domNodeElementNamespace x)

-- | Gets a list of HTML types for element names.
domTypesHTML :: [BS] -> [DOMType]
domTypesHTML = map domMakeTypeHTML

-- | Gets a list of MathML types for element names.
domTypesMathML :: [BS] -> [DOMType]
domTypesMathML = map domMakeTypeMathML

-- | Gets a list of SVG types for element names.
domTypesSVG :: [BS] -> [DOMType]
domTypesSVG = map domMakeTypeSVG

-- | Renders the DOM.
domRender :: DOM -> BS
domRender d = domRenderIndent d 0 domRoot

-- | Renders the DOM with indentaion.
domRenderIndent :: DOM -> Int -> DOMID -> BS
domRenderIndent d x y =
  case fromJust (domGetNode d y) of
    DOMDocument{..} ->
      bsConcat $ map (domRenderIndent d x) $ toList domDocumentChildren
    DOMDoctype{} ->
      bsEmpty
    DOMFragment{..} ->
      bsConcat $ map (domRenderIndent d x) $ toList domFragmentChildren
    DOMElement{..} ->
      bsConcat
        [ indent
        , domElementName
        , "\n"
        , bsConcat $ map (domRenderIndent d $ x + 1) $ toList domElementChildren
        ]
    DOMTemplate{..} ->
      bsConcat
        [ indent
        , "template"
        , "\n"
        , domRenderIndent d (x + 1) domTemplateContents
        ]
    DOMText{..} ->
      bsConcat
        [ indent
        , domTextData
        , "\n"
        ]
    DOMComment{} ->
      bsEmpty
  where
    indent = bsPack $ take x $ repeat 0x20
