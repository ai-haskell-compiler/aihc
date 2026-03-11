{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, FlexibleContexts #-}
-- | Type classes to allow for XML handling functions to be generalized to
-- work with different document types.
module Text.XML.Expat.Internal.DocumentClass where

import Text.XML.Expat.Internal.NodeClass (NodeClass)
import Control.DeepSeq
import Control.Monad (mzero)
import Data.List.Class (List)


-- | XML declaration, consisting of version, encoding and standalone.
--
-- The formatting functions always outputs only UTF-8, regardless
-- of what encoding is specified here.  If you want to produce a document in a
-- different encoding, then set the encoding here, format the document, and then
-- convert the output text from UTF-8 to your desired encoding using some
-- text conversion library.
data XMLDeclaration text = XMLDeclaration text (Maybe text) (Maybe Bool) deriving (Eq, Show)

-- | Stub for future expansion.
data DocumentTypeDeclaration (c :: * -> *) tag text = DocumentTypeDeclaration deriving (Eq, Show)

data Misc text =
    Comment !text |
    ProcessingInstruction !text !text

instance Show text => Show (Misc text) where
    showsPrec d (ProcessingInstruction t txt) = showParen (d > 10) $
        ("ProcessingInstruction "++) . showsPrec 11 t . (" "++) . showsPrec 11 txt
    showsPrec d (Comment t) = showParen (d > 10) $ ("Comment "++) . showsPrec 11 t

instance Eq text => Eq (Misc text) where
    ProcessingInstruction t1 d1 == ProcessingInstruction t2 d2 = 
        t1 == t2 &&
        d1 == d2
    Comment t1 == Comment t2 = t1 == t2
    _ == _ = False

instance NFData text => NFData (Misc text) where
    rnf (ProcessingInstruction target txt) = rnf (target, txt)
    rnf (Comment txt) = rnf txt

type family NodeType (d :: (* -> *) -> * -> * -> *) :: (* -> *) -> * -> * -> *

class (Functor c, List c, NodeClass (NodeType d) c) => DocumentClass d (c :: * -> *) where
    -- | Get the XML declaration for this document.
    getXMLDeclaration :: d c tag text -> Maybe (XMLDeclaration text)

    -- | Get the Document Type Declaration (DTD) for this document.
    getDocumentTypeDeclaration :: d c tag text -> Maybe (DocumentTypeDeclaration c tag text)

    -- | Get the top-level 'Misc' nodes for this document.
    getTopLevelMiscs :: d c tag text -> c (Misc text)

    -- | Get the root element for this document.
    getRoot :: d c tag text -> NodeType d c tag text

    -- | Make a document with the specified fields.
    mkDocument :: Maybe (XMLDeclaration text)
               -> Maybe (DocumentTypeDeclaration c tag text)
               -> c (Misc text)
               -> NodeType d c tag text
               -> d c tag text

-- | Make a document with the specified root node and all other information
-- set to defaults.
mkPlainDocument :: DocumentClass d c => NodeType d c tag text -> d c tag text
mkPlainDocument = mkDocument Nothing Nothing mzero

modifyXMLDeclaration :: DocumentClass d c =>
                        (Maybe (XMLDeclaration text) -> Maybe (XMLDeclaration text))
                     -> d c tag text
                     -> d c tag text
modifyXMLDeclaration f doc = mkDocument (f $ getXMLDeclaration doc) (getDocumentTypeDeclaration doc)
        (getTopLevelMiscs doc) (getRoot doc)

modifyDocumentTypeDeclaration :: DocumentClass d c =>
                                 (Maybe (DocumentTypeDeclaration c tag text) -> Maybe (DocumentTypeDeclaration c tag text))
                              -> d c tag text
                              -> d c tag text
modifyDocumentTypeDeclaration f doc = mkDocument (getXMLDeclaration doc) (f $ getDocumentTypeDeclaration doc)
        (getTopLevelMiscs doc) (getRoot doc)

modifyTopLevelMiscs :: DocumentClass d c =>
                       (c (Misc text) -> c (Misc text))
                    -> d c tag text
                    -> d c tag text
modifyTopLevelMiscs f doc = mkDocument (getXMLDeclaration doc) (getDocumentTypeDeclaration doc)
        (f $ getTopLevelMiscs doc) (getRoot doc)

modifyRoot :: DocumentClass d c =>
              (NodeType d c tag text -> NodeType d c tag text)
           -> d c tag text
           -> d c tag text
modifyRoot f doc = mkDocument (getXMLDeclaration doc) (getDocumentTypeDeclaration doc)
        (getTopLevelMiscs doc) (f $ getRoot doc)

