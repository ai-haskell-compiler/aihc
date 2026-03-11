{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Defines the top-level HTML types and parser functions.
module Zenacy.HTML.Internal.HTML
  ( HTMLOptions(..)
  , HTMLResult(..)
  , HTMLError(..)
  , HTMLNode(..)
  , HTMLAttr(..)
  , HTMLNamespace(..)
  , HTMLAttrNamespace(..)
  , HTMLQuirks(..)
  , htmlParse
  , htmlParseEasy
  , htmlFragment
  , htmlDefaultDocument
  , htmlDefaultDoctype
  , htmlDefaultFragment
  , htmlDefaultElement
  , htmlDefaultTemplate
  , htmlDefaultText
  , htmlDefaultComment
  , htmlAttr
  , htmlElem
  , htmlText
  ) where

import Zenacy.HTML.Internal.BS
import Zenacy.HTML.Internal.Core
import Zenacy.HTML.Internal.DOM
import Zenacy.HTML.Internal.Parser
import Zenacy.HTML.Internal.Types
import Data.Default
  ( Default(..)
  )
import Data.Either
  ( either
  )
import Data.Foldable
  ( toList
  )
import Data.Maybe
  ( fromJust
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as T
  ( empty
  )
import qualified Data.Text.Encoding as T
  ( encodeUtf8
  , decodeUtf8
  )

-- | Defines options for the HTML parser.
data HTMLOptions = HTMLOptions
  { htmlOptionLogErrors      :: !Bool
    -- ^ Indicates that errors should be logged.
  , htmlOptionIgnoreEntities :: !Bool
    -- ^ Indicates that entities should not be decoded.
  } deriving (Eq, Ord, Show)

-- | Defines an HTML parser result.
data HTMLResult = HTMLResult
  { htmlResultDocument :: !HTMLNode
    -- ^ The parsed document structure.
  , htmlResultErrors   :: ![HTMLError]
    -- ^ The errors logged while parsing if error logging was enabled.
  } deriving (Eq, Ord, Show)

-- | An HTML error type.
data HTMLError = HTMLError
  { htmlErrorText  :: !Text
    -- ^ The error message.
  } deriving (Show, Eq, Ord)

-- | Defines the model type for an HTML document.
data HTMLNode
  = HTMLDocument
    { htmlDocumentName       :: !Text           -- ^ The document name.
    , htmlDocumentMode       :: !HTMLQuirks     -- ^ The document mode.
    , htmlDocumentChildren   :: ![HTMLNode]     -- ^ The document children.
    }
  | HTMLDoctype
    { htmlDoctypeName        :: !Text           -- ^ The DOCTYPE name.
    , htmlDoctypePublicID    :: !(Maybe Text)   -- ^ The public ID.
    , htmlDoctypeSystemID    :: !(Maybe Text)   -- ^ The system ID.
    }
  | HTMLFragment
    { htmlFragmentName       :: !Text           -- ^ The fragment name.
    , htmlFragmentChildren   :: ![HTMLNode]     -- ^ The fragment children.
    }
  | HTMLElement
    { htmlElementName        :: !Text           -- ^ The element name.
    , htmlElementNamespace   :: !HTMLNamespace  -- ^ The element namespace.
    , htmlElementAttributes  :: ![HTMLAttr]     -- ^ The element attributes.
    , htmlElementChildren    :: ![HTMLNode]     -- ^ The element children.
    }
  | HTMLTemplate
    { htmlTemplateNamespace  :: !HTMLNamespace  -- ^ The template namespace.
    , htmlTemplateAttributes :: ![HTMLAttr]     -- ^ The template attributes.
    , htmlTemplateContents   :: !HTMLNode       -- ^ The template contents.
    }
  | HTMLText
    { htmlTextData           :: !Text           -- ^ The text value.
    }
  | HTMLComment
    { htmlCommentData        :: !Text           -- ^ The comment text.
    }
    deriving (Eq, Ord, Show)

-- | An HTML element attribute type.
data HTMLAttr = HTMLAttr
  { htmlAttrName      :: Text
  , htmlAttrVal       :: Text
  , htmlAttrNamespace :: HTMLAttrNamespace
  } deriving (Eq, Ord, Show)

-- | Defines default options.
instance Default HTMLOptions where
  def = HTMLOptions
    { htmlOptionLogErrors      = False
    , htmlOptionIgnoreEntities = False
    }

-- | Defines a default result.
instance Default HTMLResult where
  def = HTMLResult
    { htmlResultDocument = htmlDefaultDocument
    , htmlResultErrors   = []
    }

-- | Defines a default error.
instance Default HTMLError where
  def = HTMLError
    { htmlErrorText = T.empty
    }

-- | Defines a default attribute.
instance Default HTMLAttr where
  def = HTMLAttr
    { htmlAttrName      = T.empty
    , htmlAttrVal       = T.empty
    , htmlAttrNamespace = HTMLAttrNamespaceNone
    }

-- | Indentifies the quirks mode.
data HTMLQuirks
  = HTMLQuirksNone     -- ^ Indicates a document in "no-quirks" mode.
  | HTMLQuirksMode     -- ^ Indicates a document in "quirks" mode.
  | HTMLQuirksLimited  -- ^ Indicates a document in "limited-quirks" mode.
    deriving (Eq, Ord, Show)

-- | Parses an HTML document.
htmlParse :: HTMLOptions -> Text -> Either HTMLError HTMLResult
htmlParse HTMLOptions {..} x =
  case d of
    Right ParserResult {..} ->
      Right def
        { htmlResultDocument = domToHTML parserResultDOM
        , htmlResultErrors   = map f parserResultErrors
        }
    Left e ->
      Left (f e)
  where
    d = parseDocument def
      { parserOptionInput          = T.encodeUtf8 x
      , parserOptionLogErrors      = htmlOptionLogErrors
      , parserOptionIgnoreEntities = htmlOptionIgnoreEntities
      }
    f x = def { htmlErrorText = T.decodeUtf8 x }

-- | Parses an HTML document the easy way.
htmlParseEasy :: Text -> HTMLNode
htmlParseEasy =
  either (const htmlDefaultDocument) htmlResultDocument . htmlParse def

-- | Parses an HTML fragment.
htmlFragment :: HTMLOptions -> Text -> Either HTMLError HTMLResult
htmlFragment HTMLOptions {..} x = Left def
  { htmlErrorText = "fragment support not currently implemented" }

-- | Defines a default document.
htmlDefaultDocument :: HTMLNode
htmlDefaultDocument = HTMLDocument
  { htmlDocumentName     = T.empty
  , htmlDocumentMode     = HTMLQuirksNone
  , htmlDocumentChildren = []
  }

-- | Defines a default document type.
htmlDefaultDoctype :: HTMLNode
htmlDefaultDoctype = HTMLDoctype
  { htmlDoctypeName     = T.empty
  , htmlDoctypePublicID = Nothing
  , htmlDoctypeSystemID = Nothing
  }

-- | Defines a default document fragment.
htmlDefaultFragment :: HTMLNode
htmlDefaultFragment = HTMLFragment
  { htmlFragmentName     = T.empty
  , htmlFragmentChildren = []
  }

-- | Defines a default element.
htmlDefaultElement :: HTMLNode
htmlDefaultElement = HTMLElement
  { htmlElementName       = T.empty
  , htmlElementNamespace  = HTMLNamespaceHTML
  , htmlElementAttributes = []
  , htmlElementChildren   = []
  }

-- | Defines a default template.
htmlDefaultTemplate :: HTMLNode
htmlDefaultTemplate = HTMLTemplate
  { htmlTemplateNamespace  = HTMLNamespaceHTML
  , htmlTemplateAttributes = []
  , htmlTemplateContents   = htmlDefaultFragment
  }

-- | Defines a default text.
htmlDefaultText :: HTMLNode
htmlDefaultText = HTMLText
  { htmlTextData = T.empty
  }

-- | Defines a default comment.
htmlDefaultComment :: HTMLNode
htmlDefaultComment = HTMLComment
  { htmlCommentData = T.empty
  }

-- | Makes an attribute.
htmlAttr :: Text -> Text -> HTMLAttr
htmlAttr n v = HTMLAttr n v HTMLAttrNamespaceNone

-- | Makes an element.
htmlElem :: Text -> [HTMLAttr] -> [HTMLNode] -> HTMLNode
htmlElem n a c = HTMLElement n HTMLNamespaceHTML a c

-- | Makes a text node.
htmlText :: Text -> HTMLNode
htmlText = HTMLText

-- | Converts a DOM document to an HTML document.
domToHTML :: DOM -> HTMLNode
domToHTML d = nodeToHTML d $ domDocument d

-- | Converts a DOM node to an HTML node.
nodeToHTML :: DOM -> DOMNode -> HTMLNode
nodeToHTML d = go where
  go DOMDocument {..} = HTMLDocument
    { htmlDocumentName     = t domDocumentName
    , htmlDocumentMode     = toHTMLQuirks domDocumentQuirksMode
    , htmlDocumentChildren = f domDocumentChildren
    }
  go DOMDoctype {..} = HTMLDoctype
    { htmlDoctypeName     = t domDoctypeName
    , htmlDoctypePublicID = t <$> domDoctypePublicID
    , htmlDoctypeSystemID = t <$> domDoctypeSystemID
    }
  go DOMFragment {..} = HTMLFragment
    { htmlFragmentName     = t domFragmentName
    , htmlFragmentChildren = f domFragmentChildren
    }
  go DOMElement {..} = HTMLElement
    { htmlElementName       = t domElementName
    , htmlElementNamespace  = domElementNamespace
    , htmlElementAttributes = h domElementAttributes
    , htmlElementChildren   = f domElementChildren
    }
  go DOMTemplate {..} = HTMLTemplate
    { htmlTemplateNamespace  = domTemplateNamespace
    , htmlTemplateAttributes = h domTemplateAttributes
    , htmlTemplateContents   = g domTemplateContents
    }
  go DOMText {..} = HTMLText
    { htmlTextData = t domTextData
    }
  go DOMComment {..} = HTMLComment
    { htmlCommentData = t domCommentData
    }
  f = map go . domMapID d . toList
  g = go . fromJust . domGetNode d
  h = map attr . toList
  t = T.decodeUtf8
  attr (DOMAttr n v s) = HTMLAttr (t n) (t v) s

-- | Converts DOM quirks to HTML quirks.
toHTMLQuirks :: DOMQuirks -> HTMLQuirks
toHTMLQuirks = \case
  DOMQuirksNone    -> HTMLQuirksNone
  DOMQuirksMode    -> HTMLQuirksMode
  DOMQuirksLimited -> HTMLQuirksLimited
