{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
-- | OPML is an XML format for outlines.
--
-- Example:
--
-- > <opml version="2.0">
-- >   <head>
-- >     <title>OPML title</title>
-- >     <dateCreated>Mon, 31 Oct 2005 19:23:00 GMT</dateCreated>
-- >   </head>
-- >   <body>
-- >     <outline text="Outline 1" created="Mon, 31 Oct 2005 18:21:33 GMT"/>
-- >     <outline text="Outline 2" created="Mon, 31 Oct 2005 18:21:33 GMT"/>
-- >   </body>
-- > </opml>
module Text.OPML.Types
  ( -- * Top-level OPML structure
    Opml(..)
  , mkOpml
    -- * OPML header
  , OpmlHead(..)
  , mkOpmlHead
    -- * OPML outlines
  , OpmlOutline(..)
    -- ** Generic outlines
  , OutlineBase(..)
  , mkOutlineBase
    -- ** Subscription outlines
  , OutlineSubscription(..)
  , mkOutlineSubscription
    -- * Others
  , Null()
  ) where

-- {{{ Imports
import           Control.Monad
import           Data.List.NonEmpty
import           Data.Text           as Text
import           Data.Time.Clock
import           Data.Time.LocalTime ()
import           Data.Tree
import           Data.Typeable
import           Data.Version
import           GHC.Generics
import           Refined             hiding (NonEmpty)
import           URI.ByteString
-- }}}

-- | 'Predicate' on 'Text', true iff text is null.
data Null deriving(Typeable)

instance Predicate Null Text where
  validate p value = if not $ Text.null value
    then pure $ RefineOtherException (typeOf p) "Text is not null"
    else Nothing

data OpmlHead = OpmlHead
  { opmlTitle       :: Text
  , opmlCreated     :: Maybe UTCTime
  , modified        :: Maybe UTCTime
  , ownerName       :: Text
  , ownerEmail      :: Text
  , ownerId         :: Maybe URI
  , docs            :: Maybe URI
  , expansionState  :: [Int]
  , vertScrollState :: Maybe Int
  , windowBottom    :: Maybe Int
  , windowLeft      :: Maybe Int
  , windowRight     :: Maybe Int
  , windowTop       :: Maybe Int
  }

deriving instance Eq OpmlHead
deriving instance Generic OpmlHead
deriving instance Show OpmlHead

-- | Bare 'OpmlHead', all fields are set to 'mempty'.
mkOpmlHead :: OpmlHead
mkOpmlHead = OpmlHead mempty mzero mzero mempty mempty mzero mzero mzero mzero mzero mzero mzero mzero

data OutlineBase = OutlineBase
  { text           :: Refined (Not Null) Text
  , isComment      :: Maybe Bool
  , isBreakpoint   :: Maybe Bool
  , outlineCreated :: Maybe UTCTime
  , categories     :: [NonEmpty (Refined (Not Null) Text)] -- ^
  }

deriving instance Eq OutlineBase
deriving instance Generic OutlineBase
deriving instance Show OutlineBase


-- | Smart constructor for 'OutlineBase'.
mkOutlineBase :: Refined (Not Null) Text -> OutlineBase
mkOutlineBase t = OutlineBase t mzero mzero mzero mzero


data OutlineSubscription = OutlineSubscription
  { xmlUri              :: URI
  , htmlUri             :: Maybe URI
  , description         :: Text
  , language            :: Text
  , subscriptionTitle   :: Text
  , subscriptionVersion :: Text
  }

deriving instance Eq OutlineSubscription
deriving instance Generic OutlineSubscription
deriving instance Show OutlineSubscription


-- | Smart constructor for 'OutlineSubscription'
mkOutlineSubscription :: URI -> OutlineSubscription
mkOutlineSubscription uri = OutlineSubscription uri mzero mempty mempty mempty mempty


-- | Outlines are the main payload of an OPML document.
data OpmlOutline = OpmlOutlineGeneric { opmlOutlineBase :: OutlineBase, opmlOutlineContent :: Text }
                 | OpmlOutlineLink { opmlOutlineBase :: OutlineBase, opmlOutlineUri :: URI }
                 | OpmlOutlineSubscription { opmlOutlineBase :: OutlineBase, opmlOutlineSubscription :: OutlineSubscription }

deriving instance Eq OpmlOutline
deriving instance Generic OpmlOutline
deriving instance Show OpmlOutline

data Opml = Opml
  { opmlVersion  :: Version
  , opmlHead     :: OpmlHead
  , opmlOutlines :: Forest OpmlOutline
  }

deriving instance Eq Opml
deriving instance Generic Opml
deriving instance Show Opml

-- | Bare 'Opml'. Version is set to @2.0@.
mkOpml :: Opml
mkOpml = Opml (makeVersion [2, 0]) mkOpmlHead mempty
