{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Streaming renderer for the OPML 2.0 standard.
module Text.OPML.Conduit.Render
  ( -- * Renderers
    renderOpmlHead
  , renderOpmlOutline
  , renderOpml
  ) where

-- {{{ Imports
import           Control.Monad

import           Data.Conduit
import           Data.List.NonEmpty     hiding (filter, map)
import           Data.Monoid
import           Data.String
import           Data.Text              (Text, intercalate, pack, toLower)
import           Data.Text.Encoding
import           Data.Time.Clock
import           Data.Time.LocalTime
import           Data.Time.RFC822
import           Data.Tree
import           Data.Version
import           Data.XML.Types
import           Lens.Micro
import           Prelude                hiding (foldr, lookup, show)
import qualified Prelude                (show)
import           Refined                hiding (NonEmpty)
import           Text.OPML.Lens
import           Text.OPML.Types
import           Text.XML.Stream.Render
import           URI.ByteString
-- }}}

show :: (Show a, IsString t) => a -> t
show = fromString . Prelude.show

empty :: (Eq s, Monoid s) => s -> Bool
empty t = t == mempty

toMaybe :: (Eq s, Monoid s) => s -> Maybe s
toMaybe s | s == mempty = mempty
          | otherwise   = Just s

formatTime :: UTCTime -> Text
formatTime = formatTimeRFC822 . utcToZonedTime utc

formatCategories :: [NonEmpty (Refined (Not Null) Text)] -> Maybe Text
formatCategories = toMaybe . intercalate "," . map (intercalate "/" . toList . fmap unrefine)

formatBool :: Bool -> Text
formatBool = toLower . show

formatURI :: URI -> Text
formatURI = decodeUtf8 . serializeURIRef'

-- | Render the @\<head\>@ section.
renderOpmlHead :: (Monad m) => OpmlHead -> ConduitT () Event m ()
renderOpmlHead input = tag "head" mempty $ do
  forM_ (input^.opmlCreatedL)       $ tag "dateCreated" mempty . content . formatTime
  forM_ (input^.modifiedL)          $ tag "dateModified" mempty . content . formatTime
  forM_ (input^.docsL)              $ tag "docs" mempty . content . formatURI
  unless (null es)                  $ tag "expansionState" mempty . content . intercalate "," $ show <$> es
  unless (empty email)              $ tag "ownerEmail" mempty $ content email
  forM_ (input^.ownerIdL)           $ tag "ownerId" mempty . content . formatURI
  unless (empty name)               $ tag "ownerName" mempty $ content name
  unless (empty title)              $ tag "title" mempty $ content title
  forM_ (input^.vertScrollStateL)   $ tag "vertScrollState" mempty . content . show
  forM_ (input^.windowBottomL)      $ tag "windowBottom" mempty . content . show
  forM_ (input^.windowLeftL)        $ tag "windowLeft" mempty . content . show
  forM_ (input^.windowRightL)       $ tag "windowRight" mempty . content . show
  forM_ (input^.windowTopL)         $ tag "windowTop" mempty . content . show
  where es = input ^. expansionStateL
        email = input ^. ownerEmailL
        name = input ^. ownerNameL
        title = input ^. opmlTitleL

-- | Render an @\<outline\>@ section.
renderOpmlOutline :: (Monad m) => Tree OpmlOutline -> ConduitT () Event m ()
renderOpmlOutline (Node outline subOutlines) = tag "outline" attributes $ mapM_ renderOpmlOutline subOutlines
  where attributes = case outline of
          OpmlOutlineGeneric b t      -> baseAttr b <> optionalAttr "type" (toMaybe t)
          OpmlOutlineLink b uri       -> baseAttr b <> attr "type" "link" <> attr "url" (formatURI uri)
          OpmlOutlineSubscription b s -> baseAttr b <> subscriptionAttr s
        baseAttr b = attr "text" (unrefine $ b^.textL)
                     <> optionalAttr "isComment" (formatBool <$> b^.isCommentL)
                     <> optionalAttr "isBreakpoint" (formatBool <$> b^.isBreakpointL)
                     <> optionalAttr "created" (formatTime <$> b^.outlineCreatedL)
                     <> optionalAttr "category" (formatCategories $ b^.categoriesL)
        subscriptionAttr s = attr "type" "rss"
                             <> attr "xmlUrl" (formatURI $ s^.xmlUriL)
                             <> optionalAttr "htmlUrl" (formatURI <$> s^.htmlUriL)
                             <> optionalAttr "description" (toMaybe $ s^.descriptionL)
                             <> optionalAttr "language" (toMaybe $ s^.languageL)
                             <> optionalAttr "title" (toMaybe $ s^.subscriptionTitleL)
                             <> optionalAttr "version" (toMaybe $ s^.subscriptionVersionL)

-- | Render the top-level @\<opml\>@ section.
renderOpml :: Monad m => Opml -> ConduitT () Event m ()
renderOpml opml = tag "opml" (attr "version" . pack . showVersion $ opml^.opmlVersionL) $ do
  renderOpmlHead $ opml^.opmlHeadL
  tag "body" mempty . mapM_ renderOpmlOutline $ opml^.opmlOutlinesL
