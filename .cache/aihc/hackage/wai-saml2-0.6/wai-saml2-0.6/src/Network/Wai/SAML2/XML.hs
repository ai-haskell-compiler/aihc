--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Utility functions related to XML parsing.
module Network.Wai.SAML2.XML (
    -- * Namespaces
    saml2Name,
    saml2pName,
    xencName,
    dsName,
    mdName,
    ecName,

    -- * Utility functions
    toMaybeText,
    showUTCTime,
    parseUTCTime,

    -- * XML parsing
    FromXML(..),
    oneOrFail,
    parseSettings
) where

--------------------------------------------------------------------------------

import qualified Data.Text as T
import Data.Time
import Data.Time.Format.ISO8601 (iso8601ParseM)

import Text.XML
import Text.XML.Cursor

--------------------------------------------------------------------------------

-- | 'saml2Name' @name@ constructs a 'Name' for @name@ in the
-- urn:oasis:names:tc:SAML:2.0:assertion namespace.
saml2Name :: T.Text -> Name
saml2Name name =
    Name name (Just "urn:oasis:names:tc:SAML:2.0:assertion") (Just "saml2")

-- | 'saml2pName' @name@ constructs a 'Name' for @name@ in the
-- urn:oasis:names:tc:SAML:2.0:protocol namespace.
saml2pName :: T.Text -> Name
saml2pName name =
    Name name (Just "urn:oasis:names:tc:SAML:2.0:protocol") (Just "saml2p")

-- | 'xencName' @name@ constructs a 'Name' for @name@ in the
-- http://www.w3.org/2001/04/xmlenc# namespace.
xencName :: T.Text -> Name
xencName name =
    Name name (Just "http://www.w3.org/2001/04/xmlenc#") (Just "xenc")

-- | 'dsName' @name@ constructs a 'Name' for @name@ in the
-- http://www.w3.org/2000/09/xmldsig# namespace.
dsName :: T.Text -> Name
dsName name =
    Name name (Just "http://www.w3.org/2000/09/xmldsig#") (Just "ds")

-- | `mdName` @name@ constructs a `Name` for @name@ in the
-- @urn:oasis:names:tc:SAML:2.0:metadata@ namespace.
mdName :: T.Text -> Name
mdName name =
    Name name (Just "urn:oasis:names:tc:SAML:2.0:metadata") (Just "md")


-- | 'ecName' @name@ constructs a 'Name' for @name@ in the
-- http://www.w3.org/2001/10/xml-exc-c14n# namespace.
--
-- @since 0.5
ecName :: T.Text -> Name
ecName name =
    Name name (Just "http://www.w3.org/2001/10/xml-exc-c14n#") (Just "ec")

-- | 'toMaybeText' @xs@ returns 'Nothing' if @xs@ is the empty list, or
-- the result of concatenating @xs@ wrapped in 'Just' otherwise.
toMaybeText :: [T.Text] -> Maybe T.Text
toMaybeText [] = Nothing
toMaybeText xs = Just $ T.concat xs

-- | The time format used by SAML2.
timeFormat :: String
timeFormat = "%Y-%m-%dT%H:%M:%S%6QZ"

-- | Display a 'UTCTime' as an ISO8601 timestamp including up to
-- 6 digits for the microseconds.
--
-- @since 0.4.0.0
--
showUTCTime :: UTCTime -> T.Text
showUTCTime = T.pack . formatTime defaultTimeLocale timeFormat

-- | 'parseUTCTime' @text@ parses @text@ into a 'UTCTime' value.
parseUTCTime :: MonadFail m => T.Text -> m UTCTime
parseUTCTime = iso8601ParseM . T.unpack

-- | A class of types which can be parsed from XML.
class FromXML a where
    parseXML :: MonadFail m => Cursor -> m a

-- | 'oneOrFail' @message xs@ throws an 'XMLException' with @message@ if
-- @xs@ is the empty list. If @xs@ has at least one element, the first is
-- returned and all others are discarded.
oneOrFail :: MonadFail m => String -> [a] -> m a
oneOrFail err [] = fail err
oneOrFail _ (x:_) = pure x

--------------------------------------------------------------------------------

-- | It is important to retain namespaces in order to calculate the hash of the canonicalised XML correctly.
-- see: https://stackoverflow.com/questions/69252831/saml-2-0-digest-value-calculation-in-saml-assertion
--
-- @since 0.5
parseSettings :: ParseSettings
parseSettings = def { psRetainNamespaces = True }
