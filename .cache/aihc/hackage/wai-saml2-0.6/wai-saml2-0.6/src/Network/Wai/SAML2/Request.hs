-------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                  --
-------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE   --
-- file in the root directory of this source tree.                           --
-------------------------------------------------------------------------------

-- | Defines types and functions for SP-initiated SSO. Use `issueAuthnRequest`
-- to initialise an `AuthnRequest` value which stores the parameters for the
-- authentication request you wish to issue to the IdP. You can update this
-- value as required.
--
-- Use `renderBase64` to render the request for use with a HTTP POST binding [1], or
-- `renderUrlEncodingDeflate` for HTTP redirect binding[2] respectively.
-- You may wish to read
-- the [SAML2 overview for this process](http://docs.oasis-open.org/security/saml/Post2.0/sstc-saml-tech-overview-2.0-cd-02.html#5.1.2.SP-Initiated%20SSO:%20%20Redirect/POST%20Bindings|outline).
--
-- * [1] https://docs.oasis-open.org/security/saml/v2.0/saml-bindings-2.0-os.pdf#page=21
--   Section 3.5 HTTP POST Binding
-- * [2] https://docs.oasis-open.org/security/saml/v2.0/saml-bindings-2.0-os.pdf#page=15
--   Section 3.4 HTTP Redirect Binding
--
-- @since 0.4
module Network.Wai.SAML2.Request (
    AuthnRequest(..),
    issueAuthnRequest,
    renderBase64,
    renderUrlEncodingDeflate,
    renderXML,
) where

-------------------------------------------------------------------------------

import Crypto.Random

import Data.Time.Clock

import Network.Wai.SAML2.NameIDFormat
import Network.Wai.SAML2.XML

import Text.XML

import qualified Codec.Compression.Zlib.Raw as Deflate
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Network.HTTP.Types (urlEncode)

-------------------------------------------------------------------------------

-- | Parameters for SP-initiated SSO
data AuthnRequest
    -- Reference [AuthnRequest]
    = AuthnRequest {
        -- | The time at which 'AuthnRequest' was created.
        authnRequestTimestamp :: !UTCTime
        -- | Unique identifier for 'AuthnRequest' which should be preserved
        -- by the IdP in its response.
    ,   authnRequestID :: !T.Text
        -- | SP Entity ID
    ,   authnRequestIssuer :: !T.Text
        -- | The URI reference to which this request is to be sent. Required
        -- for signed requests
        --
        -- @since 0.5
    ,   authnRequestDestination :: !(Maybe T.Text)
        -- | Allow IdP to generate a new identifier
    ,   authnRequestAllowCreate :: !Bool
        -- | The URI reference corresponding to a name identifier format
    ,   authnRequestNameIDFormat :: !NameIDFormat
    }
    deriving (Eq, Show)

-- | Creates a default 'AuthnRequest' with the current timestamp and a
-- randomly-generated ID.
issueAuthnRequest
    :: T.Text -- ^ SP Entity ID
    -> IO AuthnRequest
issueAuthnRequest authnRequestIssuer = do
    authnRequestTimestamp <- getCurrentTime
    -- Digits are not allowed in initial position
    -- Reference [ID Values]
    authnRequestID <- ("id" <>) . T.decodeUtf8 . Base16.encode <$> getRandomBytes 16
    pure AuthnRequest{
        authnRequestAllowCreate = True
    ,   authnRequestNameIDFormat = Transient
    ,   authnRequestDestination = Nothing
    ,   ..
    }

-- | Renders an `AuthnRequest` for SP initiated SSO according to
-- @urn:oasis:names:tc:SAML:2.0:bindings:URL-Encoding:DEFLATE@ and suitable for
-- use with HTTP Redirect binding
--
-- The value should be sent as a query parameter named @SAMLRequest@
renderUrlEncodingDeflate :: AuthnRequest -> B.ByteString
renderUrlEncodingDeflate request =
    urlEncode True $ Base64.encode $ BL.toStrict $ Deflate.compress $ renderXML request

-- | Renders and base64-encodes an `AuthnRequest` for SP initiated SSO suitable
-- for use with HTTP POST binding
--
-- If used in an HTTP POST binding, the value should be sent as an invisible
-- form control named @SAMLRequest@
renderBase64 :: AuthnRequest -> B.ByteString
renderBase64 request = Base64.encode $ BL.toStrict $ renderXML request

-- | Render an `AuthnRequest` as XML
renderXML :: AuthnRequest -> BL.ByteString
renderXML AuthnRequest{..} =
    renderLBS def $
    -- Reference [HTTP redirect binding]
    Document{
        documentPrologue = Prologue [] Nothing []
    ,   documentRoot = root
    ,   documentEpilogue = []
    }
    where
        timestamp = showUTCTime authnRequestTimestamp
        root = Element
            (saml2pName "AuthnRequest")
            (Map.fromList
               ([ ("xmlns:samlp", "urn:oasis:names:tc:SAML:2.0:protocol")
                , ("xmlns:saml", "urn:oasis:names:tc:SAML:2.0:assertion")
                , ("ID", authnRequestID) -- Reference [RequestAbstractType] and see [ID Values]
                , ("Version", "2.0")  -- [RequestAbstractType]
                , ("IssueInstant", timestamp) -- [RequestAbstractType]
                , ("AssertionConsumerServiceIndex", "1") -- [AuthnRequest]
                ]
                -- [RequestAbstractType]
                ++ [("Destination", uri) | let Just uri = authnRequestDestination] ))
            [NodeElement issuer, NodeElement nameIdPolicy]
        -- Reference [RequestAbstractType]
        issuer = Element
            (saml2Name "Issuer")
            mempty
            [NodeContent authnRequestIssuer]
        -- Reference [AuthnRequest]
        nameIdPolicy = Element
            (saml2pName "NameIDPolicy")
            (Map.fromList
                [ ("allowCreate"
                    , if authnRequestAllowCreate then "true" else "false")
                , ("Format", showNameIDFormat authnRequestNameIDFormat)
                ])
            []

-------------------------------------------------------------------------------

-- Reference [RequestAbstractType]
-- Source: https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf#page=36
-- Section: 3.2.1 Complex Type RequestAbstractType

-- Reference [AuthnRequest]
-- Source: https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf#page=48
-- Section: 3.4.1 Element <AuthnRequest>

-- Reference [HTTP redirect binding]
-- Source:
-- https://docs.oasis-open.org/security/saml/v2.0/saml-bindings-2.0-os.pdf#page=15
-- Section: 3.4 HTTP Redirect Binding

-- Reference [ID Values]
-- Source: https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf#page=9
-- Section: 1.3.4 ID and ID Reference Values
-- Note: ID Values must conform to "xs:ID", which in turn has a restriction of "xs:NCName" (non-colonized name).
-- In practice that means they are a string consisting of
-- first 1 of: Letter or '_'
-- then 0 or more of: Letter, Digit, '.', '-', '_',  CombiningChar, Extender
--
-- Definitions of character classes: https://www.w3.org/TR/2000/WD-xml-2e-20000814#CharClasses
-- Compare e.g. https://stackoverflow.com/questions/1631396/what-is-an-xsncname-type-and-when-should-it-be-used
--and https://www.w3.org/TR/xmlschema-2/#dt-ccesN (see \i and \c, bute not that colons are excluded)
