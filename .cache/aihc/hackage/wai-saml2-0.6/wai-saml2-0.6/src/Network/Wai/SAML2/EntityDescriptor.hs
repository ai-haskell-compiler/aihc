--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE LambdaCase #-}

-- | This module provides a data type for IDP metadata containing certificate,
-- SSO URLs etc.
--
-- @since 0.4
module Network.Wai.SAML2.EntityDescriptor (
    IDPSSODescriptor(..),
    Binding(..)
) where

--------------------------------------------------------------------------------

import qualified Data.ByteString.Base64 as Base64
import qualified Data.X509 as X509
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Network.Wai.SAML2.XML

import Text.XML.Cursor

--------------------------------------------------------------------------------

-- | Describes metadata of an identity provider.
-- See also section 2.4.3 of [Metadata for the OASIS Security Assertion Markup Language (SAML) V2.0](https://docs.oasis-open.org/security/saml/v2.0/saml-metadata-2.0-os.pdf).
data IDPSSODescriptor
    = IDPSSODescriptor {
        -- | IdP Entity ID. 'Network.Wai.SAML2.Config.saml2ExpectedIssuer' should be compared against this identifier
        entityID :: Text
        -- | The X.509 certificate for signed assertions
    ,   x509Certificate :: X509.SignedExact X509.Certificate
        -- | Supported NameID formats
    ,   nameIDFormats :: [Text]
        -- | List of SSO urls corresponding to 'Binding's
    ,   singleSignOnServices :: [(Binding, Text)]
    } deriving Show

-- | urn:oasis:names:tc:SAML:2.0:bindings
-- https://docs.oasis-open.org/security/saml/v2.0/saml-bindings-2.0-os.pdf
data Binding
    -- | SAML protocol messages are transmitted within the base64-encoded content of an HTML form control
    = HTTPPost
    -- | SAML protocol messages are transmitted within URL parameters
    | HTTPRedirect
    -- | The request and/or response are transmitted by reference using a small stand-in called an artifact
    | HTTPArtifact
    -- | Reverse HTTP Binding for SOAP specification
    | PAOS
    -- | SOAP is a lightweight protocol intended for exchanging structured information in a decentralized, distributed environment
    | SOAP
    -- | SAML protocol messages are encoded into a URL via the DEFLATE compression method
    | URLEncodingDEFLATE
    deriving (Show, Eq)

instance FromXML IDPSSODescriptor where
    parseXML cursor = do
        let entityID = T.concat $ attribute "entityID" cursor
        descriptor <- oneOrFail "IDPSSODescriptor is required"
            $ cursor $/ element (mdName "IDPSSODescriptor")
        rawCertificate <- oneOrFail "X509Certificate is required" $ descriptor
            $/ element (mdName "KeyDescriptor")
            &/ element (dsName "KeyInfo")
            &/ element (dsName "X509Data")
            &/ element (dsName "X509Certificate")
            &/ content
        x509Certificate <- either fail pure
            $ X509.decodeSignedObject
            $ Base64.decodeLenient
            $ T.encodeUtf8 rawCertificate
        let nameIDFormats = descriptor
                $/ element (mdName "NameIDFormat")
                &/ content
        singleSignOnServices <- traverse parseService
            $ descriptor $/ element (mdName "SingleSignOnService")
        pure IDPSSODescriptor{..}

-- | `parseService` @cursor@ attempts to parse a pair of a `Binding` value
-- and a location given as a `Text` value from the XML @cursor@.
parseService :: MonadFail m => Cursor -> m (Binding, Text)
parseService cursor = do
    binding <- oneOrFail "Binding is required" (attribute "Binding" cursor)
        >>= parseBinding
    location <- oneOrFail "Location is required" $ attribute "Location" cursor
    pure (binding, location)

-- | `parseBinding` @uri@ attempts to parse a `Binding` value from @uri@.
parseBinding :: MonadFail m => Text -> m Binding
parseBinding = \case
    "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Artifact" -> pure HTTPArtifact
    "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-POST" -> pure HTTPPost
    "urn:oasis:names:tc:SAML:2.0:bindings:HTTP-Redirect" -> pure HTTPRedirect
    "urn:oasis:names:tc:SAML:2.0:bindings:PAOS" -> pure PAOS
    "urn:oasis:names:tc:SAML:2.0:bindings:SOAP" -> pure SOAP
    "urn:oasis:names:tc:SAML:2.0:bindings:URL-Encoding:DEFLATE"
        -> pure URLEncodingDEFLATE
    other -> fail $ "Unknown Binding: " <> T.unpack other

--------------------------------------------------------------------------------
