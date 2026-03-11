--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | SAML2 signatures.
module Network.Wai.SAML2.Signature (
    CanonicalisationMethod(..),
    SignatureMethod(..),
    DigestMethod(..),
    SignedInfo(..),
    Reference(..),
    Signature(..)
) where

--------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding

import Text.XML.Cursor

import Network.Wai.SAML2.XML

--------------------------------------------------------------------------------

-- | Enumerates XML canonicalisation methods.
data CanonicalisationMethod
    -- | Original C14N 1.0 specification.
    = C14N_1_0
    -- | Exclusive C14N 1.0 specification.
    | C14N_EXC_1_0
    -- | C14N 1.1 specification.
    | C14N_1_1
    deriving (Eq, Show)

instance FromXML CanonicalisationMethod where
    parseXML cursor =
        case T.concat $ attribute "Algorithm" cursor of
            "http://www.w3.org/2001/10/xml-exc-c14n#" -> pure C14N_EXC_1_0
            _ -> fail "Not a valid CanonicalisationMethod"

-- | Enumerates signature methods.
data SignatureMethod
    -- | RSA with SHA256 digest
    = RSA_SHA256
    deriving (Eq, Show)

instance FromXML SignatureMethod where
    parseXML cursor = case T.concat $ attribute "Algorithm" cursor of
        "http://www.w3.org/2001/04/xmldsig-more#rsa-sha256" -> pure RSA_SHA256
        _ -> fail "Not a valid SignatureMethod"

--------------------------------------------------------------------------------

-- | Enumerates digest methods.
data DigestMethod
    -- | SHA256
    = DigestSHA256
    deriving (Eq, Show)

instance FromXML DigestMethod where
    parseXML cursor =  case T.concat $ attribute "Algorithm" cursor of
        "http://www.w3.org/2001/04/xmlenc#sha256" -> pure DigestSHA256
        _ -> fail "Not a valid DigestMethod"

-- | Represents a reference to some entity along with a digest of it.
data Reference = Reference {
    -- | The URI of the entity that is referenced.
    referenceURI :: !T.Text,
    -- | The method that was used to calculate the digest for the
    -- entity that is referenced.
    referenceDigestMethod :: !DigestMethod,
    -- | The digest of the entity that was calculated by the IdP.
    referenceDigestValue :: !BS.ByteString
} deriving (Eq, Show)

instance FromXML Reference where
    parseXML cursor = do
        -- the reference starts with a #, drop it
        let uri = T.drop 1 $ T.concat $ attribute "URI" cursor

        digestMethod <- oneOrFail "DigestMethod is required" (
            cursor $/ element (dsName "DigestMethod")
            ) >>= parseXML

        let digestValue = encodeUtf8 $ T.concat $
                cursor $/ element (dsName "DigestValue") &/ content

        pure Reference{
            referenceURI = uri,
            referenceDigestMethod = digestMethod,
            referenceDigestValue = digestValue
        }

--------------------------------------------------------------------------------

-- | Represents references to some entities for which the IdP has calculated
-- digests. The 'SignedInfo' component is then signed by the IdP.
data SignedInfo = SignedInfo {
    -- | The XML canonicalisation method used.
    signedInfoCanonicalisationMethod :: !CanonicalisationMethod,
    -- | The method used to compute the signature for the referenced entity.
    signedInfoSignatureMethod :: !SignatureMethod,
    -- | The reference to some entity, along with a digest.
    signedInfoReference :: !Reference
} deriving (Eq, Show)

instance FromXML SignedInfo where
    parseXML cursor = do
        canonicalisationMethod <-
                oneOrFail "CanonicalizationMethod is required"
              ( cursor
             $/ element (dsName "CanonicalizationMethod")
              ) >>= parseXML

        signatureMethod <-
                oneOrFail "SignatureMethod is required"
              ( cursor
             $/ element (dsName "SignatureMethod")
            ) >>= parseXML

        reference <-
                oneOrFail "Reference is required"
              ( cursor
             $/ element (dsName "Reference")
            ) >>= parseXML

        pure SignedInfo{
            signedInfoCanonicalisationMethod = canonicalisationMethod,
            signedInfoSignatureMethod = signatureMethod,
            signedInfoReference = reference
        }

-- | Represents response signatures.
data Signature = Signature {
    -- | Information about the data for which the IdP has computed digests.
    signatureInfo :: !SignedInfo,
    -- | The signature of the 'SignedInfo' value.
    signatureValue :: !BS.ByteString
} deriving (Eq, Show)

instance FromXML Signature where
    parseXML cursor = do
        info <- oneOrFail "SignedInfo is required" (
            cursor $/ element (dsName "SignedInfo") ) >>= parseXML

        let value = encodeUtf8 $ T.concat $
                cursor $/ element (dsName "SignatureValue") &/ content

        pure Signature{
            signatureInfo = info,
            signatureValue = value
        }

--------------------------------------------------------------------------------
