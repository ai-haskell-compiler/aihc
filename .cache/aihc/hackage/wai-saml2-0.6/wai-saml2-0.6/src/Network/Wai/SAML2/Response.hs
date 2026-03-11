--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Types to reprsent SAML2 responses.
module Network.Wai.SAML2.Response (
    -- * SAML2 responses
    Response(..),
    removeSignature,
    extractSignedInfo,
    extractPrefixList,

    -- * Re-exports
    module Network.Wai.SAML2.StatusCode,
    module Network.Wai.SAML2.Signature
) where

--------------------------------------------------------------------------------

import Data.Maybe (listToMaybe)
import qualified Data.Text as T
import Data.Time

import Text.XML
import Text.XML.Cursor

import Network.Wai.SAML2.Assertion
import Network.Wai.SAML2.XML
import Network.Wai.SAML2.XML.Encrypted
import Network.Wai.SAML2.StatusCode
import Network.Wai.SAML2.Signature

--------------------------------------------------------------------------------

-- | Represents SAML2 responses.

-- Reference [StatusResponseType]
data Response = Response {
    -- | The intended destination of this response.
    responseDestination :: !T.Text,
    -- | The ID of the request this responds corresponds to, if any.
    --
    -- @since 0.4
    responseInResponseTo :: !(Maybe T.Text),
    -- | The unique ID of the response.
    responseId :: !T.Text,
    -- | The timestamp when the response was issued.
    responseIssueInstant :: !UTCTime,
    -- | The SAML version.
    responseVersion :: !T.Text,
    -- | The name of the issuer.
    responseIssuer :: !T.Text,
    -- | The status of the response.
    responseStatusCode :: !StatusCode,
    -- | The response signature.
    responseSignature :: !Signature,
    -- | The unencrypted assertion.
    --
    -- @since 0.4
    responseAssertion :: !(Maybe Assertion),
    -- | The encrypted assertion.
    --
    -- @since 0.4
    responseEncryptedAssertion :: !(Maybe EncryptedAssertion)
} deriving (Eq, Show)

instance FromXML Response where
    -- Reference [StatusResponseType]
    parseXML cursor = do
        issueInstant <- parseUTCTime
                      $ T.concat
                      $ attribute "IssueInstant" cursor

        statusCode <- case parseXML cursor of
            Nothing -> fail "Invalid status code"
            Just sc -> pure sc

        let assertion = listToMaybe
                    $ ( cursor
                    $/  element (saml2Name "Assertion")
                    ) >>= parseXML

        let encAssertion = listToMaybe
                    $ ( cursor
                    $/  element (saml2Name "EncryptedAssertion")
                    ) >>= parseXML

        signature <- oneOrFail "Signature is required" (
            cursor $/ element (dsName "Signature") ) >>= parseXML

        pure Response{
            responseDestination = T.concat $ attribute "Destination" cursor,
            responseId = T.concat $ attribute "ID" cursor,
            responseInResponseTo = listToMaybe $ attribute "InResponseTo" cursor,
            responseIssueInstant = issueInstant,
            responseVersion = T.concat $ attribute "Version" cursor,
            responseIssuer = T.concat $
                cursor $/ element (saml2Name "Issuer") &/ content,
            responseStatusCode = statusCode,
            responseSignature = signature,
            responseAssertion = assertion,
            responseEncryptedAssertion = encAssertion
        }

--------------------------------------------------------------------------------

-- | Returns 'True' if the argument is not a @<Signature>@ element.
isNotSignature :: Node -> Bool
isNotSignature (NodeElement e) = elementName e /= dsName "Signature"
isNotSignature _ = True

-- | 'removeSignature' @document@ removes all @<Signature>@ elements from
-- @document@ and returns the resulting document.
removeSignature :: Document -> Document
removeSignature (Document prologue root misc) =
    let Element n attr ns = root
    in Document prologue (Element n attr (filter isNotSignature ns)) misc

-- | Returns all nodes at @cursor@.
nodes :: MonadFail m => Cursor -> m Node
nodes = pure . node

-- | 'extractSignedInfo' @cursor@ extracts the SignedInfo element from the
-- document reprsented by @cursor@.
extractSignedInfo :: MonadFail m => Cursor -> m Element
extractSignedInfo cursor = do
    NodeElement signedInfo <- oneOrFail "SignedInfo is required"
                            ( cursor
                           $/ element (dsName "Signature")
                           &/ element (dsName "SignedInfo")
                          ) >>= nodes
    pure signedInfo

-- | Obtain a list of InclusiveNamespaces entries used for exclusive XML canonicalisation.
--
-- @since 0.5
extractPrefixList :: Cursor -> [T.Text]
extractPrefixList cursor = concatMap T.words
    $ concatMap (attribute "PrefixList")
    $ cursor
    $/ element (dsName "Reference")
    &/ element (dsName "Transforms")
    &/ element (dsName "Transform")
    &/ element (ecName "InclusiveNamespaces")

--------------------------------------------------------------------------------

-- Reference [StatusResponseType]
--   Source: https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf#page=38
--   Section: 3.2.2 Complex Type StatusResponseType
