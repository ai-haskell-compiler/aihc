--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | The SAML2 specification distinguishes between the topmost status code,
-- which is required and must contain a status value from a specific list of
-- status codes, and subordinate status codes, which are optional and may
-- contain arbitrary URIs.
module Network.Wai.SAML2.StatusCode (
    StatusCode(..),
    StatusCodeValue(..)
) where

--------------------------------------------------------------------------------

import Control.Monad

import Data.Maybe
import qualified Data.Text as T

import Text.XML.Cursor

import Network.URI (URI, parseURI)
import Network.Wai.SAML2.XML

--------------------------------------------------------------------------------

-- | Represents SAML2 status codes, which are comprised of a status value
-- and an optional, subordinate status.
data StatusCode
    = MkStatusCode {
        -- | The status code value.
        statusCodeValue :: !StatusCodeValue,
        -- | An optional, subordinate status code.
        statusCodeSubordinate :: !(Maybe StatusCode)
    }
    deriving (Eq, Show)

-- | Enumerates SAML2 status code values.
--
-- @since 0.4
data StatusCodeValue
    -- | The response indicates success!
    = Success
    -- | The request could not be performed due to an error on the part of the
    -- requester.
    | Requester
    -- | The request could not be performed due to an error on the part of the
    -- SAML responder or SAML authority.
    | Responder
    -- | The SAML responder could not process the request because the version
    -- of the request message was incorrect.
    | VersionMismatch
    -- | The responding provider was unable to successfully authenticate the
    -- principal.
    | AuthnFailed
    -- | Unexpected or invalid content was encountered within a
    -- @\<saml:Attribute\>@ or @\<saml:AttributeValue\>@ element.
    | InvalidAttrNameOrValue
    -- | The responding provider cannot or will not support the requested name
    -- identifier policy.
    | InvalidNameIDPolicy
    -- | The specified authentication context requirements cannot be met by the
    -- responder.
    | NoAuthnContext
    -- | Used by an intermediary to indicate that none of the supported
    -- identity provider @\<Loc\>@ elements in an @\<IDPList\>@ can be resolved
    -- or that none of the supported identity providers are available.
    | NoAvailableIDP
    -- | Indicates the responding provider cannot authenticate the principal
    -- passively, as has been requested.
    | NoPassive
    -- | Used by an intermediary to indicate that none of the identity
    -- providers in an @\<IDPList\>@ are supported by the intermediary.
    | NoSupportedIDP
    -- | Used by a session authority to indicate to a session participant that
    -- it was not able to propagate logout to all other session participants.
    | PartialLogout
    -- | Indicates that a responding provider cannot authenticate the principal
    -- directly and is not permitted to proxy the request further.
    | ProxyCountExceeded
    -- | The SAML responder or SAML authority is able to process the request
    -- but has chosen not to respond. This status code MAY be used when there
    -- is concern about the security context of the request message or the
    -- sequence of request messages received from a particular requester.
    | RequestDenied
    -- | The SAML responder or SAML authority does not support the request.
    | RequestUnsupported
    -- | The SAML responder cannot process any requests with the protocol
    --  version specified in the request.
    | RequestVersionDeprecated
    -- | The SAML responder cannot process the request because the protocol
    -- version specified in the request message is a major upgrade from the
    -- highest protocol version supported by the responder.
    | RequestVersionTooHigh
    -- | The SAML responder cannot process the request because the protocol
    -- version specified in the request message is too low.
    | RequestVersionTooLow
    -- | The resource value provided in the request message is invalid or
    -- unrecognized.
    | ResourceNotRecognized
    -- | The response message would contain more elements than the SAML
    -- responder is able to return.
    | TooManyResponses
    -- | An entity that has no knowledge of a particular attribute profile
    -- has been presented with an attribute drawn from that profile.
    | UnknownAttrProfile
    -- | The responding provider does not recognize the principal specified
    -- or implied by the request.
    | UnknownPrincipal
    -- | The SAML responder cannot properly fulfil the request using the
    -- protocol binding specified in the request.
    | UnsupportedBinding
    -- | The SAML2 specification notes that a status code value can be any
    -- valid URI and that additional subordinate status codes may be
    -- introduced in the future.
    | OtherStatus URI
    deriving (Eq, Show)

instance FromXML StatusCode where
    parseXML = parseStatusCode True

-- | `parseStatusCode` @isTopLevel cursor@ attempts to parse a @<StatusCode>@
-- element from the XML @cursor@. The SAML2 specification distinguishes
-- between the topmost status code, which is required and must contain a
-- status value from a specific list of status codes, and subordinate status
-- codes. The @isTopLevel@ value indicates whether we are parsing a top-level
-- @<StatusCode>@ element or not and therefore controls which status codes
-- values we accept as valid.
--
-- @since 0.4
parseStatusCode :: MonadFail m => Bool -> Cursor -> m StatusCode
parseStatusCode isTopLevel cursor = do
    statusCodeValue <- oneOrFail "Value is a required attribute" $
        cursor $/
            element (saml2pName "Status") &/
            element (saml2pName "StatusCode") >=>
            parseStatusCodeValue isTopLevel
    let statusCodeSubordinate = listToMaybe (
            cursor $/
                element (saml2pName "Status") &/
                element (saml2pName "StatusCode")) >>=
                parseStatusCode False

    pure MkStatusCode{..}

-- | `parseStatusCodeValue` @isTopLevel cursor@ attempts to parse a status code
-- value from the XML @cursor@. The @isTopLevel@ value determines which values
-- we permit as valid status code values. See the note for `parseStatusCode`.
--
-- @since 0.4
parseStatusCodeValue :: MonadFail m => Bool -> Cursor -> m StatusCodeValue
parseStatusCodeValue isTopLevel cursor =
    case T.concat $ attribute "Value" cursor of
        -- the following status codes are always permitted
        "urn:oasis:names:tc:SAML:2.0:status:Success" -> pure Success
        "urn:oasis:names:tc:SAML:2.0:status:Requester" -> pure Requester
        "urn:oasis:names:tc:SAML:2.0:status:Responder" -> pure Responder
        "urn:oasis:names:tc:SAML:2.0:status:VersionMismatch" ->
            pure VersionMismatch
        -- the following are only permitted for subordinate elements
        "urn:oasis:names:tc:SAML:2.0:status:AuthnFailed" | not isTopLevel ->
            pure AuthnFailed
        "urn:oasis:names:tc:SAML:2.0:status:InvalidAttrNameOrValue" | not isTopLevel ->
            pure InvalidAttrNameOrValue
        "urn:oasis:names:tc:SAML:2.0:status:InvalidNameIDPolicy" | not isTopLevel ->
            pure InvalidNameIDPolicy
        "urn:oasis:names:tc:SAML:2.0:status:NoAuthnContext" | not isTopLevel ->
            pure NoAuthnContext
        "urn:oasis:names:tc:SAML:2.0:status:NoAvailableIDP" | not isTopLevel ->
            pure NoAvailableIDP
        "urn:oasis:names:tc:SAML:2.0:status:NoPassive" | not isTopLevel ->
            pure NoPassive
        "urn:oasis:names:tc:SAML:2.0:status:NoSupportedIDP" | not isTopLevel ->
            pure NoSupportedIDP
        "urn:oasis:names:tc:SAML:2.0:status:PartialLogout" | not isTopLevel ->
            pure PartialLogout
        "urn:oasis:names:tc:SAML:2.0:status:ProxyCountExceeded" | not isTopLevel ->
            pure ProxyCountExceeded
        "urn:oasis:names:tc:SAML:2.0:status:RequestDenied" | not isTopLevel ->
            pure RequestDenied
        "urn:oasis:names:tc:SAML:2.0:status:RequestUnsupported" | not isTopLevel ->
            pure RequestUnsupported
        "urn:oasis:names:tc:SAML:2.0:status:RequestVersionDeprecated" | not isTopLevel ->
            pure RequestVersionDeprecated
        "urn:oasis:names:tc:SAML:2.0:status:RequestVersionTooHigh" | not isTopLevel ->
            pure RequestVersionTooHigh
        "urn:oasis:names:tc:SAML:2.0:status:RequestVersionTooLow" | not isTopLevel ->
            pure RequestVersionTooLow
        "urn:oasis:names:tc:SAML:2.0:status:ResourceNotRecognized" | not isTopLevel ->
            pure ResourceNotRecognized
        "urn:oasis:names:tc:SAML:2.0:status:TooManyResponses" | not isTopLevel ->
            pure TooManyResponses
        "urn:oasis:names:tc:SAML:2.0:status:UnknownAttrProfile" | not isTopLevel ->
            pure UnknownAttrProfile
        "urn:oasis:names:tc:SAML:2.0:status:UnknownPrincipal" | not isTopLevel ->
            pure UnknownPrincipal
        "urn:oasis:names:tc:SAML:2.0:status:UnsupportedBinding" | not isTopLevel ->
            pure UnsupportedBinding
        uriString | not isTopLevel -> case parseURI $ T.unpack uriString of
            Nothing -> fail $ "Not a valid status code: " <> T.unpack uriString
            Just uri -> pure $ OtherStatus uri
        -- not a valid URI or a status code that's not supported at the
        -- top-level
        xs -> fail $ "Not a valid status code: " <> T.unpack xs

--------------------------------------------------------------------------------
