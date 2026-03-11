--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | This modules defines the `NameIDFormat` type, which specifies the format
-- of name identifiers in an assertion.
--
-- @since 0.4
module Network.Wai.SAML2.NameIDFormat (
    NameIDFormat(..),
    parseNameIDFormat,
    showNameIDFormat
) where

--------------------------------------------------------------------------------

import GHC.Generics (Generic)

import Data.Text (Text, unpack)

--------------------------------------------------------------------------------

-- | Format of the subject identifier.
-- See 8.3 Name Identifier Format Identifiers in https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf
data NameIDFormat
    -- | The interpretation is left to individual implementations
    = Unspecified
    -- | @addr-spec@ as defined in IETF RFC 2822
    | EmailAddress
    -- | contents of the @<ds:X509SubjectName>@ element in the XML Signature Recommendation
    | X509SubjectName
    -- | String of the form @DomainName\UserName@
    | WindowsDomainQualifiedName
    -- | Kerberos principal name using the format @name[/instance]@REALM@
    | KerberosPrincipalName
    -- | identifier of an entity that provides SAML-based services
    -- (such as a SAML authority, requester, or responder) or is a participant in SAML profiles (such as a service
    -- provider supporting the browser SSO profile)
    | Entity
    -- | identifier of a provider of SAML-based services
    -- (such as a SAML authority) or a participant in SAML
    -- profiles (such as a service provider supporting the browser profiles)
    | Provider
    -- | persistent opaque identifier that corresponds to an identity
    -- federation between an identity provider and a service provider
    | Federated
    -- | an identifier with transient semantics and SHOULD be treated
    -- as an opaque and temporary value by the relying party
    | Transient
    -- | persistent opaque identifier for a principal that is specific to
    -- an identity provider and a service provider or affiliation of service providers
    | Persistent
    deriving (Eq, Ord, Show, Generic)

-- | Parse a 'NameIDFormat' (prefixed by @urn:oasis:names:tc:SAML:*:nameid-format@).
parseNameIDFormat :: MonadFail m => Text -> m NameIDFormat
parseNameIDFormat = \case
    "urn:oasis:names:tc:SAML:1.1:nameid-format:Kerberos" -> pure KerberosPrincipalName
    "urn:oasis:names:tc:SAML:1.1:nameid-format:WindowsDomainQualifiedName" -> pure WindowsDomainQualifiedName
    "urn:oasis:names:tc:SAML:1.1:nameid-format:X509SubjectName" -> pure X509SubjectName
    "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress" -> pure EmailAddress
    "urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified" -> pure Unspecified
    "urn:oasis:names:tc:SAML:2.0:nameid-format:entity" -> pure Entity
    "urn:oasis:names:tc:SAML:2.0:nameid-format:federated" -> pure Federated
    "urn:oasis:names:tc:SAML:2.0:nameid-format:persistent" -> pure Persistent
    "urn:oasis:names:tc:SAML:2.0:nameid-format:provider" -> pure Provider
    "urn:oasis:names:tc:SAML:2.0:nameid-format:transient" -> pure Transient
    unknown -> fail $ "parseNameIDFormat: unknown format " <> unpack unknown

-- | Displays a `NameIDFormat` value as a `Text` value.
showNameIDFormat :: NameIDFormat -> Text
showNameIDFormat = \case
    KerberosPrincipalName -> "urn:oasis:names:tc:SAML:1.1:nameid-format:Kerberos"
    WindowsDomainQualifiedName -> "urn:oasis:names:tc:SAML:1.1:nameid-format:WindowsDomainQualifiedName"
    X509SubjectName -> "urn:oasis:names:tc:SAML:1.1:nameid-format:X509SubjectName"
    EmailAddress -> "urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress"
    Unspecified -> "urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified"
    Entity -> "urn:oasis:names:tc:SAML:2.0:nameid-format:entity"
    Federated -> "urn:oasis:names:tc:SAML:2.0:nameid-format:federated"
    Persistent -> "urn:oasis:names:tc:SAML:2.0:nameid-format:persistent"
    Provider -> "urn:oasis:names:tc:SAML:2.0:nameid-format:provider"
    Transient -> "urn:oasis:names:tc:SAML:2.0:nameid-format:transient"

--------------------------------------------------------------------------------
