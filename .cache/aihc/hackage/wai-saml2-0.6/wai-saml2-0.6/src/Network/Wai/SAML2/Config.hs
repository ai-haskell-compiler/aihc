--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Configuration types and smart constructors for the SAML2 middleware.
module Network.Wai.SAML2.Config (
    SAML2Config(..),
    saml2Config,
    saml2ConfigNoEncryption
) where

--------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Crypto.PubKey.RSA

--------------------------------------------------------------------------------

-- | Represents configurations for the SAML2 middleware.
data SAML2Config = SAML2Config {
    -- | The path relative to the root of the web application at which the
    -- middleware should listen for SAML2 assertions (e.g. /sso/assert).
    saml2AssertionPath :: !BS.ByteString,
    -- | The service provider's private key, used to decrypt data from
    -- the identity provider.
    -- when set to Nothing, it rejects encrypted assertions.
    saml2PrivateKey :: !(Maybe PrivateKey),
    -- | The identity provider's public key, used to validate
    -- signatures.
    saml2PublicKey :: !PublicKey,
    -- | The name of the entity we expect assertions from. If this is set
    -- to 'Nothing', the issuer name is not validated.
    saml2ExpectedIssuer :: !(Maybe T.Text),
    -- | The URL we expect the SAML2 response to contain as destination.
    saml2ExpectedDestination :: !(Maybe T.Text),
    -- | The audiences we are a member of. An assertions is only valid if we are
    -- a member of at least one of its audiences. Leaving this empty disables
    -- the check
    --
    -- @since 0.4
    saml2Audiences :: ![T.Text],
    -- | A value indicating whether to disable time validity checks. This
    -- should not be set to 'True' in a production environment, but may
    -- be useful for testing purposes.
    saml2DisableTimeValidation :: !Bool,
    -- | Always decrypt assertions using 'saml2PrivateKey' and reject plaintext assertions.
    --
    -- @since 0.4
    saml2RequireEncryptedAssertion :: !Bool
}

-- | 'saml2Config' @privateKey publicKey@ constructs a 'SAML2Config' value
-- with the most basic set of options possible using @privateKey@ as the
-- SP's private key and @publicKey@ as the IdP's public key. You should
-- almost certainly change the resulting settings.
-- This requires encrypted assertions by default.
saml2Config :: PrivateKey -> PublicKey -> SAML2Config
saml2Config privKey pubKey = (saml2ConfigNoEncryption pubKey){
    saml2PrivateKey = Just privKey,
    saml2RequireEncryptedAssertion = True
}

-- | 'saml2ConfigNoEncryption' @publicKey@ constructs a 'SAML2Config' value
-- with the most basic set of options possible using @publicKey@ as the
-- IdP's public key. You should almost certainly change the resulting settings.
--
-- @since 0.4.0.0
--
saml2ConfigNoEncryption :: PublicKey -> SAML2Config
saml2ConfigNoEncryption pubKey = SAML2Config{
    saml2AssertionPath = "/sso/assert",
    saml2PrivateKey = Nothing,
    saml2PublicKey = pubKey,
    saml2ExpectedIssuer = Nothing,
    saml2ExpectedDestination = Nothing,
    saml2Audiences = [],
    saml2DisableTimeValidation = False,
    saml2RequireEncryptedAssertion = False
}

--------------------------------------------------------------------------------
