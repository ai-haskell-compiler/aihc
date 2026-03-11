--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | SAML2-related errors.
module Network.Wai.SAML2.Error (
    SAML2Error(..)
) where

--------------------------------------------------------------------------------

import Control.Exception

import Crypto.Error
import Crypto.PubKey.RSA.Types as RSA

import qualified Data.Text as T

import Network.Wai.SAML2.StatusCode

--------------------------------------------------------------------------------

-- | Enumerates errors that may arise in the SAML2 middleware.
data SAML2Error
    -- | The response received from the client is not valid XML.
    = InvalidResponseXml SomeException
    -- | The assertion is not valid XML.
    | InvalidAssertionXml SomeException
    -- | The response is not a valid SAML2 response.
    | InvalidResponse IOException
    -- | The assertion is not a valid SAML2 assertion.
    | InvalidAssertion IOException
    -- | The issuer is not who we expected.
    | InvalidIssuer T.Text
    -- | The destination is not what we expected.
    | UnexpectedDestination T.Text
    -- | The reference ID is not what we expected.
    | UnexpectedReference T.Text
    -- | The response indicates a stuatus other than 'Success'.
    | Unsuccessful StatusCode
    -- | Failed to canonicalise some XML.
    | CanonicalisationFailure IOException
    -- | Unable to decrypt the AES key.
    | DecryptionFailure RSA.Error
    -- | The initialisation vector for a symmetric cipher is invalid.
    | InvalidIV
    -- | The padding for a blockcipher is invalid.
    | InvalidPadding
    -- | The signature is incorrect.
    | InvalidSignature
    -- | The digest is incorrect.
    | InvalidDigest
    -- | The assertion is not valid.
    | NotValid
    -- | Audience restrictions don't match our audiences.
    --
    -- @since 0.4
    | AudienceMismatch [T.Text]
    -- | A general crypto error occurred.
    | CryptoError CryptoError
    -- | The request made to the configured endpoint is not valid.
    | InvalidRequest
    -- | The configuration requires an encrypted assertion, but got a
    -- plaintext assertion.
    --
    -- @since 0.4
    | EncryptedAssertionRequired
    -- | The configuration does not support decryption, but got an encrypted
    -- assertion.
    --
    -- @since 0.4
    | EncryptedAssertionNotSupported
    deriving Show

--------------------------------------------------------------------------------
