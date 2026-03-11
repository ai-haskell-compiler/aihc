--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Functions to process and validate SAML2 respones.
module Network.Wai.SAML2.Validation (
    validateResponse,
    decodeResponse,
    validateSAMLResponse,
    ansiX923
) where

--------------------------------------------------------------------------------

import Control.Exception
import Control.Monad (forM_, when, unless)
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)

import Crypto.Error
import Crypto.Hash
import qualified Crypto.PubKey.RSA.OAEP as OAEP
import Crypto.PubKey.RSA.PKCS15 as PKCS15
import Crypto.PubKey.RSA.Types (PrivateKey)
import Crypto.Cipher.AES
import Crypto.Cipher.Types

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Default.Class
import Data.Time

import Network.Wai.SAML2.XML.Encrypted
import Network.Wai.SAML2.Config
import Network.Wai.SAML2.Error
import Network.Wai.SAML2.XML
import Network.Wai.SAML2.C14N
import Network.Wai.SAML2.Response
import Network.Wai.SAML2.Assertion

import qualified Text.XML as XML
import qualified Text.XML.Cursor as XML

--------------------------------------------------------------------------------

-- | 'validateResponse' @cfg responseData@ validates a SAML2 response contained
-- in Base64-encoded @responseData@.
validateResponse :: SAML2Config
                 -> BS.ByteString
                 -> IO (Either SAML2Error (Assertion, Response))
validateResponse cfg responseData = runExceptT $ do
    -- get the current time
    now <- liftIO getCurrentTime

    (responseXmlDoc, samlResponse) <- decodeResponse responseData
    assertion <- validateSAMLResponse cfg responseXmlDoc samlResponse now
    pure (assertion, samlResponse)

-- | 'decodeResponse' @responseData@ decodes a SAML2 response contained
-- in Base64-encoded @responseData@.
--
-- @since 0.4
decodeResponse :: BS.ByteString -> ExceptT SAML2Error IO (XML.Document, Response)
decodeResponse responseData = do
    -- the response data is Base64-encoded; decode it
    let resXmlDocData = BS.decodeLenient responseData

    -- try to parse the XML document; throw an exception if it is not
    -- a valid XML document
    responseXmlDoc <- case XML.parseLBS parseSettings (LBS.fromStrict resXmlDocData) of
        Left err -> throwError $ InvalidResponseXml err
        Right responseXmlDoc -> pure responseXmlDoc

    -- try to parse the XML document into a structured SAML2 response
    resParseResult <- liftIO $ try $
        parseXML (XML.fromDocument responseXmlDoc)

    case resParseResult of
        Left err -> throwError $ InvalidResponse err
        Right samlResponse -> pure (responseXmlDoc, samlResponse)

-- | 'validateSAMLResponse' @cfg doc response timestamp@ validates a decoded SAML2
-- response using the given @timestamp@.
--
-- @since 0.4
validateSAMLResponse :: SAML2Config
                     -> XML.Document
                     -> Response
                     -> UTCTime
                     -> ExceptT SAML2Error IO Assertion
validateSAMLResponse cfg responseXmlDoc samlResponse now = do

    -- check that the response indicates success
    case statusCodeValue $ responseStatusCode samlResponse of
        Success -> pure ()
        _status -> throwError $ Unsuccessful $ responseStatusCode samlResponse

    -- check that the destination is as expected, if the configuration
    -- expects us to validate this
    let destination = responseDestination samlResponse

    case saml2ExpectedDestination cfg of
        Just expectedDestination
            | destination /= expectedDestination ->
                throwError $ UnexpectedDestination destination
        _ -> pure ()

    -- check that the issuer is as expected, if the configuration
    -- expects us to validate this
    let issuer = responseIssuer samlResponse

    case saml2ExpectedIssuer cfg of
        Just expectedIssuer
            | issuer /= expectedIssuer -> throwError $ InvalidIssuer issuer
        _ -> pure ()

    --  ***CORE VALIDATION***
    -- See https://www.w3.org/TR/xmldsig-core1/#sec-CoreValidation
    --
    --  *REFERENCE VALIDATION*
    -- 1. We extract the SignedInfo element from the SAML2 response's
    -- Signature element. This element contains
    signedInfo <- extractSignedInfo (XML.fromDocument responseXmlDoc)

    -- construct a new XML document from the SignedInfo element and render
    -- it into a textual representation
    let doc = XML.Document (XML.Prologue [] Nothing []) signedInfo []
    let signedInfoXml = XML.renderLBS def doc

    -- canonicalise the textual representation of the SignedInfo element
    let prefixList = extractPrefixList (XML.fromDocument doc)
    signedInfoCanonResult <- liftIO $ try $
        canonicalise prefixList (LBS.toStrict signedInfoXml)

    normalisedSignedInfo <- case signedInfoCanonResult of
        Left err -> throwError $ CanonicalisationFailure err
        Right result -> pure result

    -- 2. At this point we should dereference all elements identified by
    -- Reference elements inside the SignedInfo element. However, we do
    -- not currently do that and instead just assume that there is only
    -- one Reference element which targets the overall Response.
    -- We sanity check this, just in case we are wrong since we do not
    -- want an attacker to be able to exploit this.
    let documentId = responseId samlResponse
    let referenceId = referenceURI
                    $ signedInfoReference
                    $ signatureInfo
                    $ responseSignature samlResponse

    if documentId /= referenceId
    then throwError $ UnexpectedReference referenceId
    else pure ()

    -- Now that we have sanity checked that we should indeed validate
    -- the entire Response, we need to remove the Signature element
    -- from it (since the Response cannot possibly have been hashed with
    -- the Signature element present). First remove the Signature element:
    let docMinusSignature = removeSignature responseXmlDoc

    -- then render the resulting document and canonicalise it
    let renderedXml = XML.renderLBS def docMinusSignature
    refCanonResult <- liftIO $ try $ canonicalise prefixList (LBS.toStrict renderedXml)

    normalised <- case refCanonResult of
        Left err -> throwError $ CanonicalisationFailure err
        Right result -> pure result

    -- next, compute the hash for the normalised document and extract the
    -- existing hash from the response; both hash values must be the same
    -- or the response has been tampered with; if both hashes are the same,
    -- then the response has not been tampered with, assuming that the
    -- Signature has not been tampered with, which we validate next
    let documentHash = hashWith SHA256 normalised
    let referenceHash = digestFromByteString
                      $ BS.decodeLenient
                      $ referenceDigestValue
                      $ signedInfoReference
                      $ signatureInfo
                      $ responseSignature samlResponse

    if Just documentHash /= referenceHash
    then throwError InvalidDigest
    else pure ()

    --  *SIGNATURE VALIDATION*
    -- We need to check that the SignedInfo element has not been tampered
    -- with, which we do by checking the signature contained in the response;
    -- first: extract the signature data from the response
    let sig = BS.decodeLenient $ signatureValue $ responseSignature samlResponse

    -- using the IdP's public key and the canonicalised SignedInfo element,
    -- check that the signature is correct
    let pubKey = saml2PublicKey cfg

    if PKCS15.verify (Just SHA256) pubKey normalisedSignedInfo sig
    then pure ()
    else throwError InvalidSignature

    assertion <- case responseEncryptedAssertion samlResponse of
        Just encrypted -> case saml2PrivateKey cfg of
            Just pk -> decryptAssertion pk encrypted
            Nothing -> throwError EncryptedAssertionNotSupported
        Nothing
            | saml2RequireEncryptedAssertion cfg -> throwError EncryptedAssertionRequired
            | otherwise -> case responseAssertion samlResponse of
                Just plain -> pure plain
                Nothing -> throwError $ InvalidResponse $ userError "Assertion or EncryptedAssertion is required"

    -- validate that the assertion is valid at this point in time
    let Conditions{..} = assertionConditions assertion

    -- Reference [NotBefore and NotOnOrAfter]
    when ((now < conditionsNotBefore || now >= conditionsNotOnOrAfter) &&
           not (saml2DisableTimeValidation cfg))
          $ throwError NotValid

    -- Reference [AudienceRestriction]
    -- Note [Validating AudienceRestrictions]
    case saml2Audiences cfg of
        -- Check disabled
        [] -> pure ()
        ourAudiences ->
           forM_ conditionsAudienceRestrictions $
              \(AudienceRestriction audiences) ->
                 unless (any (`elem` ourAudiences) audiences)
                   $ throwError (AudienceMismatch audiences)

    -- all checks out, return the assertion
    pure assertion

-- | `decryptAssertion` @key encryptedAssertion@ decrypts the AES key in
-- @encryptedAssertion@ using `key`, then decrypts the contents using
-- the AES key.
--
-- @since 0.4
decryptAssertion :: PrivateKey -> EncryptedAssertion -> ExceptT SAML2Error IO Assertion
decryptAssertion pk encryptedAssertion = do

    oaepResult <- liftIO $ OAEP.decryptSafer (OAEP.defaultOAEPParams SHA1) pk
        $ BS.decodeLenient
        $ cipherValue
        $ encryptedKeyCipher
        $ encryptedAssertionKey
        $ encryptedAssertion

    aesKey <- case oaepResult of
        Left err -> throwError $ DecryptionFailure err
        Right cipherData -> pure cipherData

    -- next we can decrypt the assertion; initialise AES128 with
    -- the key we have just decrypted
    xmlData <- case cipherInit aesKey of
        CryptoFailed err -> throwError $ CryptoError err
        CryptoPassed aes128 -> do
            -- get the AES ciphertext
            let cipherText = BS.decodeLenient
                           $ cipherValue
                           $ encryptedAssertionCipher
                           $ encryptedAssertion

            -- the IV used for AES is 128bits (16 bytes) prepended
            -- to the ciphertext
            let (ivBytes, xmlBytes) = BS.splitAt 16 cipherText

            -- convert the bytes into the IV
            case makeIV ivBytes of
                Nothing -> throwError InvalidIV
                Just iv -> do
                    -- run AES to decrypt the assertion
                    let plaintext = cbcDecrypt (aes128 :: AES128) iv xmlBytes

                    -- remove padding from the plaintext
                    case ansiX923 plaintext of
                        Nothing -> throwError InvalidPadding
                        Just xmlData -> pure xmlData

    -- try to parse the assertion that we decrypted earlier
    case XML.parseLBS def (LBS.fromStrict xmlData) of
        Left err -> throwError $ InvalidAssertionXml err
        Right assertDoc -> do
            -- try to convert the assertion document into a more
            -- structured representation
            assertParseResult <- liftIO $ try $
                parseXML (XML.fromDocument assertDoc)

            case assertParseResult of
                Left err -> throwError $ InvalidAssertion err
                Right assertion -> pure assertion

-- | 'ansiX923' @plaintext@ removes ANSI X9.23 padding from @plaintext@.
-- See https://en.wikipedia.org/wiki/Padding_(cryptography)#ANSI_X9.23
ansiX923 :: BS.ByteString -> Maybe BS.ByteString
ansiX923 d
    | len == 0 = Nothing
    | padLen < 1 || padLen > len = Nothing
    | otherwise = Just content
    where len = BS.length d
          padBytes = BS.index d (len-1)
          padLen = fromIntegral padBytes
          (content,_) = BS.splitAt (len - padLen) d

--------------------------------------------------------------------------------

-- Reference [NotBefore and NotOnOrAfter]
-- Source: https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf#page=23
-- Section: 2.5.1.2 Attributes NotBefore and NotOnOrAfter

-- Reference [AudienceRestriction]
-- Source: https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf#page=23
-- Section: 2.5.1.4 Elements <AudienceRestriction> and <Audience>

-- Note [Validating AudienceRestrictions]
--
-- > Note that multiple <AudienceRestriction> elements MAY be included in a single
-- > assertion, and each MUST be evaluated independently. The effect of this
-- > requirement and the preceding definition is that within a given condition,
-- > the audiences form a disjunction (an "OR") while multiple conditions form a
-- > conjunction (an "AND").
--
-- Source: https://docs.oasis-open.org/security/saml/v2.0/saml-core-2.0-os.pdf#page=24
-- Lines 922-925
