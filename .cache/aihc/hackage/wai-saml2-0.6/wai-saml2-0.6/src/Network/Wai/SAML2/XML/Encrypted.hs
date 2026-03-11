--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Types representing elements of the encrypted XML standard.
-- See https://www.w3.org/TR/2002/REC-xmlenc-core-20021210/Overview.html
module Network.Wai.SAML2.XML.Encrypted (
    CipherData(..),
    EncryptionMethod(..),
    EncryptedKey(..),
    EncryptedAssertion(..)
) where

--------------------------------------------------------------------------------

import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.ByteString as BS

import Text.XML.Cursor

import Network.Wai.SAML2.XML
import Network.Wai.SAML2.KeyInfo

--------------------------------------------------------------------------------

-- | Represents some ciphertext.
data CipherData = CipherData {
    cipherValue :: !BS.ByteString
} deriving (Eq, Show)

instance FromXML CipherData where
    parseXML cursor = pure CipherData{
        cipherValue = encodeUtf8
                    $ T.concat
                    $ cursor
                    $/ element (xencName "CipherValue")
                    &/ content
    }

--------------------------------------------------------------------------------

-- | Describes an encryption method.
data EncryptionMethod = EncryptionMethod {
    -- | The name of the algorithm.
    encryptionMethodAlgorithm :: !T.Text,
    -- | The name of the digest algorithm, if any.
    encryptionMethodDigestAlgorithm :: !(Maybe T.Text)
} deriving (Eq, Show)

instance FromXML EncryptionMethod where
    parseXML cursor = pure EncryptionMethod{
        encryptionMethodAlgorithm =
            T.concat $ attribute "Algorithm" cursor,
        encryptionMethodDigestAlgorithm =
            toMaybeText $ cursor
                        $/ element (dsName "DigestMethod")
                       >=> attribute "Algorithm"
    }

--------------------------------------------------------------------------------

-- | Represents an encrypted key.
data EncryptedKey = EncryptedKey {
    -- | The ID of the key.
    encryptedKeyId :: !T.Text,
    -- | The intended recipient of the key.
    encryptedKeyRecipient :: !T.Text,
    -- | The method used to encrypt the key.
    encryptedKeyMethod :: !EncryptionMethod,
    -- | The key data.
    encryptedKeyData :: !(Maybe KeyInfo),
    -- | The ciphertext.
    encryptedKeyCipher :: !CipherData
} deriving (Eq, Show)

instance FromXML EncryptedKey where
    parseXML cursor =  do
        method <- oneOrFail "EncryptionMethod is required" (
            cursor $/ element (xencName "EncryptionMethod")
                ) >>= parseXML

        keyData <- case cursor $/ element (dsName "KeyInfo") of
                     [] -> return Nothing
                     (keyInfo :_) -> Just <$> parseXML keyInfo

        cipher <- oneOrFail "CipherData is required" (
            cursor $/ element (xencName "CipherData")
                ) >>= parseXML

        pure EncryptedKey{
            encryptedKeyId = T.concat $ attribute "Id" cursor,
            encryptedKeyRecipient = T.concat $ attribute "Recipient" cursor,
            encryptedKeyMethod = method,
            encryptedKeyData = keyData,
            encryptedKeyCipher = cipher
        }

--------------------------------------------------------------------------------

-- | Represents an encrypted SAML assertion.
data EncryptedAssertion = EncryptedAssertion {
    -- | Information about the encryption method used.
    encryptedAssertionAlgorithm :: !EncryptionMethod,
    -- | The encrypted key.
    encryptedAssertionKey :: !EncryptedKey,
    -- | The ciphertext.
    encryptedAssertionCipher :: !CipherData
} deriving (Eq, Show)

instance FromXML EncryptedAssertion where
    parseXML cursor = do
        encryptedData <- oneOrFail "EncryptedData is required"
            $   cursor
            $/  element (xencName "EncryptedData")

        algorithm <- oneOrFail "Algorithm is required"
            $   encryptedData
            $/  element (xencName "EncryptionMethod")
            >=> parseXML

        keyInfo <- oneOrFail "EncryptedKey is required" $ mconcat
            [ cursor $/ element (xencName "EncryptedKey")
            >=> parseXML
            , cursor
                $/ element (xencName "EncryptedData")
                &/ element (dsName "KeyInfo")
                &/ element (xencName "EncryptedKey")
            >=> parseXML
            ]

        cipher <- oneOrFail "CipherData is required"
               (  encryptedData
              $/  element (xencName "CipherData")
              ) >>= parseXML

        pure EncryptedAssertion{
            encryptedAssertionAlgorithm = algorithm,
            encryptedAssertionKey = keyInfo,
            encryptedAssertionCipher = cipher
        }

--------------------------------------------------------------------------------
