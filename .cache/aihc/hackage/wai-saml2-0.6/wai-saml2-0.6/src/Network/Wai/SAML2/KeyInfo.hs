--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | Types to represent keys that are contained in SAML2 responses.
module Network.Wai.SAML2.KeyInfo (
    KeyInfo(..)
) where

--------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Text.Encoding

import Text.XML.Cursor

import Network.Wai.SAML2.XML

--------------------------------------------------------------------------------

-- | Represents a key.
data KeyInfo = KeyInfo {
    -- | The key data.
    keyInfoCertificate :: BS.ByteString
} deriving (Eq, Show)

instance FromXML KeyInfo where
    parseXML cursor = pure KeyInfo{
        keyInfoCertificate =
            encodeUtf8 $ T.concat $ cursor
                      $/ element (dsName "X509Data")
                      &/ element (dsName "X509Certificate")
                      &/ content
    }

--------------------------------------------------------------------------------
