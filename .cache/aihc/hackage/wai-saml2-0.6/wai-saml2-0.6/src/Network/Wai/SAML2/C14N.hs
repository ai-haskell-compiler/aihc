--------------------------------------------------------------------------------
-- SAML2 Middleware for WAI                                                   --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | A high-level interface to XML canonicalisation for the purpose of
-- SAML2 signature validation.
module Network.Wai.SAML2.C14N (
    canonicalise
) where

--------------------------------------------------------------------------------

import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as T

import Foreign.C.Types

import Text.XML.C14N

--------------------------------------------------------------------------------

-- | 'canonicalise' @prefixList@ @xml@ produces a canonical representation of @xml@
-- while retaining namespaces matching @prefixList@.
canonicalise :: [Text] -> BS.ByteString -> IO BS.ByteString
canonicalise prefixList xml = c14n c14nOpts c14n_exclusive_1_0 (map T.encodeUtf8 prefixList) False Nothing xml

-- | The options we want to use for canonicalisation of XML documents.
c14nOpts :: [CInt]
c14nOpts =
    [ xml_opt_noent
    , xml_opt_dtdload
    , xml_opt_dtdattr
    -- disable network access
    , xml_opt_nonet
    -- compact small text nodes, this has no effect on the rendered output
    , xml_opt_compact
    -- suppress standard output; the function will still fail if
    -- something goes wrong, but the reason won't be reported
    , xml_opt_noerror
    , xml_opt_nowarning
    ]

--------------------------------------------------------------------------------
