-- | The text encoding type. Handles carry the name of their encoding
-- only, because all text goes through UTF-8.
module GHC.IO.Encoding.Types
  ( TextEncoding (..),
  )
where

import GHC.Base (String)

newtype TextEncoding = TextEncoding {textEncodingName :: String}
