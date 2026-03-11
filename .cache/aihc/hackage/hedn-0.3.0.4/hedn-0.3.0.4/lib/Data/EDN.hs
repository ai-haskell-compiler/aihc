module Data.EDN
  ( decodeText
  , encodeText

  -- * EDN AST
  , parseText
  , renderText
  , TaggedValue
  , Tagged(..)
  , stripTag
  , Value(..)
  , EDNList, EDNVec, EDNMap, EDNSet
  , mkList, mkVec, mkMap, mkSet

  -- * Encoding to EDN AST
  , ToEDN(..)
  , toEDNtagged

  -- * Decoding EDN AST
  , FromEDN(..)
  , fromEDN
  -- ** AST-parsing wrappers
  , withTagged
  , withNoTag
  , withNil
  , withBoolean
  , withString
  , withCharacter
  , withSymbol
  , withKeyword
  , withTextual
  , withInteger
  , withIntegral
  , withFloating
  , withFractional
  , withList
  , withVec
  , withMap
  , withSet
  , unexpected
  -- ** Additional helpers
  , vecGet
  , mapGetKeyword
  , mapGetString
  , mapGetSymbol
  , mapGetSymbolNS
) where

import Data.Text (Text)

import Data.EDN.AST.Parser (parseText)
import Data.EDN.AST.Printer (renderText)
import Data.EDN.AST.Types.Tagged
import Data.EDN.AST.Types.Value
import Data.EDN.Class

-- | Convert value to AST using 'ToEDN' instance and render it.
encodeText :: ToEDN a => a -> Text
encodeText =
  renderText . toEDN

-- | Decode EDN document into AST and parse value using its 'FromEDN' instance.
decodeText
  :: (FromEDN a)
  => String -- ^ Source name, for megaparsec error reports
            -- e.g. @/path/to/file.edn@ or @<stdin>@
  -> Text   -- ^ EDN document body
  -> Either String a
decodeText sourceName source =
  parseText sourceName source >>= fromEDN
