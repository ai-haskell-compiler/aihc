{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.BCP47.Internal.CIText
  ( CIText (..)
  , fromText
  , pack
  , original
  ) where

import Data.CaseInsensitive (CI)
import qualified Data.CaseInsensitive as CI
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T

-- | Internal newtype for 'CI Text'
--
-- Constructor is exposed since this is an internal module, but ths
-- interface may change. Module is meant to be imported qualified.
newtype CIText = CIText
  { unCIText :: CI Text
  }
  deriving newtype (Eq, Show, Ord, IsString)

-- | Convert 'Text' to 'CIText'
fromText :: Text -> CIText
fromText = CIText . CI.mk
{-# INLINE fromText #-}

-- | Convert 'String' to 'CIText'
pack :: String -> CIText
pack = fromText . T.pack
{-# INLINE pack #-}

-- | Recover original 'Text'
original :: CIText -> Text
original = CI.original . unCIText
{-# INLINE original #-}
