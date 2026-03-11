{-# LANGUAGE OverloadedStrings #-}

module TestImport
  ( module X
  , enTJP
  , enTJPUpper
  , enGBTJP
  , enGBTJPUpper
  , unsafeFromText
  , fromTextThrows
  ) where

import Prelude as X

import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO(..))
import Data.BCP47 (BCP47, fromText)
import Data.Text (Text, unpack)
import Test.Hspec as X
import Test.QuickCheck as X
import Text.Read as X (readMaybe)

-- | A nonsense tag @en-t-jp@
enTJP :: BCP47
enTJP = unsafeFromText "en-t-jp"

-- | A nonsense tag @en-T-jp@
enTJPUpper :: BCP47
enTJPUpper = unsafeFromText "en-T-jp"

-- | A nonsense tag @en-GB-t-jp@
enGBTJP :: BCP47
enGBTJP = unsafeFromText "en-GB-t-jp"

-- | A nonsense tag @en-GB-t-jp@
enGBTJPUpper :: BCP47
enGBTJPUpper = unsafeFromText "en-GB-t-jp"

-- | Parse 'BCP47' or crash with pure error
unsafeFromText :: HasCallStack => Text -> BCP47
unsafeFromText = either (error . unpack) id . fromText

-- | Parse 'BCP47' or throw an IO error
fromTextThrows :: MonadIO m => Text -> m BCP47
fromTextThrows = either (liftIO . throwIO . userError . unpack) pure . fromText
