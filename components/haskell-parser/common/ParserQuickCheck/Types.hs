{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserQuickCheck.Types
  ( BatchResult (..),
    PropertyResult (..),
    PropertyStatus (..),
    failureFingerprint,
    normalizeFailureTranscript,
    statusRequiresAttention,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Bits (xor)
import Data.Char (ord)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Generics (Generic)

data PropertyStatus
  = PASS
  | FAIL
  | GAVE_UP
  | XPASS
  deriving (Eq, Generic, Show)

instance FromJSON PropertyStatus

instance ToJSON PropertyStatus

data PropertyResult = PropertyResult
  { propertyName :: Text,
    status :: PropertyStatus,
    seed :: Int,
    configuredMaxSuccess :: Int,
    actualTests :: Int,
    actualDiscarded :: Int,
    failureTranscript :: Maybe Text,
    fingerprint :: Maybe Text,
    reason :: Maybe Text,
    reproductionCommand :: Text
  }
  deriving (Eq, Generic, Show)

instance FromJSON PropertyResult

instance ToJSON PropertyResult

data BatchResult = BatchResult
  { generatedAt :: Text,
    batchSeed :: Int,
    commitSha :: Text,
    configuredMaxSuccess :: Int,
    selectedProperties :: [Text],
    results :: [PropertyResult]
  }
  deriving (Eq, Generic, Show)

instance FromJSON BatchResult

instance ToJSON BatchResult

statusRequiresAttention :: PropertyStatus -> Bool
statusRequiresAttention propertyStatus =
  case propertyStatus of
    PASS -> False
    FAIL -> True
    GAVE_UP -> True
    XPASS -> True

failureFingerprint :: Text -> Text -> Text
failureFingerprint property transcript =
  renderHex64 $
    fnv1a64 $
      T.unpack property <> "\n" <> T.unpack (normalizeFailureTranscript transcript)

normalizeFailureTranscript :: Text -> Text
normalizeFailureTranscript =
  T.intercalate "\n"
    . dropTrailingBlankLines
    . map T.stripEnd
    . T.splitOn "\n"
    . T.replace "\r\n" "\n"

dropTrailingBlankLines :: [Text] -> [Text]
dropTrailingBlankLines = reverse . dropWhile T.null . reverse

fnv1a64 :: String -> Word64
fnv1a64 =
  List.foldl' step offsetBasis
  where
    offsetBasis = 14695981039346656037
    prime = 1099511628211

    step :: Word64 -> Char -> Word64
    step acc ch = (acc `xor` fromIntegral (ord ch)) * prime

renderHex64 :: Word64 -> Text
renderHex64 word =
  T.pack (go 16 word)
  where
    go :: Int -> Word64 -> String
    go digitsLeft current
      | digitsLeft <= 0 = []
      | otherwise =
          let rest = go (digitsLeft - 1) (current `div` 16)
              digit = fromIntegral (current `mod` 16) :: Int
           in rest <> [hexDigit digit]

hexDigit :: Int -> Char
hexDigit value
  | value >= 0 && value <= 9 = toEnum (fromEnum '0' + value)
  | value >= 10 && value <= 15 = toEnum (fromEnum 'a' + value - 10)
  | otherwise = error "hexDigit: value out of range"
