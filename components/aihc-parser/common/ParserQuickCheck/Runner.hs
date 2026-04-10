{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module ParserQuickCheck.Runner
  ( BatchConfig (..),
    RegisteredProperty (..),
    availablePropertyNames,
    derivePropertySeed,
    resolveBatchSeed,
    runBatch,
    selectProperties,
  )
where

import Data.Char (ord)
import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import ParserQuickCheck.Types
import Test.QuickCheck (Args (..), Result (..), Testable, quickCheckWithResult, stdArgs)
import Test.QuickCheck.Random (mkQCGen)

data RegisteredProperty
  = forall prop.
  (Testable prop) =>
  RegisteredProperty
  { registeredPropertyName :: Text,
    registeredPropertyValue :: prop
  }

data BatchConfig = BatchConfig
  { batchCommitSha :: Text,
    batchMaxSuccess :: Int,
    batchSeedValue :: Int
  }
  deriving (Eq, Show)

availablePropertyNames :: [RegisteredProperty] -> [Text]
availablePropertyNames = map registeredPropertyName

selectProperties :: Maybe Text -> [RegisteredProperty] -> Either String [RegisteredProperty]
selectProperties Nothing props = Right props
selectProperties (Just selectedName) props =
  case find (\prop -> registeredPropertyName prop == selectedName) props of
    Just prop -> Right [prop]
    Nothing ->
      Left $
        "Unknown property: "
          <> T.unpack selectedName
          <> "\nAvailable properties:\n"
          <> unlines (map (("  - " <>) . T.unpack) (availablePropertyNames props))

resolveBatchSeed :: Maybe Int -> IO Int
resolveBatchSeed (Just seedValue) = pure seedValue
resolveBatchSeed Nothing = do
  now <- getPOSIXTime
  let micros = floor (now * 1000000) :: Integer
      positive = fromIntegral ((micros `mod` 2147483646) + 1)
  pure positive

runBatch :: UTCTime -> BatchConfig -> [RegisteredProperty] -> IO BatchResult
runBatch now config props = do
  propertyResults <- mapM (runProperty config) props
  pure $
    BatchResult
      { generatedAt = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now),
        batchSeed = batchSeedValue config,
        commitSha = batchCommitSha config,
        configuredMaxSuccess = batchMaxSuccess config,
        selectedProperties = availablePropertyNames props,
        results = propertyResults
      }

runProperty :: BatchConfig -> RegisteredProperty -> IO PropertyResult
runProperty config registeredProperty@(RegisteredProperty propName prop) = do
  let propertySeed = derivePropertySeed (batchSeedValue config) propName
      args = quickCheckArgs (batchMaxSuccess config) propertySeed
  result <- quickCheckWithResult args prop
  pure (mkPropertyResult config registeredProperty propertySeed result)

quickCheckArgs :: Int -> Int -> Args
quickCheckArgs maxSuccessValue propertySeed =
  stdArgs
    { maxSuccess = maxSuccessValue,
      replay = Just (mkQCGen propertySeed, 0),
      chatty = False
    }

mkPropertyResult :: BatchConfig -> RegisteredProperty -> Int -> Result -> PropertyResult
mkPropertyResult config registeredProperty propertySeed result =
  let propertyStatus = toPropertyStatus result
      transcript =
        if statusRequiresAttention propertyStatus
          then Just (normalizeFailureTranscript (T.pack (output result)))
          else Nothing
   in PropertyResult
        { propertyName = registeredPropertyName registeredProperty,
          status = propertyStatus,
          seed = propertySeed,
          configuredMaxSuccess = batchMaxSuccess config,
          actualTests = numTests result,
          actualDiscarded = numDiscarded result,
          failureTranscript = transcript,
          fingerprint = failureFingerprint (registeredPropertyName registeredProperty) <$> transcript,
          reason = failureReason result,
          reproductionCommand =
            T.pack $
              "nix run .#parser-quickcheck-batch -- --max-success "
                <> show (batchMaxSuccess config)
                <> " --seed "
                <> show (batchSeedValue config)
                <> " --property "
                <> shellQuote (T.unpack (registeredPropertyName registeredProperty))
        }

toPropertyStatus :: Result -> PropertyStatus
toPropertyStatus result =
  case result of
    Success {} -> PASS
    Failure {} -> FAIL
    GaveUp {} -> GAVE_UP
    NoExpectedFailure {} -> XPASS

failureReason :: Result -> Maybe Text
failureReason result =
  case result of
    Failure {reason = failureMessage} -> Just (T.pack failureMessage)
    GaveUp {} -> Just "Gave up"
    NoExpectedFailure {} -> Just "No expected failure"
    Success {} -> Nothing

derivePropertySeed :: Int -> Text -> Int
derivePropertySeed seedBase propName =
  let mixed = stableWord (show seedBase <> ":" <> T.unpack propName)
   in fromIntegral ((mixed `mod` 2147483646) + 1)

stableWord :: String -> Integer
stableWord =
  foldl step 0
  where
    step :: Integer -> Char -> Integer
    step acc ch = (acc * 131 + toInteger (ord ch)) `mod` 2147483647

shellQuote :: String -> String
shellQuote raw = "'" <> concatMap escape raw <> "'"
  where
    escape '\'' = "'\"'\"'"
    escape ch = [ch]
