{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Known fuzz bugs: fingerprints, the on-disk store, replay arguments, and issue text.
module Aihc.Dev.Fuzz.Bugs
  ( Bug (..),
    Failure (..),
    IssueRef (..),
    bugFingerprint,
    bugStorePath,
    commentBody,
    formatSeen,
    issueBody,
    issueTitle,
    loadBugs,
    parseIssueRef,
    parseReplay,
    recordFailure,
    replayCommand,
    saveBugs,
  )
where

import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Bits (xor)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Char (ord)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Word (Word64)
import Hedgehog.Internal.Property (Skip (..), skipDecompress)
import Hedgehog.Internal.Seed (Seed)
import Numeric (showHex)
import System.Directory (XdgDirectory (XdgCache), createDirectoryIfMissing, doesFileExist, getXdgDirectory, renameFile)
import System.FilePath (takeDirectory, (</>))
import Text.Read (readMaybe)

-- | One observed Hedgehog failure.
data Failure = Failure
  { -- | The component-qualified property name.
    failureProperty :: String,
    -- | The Hedgehog seed, as @Seed value gamma@.
    failureSeed :: String,
    -- | The argument of @aihc-dev fuzz replay@: the shrink path and the seed.
    failureReplay :: String,
    -- | The failure message, or a note that the property gave up.
    failureText :: String,
    -- | The shrunk values that the property showed.
    failureValues :: [String],
    -- | The source location of the failed assertion.
    failureSite :: Maybe String,
    -- | The full Hedgehog report without colors.
    failureReport :: String
  }
  deriving (Eq, Show)

-- | A GitHub issue that tracks a bug.
data IssueRef = IssueRef
  { issueNumber :: Int,
    issueUrl :: String
  }
  deriving (Eq, Show)

-- | A known bug: the same shrunk counterexample for the same property.
data Bug = Bug
  { bugId :: String,
    bugProperty :: String,
    bugFirstSeen :: UTCTime,
    bugLastSeen :: UTCTime,
    bugHits :: Int,
    bugFailure :: Failure,
    bugIssue :: Maybe IssueRef
  }
  deriving (Eq, Show)

-- | Identify a bug by its property, message, location, and shrunk values.
-- The seed is not part of the fingerprint, so different seeds that shrink to
-- the same counterexample give the same bug.
bugFingerprint :: Failure -> String
bugFingerprint Failure {failureProperty, failureText, failureValues, failureSite} =
  replicate (16 - length hex) '0' <> hex
  where
    hex = showHex (foldl step offset (intercalate "\0" parts)) ""
    parts = failureProperty : failureText : fromMaybe "" failureSite : failureValues
    offset = 14695981039346656037 :: Word64
    prime = 1099511628211 :: Word64
    step hash char = (hash `xor` fromIntegral (ord char)) * prime

-- | Add a failure to the known bugs. The flag is true for a new bug.
recordFailure :: UTCTime -> Failure -> [Bug] -> (Bug, Bool, [Bug])
recordFailure now failure bugs =
  case break ((== fingerprint) . bugId) bugs of
    (before, known : after) ->
      let updated = known {bugLastSeen = now, bugHits = bugHits known + 1}
       in (updated, False, before <> [updated] <> after)
    (_, []) ->
      let new =
            Bug
              { bugId = fingerprint,
                bugProperty = failureProperty failure,
                bugFirstSeen = now,
                bugLastSeen = now,
                bugHits = 1,
                bugFailure = failure,
                bugIssue = Nothing
              }
       in (new, True, bugs <> [new])
  where
    fingerprint = bugFingerprint failure

instance Aeson.ToJSON Failure where
  toJSON Failure {failureProperty, failureSeed, failureReplay, failureText, failureValues, failureSite, failureReport} =
    Aeson.object
      [ "property" .= failureProperty,
        "seed" .= failureSeed,
        "replay" .= failureReplay,
        "message" .= failureText,
        "values" .= failureValues,
        "location" .= failureSite,
        "report" .= failureReport
      ]

instance Aeson.FromJSON Failure where
  parseJSON = Aeson.withObject "Failure" $ \object ->
    Failure
      <$> object .: "property"
      <*> object .: "seed"
      <*> object .: "replay"
      <*> object .: "message"
      <*> object .: "values"
      <*> object .:? "location"
      <*> object .: "report"

instance Aeson.ToJSON IssueRef where
  toJSON IssueRef {issueNumber, issueUrl} =
    Aeson.object ["number" .= issueNumber, "url" .= issueUrl]

instance Aeson.FromJSON IssueRef where
  parseJSON = Aeson.withObject "IssueRef" $ \object ->
    IssueRef <$> object .: "number" <*> object .: "url"

instance Aeson.ToJSON Bug where
  toJSON Bug {bugId, bugProperty, bugFirstSeen, bugLastSeen, bugHits, bugFailure, bugIssue} =
    Aeson.object
      [ "id" .= bugId,
        "property" .= bugProperty,
        "first-seen" .= bugFirstSeen,
        "last-seen" .= bugLastSeen,
        "hits" .= bugHits,
        "failure" .= bugFailure,
        "issue" .= bugIssue
      ]

instance Aeson.FromJSON Bug where
  parseJSON = Aeson.withObject "Bug" $ \object ->
    Bug
      <$> object .: "id"
      <*> object .: "property"
      <*> object .: "first-seen"
      <*> object .: "last-seen"
      <*> object .: "hits"
      <*> object .: "failure"
      <*> object .:? "issue"

-- | The default bug store: @$XDG_CACHE_HOME/aihc/fuzz/bugs.json@.
bugStorePath :: IO FilePath
bugStorePath = getXdgDirectory XdgCache ("aihc" </> "fuzz" </> "bugs.json")

-- | Read the bug store. A missing store is empty.
loadBugs :: FilePath -> IO (Either String [Bug])
loadBugs path = do
  exists <- doesFileExist path
  if exists
    then Aeson.eitherDecodeStrict <$> BS.readFile path
    else pure (Right [])

-- | Replace the bug store with one write and one rename.
saveBugs :: FilePath -> [Bug] -> IO ()
saveBugs path bugs = do
  createDirectoryIfMissing True (takeDirectory path)
  let temporary = path <> ".tmp"
  LBS.writeFile temporary (encodePretty bugs <> "\n")
  renameFile temporary path

-- | Parse a replay argument: an optional shrink path and then a seed.
-- Examples are @"3:b2 Seed 1 3"@ and @"Seed 1 3"@.
parseReplay :: String -> Either String (Skip, Seed)
parseReplay argument =
  case words argument of
    [] -> Left "expected a replay argument such as \"3:b2 Seed 1 3\""
    first : rest
      | Just skip <- skipDecompress first,
        Just seed <- readMaybe (unwords rest) ->
          Right (skip, seed)
      | Just seed <- readMaybe argument ->
          Right (SkipNothing, seed)
      | otherwise ->
          Left ("cannot parse the replay argument: " <> argument)

-- | The shell command that runs one failure again.
replayCommand :: Failure -> String
replayCommand Failure {failureProperty, failureReplay} =
  "cabal run -v0 aihc-dev -- fuzz replay " <> quote failureProperty <> " " <> quote failureReplay
  where
    quote text = "\"" <> text <> "\""

issueTitle :: Bug -> String
issueTitle bug = "fuzz: " <> bugProperty bug <> " fails"

-- | The body of a new tracking issue.
issueBody :: Bug -> String
issueBody bug =
  unlines $
    [ "`aihc-dev fuzz` found a failure in the Hedgehog property `" <> bugProperty bug <> "`.",
      ""
    ]
      <> failureSection bug

-- | The body of a comment on the tracking issue of the same property.
commentBody :: Bug -> String
commentBody bug =
  unlines $
    [ "`aihc-dev fuzz` found a different failure of the property `" <> bugProperty bug <> "`.",
      ""
    ]
      <> failureSection bug

failureSection :: Bug -> [String]
failureSection Bug {bugId, bugFirstSeen, bugFailure} =
  [ "- Bug id: `" <> bugId <> "`",
    "- First seen: " <> formatSeen bugFirstSeen <> " UTC",
    "- Seed: `" <> failureSeed bugFailure <> "`",
    "",
    "## Replay",
    "",
    "```bash",
    replayCommand bugFailure,
    "```",
    "",
    "## Hedgehog report",
    "",
    "```text"
  ]
    <> lines (failureReport bugFailure)
    <> ["```"]

formatSeen :: UTCTime -> String
formatSeen = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

-- | Read the issue reference from the output of @gh issue create@.
-- The last word of the output is the issue URL.
parseIssueRef :: String -> Maybe IssueRef
parseIssueRef output =
  case reverse (words output) of
    url : _
      | Just number <- readMaybe (reverse (takeWhile (/= '/') (reverse url))),
        number > (0 :: Int) ->
          Just (IssueRef number url)
    _ -> Nothing
