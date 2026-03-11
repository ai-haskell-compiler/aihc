----------------------------------------------------------------------------------------------------

-- | Streaming interface to journalctl. Use 'entryStream' to stream
--   journalctl entries as they are created.
--
--   Designed with qualified import in mind.
--   For example, if you import it as @Journal@, then 'Entry' becomes
--   @Journal.Entry@, and 'Exception' becomes @Journal.Exception@.
--
module Systemd.Journalctl.Stream (
    -- * Journal entry
    Entry (..)
  , Cursor
    -- * Streaming
  , StreamStart (..)
  , entryStream
    -- * Exceptions
  , Exception (..)
  ) where

-- base
import System.IO (Handle)
import Data.Maybe (fromJust)
import Control.Exception qualified as Base
import System.Posix.Types (CPid (..), ProcessID)
import Data.Foldable (toList)
-- text
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Text qualified as Text
-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
-- aeson
import Data.Aeson (FromJSON, parseJSON, (.:), (.:?), ToJSON)
import Data.Aeson qualified as JSON
import Data.Aeson.Types qualified as JSON
import Data.Aeson.KeyMap qualified as KeyMap
-- time
import Data.Time.Clock (secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
-- process
import System.Process qualified as System
-- conduit
import Conduit (MonadResource, MonadThrow, throwM)
import Data.Conduit (ConduitT, (.|))
import Data.Conduit.Combinators qualified as Conduit

-- | A cursor is an opaque text string that uniquely describes
--   the position of an entry in the journal and is portable
--   across machines, platforms and journal files.
--
--   The 'Ord' instance does not order by time. Given two entries
--   @e1@ and @e2@, @e1@ having an earlier timestamp than @e2@ doesn't
--   mean that @entryCursor e1 < entryCursor e2@.
newtype Cursor = Cursor Text deriving (Eq, Ord, Show)

instance FromJSON Cursor where
  parseJSON = JSON.withText "Cursor" $ pure . Cursor

instance ToJSON Cursor where
  toJSON (Cursor t) = JSON.String t

-- | A journal entry.
data Entry = Entry
  { -- | Process ID.
    entryPID :: Maybe ProcessID
    -- | The name of the originating host.
  , entryHostname :: Text
    -- | Namespace identifier.
  , entryNamespace :: Maybe Text
    -- | Process name.
  , entryProcess :: Maybe Text
    -- | File path to the executable.
  , entryExecutable :: Maybe FilePath
    -- | The cursor for the entry.
  , entryCursor :: Cursor
    -- | The time the entry was received by the journal.
  , entryTimestamp :: POSIXTime
    -- | Unit name, if present.
  , entryUnit :: Maybe Text
    -- | Entry message. It may come in binary or textual format.
  , entryMessage :: Maybe (Either ByteString Text)
    } deriving Show

-- | Utility type to parse values (mainly numbers) that are received
--   as text.
newtype AsText a = AsText { asText :: a } deriving Show

instance FromJSON a => FromJSON (AsText a) where
  parseJSON = JSON.withText "AsText" $
    either fail (pure . AsText) . JSON.eitherDecodeStrict . encodeUtf8

{- Journal fields

For a more complete list of fields and documentation, go to:

https://www.freedesktop.org/software/systemd/man/systemd.journal-fields.html

-}

instance FromJSON Entry where
  parseJSON = JSON.withObject "Entry" $ \o -> Entry
    <$> (fmap (CPid . asText) <$> o .:? "_PID")
    <*> o .: "_HOSTNAME"
    <*> o .:? "_NAMESPACE"
    <*> o .:? "_COMM"
    <*> o .:? "_EXE"
    <*> o .: "__CURSOR"
    <*> (secondsToNominalDiffTime . (/1000000) . asText <$> o .: "__REALTIME_TIMESTAMP")
    <*> o .:? "UNIT"
    <*> messageParser o

messageParser :: JSON.Object -> JSON.Parser (Maybe (Either ByteString Text))
messageParser obj =
  case KeyMap.lookup "MESSAGE" obj of
    Just (JSON.String t) -> pure $ Just $ Right t
    Just (JSON.Array arr) -> Just . Left . ByteString.pack <$> mapM parseJSON (toList arr)
    Just JSON.Null -> pure Nothing
    Nothing -> pure Nothing
    _ -> fail $ "Couldn't parse MESSAGE. Expected String, Array or Null."

-- | Exception raised while streaming entries from journalctl.
data Exception = JSONError String deriving Show

instance Base.Exception Exception where

-- | Where to start a stream.
data StreamStart =
    -- | Start from the given time.
    StartTime LocalTime
    -- | Start from the given number of lines back.
    --   You can use @Lines 0@ to start the stream without
    --   looking for previous lines.
  | Lines Int
    -- | Start /at/ the given cursor.
  | AtCursor Cursor
    -- | Start /after/ the given cursor.
  | AfterCursor Cursor

-- | Translate a 'StreamStart' into the arguments required
--   for journalctl.
streamStartArgs :: StreamStart -> [String]
streamStartArgs (StartTime t) =
  [ "--since", formatTime defaultTimeLocale "%F %T" t ]
streamStartArgs (Lines n) =
  [ "--lines" , show n ]
streamStartArgs (AtCursor (Cursor t)) =
  [ "--cursor", Text.unpack t ]
streamStartArgs (AfterCursor (Cursor t)) =
  [ "--after-cursor", Text.unpack t ]

-- | Stream all journal entries starting from the given point.
--   If an entry fails to parse, a 'JSONError' will be thrown.
entryStream
  :: (MonadResource m, MonadThrow m)
  => StreamStart -- ^ Where to start streaming entries.
  -> ConduitT i Entry m () -- ^ Stream of journal entries.
entryStream start =
  let args :: [String]
      args = streamStartArgs start ++ [ "--follow", "--output", "json" ]
      hdl :: IO Handle
      hdl = fmap (\(_, h, _, _) -> fromJust h)
          $ System.createProcess
          $ (System.proc "journalctl" args)
              { System.std_out = System.CreatePipe
                }
  in  Conduit.sourceIOHandle hdl
        .| Conduit.linesUnboundedAscii
        .| Conduit.mapM (either (throwM . JSONError) pure . JSON.eitherDecodeStrict)
