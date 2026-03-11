module ScrapeReddit where

import Network.HTTP.Client (Manager)
import RIO
import qualified RIO.Text as Text
import RIO.Time (UTCTime, defaultTimeLocale, iso8601DateFormat, parseTimeM)
import Text.HTML.Scalpel
  ( Config (..),
    Scraper,
    anySelector,
    attr,
    chroot,
    chroots,
    defaultDecoder,
    hasClass,
    scrapeURLWithConfig,
    text,
    (@:),
  )

data Link = Link
  { title :: Text,
    href :: String,
    currentScore :: Maybe Int,
    comments :: Maybe Int,
    date :: Maybe UTCTime
  }
  deriving (Eq, Show, Generic)

scrapeSubReddit :: Manager -> String -> IO (Maybe [Link])
scrapeSubReddit manager subReddit =
  scrapeURLWithConfig
    (Config {decoder = defaultDecoder, manager = Just manager})
    (mconcat ["https://old.reddit.com/r/", subReddit, "/"])
    links

links :: Scraper Text [Link]
links = chroots ("div" @: [hasClass "thing", hasClass "link"]) link'

link' :: Scraper Text Link
link' = do
  title <- text $ "a" @: [hasClass "title"]
  href <- Text.unpack <$> attr "href" ("a" @: [hasClass "title"])
  currentScore <- (Text.unpack >>> readMaybe) <$> attr "title" ("div" @: [hasClass "score"])
  comments <- chroot ("a" @: [hasClass "comments"]) commentNumber
  date <- chroot ("time" @: [hasClass "live-timestamp"]) dateTimeFromTime
  pure $ Link {title, href, currentScore, comments, date}

commentNumber :: Scraper Text (Maybe Int)
commentNumber = do
  commentLinkText <- Text.unpack <$> text anySelector
  case words commentLinkText of
    [numberText, _comments] -> pure $ readMaybe numberText
    _anythingElse -> pure Nothing

dateTimeFromTime :: Scraper Text (Maybe UTCTime)
dateTimeFromTime = do
  timeString <- Text.unpack <$> attr "datetime" anySelector
  pure $ parseTimeM True defaultTimeLocale dateFormat timeString

dateFormat :: String
dateFormat = iso8601DateFormat $ Just "%H:%M:%S+00:00"
