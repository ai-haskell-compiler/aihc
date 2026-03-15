{-# LANGUAGE OverloadedStrings #-}

module CppSupport
  ( preprocessForParser,
    preprocessForParserWithoutIncludes,
  )
where

import Cpp
  ( Config (..),
    IncludeRequest,
    Result (..),
    Step (..),
    defaultConfig,
    preprocess,
  )
import Data.Functor.Identity (Identity (..), runIdentity)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

preprocessForParser :: (Monad m) => FilePath -> (IncludeRequest -> m (Maybe Text)) -> Text -> m Result
preprocessForParser inputFile resolveInclude source = do
  let cfg =
        defaultConfig
          { configInputFile = inputFile,
            configDateTime = getDeterministicDateTime
          }
  result <- drive (preprocess cfg source)
  pure result {resultOutput = stripLinePragmas (resultOutput result)}
  where
    drive (Done result) = pure result
    drive (NeedInclude req k) = resolveInclude req >>= drive . k

preprocessForParserWithoutIncludes :: FilePath -> Text -> Result
preprocessForParserWithoutIncludes inputFile source =
  runIdentity (preprocessForParser inputFile (\_ -> Identity Nothing) source)

{-# NOINLINE getDeterministicDateTime #-}
getDeterministicDateTime :: (Text, Text)
getDeterministicDateTime = unsafePerformIO $ do
  mEpoch <- lookupEnv "SOURCE_DATE_EPOCH"
  let epoch = case mEpoch of
        Just s | not (null s) && all (`elem` ['0' .. '9']) s -> read s
        _ -> (0 :: Integer)
      utcTime = posixSecondsToUTCTime (fromIntegral epoch)
      date = T.pack $ formatTime defaultTimeLocale "%b %e %Y" utcTime
      time = T.pack $ formatTime defaultTimeLocale "%H:%M:%S" utcTime
  pure (date, time)

stripLinePragmas :: Text -> Text
stripLinePragmas =
  T.unlines
    . filter (not . isLinePragma)
    . T.lines
  where
    isLinePragma line = "#line " `T.isPrefixOf` T.stripStart line
