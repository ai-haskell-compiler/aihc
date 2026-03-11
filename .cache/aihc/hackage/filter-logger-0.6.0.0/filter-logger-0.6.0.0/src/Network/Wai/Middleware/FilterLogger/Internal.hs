{-|
Module      : Network.Wai.Middleware.FilterLogger.Internal
Description : Internal module and core code
Copyright   : (c) Joseph Canero, 2017
License     : MIT
Maintainer  : jmc41493@gmail.com
Stability   : experimental
Portability : POSIX
-}

{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Network.Wai.Middleware.FilterLogger.Internal where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import           Data.ByteString                               (ByteString)
import qualified Data.ByteString                               as BS hiding (ByteString)
import           Data.ByteString.Builder                       (Builder)
import qualified Data.ByteString.Lazy                          as BL (ByteString,
                                                                      fromStrict,
                                                                      toStrict)
import           Data.Char
import           Data.Default
import           Data.Semigroup
import           Data.Time.Clock                               (NominalDiffTime)
import           Data.Word
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Logger
import           Network.Wai.Middleware.FilterLogger.Colorizer
import           Network.Wai.Middleware.RequestLogger
import           System.IO.Unsafe
import           System.Log.FastLogger
import           Text.Printf                                   (printf)

-- | Options for controlling log filtering.
data FilterOptions = FilterOptions {
    -- | Boolean value indicating whether to log output should be detailed or not.
    -- Details include the response size and request duration in ms.
    -- Default is True.
    detailed       :: Bool

    -- | Boolean value indicating whether to log messages when there is no request body.
    -- Default is True.
  , logOnEmptyBody :: Bool
  }

instance Default FilterOptions where
  def = FilterOptions True True

-- | Typeclass for types that can be converted into a strict 'ByteString'
-- and be shown in a log.
class LogShowable a where
  -- | Convert the type into a strict 'ByteString' to be displayed in the logs.
  logShow :: a -> ByteString

instance LogShowable ByteString where
  logShow = id

instance LogShowable BL.ByteString where
  logShow = BL.toStrict

-- | Helper function that can be used when you want to make an instance of 'ToJSON' an instance of
-- 'LogShowable'. This helps avoid having to use UndecidableInstances.
logShowJSON :: (ToJSON a) => a -> ByteString
logShowJSON = BL.toStrict . encodePretty

-- | Helper function that can be used when you want to make an instance of 'FromJSON' an instance of
-- 'LogFilterable'. This helps avoid having to use UndecidableInstances.
logFilterJSON :: (FromJSON a) => ByteString -> Maybe a
logFilterJSON = decodeStrict'

-- | Typeclass for types that can be converted into from a strict 'ByteString' and will be used as
-- arguments to 'LogFilter'
class LogFilterable a where
  -- | Try to convert the type from a strict 'ByteString'.
  prep :: ByteString -> Maybe a

instance LogFilterable ByteString where
  prep = return

instance LogFilterable BL.ByteString where
  prep = return . BL.fromStrict

-- | Helper Typeclass for types that implement both 'LogFilterable' and 'LogShowable'
class (LogFilterable a, LogShowable a) => Loggable a where

instance Loggable ByteString where
instance Loggable BL.ByteString where

-- | Type that represents a log filtering function. If the return type
-- is Nothing, then no log message will be created. Otherwise, a log message
-- will be created using the (potentially different) returned value.
type LogFilter a = a -> Maybe a

logFilter :: (Loggable a) => ByteString -> LogFilter a -> Maybe a
logFilter bs lf = prep bs >>= lf

-- | Make a filtering request logger with the default 'FilterOptions'.
mkDefaultFilterLogger :: (Loggable a) => LogFilter a -> Middleware
mkDefaultFilterLogger = mkFilterLogger def

-- | Given a valid 'LogFilter' and custom 'FilterOptions', construct a filtering request logger.
mkFilterLogger :: (Loggable a) => FilterOptions -> LogFilter a -> Middleware
mkFilterLogger opts lf = unsafePerformIO $
  mkRequestLogger def { outputFormat = CustomOutputFormatWithDetails $ customOutputFormatter opts lf }
{-# NOINLINE mkFilterLogger #-}

customOutputFormatter :: (Loggable a) => FilterOptions -> LogFilter a -> OutputFormatterWithDetails
customOutputFormatter FilterOptions{..} lf date req status responseSize time reqBody builder =
  maybe mempty (buildLog detailed date req status responseSize time builder) bodyToLog
  where bodyToLog
          | null reqBody && logOnEmptyBody = Just BS.empty
          | otherwise                      = logShow <$> logFilter (BS.concat reqBody) lf

type MyOutputFormatter = ZonedDate -> Request -> Status -> Maybe Integer -> NominalDiffTime  -> Builder -> ByteString -> LogStr

buildLog :: Bool -> MyOutputFormatter
buildLog detail date req status responseSize time builder body = logString detail
  where
    dfromRational :: Rational -> Double
    dfromRational = fromRational

    inMS   = printf "%.2f" . dfromRational $ toRational time * 1000

    header = colorizedUrl        <> "\n" <>
             date                <> "\n" <>
             colorizedStatusCode <> "\n"

    buildRespSize (Just s) = "Response Size: " <> toBS (show s) <> "\n"
    buildRespSize Nothing  = ""

    buildDuration = toBS inMS <> "ms" <> "\n"

    buildDetails True  = buildRespSize responseSize <> buildDuration
    buildDetails False = ""

    formattedBody
      | BS.null body = body
      | otherwise    = yellow $ body <> "\n"

    logString detailed = toLogStr (
      header              <>
      buildDetails detail <>
      formattedBody)

    colorizedUrl = cyan $ rawPathInfo req <> rawQueryString req

    colorizedStatusCode
      | code < 300 = green str
      | code < 400 = yellow str
      | otherwise  = red str
      where str = toBS (show code) <> " " <> codeMsg
            code = statusCode status
            codeMsg = statusMessage status
