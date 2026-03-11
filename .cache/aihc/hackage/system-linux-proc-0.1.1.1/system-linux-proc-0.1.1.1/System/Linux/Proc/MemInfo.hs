{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Linux.Proc.MemInfo
  ( MemInfo (..)
  , readProcMemInfo
  , readProcMemInfoKey
  , readProcMemUsage
  , renderSizeBytes
  ) where

import           Control.Error (ExceptT (..), fromMaybe, runExceptT, throwE)

import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Maybe (mapMaybe)
#if ! MIN_VERSION_base(4,14,0)
import           Data.Monoid ((<>))
#endif
import           Data.Text (Text)
import qualified Data.Text as Text
import           Data.Word (Word64)

import           System.Linux.Proc.IO
import           System.Linux.Proc.Errors

-- | A struct to contain information parsed from the `/proc/meminfo` file
-- (Linux only AFAIK). Fields that are listed as being in kilobytes in the proc
-- filesystem are converted to bytes.
-- Not all versions of the Linux kernel make all the fields in this struct
-- available in which case they will be assigned a value of zero.
data MemInfo = MemInfo
  { memTotal :: !Word64 -- ^ Total physical RAM.
  , memFree :: !Word64 -- ^ Total free RAM (which includes memory used for filesystem caching).
  , memAvailable :: !Word64 -- ^ Available memory.
  , memBuffers :: !Word64 -- ^ Amount of RAM used for file buffers.
  , memSwapTotal :: !Word64 -- ^ Total amount of swap space.
  , memSwapFree :: !Word64 -- ^ Amount of swap space that is free.
  } deriving (Eq, Show)

-- | Read the `/proc/meminfo` file (Linux only AFAIK) and return a
-- `MemInfo` structure.
-- Although this is in `IO` all exceptions and errors should be caught and
-- returned as a `ProcError`.
readProcMemInfo :: IO (Either ProcError MemInfo)
readProcMemInfo =
  runExceptT $ do
    bs <- readProcFile fpMemInfo
    case Atto.parseOnly parseFields bs of
      Left e -> throwE $ ProcParseError fpMemInfo (Text.pack e)
      Right xs -> pure $ construct xs

-- | Read `/proc/meminfo` file and return a value calculated from:
--
--      MemAvailable / MemTotal
--
-- Although this is in `IO` all exceptions and errors should be caught and
-- returned as a `ProcError`.
readProcMemUsage :: IO (Either ProcError Double)
readProcMemUsage =
  runExceptT $ do
    xs <- BS.lines <$> readProcFile fpMemInfo
    pure . convert $ List.foldl' getValues (0, 1) xs
  where
    getValues :: (Word64, Word64) -> ByteString -> (Word64, Word64)
    getValues (avail, total) bs =
      case BS.break (== ':') bs of
        ("MemTotal", rest) -> (avail, fromEither total $ Atto.parseOnly pValue rest)
        ("MemAvailable", rest) -> (fromEither avail $ Atto.parseOnly pValue rest, total)
        _ -> (avail, total)

    convert :: (Word64, Word64) -> Double
    convert (avail, total) = fromIntegral avail / fromIntegral total

-- | Read the value for the given key from `/proc/meminfo`.
-- Although this is in `IO` all exceptions and errors should be caught and
-- returned as a `ProcError`.
readProcMemInfoKey :: ByteString -> IO (Either ProcError Word64)
readProcMemInfoKey target =
  runExceptT $ do
    xs <- BS.lines <$> readProcFile fpMemInfo
    hoistEither . headEither keyError $ mapMaybe findValue xs
  where
    findValue :: ByteString -> Maybe Word64
    findValue bs =
      let (key, rest) = BS.break (== ':') bs in
      if key /= target
        then Nothing
        else either (const Nothing) Just $ Atto.parseOnly pValue rest
    keyError :: ProcError
    keyError = ProcMemInfoKeyError $ Text.pack (BS.unpack target)

-- | Render a Word64 as an easy to read size with a bytes, kB, MB, GB TB or PB
-- suffix.
renderSizeBytes :: Word64 -> Text
renderSizeBytes s
  | d >= 1e15 = render (d * 1e15) <> " PB"
  | d >= 1e12 = render (d * 1e12) <> " TB"
  | d >= 1e12 = render (d * 1e12) <> " TB"
  | d >= 1e9 = render (d * 1e-9) <> " GB"
  | d >= 1e6 = render (d * 1e-6) <> " MB"
  | d >= 1e3 = render (d * 1e-3) <> " kB"
  | otherwise = Text.pack (show s) <> " bytes"
  where
    d = fromIntegral s :: Double
    render = Text.pack . List.take 5 . show

-- -----------------------------------------------------------------------------
-- Internals.

fpMemInfo :: FilePath
fpMemInfo = "/proc/meminfo"

fromEither :: a -> Either e a -> a
fromEither a = either (const a) id

headEither :: e -> [a] -> Either e a
headEither e [] = Left e
headEither _ (x:_) = Right x

hoistEither :: Monad m => Either e a -> ExceptT e m a
hoistEither = ExceptT . pure

construct :: [(ByteString, Word64)] -> MemInfo
construct xs =
  MemInfo
    (fromMaybe 0 $ Map.lookup "MemTotal" mp)
    (fromMaybe 0 $ Map.lookup "MemFree" mp)
    (fromMaybe 0 $ Map.lookup "MemAvailable" mp)
    (fromMaybe 0 $ Map.lookup "Buffers" mp)
    (fromMaybe 0 $ Map.lookup "SwapTotal" mp)
    (fromMaybe 0 $ Map.lookup "SwapFree" mp)
  where
    mp = Map.fromList xs

-- -----------------------------------------------------------------------------
-- Parsers.

parseFields :: Parser [(ByteString, Word64)]
parseFields =
  Atto.many1 (pFieldValue <* Atto.endOfLine)


{-
The /proc/meminfo file's contents takes the form:

    MemTotal:       16336908 kB
    MemFree:         9605680 kB
    MemAvailable:   12756896 kB
    Buffers:         1315348 kB
    ....

-}

pFieldValue :: Parser (ByteString, Word64)
pFieldValue =
  (,) <$> pName <*> pValue


pName :: Parser ByteString
pName =
  Atto.takeWhile (/= ':')

pValue :: Parser Word64
pValue = do
  val <- Atto.char ':' *> Atto.skipSpace *> Atto.decimal
  Atto.skipSpace
  rest <- Atto.takeWhile (not . Atto.isSpace)
  case rest of
    "" -> pure val
    "kB" -> pure $ 1024 * val
    _ -> fail $ "Unexpected '" ++ BS.unpack rest ++ "'"

