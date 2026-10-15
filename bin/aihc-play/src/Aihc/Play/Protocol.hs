{-# LANGUAGE OverloadedStrings #-}

-- | The request framing of the helper.
--
-- A request is one header line and a body. The header line is the command
-- name, a space, and the byte count of the body. The body follows the
-- newline of the header and has exactly that many bytes.
--
-- A @load@ body holds source files. Each file is one header line, the path
-- and the byte count of the file, and then the file bytes.
--
-- Byte counts keep the framing simple on both sides. The page encodes the
-- source as UTF-8 and counts the bytes. The helper does not have to parse
-- JSON for that.
module Aihc.Play.Protocol
  ( Request (..),
    readRequest,
    parseFiles,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import System.IO (Handle, hIsEOF)
import Text.Read (readMaybe)

data Request = Request
  { requestCommand :: !Text,
    requestBody :: !ByteString
  }

-- | The next request, or 'Nothing' at the end of the input. A header line
-- that does not parse gives 'Left'.
readRequest :: Handle -> IO (Maybe (Either String Request))
readRequest handle = do
  atEnd <- hIsEOF handle
  if atEnd
    then pure Nothing
    else do
      header <- BC.hGetLine handle
      case parseHeader header of
        Nothing -> pure (Just (Left ("invalid request header: " <> BC.unpack header)))
        Just (command, count) -> do
          body <- BS.hGet handle count
          pure (Just (Right (Request (decode command) body)))

-- | The files of a @load@ body.
parseFiles :: ByteString -> Either String [(FilePath, Text)]
parseFiles = go
  where
    go bytes
      | BS.null bytes = Right []
      | otherwise =
          let (header, rest) = BC.break (== '\n') bytes
           in case parseHeader header of
                Nothing -> Left ("invalid file header: " <> BC.unpack header)
                Just (path, count) ->
                  let afterHeader = BS.drop 1 rest
                      (content, remaining) = BS.splitAt count afterHeader
                   in if BS.length content /= count
                        then Left ("short file body for " <> BC.unpack path)
                        else ((BC.unpack path, decode content) :) <$> go remaining

parseHeader :: ByteString -> Maybe (ByteString, Int)
parseHeader line =
  case BC.words (BC.filter (/= '\r') line) of
    [name, countText] -> (name,) <$> readMaybe (BC.unpack countText)
    _ -> Nothing

decode :: ByteString -> Text
decode = decodeUtf8With lenientDecode
