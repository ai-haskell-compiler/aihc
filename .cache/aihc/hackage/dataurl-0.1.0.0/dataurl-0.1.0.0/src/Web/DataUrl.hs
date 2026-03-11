{-# LANGUAGE OverloadedStrings #-}
module Web.DataUrl
    ( parseDataUrl
    , DataUrlEnc(..), DataUrl(..)
    )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.Text
import Data.Char
import Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

-- | Source encoding of the data url string
data DataUrlEnc
   = DataUrlEncB64
   | DataUrlEncUrl
    deriving (Show, Eq)

-- | A parsed data url
data DataUrl
   = DataUrl
   { du_contentType :: !T.Text
   , du_charset :: !(Maybe T.Text) -- todo: fully strict
   , du_sourceEncoding :: !DataUrlEnc
   , du_data :: !BS.ByteString
   } deriving (Show, Eq)

-- | Parse a data url string
parseDataUrl :: T.Text -> Either String DataUrl
parseDataUrl = parseOnly dataUrlP

dataUrlP :: Parser DataUrl
dataUrlP =
    do _ <- string "data:"
       mContentType <-
           optional $
           do ct <- fieldContent
              mCharSet <-
                  optional (string ";charset=" *> fieldContent)
              return (ct, mCharSet)
       isBase64 <-
           isJust <$>
           optional (string ";base64")
       _ <- char ','
       rawData <- takeWhile1 (const True)
       endOfInput
       decodedData <-
           if isBase64
           then case B64.decode (T.encodeUtf8 rawData) of
                  Left err -> fail err
                  Right val -> return val
           else case urlDecode (T.unpack rawData) of
                  Nothing -> fail "Invalid url encoded data"
                  Just val -> return (BSC.pack val)
       return
          DataUrl
          { du_contentType = maybe "text/plain" fst mContentType
          , du_charset = join $ fmap snd mContentType
          , du_sourceEncoding =
              if isBase64 then DataUrlEncB64 else DataUrlEncUrl
          , du_data = decodedData
          }
    where
      fieldContent =
          takeWhile1 (\c -> c /= ';' && c /= ',')

-- hacky piece straight of some internet site :-(
urlDecode :: String -> Maybe String
urlDecode [] = Just []
urlDecode ('%':xs) =
    case xs of
      (a:b:xss) ->
          liftM ((chr . read $ "0x" ++ [a,b]) :) $
          urlDecode xss
      _ -> Nothing
urlDecode ('+':xs) = liftM (' ' :) $ urlDecode xs
urlDecode (x:xs) = liftM (x :) $ urlDecode xs
