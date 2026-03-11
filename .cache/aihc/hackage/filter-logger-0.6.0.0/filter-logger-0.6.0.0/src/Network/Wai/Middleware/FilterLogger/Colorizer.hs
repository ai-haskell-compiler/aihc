module Network.Wai.Middleware.FilterLogger.Colorizer where

import           Data.ByteString           (ByteString, pack)
import           Data.Char
import           Data.Monoid
import           System.Console.ANSI
import           System.Console.ANSI.Codes
import           System.Console.ANSI.Types

toBS :: String -> ByteString
toBS = pack . map (fromIntegral . ord)

colored :: Color -> ByteString -> ByteString
colored c bs = colorCode <> bs <> reset
  where colorCode = toBS $ setSGRCode [SetColor Foreground Vivid c]

reset :: ByteString
reset = toBS $ setSGRCode []

red :: ByteString -> ByteString
red = colored Red

blue :: ByteString -> ByteString
blue = colored Blue

yellow :: ByteString -> ByteString
yellow = colored Yellow

green :: ByteString -> ByteString
green = colored Green

cyan :: ByteString -> ByteString
cyan = colored Cyan
