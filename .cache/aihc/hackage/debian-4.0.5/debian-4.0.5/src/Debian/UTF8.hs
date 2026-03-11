{-# LANGUAGE CPP #-}
-- | There are old index files that have funky characters like 'ø'
-- that are not properly UTF8 encoded.  As far as I can tell, these
-- files are otherwise plain ascii, so just naivelyinsert the
-- character into the output stream.
module Debian.UTF8
    ( decode
    , readFile
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import qualified Data.ByteString.Char8 as B (concat)
import qualified Data.ByteString.Lazy.Char8 as L (ByteString, readFile, toChunks)
import Data.Char (chr)
import Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Word (Word8)
import Prelude hiding (readFile)

decode :: L.ByteString -> T.Text
decode b = decodeUtf8With e (B.concat (L.toChunks b))
    where
      e :: String -> Maybe Word8 -> Maybe Char
      e _description w = fmap (chr . fromIntegral) w

readFile :: FilePath -> IO T.Text
readFile path = decode <$> L.readFile path
