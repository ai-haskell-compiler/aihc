module HaskellWorks.Data.ByteString.Builder
  ( chainInterleaveIO
  ) where

import Data.Function

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import qualified System.IO.Unsafe     as IO

chainInterleaveIO :: [IO LBS.ByteString] -> IO LBS.ByteString
chainInterleaveIO iobs = LBS.fromChunks . ([] &) <$> go diobs
  where diobs :: [IO ([BS.ByteString] -> [BS.ByteString])]
        diobs = ((++) . LBS.toChunks <$>) <$> iobs
        go :: [IO ([BS.ByteString] -> [BS.ByteString])] -> IO ([BS.ByteString] -> [BS.ByteString])
        go (ma:mas) = do
          a <- ma
          as <- IO.unsafeInterleaveIO $ go mas
          return (a . as)
        go [] = return mempty
