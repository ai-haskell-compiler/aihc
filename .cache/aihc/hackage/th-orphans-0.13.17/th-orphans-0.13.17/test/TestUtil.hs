{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module TestUtil where

#if MIN_VERSION_template_haskell(2,16,0)

import Language.Haskell.TH.Syntax
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import GHC.Exts (Addr#)
import GHC.ForeignPtr (newForeignPtr_)
import GHC.Ptr (Ptr(Ptr))

testBytes :: BS.ByteString
testBytes =
  "test bytes " <>
  (BS.take (len - 2) $ BS.drop 1 $ BS.replicate len 42)
  where
    len = 20 * 1024 * 1024

bsToBytes :: BS.ByteString -> Bytes
bsToBytes bs =
  Bytes
    { bytesPtr = fp
    , bytesOffset = fromIntegral offset
    , bytesSize = fromIntegral size
    }
  where
    (fp, offset, size) = BS.toForeignPtr bs

addrToBs :: Addr# -> Int -> IO BS.ByteString
addrToBs addr len = do
  fp <- newForeignPtr_ (Ptr addr)
  return $ BS.fromForeignPtr fp 0 len

#endif
