-- | HTTP GET over wasi:http. The aihc wasm32-wasip3 host opens a path that
-- starts with @http://@ or @https://@ as a read-only stream of the response
-- body, so a get is an ordinary read of such a "file". The host fails the
-- open with the error number 'statusErrnoBase' plus the status when the
-- status is outside 200 to 299. The native build of this package has the
-- same interface over http-client.
module Aihc.Http
  ( httpGet,
    httpGetToFile,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Foreign.C.Types (CInt (..))
import GHC.IO.Exception (IOException (ioe_errno))
import System.IO (Handle, IOMode (ReadMode, WriteMode), withBinaryFile)

-- | The host reports HTTP status @s@ as the error number @statusErrnoBase + s@.
statusErrnoBase :: CInt
statusErrnoBase = 10000

-- | Get a URL and return the whole body. A status outside 200 to 299 is an
-- error, as is any failure of the connection.
httpGet :: String -> IO (Either String LBS.ByteString)
httpGet url = do
  result <- try (withBinaryFile url ReadMode (readChunks []))
  pure $ case result of
    Left err -> Left (describe url err)
    Right body -> Right body

-- | Get a URL and stream the body to a file, replacing the file. The body
-- is never held in memory, so this is the call for a large download. A
-- failure is an 'IOError' and leaves the file in an unspecified state.
httpGetToFile :: String -> FilePath -> IO ()
httpGetToFile url path = do
  result <- try (withBinaryFile url ReadMode (withBinaryFile path WriteMode . copy))
  either (ioError . userError . describe url) pure result
  where
    copy :: Handle -> Handle -> IO ()
    copy source target = do
      chunk <- BS.hGetSome source 65536
      unless (BS.null chunk) $ do
        BS.hPut target chunk
        copy source target

-- | Read to the end of the stream. 'BS.hGetContents' is no use here: it
-- sizes its buffer from the file, and a response body has no size.
readChunks :: [BS.ByteString] -> Handle -> IO LBS.ByteString
readChunks chunks source = do
  chunk <- BS.hGetSome source 65536
  if BS.null chunk
    then pure (LBS.fromChunks (reverse chunks))
    else readChunks (chunk : chunks) source

describe :: String -> IOException -> String
describe url err = case ioe_errno err of
  Just code
    | code > statusErrnoBase && code < statusErrnoBase + 1000 ->
        "HTTP " ++ show (code - statusErrnoBase) ++ " for " ++ url
  _ -> "Failed to get " ++ url ++ ": " ++ show err
