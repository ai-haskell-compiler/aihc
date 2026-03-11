import qualified System.IO.Streams.Brotli as Brotli
import qualified System.IO.Streams as Streams
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit                           hiding (Test)
import qualified Data.ByteString as BS

import System.IO.Unsafe

main :: IO ()
main = defaultMain [ testGroup "System.IO.Streams.Brotli" tests ]
  where
    tests =  [ test0, test1, test2, prop1 ]

test0 :: Test
test0 = testCase "empty" $ (decode . encode) BS.empty @?= BS.empty

test1 :: Test
test1 = testCase "hello" $ (decode . encode) bs @?= bs
  where
    bs = BS.pack [104,101,108,108,111]

test2 :: Test
test2 = testCase "10MiB" $ (decode . encode) bs @?= bs
  where
    bs = BS.replicate (10*1024*1024) 0xaa

prop1 :: Test
prop1 = testProperty "random" go
  where
    go s = (decode . encode) bs == bs
      where
        bs = BS.pack s

{- concat not supported yet
prop2 :: Test
prop2 = testProperty "random-concat" go
  where
    go s s2 = decode (encode bs `BS.append` encode bs2) == bs `BS.append` bs2
      where
        bs  = BS.pack s
        bs2 = BS.pack s2
-}

encode :: BS.ByteString -> BS.ByteString
encode bs = unsafePerformIO $ do
    lst <- Streams.outputToList $ \obs -> do
        ibs  <- Streams.fromByteString bs
        obs' <- Brotli.compress obs
        Streams.connect ibs obs'

    return (BS.concat lst)
{-# NOINLINE encode #-}

decode :: BS.ByteString -> BS.ByteString
decode bs = unsafePerformIO $ do
    lst <- Streams.outputToList $ \obs -> do
        ibs  <- Streams.fromByteString bs
        ibs' <- Brotli.decompress ibs
        Streams.connect ibs' obs

    return (BS.concat lst)
{-# NOINLINE decode #-}
