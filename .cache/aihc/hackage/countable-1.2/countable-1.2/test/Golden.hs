module Golden where

import Data.ByteString.Builder
import Data.ByteString.Lazy.Char8
import Data.IORef
import Test.Tasty
import Test.Tasty.Golden

accumulate :: Monoid a => ((a -> IO ()) -> IO r) -> IO a
accumulate f = do
    ref <- newIORef mempty
    _ <- f $ \a1 -> modifyIORef ref (\a0 -> mappend a0 a1)
    readIORef ref

goldenVsWrite :: TestName -> FilePath -> ((ByteString -> IO ()) -> IO a) -> TestTree
goldenVsWrite name path action =
    goldenVsString name path $ fmap toLazyByteString $ accumulate $ \write -> action (write . lazyByteString)

goldenVsWriteString :: TestName -> FilePath -> ((String -> IO ()) -> IO a) -> TestTree
goldenVsWriteString name path action = goldenVsWrite name path $ \write -> action (write . pack)
