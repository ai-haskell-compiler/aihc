-- | This is a module which runs the 'HtmlBenchmarks' module using the different
-- renderers available.
--

import qualified Blaze.ByteString.Builder as Blaze
import qualified Blaze.ByteString.Builder.Char.Utf8 as Blaze
import           Criterion.Main
import qualified Data.ByteString.Lazy as LB
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           HtmlBenchmarks (HtmlBenchmark (..), benchmarks)
import           Lucid (renderBS)

-- | Function to run the benchmarks using criterion
--
main :: IO ()
main = defaultMain $ map benchHtml benchmarks
  where
    benchHtml (HtmlBenchmark name f x _) = bgroup name $
        [bench "ByteString"   $ nf (LB.length . renderBS . f) x
        ]
