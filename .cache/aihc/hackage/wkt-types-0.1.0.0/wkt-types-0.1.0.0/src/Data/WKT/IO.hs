module Data.WKT.IO (module Data.WKT.IO) where

import qualified Data.Text.IO as TIO
import Data.WKT.Geometries (Geometries(..))
import Data.WKT.Classes (FromWKT(..), ToWKT (..))

readWKTFile :: FilePath -> IO (Geometries Double)
readWKTFile fp = fromWKT <$> TIO.readFile fp

writeWKTFile :: Show a => Geometries a -> FilePath -> IO ()
writeWKTFile  = flip TIO.writeFile . toWKT