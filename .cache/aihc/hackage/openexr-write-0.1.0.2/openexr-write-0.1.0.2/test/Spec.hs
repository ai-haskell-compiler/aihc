-- TODO: maybe add function that returns bytestring
-- TODO: test against images saved by the official OpenEXR library

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector          as V
import           System.Directory     (removeFile)
import           Test.Hspec

import qualified Graphics.OpenEXR     as EXR




main :: IO ()
main = hspec $ after_ (removeFile "tmp.exr") $ do
        let image  = EXR.ImageRGBF 1 1 (V.fromList [EXR.PixelRGBF 1 0 0])
            folder = "test/images/"

        describe "OpenEXR.writeFile" $ do
                it "saves image without compression" $ do
                        EXR.writeFile "tmp.exr" image EXR.NoCompression
                        output <- BL.readFile "tmp.exr"
                        valid  <- BL.readFile (folder ++ "red_1x1_no_compression.exr")
                        output `shouldBe` valid
                it "saves image with ZIPS compression" $ do
                        EXR.writeFile "tmp.exr" image EXR.ZipsCompression
                        output <- BL.readFile "tmp.exr"
                        valid  <- BL.readFile (folder ++ "red_1x1_zips_compression.exr")
                        output `shouldBe` valid
                it "saves image with ZIP compression" $ do
                        EXR.writeFile "tmp.exr" image EXR.ZipCompression
                        output <- BL.readFile "tmp.exr"
                        valid  <- BL.readFile (folder ++ "red_1x1_zip_compression.exr")
                        output `shouldBe` valid
