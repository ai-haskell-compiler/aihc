{-# LANGUAGE OverloadedStrings #-}
import Control.Monad.Trans.Resource (runResourceT)
import Test.Hspec
import qualified Data.Conduit        as C
import qualified Data.Conduit.Binary as CB

import Data.Conduit.ImageSize

main :: IO ()
main = hspec $ do
    describe "image size" $ do
        it "png" $ check (Just (Size 271 61, PNG)) "test/logo.png"
        it "jpg" $ check (Just (Size 271 61, JPG)) "test/logo.jpg"
        it "gif" $ check (Just (Size 271 61, GIF)) "test/logo.gif"
        it "invalid" $ check Nothing "test/main.hs"

check :: Maybe (Size, FileFormat) -> FilePath -> Expectation
check ex fp = do
    size <- runResourceT $ CB.sourceFile fp C.$$ sinkImageInfo
    size `shouldBe` ex
