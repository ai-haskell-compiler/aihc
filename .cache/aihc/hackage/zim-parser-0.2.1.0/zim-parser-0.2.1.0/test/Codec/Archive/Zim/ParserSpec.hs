{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Codec.Archive.Zim.ParserSpec (main, spec) where

import Prelude ()
import Prelude.Compat
import Control.Monad (forM_)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromJust, isJust)
import System.IO (Handle, openBinaryFile, IOMode(ReadMode))

import Data.Array.IArray (bounds, (!))

import Test.Hspec

import Codec.Archive.Zim.Parser

testSmallFilePath :: FilePath
testSmallFilePath = "test/wikipedia_en_ray_charles_2015-06.zim"

main :: IO ()
main = hspec spec

setup :: IO (Handle, ZimHeader)
setup = do
    small <- openBinaryFile testSmallFilePath ReadMode
    smallZim <- getHeader small
    return (small, smallZim)

spec :: Spec
spec = do
    describe "zim" $ do
      beforeAll setup $ do

        it "can open Ray Charles ZIM" $ \(_, smallZim) -> do
            zimMagicNumber smallZim `shouldBe` 72173914

        it "can get version" $ \(_, smallZim) -> do
            zimVersion smallZim `shouldBe` 5

        it "can get article count" $ \(_, smallZim) -> do
            zimArticleCount smallZim `shouldBe` 458

        it "can get cluster count" $ \(_, smallZim) -> do
            zimClusterCount smallZim `shouldBe` 215

        it "can parse MIME list" $ \h -> do
            mimeList <- getMimeList h
            mimeList ! 0 `shouldBe` "application/javascript"
            bounds mimeList `shouldBe` (0, 9)

        it "can get main page" $ \(_, smallZim) -> do
            zimMainPage smallZim `shouldBe` Just 238

        it "can get main page URL" $ \_ -> do
            url <- getMainPageUrl testSmallFilePath
            url `shouldBe` Just (Url "A/index.htm")

        it "can lookup directory entry by URL index" $ \h -> do
            de <- h `getDE` UrlIndex 0
            zimDeType de `shouldBe` ZimRedirectEntry
            zimDeUrl de `shouldBe` "favicon"

        it "can lookup directory entry by Title index" $ \h -> do
            de <- h `getDE` TitleIndex 3
            zimDeTitle de `shouldBe` "(The Night Time Is) The Right Time"
            zimDeNamespace de `shouldBe` 'A'

        it "can lookup cluster 0" $ \h -> do
            cluster <- unCluster <$> h `getCluster` ClusterNumber 0
            BL.length  cluster `shouldBe` 2160083

        it "can get cluster 2 blob 0" $ \h -> do
            Blob blob <- h `getBlob` (ClusterNumber 2, BlobNumber 0)
            BL.take 4 blob `shouldBe` "\137PNG"

        it "can get blob for article 11" $ \h -> do
            bs <- h `getContent` UrlIndex 11
            BL.take 12 (snd $ fromJust bs) `shouldBe` "<html><head>"

        it "can search for title" $ \h -> do
            let title = B8.pack "(The Night Time Is) The Right Time"
            res <- h `searchDE` mkNsTitle 'A' title
            length res `shouldBe` 1
            (fst . head) res `shouldBe` 3

        it "can search for non-existent title" $ \h -> do
            let title = B8.pack "nonexistent-title"
            res <- h `searchDE` mkNsTitle 'z' title
            length res `shouldBe` 0

        it "can search for title prefix" $ \h -> do
            let title = B8.pack "Blue"
            res <- h `searchDE` mkNsTitlePrefix 'A' title
            length res `shouldBe` 2
            let [(lb, _), (ub, _)] = res
            (lb, ub) `shouldBe` (22, 25)

        it "can search for non-existent title prefix" $ \h -> do
            let title = B8.pack "ZZZ"
            res <-h `searchDE` mkNsTitlePrefix 'A' title
            length res `shouldBe` 0

        it "can search for first title prefix" $ \h -> do
            let title = B8.pack "fav"
            res <- h `searchDE` mkNsTitlePrefix '-' title
            length res `shouldBe` 2
            let [(lb, _), (ub, _)] = res
            (lb, ub) `shouldBe` (0, 0)

        it "can search for last title prefix" $ \h -> do
            let title = B8.pack "Title"
            res <- h `searchDE` mkNsTitlePrefix 'M' title
            length res `shouldBe` 2
            let [(lb, _), (ub, _)] = res
            (lb, ub) `shouldBe` (457, 457)

        it "can search for URLs" $ \h -> do
            let url = "A/A_Fool_for_You.html"
            res <- h `searchDE` Url url
            length res `shouldBe` 1
            let [(r, _)] = res
            r `shouldBe` 4

        it "can search for non-existent URL" $ \h -> do
            let url = "a/zzz.html"
            res <- h `searchDE` Url url
            length res `shouldBe` 0

        it "can get (mimetype, content)" $ \_ -> do
            res <- testSmallFilePath `getContent` Url "A/index.htm"
            res `shouldSatisfy` isJust
            let Just (m, c) = res
            (m, BL.length c) `shouldBe` ("text/html", 8637)

        let searchAllUrls hdl hdr des =
              forM_ des $ \de -> do
                let ns = zimDeNamespace de
                    url = zimDeUrl de
                    url' = mkNsUrl ns url
                res <- (hdl, hdr) `searchDE` url'
                length res `shouldBe` 1
                (zimDeUrl . snd . head $ res) `shouldBe` url
            getAllArticles hdl hdr n = do
              let ns = [0 .. n - 1]
              forM_ ns $ \i -> do
                de <- (hdl, hdr) `getDE` UrlIndex i
                let url = zimDeUrl de
                res <- (hdl, hdr) `getContent` UrlIndex i
                res `shouldSatisfy` isJust
                (url, res) `shouldSatisfy` (\(_, Just (_, bs)) -> BL.length bs > 0)

        it "can search all URLs" $ \(hdl, hdr) -> do
          deList <- mapM (((hdl, hdr) `getDE`) . UrlIndex) [0 .. zimArticleCount hdr - 1]
          searchAllUrls hdl hdr deList

        it "can get content from all URLs" $ \(hdl, hdr) -> do
          getAllArticles hdl hdr 457
