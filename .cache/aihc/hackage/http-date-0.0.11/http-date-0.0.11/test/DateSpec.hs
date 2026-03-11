module DateSpec where

import Control.Monad
import Model
import Network.HTTP.Date
import Test.Hspec

----------------------------------------------------------------

spec :: Spec
spec = do
    describe "formatHTTPDate" $ do
        it "behaves like the model" $ 
            forM_ [0,100000..10000000000] $ \epochtime -> do
                let m = model epochtime
                    o = ours epochtime
                    model = utcToDate . epochTimeToUtcTime
                    ours  = formatHTTPDate . epochTimeToHTTPDate
                o `shouldBe` m
    describe "httpDateToUTC" $ do
        it "behaves like the model" $ 
            forM_ [0,100000..10000000000] $ \epochtime -> do
                let m = epochTimeToUtcTime epochtime
                    o = httpDateToUTC $ epochTimeToHTTPDate epochtime
                o `shouldBe` m
    describe "parseHTTPDate" $ do
        it "behaves like the model" $
            forM_ [0,100000..10000000000] $ \epochtime -> do
                let m = epochTimeToHTTPDate epochtime
                    Just o = parseHTTPDate $ formatHTTPDate m
                o `shouldBe` m
    describe "utcToHTTPDate" $ do
        it "behaves like the model" $
            forM_ [0,100000..10000000000] $ \epochtime -> do
                let m = epochTimeToHTTPDate epochtime
                    o = utcToHTTPDate $ epochTimeToUtcTime epochtime
                o `shouldBe` m
