{-# LANGUAGE OverloadedStrings #-}
import           Data.List.NonEmpty.Compat      ( NonEmpty(..) )
import           Test.Tasty
import           Test.Tasty.Wai
import           Network.HTTP.Types             ( status200 )
import           Network.Wai                    ( Application
                                                , responseLBS
                                                )

import           Network.Wai.Middleware.Clacks

main :: IO ()
main = defaultMain tests


tests :: TestTree
tests = testGroup "Wai Tests" [testSingle, testMultiple, testExisting]
  where
    testSingle :: TestTree
    testSingle = testWai (simpleApp gnuTerryPratchett) "Single Message" $ do
        get "42" >>= assertHeader clacksHeaderName "GNU Terry Pratchett"
    testMultiple :: TestTree
    testMultiple =
        let settings = Clacks $ "GNU Terry Pratchett" :| ["GNU Ada Lovelace"]
        in  testWai (simpleApp settings) "Multiple Messages"
                $   get "9001"
                >>= assertHeader clacksHeaderName
                                 "GNU Terry Pratchett,GNU Ada Lovelace"
    testExisting :: TestTree
    testExisting =
        testWai (existingHeaderApp gnuTerryPratchett) "Existing Clacks Header"
            $ do
                  get "neko" >>= assertHeader
                      clacksHeaderName
                      "GNU Ada Lovelace,GNU Terry Pratchett"


simpleApp :: Clacks -> Application
simpleApp settings =
    clacks settings $ \_ cb -> cb $ responseLBS status200 [] "Hello World"

existingHeaderApp :: Clacks -> Application
existingHeaderApp settings = clacks settings $ \_ cb -> cb $ responseLBS
    status200
    [(clacksHeaderName, "GNU Ada Lovelace")]
    "Hello World"
