{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import           Network.HTTP.Client
import           Network.HTTP.Types.Status (statusCode)
import           Data.Vector
import           Test.Hspec

import           Network.IPv6DB.Types

main :: IO ()
main = hspec $ do

    describe "POST /ipv6db/v1/list/test0/addresses/abcd::1234" $

        it "Creates the resource" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/test0/addresses/abcd::1234"
          let req = initReq
                      { method = "POST"
                      , requestBody =
                          RequestBodyLBS $
                            encode (object [("ttl", Null),("source", object [("data",String "A B C D")])]) }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "PUT /ipv6db/v1/list/test0/addresses/abcd::1234" $

        it "Updates the resource" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/test0/addresses/abcd::1234"
          let req = initReq
                      { method = "PUT"
                      , requestBody =
                          RequestBodyLBS $
                            encode (object [("ttl", Null),("source", object [("data",String "E F G H")])])
                      }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "GET /ipv6db/v1/list/test0/addresses/abcd::1234" $

        it "Gets the resource" $ do

          mngr <- newManager defaultManagerSettings
          req <- parseRequest "http://localhost:4446/ipv6db/v1/list/test0/addresses/abcd::1234"
          res <- httpLbs req mngr
          responseBody res `shouldBe` "{\"ttl\":null,\"list\":\"test0\",\"address\":\"abcd::1234\",\"source\":{\"data\":\"E F G H\"}}"

    describe "DELETE /ipv6db/v1/list/test0/addresses/abcd::1234" $

        it "Deletes the resource" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/test0/addresses/abcd::1234"
          let req = initReq { method = "DELETE" }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "POST /ipv6db/v1/list/test0/addresses" $

        it "Creates resources for the given list" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/test0/addresses"
          let req = initReq
                      { method = "POST"
                      , requestBody =
                          RequestBodyLBS $
                            encode $
                              Array $
                                fromList
                                  [ object
                                      [ ("address","abcd::1234")
                                      , ("ttl", Null)
                                      , ("source", object [ ("data", String "E F G H") ])
                                      ]
                                  , object
                                      [ ("address","abcd::5678")
                                      , ("ttl", Null)
                                      , ("source", object [ ("data", String "I J K L") ])
                                      ]
                                  ]
                       }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "PUT /ipv6db/v1/list/test0/addresses" $

        it "Updates the resources that belong to the given list" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/test0/addresses"
          let req = initReq
                      { method = "PUT"
                      , requestBody =
                          RequestBodyLBS $
                            encode $
                              Array $
                                fromList
                                  [ object
                                      [ ("address","abcd::1234")
                                      , ("ttl", Null)
                                      , ("source", object [ ("data", String "H G F E") ])
                                      ]
                                  , object
                                      [ ("address","abcd::5678")
                                      , ("ttl", Null)
                                      , ("source", object [ ("data", String "L K J I") ])
                                      ]
                                  ]
                       }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "GET /ipv6db/v1/list/test0/addresses" $

        it "Gets the resources that belong to the given list" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/test0/addresses"
          let req = initReq
                      { requestBody =
                          RequestBodyLBS $
                            encode $
                              Array $
                                fromList
                                  [ String "abcd::1234"
                                  , String "abcd::5678"
                                  ]
                       }
          res <- httpLbs req mngr
          responseBody res `shouldBe` "[{\"ttl\":null,\"list\":\"test0\",\"address\":\"abcd::1234\",\"source\":{\"data\":\"H G F E\"}},{\"ttl\":null,\"list\":\"test0\",\"address\":\"abcd::5678\",\"source\":{\"data\":\"L K J I\"}}]"

    describe "DELETE /ipv6db/v1/list/test0/addresses" $

        it "Deletes the resources that belong to the given list" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/list/test0/addresses"
          let req = initReq
                      { method = "DELETE"
                      , requestBody =
                          RequestBodyLBS $
                            encode $
                              Array $
                                fromList
                                  [ String "abcd::1234"
                                  , String "abcd::5678"
                                  ]
                       }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "POST /ipv6db/v1/batch" $

        it "Creates many resources on different lists" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/batch"
          let req = initReq
                      { method = "POST"
                      , requestBody =
                          RequestBodyLBS $
                            encode $
                              Array $
                                fromList
                                  [ object
                                      [ ("list","test1")
                                      , ("address","abcd::1234")
                                      , ("ttl", Null)
                                      , ("source", object [ ("data", String "E F G H") ])
                                      ]
                                  , object
                                      [ ("list","test2")
                                      , ("address","abcd::5678")
                                      , ("ttl", Null)
                                      , ("source", object [ ("data", String "I J K L") ])
                                      ]
                                  ]
                       }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "PUT /ipv6db/v1/batch" $

        it "Updates many resources on different lists" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/batch"
          let req = initReq
                      { method = "PUT"
                      , requestBody =
                          RequestBodyLBS $
                            encode $
                              Array $
                                fromList
                                  [ object
                                      [ ("list", "test1")
                                      , ("address", "abcd::1234")
                                      , ("ttl", Null)
                                      , ("source", object [ ("data", String "1 2 3 4") ])
                                      ]
                                  , object
                                      [ ("list", "test2")
                                      , ("address", "abcd::5678")
                                      , ("ttl", Null)
                                      , ("source", object [ ("data", String "5 6 7 8") ])
                                      ]
                                  ]
                       }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

    describe "GET /ipv6db/v1/batch" $

        it "Gets many resources on different lists" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/batch"
          let req = initReq
                      { requestBody =
                          RequestBodyLBS $
                            encode $
                              Array $
                                fromList
                                  [ object
                                      [ ("list", "test1")
                                      , ("address", "abcd::1234")
                                      ]
                                  , object
                                      [ ("list", "test2")
                                      , ("address", "abcd::5678")
                                      ]
                                  ]
                       }
          res <- httpLbs req mngr
          responseBody res `shouldBe` "[{\"ttl\":null,\"list\":\"test1\",\"address\":\"abcd::1234\",\"source\":{\"data\":\"1 2 3 4\"}},{\"ttl\":null,\"list\":\"test2\",\"address\":\"abcd::5678\",\"source\":{\"data\":\"5 6 7 8\"}}]"

    describe "DELETE /ipv6db/v1/batch" $

        it "Deletes many resources on different lists" $ do

          mngr <- newManager defaultManagerSettings
          initReq <- parseRequest "http://localhost:4446/ipv6db/v1/batch"
          let req = initReq
                      { method = "DELETE"
                      , requestBody =
                          RequestBodyLBS $
                            encode $
                              Array $
                                fromList
                                  [ object
                                      [ ("list", "test1")
                                      , ("address", "abcd::1234")
                                      ]
                                  , object
                                      [ ("list", "test2")
                                      , ("address", "abcd::5678")
                                      ]
                                  ]
                       }
          res <- httpLbs req mngr
          statusCode (responseStatus res) `shouldBe` 204

