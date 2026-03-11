{-# LANGUAGE OverloadedStrings #-}

module Network.WebexTeamsSpec where

import           Control.Concurrent.Async    (withAsync)
import           Control.Concurrent.MVar     (MVar, newEmptyMVar, putMVar,
                                              takeMVar)
import           Control.Monad               (void)
import           Data.Aeson                  (decode, encode)
import           Data.Attoparsec.ByteString  (parseOnly)
import qualified Data.ByteString             as S (ByteString)
import qualified Data.ByteString.Char8       as C8 (pack, unpack)
import qualified Data.ByteString.Lazy        as L (ByteString)
import           Data.Default                (def)
import           Data.List                   (sort)
import           Data.Maybe                  (fromJust)
import           Data.Monoid                 ((<>))
import           Data.Text                   (pack)
import           Network.HTTP.Simple         as C (addRequestHeader,
                                                   defaultRequest,
                                                   getResponseBody, httpJSON,
                                                   parseRequest, setRequestHost,
                                                   setRequestMethod,
                                                   setRequestPath,
                                                   setRequestPort,
                                                   setRequestSecure)
import           Network.HTTP.Types          (Header, status200)
import           Network.URI                 (URIAuth (..))
import           Network.Wai                 (Application, queryString,
                                              rawPathInfo, requestHeaders,
                                              requestMethod, responseLBS,
                                              strictRequestBody)
import           Network.Wai.Handler.Warp    (defaultSettings, runSettings,
                                              setBeforeMainLoop, setPort)

import           Test.Hspec

import           Network.WebexTeams
import           Network.WebexTeams.Internal (LinkHeader (..), LinkParam (..),
                                              getNextUrl, linkHeader)


import           Data.Typeable               (typeOf)


listenPort = 3000
listenPortBS = (C8.pack . show) listenPort

mockBaseRequestRequest
    = C.addRequestHeader "Content-Type" "application/json; charset=utf-8"
    $ C.setRequestPort listenPort
    $ C.setRequestHost "127.0.0.1"
    $ C.setRequestSecure False
    $ C.defaultRequest

mockBaseRequest :: WebexTeamsRequest
mockBaseRequest = WebexTeamsRequest mockBaseRequestRequest "http:" $
    URIAuth "" "127.0.0.1" (":" <> show listenPort)

dummyAuth :: Authorization
dummyAuth = Authorization "dummyAuth"

extractRight :: Show err => Either err r -> r
extractRight (Right r)  = r
extractRight (Left err) = error $ show err

withMockServer :: Application -> IO () -> IO ()
withMockServer app inner = do
    readyToConnect <- newEmptyMVar
    let set = setBeforeMainLoop (putMVar readyToConnect ()) $ setPort listenPort defaultSettings
    withAsync (runSettings set app) (\_ -> takeMVar readyToConnect >> inner)

helloApp :: Application
helloApp _ respond = respond $ responseLBS status200 [] "hello, world"

contentType :: Header
contentType = ("Content-Type", "application/json; charset=utf-8")

simpleApp :: L.ByteString -> Application
simpleApp res _ respond = respond $ responseLBS status200 [contentType] res

paginationApp :: [L.ByteString] -> Application
paginationApp ress req respond = do
    let (cTypes, body) = dispatch $ rawPathInfo req
    respond $ responseLBS status200 cTypes body
      where
        dispatch "/1"   = ([contentType, ("Link", "<http://127.0.0.1:" <> listenPortBS <> "/2>; rel=\"next\"")], ress !! 1)
        dispatch "/2"   = ([contentType, ("Link", "<http://127.0.0.1:" <> listenPortBS <> "/3>; rel=\"next\"")], ress !! 2)
        dispatch "/3"   = ([contentType], ress !! 3)
        dispatch _      = ([contentType, ("Link", "<http://127.0.0.1:" <> listenPortBS <> "/1>; rel=\"next\"")], ress !! 0)

invalidPaginationApp :: [L.ByteString] -> Application
invalidPaginationApp ress req respond = do
    let (cTypes, body) = dispatch $ rawPathInfo req
    respond $ responseLBS status200 cTypes body
      where
        dispatch "/1"   = ([contentType, ("Link", "<http://127.0.0.1:8888/2>; rel=\"next\"")], ress !! 1)
        dispatch "/2"   = ([contentType, ("Link", "<http://127.0.0.1:" <> listenPortBS <> "/3>; rel=\"next\"")], ress !! 2)
        dispatch "/3"   = ([contentType], ress !! 3)
        dispatch _      = ([contentType, ("Link", "<http://127.0.0.1:" <> listenPortBS <> "/1>; rel=\"next\"")], ress !! 0)


teamGen :: String -> Team
teamGen i = Team { teamId           = TeamId . pack $ "teamId" <> i
                 , teamErrors       = Nothing
                 , teamName         = Just . TeamName . pack $ "teamName" <> i
                 , teamCreatorId    = Just . PersonId . pack $ "teamCreatorId" <> i
                 , teamCreated      = Just . Timestamp . pack $ "teamCreated" <> i
                 }

teamList :: String -> [Team]
teamList j = [ teamGen $ j <> show i | i <- [1..3] ]

teamListList :: [[Team]]
teamListList = [ teamList [c] | c <- ['a'..'d'] ]

readAllList :: ListReader i -> IO [i]
readAllList reader = go []
  where
    go xs = reader >>= \chunk -> case chunk of
        [] -> pure xs
        ys -> go (xs <> ys)

spec :: Spec
spec = do
    describe "Mock Applications" $ do
        it "simple mock app returns list of team" $ do
            receivedReqMVar <- newEmptyMVar
            let (WebexTeamsRequest baseReq _ _) = mockBaseRequest
                req = setRequestPath "/v1/teams"
                    $ setRequestMethod "GET"
                    $ baseReq
                testData = TeamList $ teamList ['Z']

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode testData) req respond
                ) $ do
                res <- getResponseBody <$> httpJSON req
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "GET"
                rawPathInfo receivedReq `shouldBe` "/v1/teams"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

        it "pagenation mock app returns list of team and Link header" $ do
            receivedReqMVar <- newEmptyMVar
            let (WebexTeamsRequest baseReq _ _) = mockBaseRequest
                req = setRequestPath "/v1/teams"
                    $ setRequestMethod "GET"
                    $ baseReq
                testData = encode . TeamList <$> teamListList

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    paginationApp testData req respond
                ) $ do
                res1 <- httpJSON req
                getResponseBody res1 `shouldBe` TeamList (teamListList !! 0)

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/teams"

                let path = getNextUrl res1
                path `shouldBe` (Just $ "http://127.0.0.1:" <> listenPortBS <> "/1")

                req2 <- parseRequest $ "GET " <> C8.unpack (fromJust path)
                res2 <- httpJSON req2
                getResponseBody res2 `shouldBe` TeamList (teamListList !! 1)

    describe "People" $ do
        let personJson1 = "{\
                          \  \"id\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                          \  \"emails\" : [ \"johnny.chang@foomail.com\", \"jchang@barmail.com\" ],\
                          \  \"displayName\" : \"John Andersen\",\
                          \  \"firstName\" : \"John\",\
                          \  \"lastName\" : \"Andersen\",\
                          \  \"avatar\" : \"https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600\",\
                          \  \"orgId\" : \"Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE\",\
                          \  \"roles\" : [ \"Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy\", \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX\" ],\
                          \  \"licenses\" : [ \"Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy\", \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX\" ],\
                          \  \"created\" : \"2015-10-18T14:26:16.000Z\",\
                          \  \"timezone\" : \"America/Denver\",\
                          \  \"lastActivity\" : \"2015-10-18T14:26:16.028Z\",\
                          \  \"status\" : \"active\",\
                          \  \"invitePending\" : false,\
                          \  \"loginEnabled\" : true\
                          \}"
            person1 = Person { personId            = PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                             , personErrors        = Nothing
                             , personEmails        = Just [Email "johnny.chang@foomail.com", Email "jchang@barmail.com"]
                             , personDisplayName   = Just $ DisplayName "John Andersen"
                             , personNickName      = Nothing
                             , personFirstName     = Just $ FirstName "John"
                             , personLastName      = Just $ LastName "Andersen"
                             , personAvatar        = Just $ AvatarUrl "https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600"
                             , personOrgId         = Just $ OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                             , personRoles         = Just [ RoleId "Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy"
                                                          , RoleId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"]
                             , personLicenses      = Just [ LicenseId "Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy"
                                                          , LicenseId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"]
                             , personCreated       = Just $ Timestamp "2015-10-18T14:26:16.000Z"
                             , personTimezone      = Just $ Timezone "America/Denver"
                             , personLastActivity  = Just $ Timestamp "2015-10-18T14:26:16.028Z"
                             , personStatus        = Just PersonStatusActive
                             , personInvitePending = Just False
                             , personLoginEnabled  = Just True
                             , personType          = Nothing
                             }
            personGen i = Person { personId            = PersonId . pack $ "PersonId" <> i
                                 , personErrors        = Nothing
                                 , personEmails        = Just [Email . pack $ "email" <> i <> "@foomail.com", Email . pack $ "email" <> i <> "@barmail.com"]
                                 , personDisplayName   = Just . DisplayName . pack $ "John Andersen" <> i
                                 , personNickName      = Nothing
                                 , personFirstName     = Just $ FirstName . pack $ "John" <> i
                                 , personLastName      = Just $ LastName . pack $ "Andersen" <> i
                                 , personAvatar        = Just $ AvatarUrl . pack $ "https://AvatarUrl" <> i
                                 , personOrgId         = Just . OrganizationId . pack $ "OrganizationId" <> i
                                 , personRoles         = Just [ RoleId . pack $ "RoleIdA" <> i
                                                              , RoleId . pack $ "RoleIdB" <> i]
                                 , personLicenses      = Just [ LicenseId . pack $ "LicenseIdX" <> i
                                                              , LicenseId . pack $ "LicenseIdY" <> i]
                                 , personCreated       = Just . Timestamp . pack $ "Created" <> i
                                 , personTimezone      = Just $ Timezone . pack $ "Timezone" <> i
                                 , personLastActivity  = Just $ Timestamp . pack $ "LastActivity" <> i
                                 , personStatus        = Just PersonStatusActive
                                 , personInvitePending = Just False
                                 , personLoginEnabled  = Just True
                                 , personType          = Nothing
                                 }
            personList j = [ personGen $ j <> show i | i <- [1..3] ]
            personListList = [ personList [c] | c <- ['a'..'d'] ]
            newPerson = CreatePerson { createPersonEmails       = Just $ [Email "johnny.chang@foomail.com", Email "jchang@barmail.com"]
                                     , createPersonDisplayName  = Just $ DisplayName "John Andersen"
                                     , createPersonFirstName    = Just $ FirstName "John"
                                     , createPersonLastName     = Just $ LastName "Andersen"
                                     , createPersonAvatar       = Just $ AvatarUrl "https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600"
                                     , createPersonOrgId        = Just $ OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                                     , createPersonRoles        = Just [ RoleId "Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy"
                                                                       , RoleId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"]
                                     , createPersonLicenses     = Just [ LicenseId "Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy"
                                                                       , LicenseId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"]
                                     }
            updatePerson = UpdatePerson { updatePersonDisplayName   = Just $ DisplayName "New John Andersen"
                                        , updatePersonFirstName     = Just $ FirstName "New John"
                                        , updatePersonLastName      = Just $ LastName "New Andersen"
                                        , updatePersonAvatar        = Just $ AvatarUrl "https://example.com/newAvatarUrl"
                                        , updatePersonOrgId         = Just $ OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                                        , updatePersonRoles         = Just $ [ RoleId "newRoleId1", RoleId "newRoleId2" ]
                                        , updatePersonLicenses      = Just $ [ LicenseId "newLicenseId1", LicenseId "newLicenseId2" ]
                                        }

        it "getPersonList returns ListReader of Person" $ do
            let testData = personList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (PersonList testData)) req respond
                ) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest (def :: PersonFilter) >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/people"
                queryString receivedReq `shouldBe` []

        it "getPersonList passes query strings build from PersonFilter to server" $ do
            let testData = personList ['Z']
                personFilter = PersonFilter (Just $ Email "person@filter.com")
                                            (Just $ DisplayName "DisplayNameFilter")
                                            (Just $ OrganizationId "OrgIdFilter")

            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (PersonList testData)) req respond
                ) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest personFilter >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/people"
                (sort . queryString) receivedReq `shouldBe` sort [ ("orgId", Just "OrgIdFilter")
                                                                 , ("displayName", Just "DisplayNameFilter")
                                                                 , ("email", Just "person@filter.com") ]

        it "getPersonList returns ListReader of Person performing automatic pagination" $ do
            withMockServer (paginationApp $ encode . PersonList <$> personListList) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest (def :: PersonFilter) >>= readAllList
                res `shouldBe` concat personListList

        it "getPersonList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . PersonList <$> personListList) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest (def :: PersonFilter) >>= readAllList
                res `shouldBe` concat (take 2 personListList)

        it "getDetail for a person returns a Person" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp personJson1 req respond
                ) $ do
                resPerson <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (PersonId "testPersonId")
                resPerson `shouldBe` person1

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "GET"
                rawPathInfo receivedReq `shouldBe` "/v1/people/testPersonId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

        it "getDetailEither for a person returns a (Right Person)" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp personJson1 req respond
                ) $ do
                (Right resPerson) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (PersonId "testPersonId")
                resPerson `shouldBe` person1

        it "createEntity for a person sends JSON encoded CreatePerson as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp personJson1 req respond
                ) $ do
                resPerson <- getResponseBody <$> createEntity dummyAuth mockBaseRequest newPerson
                resPerson `shouldBe` person1

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "POST"
                rawPathInfo receivedReq `shouldBe` "/v1/people"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just newPerson

        it "createEntityEither for a person sends JSON encoded CreatePerson as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp personJson1 req respond
                ) $ do
                (Right resPerson) <- getResponseBody <$> createEntityEither dummyAuth mockBaseRequest newPerson
                resPerson `shouldBe` person1

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "POST"
                rawPathInfo receivedReq `shouldBe` "/v1/people"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just newPerson

        it "updateEntity for a person sends JSON encoded UpdatePerson as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp personJson1 req respond
                ) $ do
                resPerson <- getResponseBody <$> updateEntity dummyAuth mockBaseRequest updatePerson
                resPerson `shouldBe` person1

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "PUT"
                rawPathInfo receivedReq `shouldBe` "/v1/people"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just updatePerson

        it "updateEntityEither for a person sends JSON encoded UpdatePerson as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp personJson1 req respond
                ) $ do
                (Right resPerson) <- getResponseBody <$> updateEntityEither dummyAuth mockBaseRequest updatePerson
                resPerson `shouldBe` person1

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "PUT"
                rawPathInfo receivedReq `shouldBe` "/v1/people"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just updatePerson

    describe "Team" $ do
        let teamJson = "{\
                       \  \"id\" : \"Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5\",\
                       \  \"name\" : \"Build Squad\",\
                       \  \"creatorId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                       \  \"created\" : \"2015-10-18T14:26:16+00:00\"\
                       \}"
            team = Team { teamId        = TeamId "Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5"
                        , teamErrors    = Nothing
                        , teamName      = Just $ TeamName "Build Squad"
                        , teamCreatorId = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                        , teamCreated   = Just $ Timestamp "2015-10-18T14:26:16+00:00"
                        }
            newTeam = CreateTeam $ TeamName "Build Squad"
            updateTeam = UpdateTeam $ TeamName "updatedTeamName"

        it "getTeamList returns ListReader of Team" $ do
            let testData = teamList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (TeamList testData)) req respond
                ) $ do
                res <- getTeamList dummyAuth mockBaseRequest >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/teams"
                queryString receivedReq `shouldBe` []

        it "getTeamList returns ListReader of Team performing automatic pagination" $ do
            withMockServer (paginationApp $ encode . TeamList <$> teamListList) $ do
                res <- getTeamList dummyAuth mockBaseRequest >>= readAllList
                res `shouldBe` concat teamListList

        it "getTeamList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . TeamList <$> teamListList) $ do
                res <- getTeamList dummyAuth mockBaseRequest >>= readAllList
                res `shouldBe` concat (take 2 teamListList)

        it "getDetail for a team returns a Team" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp teamJson req respond
                ) $ do
                    resTeam <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (TeamId "testTeamId")
                    resTeam `shouldBe` team

                    receivedReq <- takeMVar receivedReqMVar
                    requestMethod receivedReq `shouldBe` "GET"
                    rawPathInfo receivedReq `shouldBe` "/v1/teams/testTeamId"
                    (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                    (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

        it "getDetailEither for a team returns a (Right Team)" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp teamJson req respond
                ) $ do
                (Right resTeam) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (TeamId "testTeamId")
                resTeam `shouldBe` team

        it "createEntity for a team sends JSON encoded CreateTeam as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp teamJson req respond
                ) $ do
                resTeam <- getResponseBody <$> createEntity dummyAuth mockBaseRequest newTeam
                resTeam `shouldBe` team

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "POST"
                rawPathInfo receivedReq `shouldBe` "/v1/teams"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just newTeam

        it "createEntityEither for a team sends JSON encoded CreateTeam as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp teamJson req respond
                ) $ do
                (Right resTeam) <- getResponseBody <$> createEntityEither dummyAuth mockBaseRequest newTeam
                resTeam `shouldBe` team

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "POST"
                rawPathInfo receivedReq `shouldBe` "/v1/teams"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just newTeam

        it "updateEntity for a team sends JSON encoded UpdateTeam as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp teamJson req respond
                ) $ do
                resTeam <- getResponseBody <$> updateEntity dummyAuth mockBaseRequest updateTeam
                resTeam `shouldBe` team

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "PUT"
                rawPathInfo receivedReq `shouldBe` "/v1/teams"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just updateTeam

        it "updateEntityEither for a team sends JSON encoded UpdateTeam as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp teamJson req respond
                ) $ do
                (Right resTeam) <- getResponseBody <$> updateEntityEither dummyAuth mockBaseRequest updateTeam
                resTeam `shouldBe` team

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "PUT"
                rawPathInfo receivedReq `shouldBe` "/v1/teams"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just updateTeam

        it "deleteTeam sends DELETE request with teamId on URL path" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp "" req respond
                ) $ do
                deleteTeam dummyAuth mockBaseRequest (TeamId "testTeamId")

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "DELETE"
                rawPathInfo receivedReq `shouldBe` "/v1/teams/testTeamId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

    describe "TeamMemberShip" $ do
        let teamMembershipJson = "{\
                                 \  \"id\" : \"Y2lzY29zcGFyazovL3VzL1RFQU1fTUVNQkVSU0hJUC8wZmNmYTJiOC1hZGNjLTQ1ZWEtYTc4Mi1lNDYwNTkyZjgxZWY6MTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5\",\
                                 \  \"teamId\" : \"Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5\",\
                                 \  \"personId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                                 \  \"personEmail\" : \"john.andersen@example.com\",\
                                 \  \"personDisplayName\" : \"John Andersen\",\
                                 \  \"personOrgId\" : \"Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE\",\
                                 \  \"isModerator\" : true,\
                                 \  \"created\" : \"2015-10-18T14:26:16.057Z\"\
                                 \}"
            teamMembership = TeamMembership { teamMembershipId                  = TeamMembershipId "Y2lzY29zcGFyazovL3VzL1RFQU1fTUVNQkVSU0hJUC8wZmNmYTJiOC1hZGNjLTQ1ZWEtYTc4Mi1lNDYwNTkyZjgxZWY6MTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5"
                                            , teamMembershipErrors              = Nothing
                                            , teamMembershipTeamId              = Just $ TeamId "Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5"
                                            , teamMembershipPersonId            = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                                            , teamMembershipPersonEmail         = Just $ Email "john.andersen@example.com"
                                            , teamMembershipPersonDisplayName   = Just $ DisplayName "John Andersen"
                                            , teamMembershipPersonOrgId         = Just $ OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                                            , teamMembershipIsModerator         = Just True
                                            , teamMembershipCreated             = Just $ Timestamp "2015-10-18T14:26:16.057Z"
                                            }
            teamMembershipGen i = TeamMembership { teamMembershipId                  = TeamMembershipId . pack $ "teamMembershipId" <> i
                                                 , teamMembershipErrors              = Nothing
                                                 , teamMembershipTeamId              = Just . TeamId . pack $ "teamId" <> i
                                                 , teamMembershipPersonId            = Just . PersonId . pack $ "personId" <> i
                                                 , teamMembershipPersonEmail         = Just . Email . pack $ "email" <> i <> "@example.com"
                                                 , teamMembershipPersonDisplayName   = Just . DisplayName . pack $ "DisplayName" <> i
                                                 , teamMembershipPersonOrgId         = Just . OrganizationId . pack $ "OrganizationId" <> i
                                                 , teamMembershipIsModerator         = Just True
                                                 , teamMembershipCreated             = Just . Timestamp . pack $ "Timestamp" <> i
                                                 }
            teamMembershipList j = [ teamMembershipGen $ j <> show i | i <- [1..3] ]
            teamMembershipListList = [ teamMembershipList [c] | c <- ['a'..'d'] ]
            defFilter = defaultTeamMembershipFilter $ TeamId "dummyTeamId"
            newTeamMembership = CreateTeamMembership { createTeamMembershipTeamId       = TeamId "targetTeam"
                                                     , createTeamMembershipPersonId     = Just $ PersonId "addedPerson"
                                                     , createTeamMembershipPersonEmail  = Just $ Email "added@example.com"
                                                     , createTeamMembershipIsModerator  = Just True
                                                     }
            updateTeamMembership = UpdateTeamMembership False

        it "getTeamMembershipList returns ListReader of TeamMembership" $ do
            let testData = teamMembershipList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (TeamMembershipList testData)) req respond
                ) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest defFilter >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
                queryString receivedReq `shouldBe` [ ("teamId", Just "dummyTeamId") ]

        it "getTeamMembershipList passes query strings build from TeamMembershipFilter to server" $ do
            let testData = teamMembershipList ['Z']
                teamMembershipFilter = TeamMembershipFilter $ TeamId "DummyTeamId"

            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (TeamMembershipList testData)) req respond
                ) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest teamMembershipFilter >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
                queryString receivedReq `shouldBe` [ ("teamId", Just "DummyTeamId") ]

        it "getTeamMembershipList returns ListReader of TeamMembership performing automatic pagination" $ do
            withMockServer (paginationApp $ encode . TeamMembershipList <$> teamMembershipListList) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest defFilter >>= readAllList
                res `shouldBe` concat teamMembershipListList

        it "getTeamMembershipList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . TeamMembershipList <$> teamMembershipListList) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest defFilter >>= readAllList
                res `shouldBe` concat (take 2 teamMembershipListList)

        it "getDetail for a team membership returns a TeamMembership" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp teamMembershipJson req respond
                ) $ do
                resTeamMembership <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (TeamMembershipId "testTeamMembershipId")
                resTeamMembership `shouldBe` teamMembership

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "GET"
                rawPathInfo receivedReq `shouldBe` "/v1/team/memberships/testTeamMembershipId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

        it "getDetailEither for a team membership  returns a (Right TeamMembership)" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp teamMembershipJson req respond
                ) $ do
                (Right resTeamMembership) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (TeamMembershipId "testTeamMembershipId")
                resTeamMembership `shouldBe` teamMembership

        it "createEntity for a team membership sends JSON encoded CreateTeamMembership as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp teamMembershipJson req respond
                ) $ do
                resTeamMembership <- getResponseBody <$> createEntity dummyAuth mockBaseRequest newTeamMembership
                resTeamMembership `shouldBe` teamMembership

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "POST"
                rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just newTeamMembership

        it "createEntityEither for a team membership sends JSON encoded CreateTeamMembership as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp teamMembershipJson req respond
                ) $ do
                (Right resTeamMembership) <- getResponseBody <$> createEntityEither dummyAuth mockBaseRequest newTeamMembership
                resTeamMembership `shouldBe` teamMembership

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "POST"
                rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just newTeamMembership

        it "updateEntity for a teamMembership sends JSON encoded UpdateTeamMembership as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp teamMembershipJson req respond
                ) $ do
                resTeamMembership <- getResponseBody <$> updateEntity dummyAuth mockBaseRequest updateTeamMembership
                resTeamMembership `shouldBe` teamMembership

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "PUT"
                rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just updateTeamMembership

        it "updateEntityEither for a teamMembership sends JSON encoded UpdateTeamMembership as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp teamMembershipJson req respond
                ) $ do
                (Right resTeamMembership) <- getResponseBody <$> updateEntityEither dummyAuth mockBaseRequest updateTeamMembership
                resTeamMembership `shouldBe` teamMembership

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "PUT"
                rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just updateTeamMembership

        it "deleteTeamMembership sends DELETE request with teamMembershipId on URL path" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp "" req respond
                ) $ do
                deleteTeamMembership dummyAuth mockBaseRequest (TeamMembershipId "testTeamMembershipId")

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "DELETE"
                rawPathInfo receivedReq `shouldBe` "/v1/team/memberships/testTeamMembershipId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

    describe "Room" $ do
        let roomJson = "{\
                       \  \"id\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0\",\
                       \  \"title\" : \"Project Unicorn - Sprint 0\",\
                       \  \"type\" : \"group\",\
                       \  \"isLocked\" : true,\
                       \  \"sipAddress\" : \"01234567890@meet.ciscospark.com\",\
                       \  \"teamId\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vNjRlNDVhZTAtYzQ2Yi0xMWU1LTlkZjktMGQ0MWUzNDIxOTcz\",\
                       \  \"lastActivity\" : \"2016-04-21T19:12:48.920Z\",\
                       \  \"creatorId\": \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                       \  \"created\" : \"2016-04-21T19:01:55.966Z\"\
                       \}"
            room = Room { roomId            = RoomId  "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                        , roomErrors        = Nothing
                        , roomTitle         = Just $ RoomTitle "Project Unicorn - Sprint 0"
                        , roomType          = Just RoomTypeGroup
                        , roomIsLocked      = Just True
                        , roomSipAddress    = Just $ SipAddr "01234567890@meet.ciscospark.com"
                        , roomLastActivity  = Just $ Timestamp "2016-04-21T19:12:48.920Z"
                        , roomTeamId        = Just $ TeamId "Y2lzY29zcGFyazovL3VzL1JPT00vNjRlNDVhZTAtYzQ2Yi0xMWU1LTlkZjktMGQ0MWUzNDIxOTcz"
                        , roomCreatorId     = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                        , roomCreated       = Just $ Timestamp "2016-04-21T19:01:55.966Z"
                        }
            roomGen i = Room { roomId           = RoomId . pack $ "roomId" <> i
                             , roomErrors       = Nothing
                             , roomTitle        = Just . RoomTitle . pack $ "roomTitle" <> i
                             , roomType         = Just RoomTypeGroup
                             , roomIsLocked     = Just True
                             , roomSipAddress   = Just $ SipAddr . pack $ "rooomSipAddress" <> i <> "@meet.ciscospark.com"
                             , roomLastActivity = Just . Timestamp . pack $ "roomLastActivity" <> i
                             , roomTeamId       = Just $ TeamId . pack $ "roomTeamId" <> i
                             , roomCreatorId    = Just . PersonId . pack $ "personId" <> i
                             , roomCreated      = Just .Timestamp .pack $ "roomCreated" <> i
                             }
            roomList j = [ roomGen $ j <> show i | i <- [1..3] ]
            roomListList = [ roomList [c] | c <- ['a'..'d'] ]
            newRoom = CreateRoom (RoomTitle "New Room") (Just $ TeamId "belongingTeam")
            updateRoom = UpdateRoom $ RoomTitle "updatedRoomTitle"

        it "getRoomList returns ListReader of Room" $ do
            let testData = roomList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (RoomList testData)) req respond
                ) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest (def :: RoomFilter) >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/rooms"
                queryString receivedReq `shouldBe` []

        it "getRoomList passes query strings build from RoomFilter to server" $ do
            let testData = roomList ['Z']
                roomFilter = RoomFilter (Just $ TeamId "dummyTeamId")
                                        (Just RoomTypeGroup)
                                        (Just RoomFilterSortByLastActivity)

            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (RoomList testData)) req respond
                ) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest roomFilter >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/rooms"
                (sort . queryString) receivedReq `shouldBe` sort [ ("type", Just "group")
                                                                 , ("sortBy", Just "lastactivity")
                                                                 , ("teamId", Just "dummyTeamId") ]

        it "getRoomList returns ListReader of Room performing automatic pagination" $ do
            withMockServer (paginationApp $ encode . RoomList <$> roomListList) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest (def :: RoomFilter) >>= readAllList
                res `shouldBe` concat roomListList

        it "getRoomList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . RoomList <$> roomListList) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest (def :: RoomFilter) >>= readAllList
                res `shouldBe` concat (take 2 roomListList)

        it "getDetail for a room returns a Room" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp roomJson req respond
                ) $ do
                resRoom <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (RoomId "testRoomId")
                resRoom `shouldBe` room

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "GET"
                rawPathInfo receivedReq `shouldBe` "/v1/rooms/testRoomId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

        it "getDetailEither for a room returns a (Right Room)" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp roomJson req respond
                ) $ do
                (Right resRoom) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (RoomId "testRoomId")
                resRoom `shouldBe` room

        it "createEntity for a room sends JSON encoded CreateRoom as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp roomJson req respond
                ) $ do
                resRoom <- getResponseBody <$> createEntity dummyAuth mockBaseRequest newRoom
                resRoom `shouldBe` room

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "POST"
                rawPathInfo receivedReq `shouldBe` "/v1/rooms"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just newRoom

        it "createEntithEither for a room sends JSON encoded CreateRoom as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp roomJson req respond
                ) $ do
                (Right resRoom) <- getResponseBody <$> createEntityEither dummyAuth mockBaseRequest newRoom
                resRoom `shouldBe` room

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "POST"
                rawPathInfo receivedReq `shouldBe` "/v1/rooms"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just newRoom

        it "updateEntity for a room sends JSON encoded UpdateRoom as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp roomJson req respond
                ) $ do
                resRoom <- getResponseBody <$> updateEntity dummyAuth mockBaseRequest updateRoom
                resRoom `shouldBe` room

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "PUT"
                rawPathInfo receivedReq `shouldBe` "/v1/rooms"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just updateRoom

        it "updateEntityEither for a room sends JSON encoded UpdateRoom as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp roomJson req respond
                ) $ do
                (Right resRoom) <- getResponseBody <$> updateEntityEither dummyAuth mockBaseRequest updateRoom
                resRoom `shouldBe` room

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "PUT"
                rawPathInfo receivedReq `shouldBe` "/v1/rooms"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just updateRoom

        it "deleteRoom sends DELETE request with roomId on URL path" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp "" req respond
                ) $ do
                deleteRoom dummyAuth mockBaseRequest (RoomId "testRoomId")

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "DELETE"
                rawPathInfo receivedReq `shouldBe` "/v1/rooms/testRoomId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

    describe "Membership" $ do
        let membershipJson = "{\
                             \  \"id\" : \"Y2lzY29zcGFyazovL3VzL01FTUJFUlNISVAvMGQwYzkxYjYtY2U2MC00NzI1LWI2ZDAtMzQ1NWQ1ZDExZWYzOmNkZTFkZDQwLTJmMGQtMTFlNS1iYTljLTdiNjU1NmQyMjA3Yg\",\
                             \  \"roomId\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0\",\
                             \  \"personId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                             \  \"personEmail\" : \"john.andersen@example.com\",\
                             \  \"personDisplayName\" : \"John Andersen\",\
                             \  \"personOrgId\" : \"Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE\",\
                             \  \"isModerator\" : true,\
                             \  \"isMonitor\" : true,\
                             \  \"created\" : \"2015-10-18T14:26:16.203Z\"\
                             \}"
            membership = Membership { membershipId                  = MembershipId "Y2lzY29zcGFyazovL3VzL01FTUJFUlNISVAvMGQwYzkxYjYtY2U2MC00NzI1LWI2ZDAtMzQ1NWQ1ZDExZWYzOmNkZTFkZDQwLTJmMGQtMTFlNS1iYTljLTdiNjU1NmQyMjA3Yg"
                                    , membershipErrors              = Nothing
                                    , membershipRoomId              = Just $ RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                                    , membershipPersonId            = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                                    , membershipPersonEmail         = Just $ Email "john.andersen@example.com"
                                    , membershipPersonDisplayName   = Just $ DisplayName "John Andersen"
                                    , membershipPersonOrgId         = Just $ OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                                    , membershipIsModerator         = Just True
                                    , membershipIsMonitor           = Just True
                                    , membershipCreated             = Just $ Timestamp "2015-10-18T14:26:16.203Z"
                                    }
            membershipGen i = Membership { membershipId                 = MembershipId . pack $ "membershipId" <> i
                                         , membershipErrors             = Nothing
                                         , membershipRoomId             = Just . RoomId . pack $ "roomId" <> i
                                         , membershipPersonId           = Just . PersonId . pack $ "personId" <> i
                                         , membershipPersonEmail        = Just . Email . pack $ "email" <> i <> "@example.com"
                                         , membershipPersonDisplayName  = Just . DisplayName . pack $ "displayName" <> i
                                         , membershipPersonOrgId        = Just . OrganizationId . pack $ "orgId" <> i
                                         , membershipIsModerator        = Just True
                                         , membershipIsMonitor          = Just True
                                         , membershipCreated            = Just . Timestamp . pack $ "timestamp" <> i
                                         }
            membershipList j = [ membershipGen $ j <> show i | i <- [1..3] ]
            membershipListList = [ membershipList [c] | c <- ['a'..'d'] ]
            newMembership = CreateMembership { createMembershipRoomId       = RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                                             , createMembershipPersonId     = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                                             , createMembershipPersonEmail  = Just $ Email "john.andersen@example.com"
                                             , createMembershipIsModerator  = Just $ True
                                             }
            updateMembership = UpdateMembership False

        it "getMembershipList returns ListReader of Membership" $ do
            let testData = membershipList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (MembershipList testData)) req respond
                ) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest (def :: MembershipFilter) >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/memberships"
                queryString receivedReq `shouldBe` []

        it "getMembershipList passes query strings build from MembershipFilter to server" $ do
            let testData = membershipList ['Z']
                membershipFilter = MembershipFilter (Just $ RoomId "dummyRoomId")
                                                    (Just $ PersonId "personIdFilter")
                                                    (Just $ Email "personEmailFilter")

            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (MembershipList testData)) req respond
                ) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest membershipFilter >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/memberships"
                (sort . queryString) receivedReq `shouldBe` sort [ ("personId", Just "personIdFilter")
                                                                 , ("personEmail", Just "personEmailFilter")
                                                                 , ("roomId", Just "dummyRoomId") ]

        it "getMembershipList returns ListReader of Membership performing automatic pagination" $ do
            withMockServer (paginationApp $ encode . MembershipList <$> membershipListList) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest (def :: MembershipFilter) >>= readAllList
                res `shouldBe` concat membershipListList

        it "getMembershipList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . MembershipList <$> membershipListList) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest (def :: MembershipFilter) >>= readAllList
                res `shouldBe` concat (take 2 membershipListList)

        it "getDetail for a menbership returns a Membership" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                        putMVar receivedReqMVar req
                        simpleApp membershipJson req respond
                    ) $ do
                resMembership <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (MembershipId "testMembershipId")
                resMembership `shouldBe` membership

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "GET"
                rawPathInfo receivedReq `shouldBe` "/v1/memberships/testMembershipId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

        it "getDetailEither for a membership returns a (Right Membership)" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp membershipJson req respond
                ) $ do
                (Right resMembership) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (MembershipId "testMembershipId")
                resMembership `shouldBe` membership

        it "createEntity for a membership sends JSON encoded CreateMembership as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp membershipJson req respond
                ) $ do
                resMembership <- getResponseBody <$> createEntity dummyAuth mockBaseRequest newMembership
                resMembership `shouldBe` membership

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "POST"
                rawPathInfo receivedReq `shouldBe` "/v1/memberships"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just newMembership

        it "createEntithEither for a membership sends JSON encoded CreateMembership as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp membershipJson req respond
                ) $ do
                (Right resMembership) <- getResponseBody <$> createEntityEither dummyAuth mockBaseRequest newMembership
                resMembership `shouldBe` membership

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "POST"
                rawPathInfo receivedReq `shouldBe` "/v1/memberships"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just newMembership

        it "updateEntity for a membership sends JSON encoded UpdateMembership as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp membershipJson req respond
                ) $ do
                resMembership <- getResponseBody <$> updateEntity dummyAuth mockBaseRequest updateMembership
                resMembership `shouldBe` membership

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "PUT"
                rawPathInfo receivedReq `shouldBe` "/v1/memberships"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just updateMembership

        it "updateEntityEither for a membership sends JSON encoded UpdateMembership as its body of PUT request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp membershipJson req respond
                ) $ do
                (Right resMembership) <- getResponseBody <$> updateEntityEither dummyAuth mockBaseRequest updateMembership
                resMembership `shouldBe` membership

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "PUT"
                rawPathInfo receivedReq `shouldBe` "/v1/memberships"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just updateMembership

        it "deleteMembership sends DELETE request with membershipId on URL path" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp "" req respond
                ) $ do
                deleteMembership dummyAuth mockBaseRequest (MembershipId "testMembershipId")

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "DELETE"
                rawPathInfo receivedReq `shouldBe` "/v1/memberships/testMembershipId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

    describe "Message" $ do
        let messageJson = "{\
                          \  \"id\" : \"Y2lzY29zcGFyazovL3VzL01FU1NBR0UvOTJkYjNiZTAtNDNiZC0xMWU2LThhZTktZGQ1YjNkZmM1NjVk\",\
                          \  \"roomId\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0\",\
                          \  \"roomType\" : \"group\",\
                          \  \"toPersonId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX\",\
                          \  \"toPersonEmail\" : \"julie@example.com\",\
                          \  \"text\" : \"PROJECT UPDATE - A new project plan has been published on Box: http://box.com/s/lf5vj. The PM for this project is Mike C. and the Engineering Manager is Jane W.\",\
                          \  \"html\" : \"<h1>HTML formatted message goes here</h1><p>when the message was posted in markdown format.</p>\",\
                          \  \"files\" : [ \"http://www.example.com/images/media.png\" ],\
                          \  \"personId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                          \  \"personEmail\" : \"matt@example.com\",\
                          \  \"created\" : \"2015-10-18T14:26:16+00:00\",\
                          \  \"mentionedPeople\" : [ \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS8yNDlmNzRkOS1kYjhhLTQzY2EtODk2Yi04NzllZDI0MGFjNTM\", \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS83YWYyZjcyYy0xZDk1LTQxZjAtYTcxNi00MjlmZmNmYmM0ZDg\" ]\
                          \}"
            message = Message { messageId               = MessageId "Y2lzY29zcGFyazovL3VzL01FU1NBR0UvOTJkYjNiZTAtNDNiZC0xMWU2LThhZTktZGQ1YjNkZmM1NjVk"
                              , messageErrors           = Nothing
                              , messageRoomId           = Just $ RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                              , messageRoomType         = Just $ RoomTypeGroup
                              , messageToPersonId       = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"
                              , messageToPersonEmail    = Just $ Email "julie@example.com"
                              , messageText             = Just $ MessageText "PROJECT UPDATE - A new project plan has been published on Box: http://box.com/s/lf5vj. The PM for this project is Mike C. and the Engineering Manager is Jane W."
                              , messageHtml             = Just $ MessageHtml "<h1>HTML formatted message goes here</h1><p>when the message was posted in markdown format.</p>"
                              , messageFiles            = Just [ FileUrl "http://www.example.com/images/media.png" ]
                              , messagePersonId         = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                              , messagePersonEmail      = Just $ Email "matt@example.com"
                              , messageCreated          = Just $ Timestamp "2015-10-18T14:26:16+00:00"
                              , messageMentionedPeople  = Just [ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS8yNDlmNzRkOS1kYjhhLTQzY2EtODk2Yi04NzllZDI0MGFjNTM"
                                                               , PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS83YWYyZjcyYy0xZDk1LTQxZjAtYTcxNi00MjlmZmNmYmM0ZDg" ]
                              }
            messageGen i = Message { messageId               = MessageId . pack $ "messageId" <> i
                                   , messageErrors           = Nothing
                                   , messageRoomId           = Just . RoomId . pack $ "roomId" <> i
                                   , messageRoomType         = Just RoomTypeGroup
                                   , messageToPersonId       = Just . PersonId . pack $ "toPersonId" <> i
                                   , messageToPersonEmail    = Just . Email . pack $ "julie" <> i <> "@example.com"
                                   , messageText             = Just . MessageText . pack $ "messageText" <> i
                                   , messageHtml             = Just . MessageHtml . pack $ "messageHtml" <> i
                                   , messageFiles            = Just [ FileUrl . pack $ "http://www.example.com/images/media" <> i <> ".png" ]
                                   , messagePersonId         = Just . PersonId . pack $ "personId" <> i
                                   , messagePersonEmail      = Just . Email . pack $ "matt" <> i <> "@example.com"
                                   , messageCreated          = Just . Timestamp . pack $ "created" <> i
                                   , messageMentionedPeople  = Just [ PersonId . pack $ "memtionedPeople1-" <> i
                                                                    , PersonId . pack $ "memtionedPeople2-" <> i ]
                                   }
            messageList j = [ messageGen $ j <> show i | i <- [1..3] ]
            messageListList = [ messageList [c] | c <- ['a'..'d'] ]
            defFilter = defaultMessageFilter $ RoomId "dummyRoomId"
            newMessage = CreateMessage { createMessageRoomId        = Just $ RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                                       , createMessageToPersonId    = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"
                                       , createMessageToPersonEmail = Just $ Email "julie@example.com"
                                       , createMessageText          = Just $ MessageText "PROJECT UPDATE - A new project plan has been published on Box: http://box.com/s/lf5vj. The PM for this project is Mike C. and the Engineering Manager is Jane W."
                                       , createMessageMarkdown      = Just $ MessageMarkdown "**PROJECT UPDATE** A new project plan has been published [on Box](http://box.com/s/lf5vj). The PM for this project is <@personEmail:mike@example.com> and the Engineering Manager is <@personEmail:jane@example.com>."
                                       , createMessageFiles         = Just $ [ FileUrl "http://www.example.com/images/media.png" ]
                                       }

        it "getMessageList returns ListReader of Message" $ do
            let testData = messageList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (MessageList testData)) req respond
                ) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest defFilter >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/messages"
                queryString receivedReq `shouldBe` [ ("roomId", Just "dummyRoomId") ]

        it "getMessageList passes query strings build from MessageFilter to server" $ do
            let testData = messageList ['Z']
                messageFilter = MessageFilter (RoomId "dummyRoomId")
                                              (Just $ MentionedPeopleMe)
                                              (Just $ Timestamp "beforeFilter")
                                              (Just $ MessageId "beforeMessageFilter")

            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (MessageList testData)) req respond
                ) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest messageFilter >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/messages"
                (sort . queryString) receivedReq `shouldBe` sort [ ("roomId", Just "dummyRoomId")
                                                                 , ("mentionedPeople", Just "me")
                                                                 , ("beforeMessage", Just "beforeMessageFilter")
                                                                 , ("before", Just "beforeFilter") ]

        it "getMessageList returns ListReader of Message with automatic pagination" $ do
            withMockServer (paginationApp $ encode . MessageList <$> messageListList) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest defFilter >>= readAllList
                res `shouldBe` concat messageListList

        it "getMessageList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . MessageList <$> messageListList) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest defFilter >>= readAllList
                res `shouldBe` concat (take 2 messageListList)

        it "getDetail for a message returns a Message" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp messageJson req respond
                ) $ do
                resMessage <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (MessageId "testMessageId")
                resMessage `shouldBe` message

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "GET"
                rawPathInfo receivedReq `shouldBe` "/v1/messages/testMessageId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

        it "getDetailEither for a message returns a (Right Message)" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp messageJson req respond
                ) $ do
                (Right resMessage) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (MessageId "testMessageId")
                resMessage `shouldBe` message

        it "createEntity for a message sends JSON encoded CreateMessage as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp messageJson req respond
                ) $ do
                resMessage <- getResponseBody <$> createEntity dummyAuth mockBaseRequest newMessage
                resMessage `shouldBe` message

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "POST"
                rawPathInfo receivedReq `shouldBe` "/v1/messages"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just newMessage

        it "createEntithEither for a message sends JSON encoded CreateMessage as its body of POST request" $ do
            receivedReqMVar <- newEmptyMVar
            receivedBodyMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    strictRequestBody req >>= putMVar receivedBodyMVar
                    simpleApp messageJson req respond
                ) $ do
                (Right resMessage) <- getResponseBody <$> createEntityEither dummyAuth mockBaseRequest newMessage
                resMessage `shouldBe` message

                receivedReq <- takeMVar receivedReqMVar
                receivedBody <- takeMVar receivedBodyMVar
                requestMethod receivedReq `shouldBe` "POST"
                rawPathInfo receivedReq `shouldBe` "/v1/messages"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"
                decode receivedBody `shouldBe` Just newMessage

        it "deleteMessage sends DELETE request with messageId on URL path" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp "" req respond
                ) $ do
                deleteMessage dummyAuth mockBaseRequest (MessageId "testMessageId")

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "DELETE"
                rawPathInfo receivedReq `shouldBe` "/v1/messages/testMessageId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

    describe "Organization" $ do
        let organizationJson = "{\
                               \  \"id\" : \"OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh\",\
                               \  \"displayName\" : \"Cisco, Inc.\",\
                               \  \"created\" : \"2015-10-18T14:26:16+00:00\"\
                               \}"
            organization = Organization { organizationId            = OrganizationId "OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh"
                                        , organizationErrors        = Nothing
                                        , organizationDisplayName   = Just $ OrganizationDisplayName "Cisco, Inc."
                                        , organizationCreated       = Just $ Timestamp "2015-10-18T14:26:16+00:00"
                                        }
            organizationGen i = Organization { organizationId            = OrganizationId . pack $ "organizationId" <> i
                                             , organizationErrors        = Nothing
                                             , organizationDisplayName   = Just $ OrganizationDisplayName . pack $ "displayName" <> i
                                             , organizationCreated       = Just $ Timestamp . pack $ "timestamp" <> i
                                             }
            organizationList j = [ organizationGen $ j <> show i | i <- [1..3] ]
            organizationListList = [ organizationList [c] | c <- ['a'..'d'] ]

        it "getOrganizationList returns ListReader of Organization" $ do
            let testData = organizationList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (OrganizationList testData)) req respond
                ) $ do
                res <- getOrganizationList dummyAuth mockBaseRequest >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/organizations"
                queryString receivedReq `shouldBe` []

        it "getOrganizationList returns ListReader of Organization performing automatic pagination" $ do
            withMockServer (paginationApp $ encode . OrganizationList <$> organizationListList) $ do
                res <- getOrganizationList dummyAuth mockBaseRequest >>= readAllList
                res `shouldBe` concat organizationListList

        it "getOrganizationList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . OrganizationList <$> organizationListList) $ do
                res <- getOrganizationList dummyAuth mockBaseRequest >>= readAllList
                res `shouldBe` concat (take 2 organizationListList)

        it "getDetail for an organization returns a Organization" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp organizationJson req respond
                ) $ do
                resOrganization <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (OrganizationId "testOrganizationId")
                resOrganization `shouldBe` organization

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "GET"
                rawPathInfo receivedReq `shouldBe` "/v1/organizations/testOrganizationId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

        it "getDetailEither for an organization returns a (Right Organization)" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp organizationJson req respond
                ) $ do
                (Right resOrganization) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (OrganizationId "testOrganizationId")
                resOrganization `shouldBe` organization

    describe "License" $ do
        let licenseJson = "{\
                          \  \"id\" : \"OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh\",\
                          \  \"name\" : \"Spark Calling\",\
                          \  \"totalUnits\" : 42,\
                          \  \"consumedUnits\" : 8\
                          \}"
            license = License { licenseId               = LicenseId "OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh"
                              , licenseErrors           = Nothing
                              , licenseName             = Just $ LicenseName "Spark Calling"
                              , licenseTotalUnits       = Just $ LicenseUnit 42
                              , licenseConsumedUnits    = Just $ LicenseUnit 8
                              }
            licenseGen i = License { licenseId               = LicenseId . pack $ "licenseId" <> i
                                   , licenseErrors           = Nothing
                                   , licenseName             = Just $ LicenseName . pack $ "licenseName" <> i
                                   , licenseTotalUnits       = Just $ LicenseUnit 42
                                   , licenseConsumedUnits    = Just $ LicenseUnit 8
                                   }
            licenseList j = [ licenseGen $ j <> show i | i <- [1..3] ]
            licenseListList = [ licenseList [c] | c <- ['a'..'d'] ]

        it "getLicenseList returns ListReader of License" $ do
            let testData = licenseList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (LicenseList testData)) req respond
                ) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest (def :: LicenseFilter) >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/licenses"
                queryString receivedReq `shouldBe` []

        it "getLicenseList passes query strings build from LicenseFilter to server" $ do
            let testData = licenseList ['Z']
                licenseFilter = LicenseFilter (Just $ OrganizationId "orgIdFilter")

            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (LicenseList testData)) req respond
                ) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest licenseFilter >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/licenses"
                queryString receivedReq `shouldBe` [ ("orgId", Just "orgIdFilter") ]

        it "getLicenseList returns ListReader of License with automatic pagination" $ do
            withMockServer (paginationApp $ encode . LicenseList <$> licenseListList) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest (def :: LicenseFilter) >>= readAllList
                res `shouldBe` concat licenseListList

        it "getLicenseList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . LicenseList <$> licenseListList) $ do
                res <- getListWithFilter dummyAuth mockBaseRequest (def :: LicenseFilter) >>= readAllList
                res `shouldBe` concat (take 2 licenseListList)

        it "getDetail for a license returns a License" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp licenseJson req respond
                ) $ do
                resLicense <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (LicenseId "testLicenseId")
                resLicense `shouldBe` license

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "GET"
                rawPathInfo receivedReq `shouldBe` "/v1/licenses/testLicenseId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

        it "getDetailEither for a license returns a (Right License)" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp licenseJson req respond
                ) $ do
                (Right resLicense) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (LicenseId "testLicenseId")
                resLicense `shouldBe` license

    describe "Role" $ do
        let roleJson = "{\
                       \  \"id\" : \"OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh\",\
                       \  \"name\" : \"Full Administrator\"\
                       \}"
            role = Role { roleId        = RoleId "OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh"
                        , roleErrors    = Nothing
                        , roleName      = Just $ RoleName "Full Administrator"
                        }
            roleGen i = Role { roleId       = RoleId . pack $ "roleId" <> i
                             , roleErrors   = Nothing
                             , roleName     = Just . RoleName . pack $ "roleName" <> i
                             }
            roleList j = [ roleGen $ j <> show i | i <- [1..3] ]
            roleListList = [ roleList [c] | c <- ['a'..'d'] ]

        it "getRoleList returns ListReader of Role" $ do
            let testData = roleList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (RoleList testData)) req respond
                ) $ do
                res <- getRoleList dummyAuth mockBaseRequest >>= readAllList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/roles"
                queryString receivedReq `shouldBe` []

        it "getRoleList returns ListReader of Role performing automatic pagination" $ do
            withMockServer (paginationApp $ encode . RoleList <$> roleListList) $ do
                res <- getRoleList dummyAuth mockBaseRequest >>= readAllList
                res `shouldBe` concat roleListList

        it "getRoleList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . RoleList <$> roleListList) $ do
                res <- getRoleList dummyAuth mockBaseRequest >>= readAllList
                res `shouldBe` concat (take 2 roleListList)

        it "getDetail for a role returns a Role" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp roleJson req respond
                ) $ do
                resRole <- getResponseBody <$> getDetail dummyAuth mockBaseRequest (RoleId "testRoleId")
                resRole `shouldBe` role

                receivedReq <- takeMVar receivedReqMVar
                requestMethod receivedReq `shouldBe` "GET"
                rawPathInfo receivedReq `shouldBe` "/v1/roles/testRoleId"
                (lookup "Authorization" . requestHeaders) receivedReq `shouldBe` Just "Bearer dummyAuth"
                (lookup "Content-Type" . requestHeaders) receivedReq `shouldBe` Just "application/json; charset=utf-8"

        it "getDetailEither for a role returns a (Right Role)" $ do
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp roleJson req respond
                ) $ do
                (Right resRole) <- getResponseBody <$> getDetailEither dummyAuth mockBaseRequest (RoleId "testRoleId")
                resRole `shouldBe` role
