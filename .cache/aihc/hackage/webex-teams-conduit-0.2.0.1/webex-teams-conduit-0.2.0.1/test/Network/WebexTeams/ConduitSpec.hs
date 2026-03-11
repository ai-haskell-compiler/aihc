{-# LANGUAGE OverloadedStrings #-}

module Network.WebexTeams.ConduitSpec where

import           Conduit                     (runConduit, sinkList, (.|))
import           Control.Concurrent.Async    (withAsync)
import           Control.Concurrent.MVar     (MVar, newEmptyMVar, putMVar,
                                              takeMVar)
import           Control.Monad               (void)
import           Data.Aeson                  (encode)
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
                                              requestMethod, responseLBS)
import           Network.Wai.Handler.Warp    (defaultSettings, runSettings,
                                              setBeforeMainLoop, setPort)

import           Test.Hspec

import           Network.WebexTeams          hiding (streamOrganizationList,
                                              streamRoleList, streamTeamList)
import           Network.WebexTeams.Conduit
import           Network.WebexTeams.Internal (getNextUrl)


listenPort = 3001
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

        it "streamPersonList streams Person" $ do
            let testData = personList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (PersonList testData)) req respond
                ) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest (def :: PersonFilter) .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/people"
                queryString receivedReq `shouldBe` []

        it "streamPersonList passes query strings build from PersonFilter to server" $ do
            let testData = personList ['Z']
                personFilter = PersonFilter (Just $ Email "person@filter.com")
                                            (Just $ DisplayName "DisplayNameFilter")
                                            (Just $ OrganizationId "OrgIdFilter")

            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (PersonList testData)) req respond
                ) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest personFilter .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/people"
                (sort . queryString) receivedReq `shouldBe` sort [ ("orgId", Just "OrgIdFilter")
                                                                 , ("displayName", Just "DisplayNameFilter")
                                                                 , ("email", Just "person@filter.com") ]

        it "streamPersonList streams Person with automatic pagination" $ do
            withMockServer (paginationApp $ encode . PersonList <$> personListList) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest (def :: PersonFilter) .| sinkList
                res `shouldBe` concat personListList

        it "streamPersonList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . PersonList <$> personListList) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest (def :: PersonFilter) .| sinkList
                res `shouldBe` concat (take 2 personListList)

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

        it "streamTeamList streams Team" $ do
            let testData = teamList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (TeamList testData)) req respond
                ) $ do
                res <- runConduit $ streamTeamList dummyAuth mockBaseRequest .| sinkList
                res `shouldBe` testData
                path <- rawPathInfo <$> takeMVar receivedReqMVar
                path `shouldBe` "/v1/teams"

        it "streamTeamList streams Team with automatic pagination" $ do
            withMockServer (paginationApp $ encode . TeamList <$> teamListList) $ do
                res <- runConduit $ streamTeamList dummyAuth mockBaseRequest .| sinkList
                res `shouldBe` concat teamListList

        it "streamTeamList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . TeamList <$> teamListList) $ do
                res <- runConduit $ streamTeamList dummyAuth mockBaseRequest .| sinkList
                res `shouldBe` concat (take 2 teamListList)

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

        it "streamTeamMembershipList streams TeamMembership" $ do
            let testData = teamMembershipList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (TeamMembershipList testData)) req respond
                ) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest defFilter .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
                queryString receivedReq `shouldBe` [ ("teamId", Just "dummyTeamId") ]

        it "streamMembershipList passes query strings build from TeamMembershipFilter to server" $ do
            let testData = teamMembershipList ['Z']
                teamMembershipFilter = TeamMembershipFilter $ TeamId "DummyTeamId"

            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (TeamMembershipList testData)) req respond
                ) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest teamMembershipFilter .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/team/memberships"
                queryString receivedReq `shouldBe` [ ("teamId", Just "DummyTeamId") ]

        it "streamTeamMembershipList streams TeamMembership with automatic pagination" $ do
            withMockServer (paginationApp $ encode . TeamMembershipList <$> teamMembershipListList) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest defFilter .| sinkList
                res `shouldBe` concat teamMembershipListList

        it "streamTeamMembershipList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . TeamMembershipList <$> teamMembershipListList) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest defFilter .| sinkList
                res `shouldBe` concat (take 2 teamMembershipListList)

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

        it "streamRoomList streams Room" $ do
            let testData = roomList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (RoomList testData)) req respond
                ) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest (def :: RoomFilter) .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/rooms"
                queryString receivedReq `shouldBe` []

        it "streamRoomList passes query strings build from RoomFilter to server" $ do
            let testData = roomList ['Z']
                roomFilter = RoomFilter (Just $ TeamId "dummyTeamId")
                                        (Just RoomTypeGroup)
                                        (Just RoomFilterSortByLastActivity)

            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (RoomList testData)) req respond
                ) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest roomFilter .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/rooms"
                (sort . queryString) receivedReq `shouldBe` sort [ ("type", Just "group")
                                                                 , ("sortBy", Just "lastactivity")
                                                                 , ("teamId", Just "dummyTeamId") ]

        it "streamRoomList streams Room with automatic pagination" $ do
            withMockServer (paginationApp $ encode . RoomList <$> roomListList) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest (def :: RoomFilter) .| sinkList
                res `shouldBe` concat roomListList

        it "streamRoomList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . RoomList <$> roomListList) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest (def :: RoomFilter) .| sinkList
                res `shouldBe` concat (take 2 roomListList)

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

        it "streamMembershipList streams Membership" $ do
            let testData = membershipList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (MembershipList testData)) req respond
                ) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest (def :: MembershipFilter) .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/memberships"
                queryString receivedReq `shouldBe` []

        it "streamMembershipList passes query strings build from MembershipFilter to server" $ do
            let testData = membershipList ['Z']
                membershipFilter = MembershipFilter (Just $ RoomId "dummyRoomId")
                                                    (Just $ PersonId "personIdFilter")
                                                    (Just $ Email "personEmailFilter")

            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (MembershipList testData)) req respond
                ) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest membershipFilter .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/memberships"
                (sort . queryString) receivedReq `shouldBe` sort [ ("personId", Just "personIdFilter")
                                                                 , ("personEmail", Just "personEmailFilter")
                                                                 , ("roomId", Just "dummyRoomId") ]

        it "streamMembershipList streams Membership with automatic pagination" $ do
            withMockServer (paginationApp $ encode . MembershipList <$> membershipListList) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest (def :: MembershipFilter) .| sinkList
                res `shouldBe` concat membershipListList

        it "streamMembershipList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . MembershipList <$> membershipListList) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest (def :: MembershipFilter) .| sinkList
                res `shouldBe` concat (take 2 membershipListList)

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

        it "streamMessageList streams Message" $ do
            let testData = messageList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (MessageList testData)) req respond
                ) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest defFilter .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/messages"
                queryString receivedReq `shouldBe` [ ("roomId", Just "dummyRoomId") ]

        it "streamMessageList passes query strings build from MessageFilter to server" $ do
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
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest messageFilter .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/messages"
                (sort . queryString) receivedReq `shouldBe` sort [ ("roomId", Just "dummyRoomId")
                                                                 , ("mentionedPeople", Just "me")
                                                                 , ("beforeMessage", Just "beforeMessageFilter")
                                                                 , ("before", Just "beforeFilter") ]

        it "streamMessageList streams Message with automatic pagination" $ do
            withMockServer (paginationApp $ encode . MessageList <$> messageListList) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest defFilter .| sinkList
                res `shouldBe` concat messageListList

        it "streamMessageList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . MessageList <$> messageListList) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest defFilter .| sinkList
                res `shouldBe` concat (take 2 messageListList)

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

        it "streamOrganizationList streams Organization" $ do
            let testData = organizationList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (OrganizationList testData)) req respond
                ) $ do
                res <- runConduit $ streamOrganizationList dummyAuth mockBaseRequest .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/organizations"
                queryString receivedReq `shouldBe` []

        it "streamOrganizationList streams Organization with automatic pagination" $ do
            withMockServer (paginationApp $ encode . OrganizationList <$> organizationListList) $ do
                res <- runConduit $ streamOrganizationList dummyAuth mockBaseRequest .| sinkList
                res `shouldBe` concat organizationListList

        it "streamOrganizationList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . OrganizationList <$> organizationListList) $ do
                res <- runConduit $ streamOrganizationList dummyAuth mockBaseRequest .| sinkList
                res `shouldBe` concat (take 2 organizationListList)

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

        it "streamLicenseList streams License" $ do
            let testData = licenseList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (LicenseList testData)) req respond
                ) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest (def :: LicenseFilter) .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/licenses"
                queryString receivedReq `shouldBe` []

        it "streamLicenseList passes query strings build from LicenseFilter to server" $ do
            let testData = licenseList ['Z']
                licenseFilter = LicenseFilter (Just $ OrganizationId "orgIdFilter")

            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (LicenseList testData)) req respond
                ) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest licenseFilter .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/licenses"
                queryString receivedReq `shouldBe` [ ("orgId", Just "orgIdFilter") ]

        it "streamLicenseList streams License with automatic pagination" $ do
            withMockServer (paginationApp $ encode . LicenseList <$> licenseListList) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest (def :: LicenseFilter) .| sinkList
                res `shouldBe` concat licenseListList

        it "streamLicenseList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . LicenseList <$> licenseListList) $ do
                res <- runConduit $ streamListWithFilter dummyAuth mockBaseRequest (def :: LicenseFilter) .| sinkList
                res `shouldBe` concat (take 2 licenseListList)

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

        it "streamRoleList streams Role" $ do
            let testData = roleList ['Z']
            receivedReqMVar <- newEmptyMVar

            withMockServer (\req respond -> do
                    putMVar receivedReqMVar req
                    simpleApp (encode (RoleList testData)) req respond
                ) $ do
                res <- runConduit $ streamRoleList dummyAuth mockBaseRequest .| sinkList
                res `shouldBe` testData

                receivedReq <- takeMVar receivedReqMVar
                rawPathInfo receivedReq `shouldBe` "/v1/roles"
                queryString receivedReq `shouldBe` []

        it "streamRoleList streams Role with automatic pagination" $ do
            withMockServer (paginationApp $ encode . RoleList <$> roleListList) $ do
                res <- runConduit $ streamRoleList dummyAuth mockBaseRequest .| sinkList
                res `shouldBe` concat roleListList

        it "streamRoleList stops pagination at invalid Link Header" $ do
            withMockServer (invalidPaginationApp $ encode . RoleList <$> roleListList) $ do
                res <- runConduit $ streamRoleList dummyAuth mockBaseRequest .| sinkList
                res `shouldBe` concat (take 2 roleListList)
