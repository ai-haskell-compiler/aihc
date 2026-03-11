{-# LANGUAGE OverloadedStrings #-}

module Network.WebexTeams.TypesSpec where

import           Data.Aeson               (decode, eitherDecode, encode)
import           Data.Monoid              ((<>))

import           Test.Hspec

import           Network.WebexTeams.Types

spec :: Spec
spec = do
    describe "Error" $ do
        let errorsJson = "{\
                         \  \"title\": {\
                         \    \"code\": \"kms_failure\",\
                         \    \"reason\": \"Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html\"\
                         \  }\
                         \}"
            errors = Errors { errorsTitle = ErrorTitle { errorTitleCode     = ErrorCode "kms_failure"
                                                       , errorTitleReason   = "Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html"
                                                       }
                            }

        it "decodes errors field in response JSON containd in List API" $ do
            eitherDecode errorsJson `shouldBe` Right errors
            (decode . encode) errors `shouldBe` Just errors

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
            personJson2 = "{\
                          \  \"id\": \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                          \  \"emails\": [\
                          \    \"sample@email.com\"\
                          \  ],\
                          \  \"displayName\": \"Taro Yamada\",\
                          \  \"nickName\": \"Taro\",\
                          \  \"firstName\": \"Taro\",\
                          \  \"lastName\": \"Yamada\",\
                          \  \"avatar\": \"https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600\",\
                          \  \"orgId\": \"Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE\",\
                          \  \"created\": \"2012-06-15T20:35:48.682Z\",\
                          \  \"lastActivity\": \"2017-06-30T15:30:20.016Z\",\
                          \  \"status\": \"inactive\",\
                          \  \"type\": \"person\"\
                          \}"
            person2 = Person { personId            = PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                             , personErrors        = Nothing
                             , personEmails        = Just [Email "sample@email.com" ]
                             , personDisplayName   = Just $ DisplayName "Taro Yamada"
                             , personNickName      = Just $ NickName "Taro"
                             , personFirstName     = Just $ FirstName "Taro"
                             , personLastName      = Just $ LastName "Yamada"
                             , personAvatar        = Just $ AvatarUrl "https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600"
                             , personOrgId         = Just $ OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                             , personRoles         = Nothing
                             , personLicenses      = Nothing
                             , personCreated       = Just $ Timestamp "2012-06-15T20:35:48.682Z"
                             , personTimezone      = Nothing
                             , personLastActivity  = Just $ Timestamp "2017-06-30T15:30:20.016Z"
                             , personStatus        = Just PersonStatusInactive
                             , personInvitePending = Nothing
                             , personLoginEnabled  = Nothing
                             , personType          = Just PersonTypePerson
                             }
            personErrorJson = "{\
                              \  \"id\": \"Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5\",\
                              \  \"errors\": {\
                              \    \"title\": {\
                              \      \"code\": \"kms_failure\",\
                              \      \"reason\": \"Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html\"\
                              \    }\
                              \  }\
                              \}"
            personError = Person { personId                 = PersonId "Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5"
                                 , personErrors             = Just $ Errors
                                     { errorsTitle          = ErrorTitle
                                         { errorTitleCode   = ErrorCode "kms_failure"
                                         , errorTitleReason = "Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html"
                                         }
                                     }
                                 ,personEmails              = Nothing
                                 ,personDisplayName         = Nothing
                                 ,personNickName            = Nothing
                                 ,personFirstName           = Nothing
                                 ,personLastName            = Nothing
                                 ,personAvatar              = Nothing
                                 ,personOrgId               = Nothing
                                 ,personRoles               = Nothing
                                 ,personLicenses            = Nothing
                                 ,personCreated             = Nothing
                                 ,personTimezone            = Nothing
                                 ,personLastActivity        = Nothing
                                 ,personStatus              = Nothing
                                 ,personInvitePending       = Nothing
                                 ,personLoginEnabled        = Nothing
                                 ,personType                = Nothing
                             }
            personListJson = "{\"items\":[" <> personJson1 <> "," <> personJson2 <> "," <> personErrorJson <> "]}"
            personList = PersonList [ person1, person2, personError ]

        it "can be unwrapped from PersonList" $ do
            unwrap personList `shouldBe` [ person1, person2, personError ]

        it "decodes People API response JSON" $ do
            eitherDecode personJson1 `shouldBe` Right person1
            (decode . encode) person1 `shouldBe` Just person1

        it "decodes People API response JSON 2" $ do
            eitherDecode personJson2 `shouldBe` Right person2
            (decode . encode) person2 `shouldBe` Just person2

        it "decodes People list" $ do
            eitherDecode personListJson `shouldBe` Right personList
            (decode . encode) personList `shouldBe` Just personList

        it "encodes CreatePerson to JSON" $ do
            let src = CreatePerson
                    { createPersonEmails      = Just [ Email "johnny.chang@foomail.com", Email "jchang@barmail.com"]
                    , createPersonDisplayName = Just $ DisplayName "John Andersen"
                    , createPersonFirstName   = Just $ FirstName "John"
                    , createPersonLastName    = Just $ LastName "Andersen"
                    , createPersonAvatar      = Just $ AvatarUrl "https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600"
                    , createPersonOrgId       = Just $ OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                    , createPersonRoles       = Just [ RoleId "Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy"
                                                     , RoleId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX" ]
                    , createPersonLicenses    = Just [ LicenseId "Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy"
                                                     , LicenseId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX" ]
                    }
                dst = "{\
                      \  \"emails\" : [ \"johnny.chang@foomail.com\", \"jchang@barmail.com\" ],\
                      \  \"displayName\" : \"John Andersen\",\
                      \  \"firstName\" : \"John\",\
                      \  \"lastName\" : \"Andersen\",\
                      \  \"avatar\" : \"https://1efa7a94ed21783e352-c62266528714497a17239ececf39e9e2.ssl.cf1.rackcdn.com/V1~54c844c89e678e5a7b16a306bc2897b9~wx29yGtlTpilEFlYzqPKag==~1600\",\
                      \  \"orgId\" : \"Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE\",\
                      \  \"roles\" : [ \"Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy\", \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX\" ],\
                      \  \"licenses\" : [ \"Y2lzY29zcGFyazovL3VzL1JPT00vOGNkYzQwYzQtZjA5ZS0zY2JhLThjMjYtZGQwZTcwYWRlY2Iy\", \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX\" ]\
                      \}"
            eitherDecode dst `shouldBe` Right src
            (decode . encode) src `shouldBe` (decode dst :: Maybe CreatePerson)

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
            teamErrorJson = "{\
                            \  \"id\": \"Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5\",\
                            \  \"errors\": {\
                            \    \"title\": {\
                            \      \"code\": \"kms_failure\",\
                            \      \"reason\": \"Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html\"\
                            \    }\
                            \  }\
                            \}"
            teamError = Team { teamId                   = TeamId "Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5"
                             , teamErrors               = Just $ Errors
                                 { errorsTitle          = ErrorTitle
                                     { errorTitleCode   = ErrorCode "kms_failure"
                                     , errorTitleReason = "Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html"
                                     }
                                 }
                             , teamName                 = Nothing
                             , teamCreatorId            = Nothing
                             , teamCreated              = Nothing
                             }
            teamListJson = "{\"items\":[" <> teamJson <> "," <> teamErrorJson <> "]}"
            teamList = TeamList [ team, teamError ]

        it "can be unwrapped from TeamList" $ do
            unwrap teamList `shouldBe` [ team, teamError ]

        it "decodes Team API response JSON" $ do
            eitherDecode teamJson `shouldBe` Right team
            (decode . encode) team `shouldBe` Just team

        it "decodes Team list" $ do
            eitherDecode teamListJson `shouldBe` Right teamList
            (decode . encode) teamList `shouldBe` Just teamList

        it "encodes CreateTeam to JSON" $ do
            let src = CreateTeam { createTeamName = TeamName "My Team" }
                dst = "{\"name\":\"My Team\"}"
            encode src `shouldBe` dst
            (decode . encode) src `shouldBe` Just src

        it "encodes UpdateTeam to JSON" $ do
            let src = UpdateTeam { updateTeamName = TeamName "New Team Name" }
                dst = "{\"name\":\"New Team Name\"}"
            encode src `shouldBe` dst
            (decode . encode) src `shouldBe` Just src

    describe "TeamMembership" $ do
        let teamMembershipJson = "{\
                                 \  \"id\" : \"Y2lzY29zcGFyazovL3VzL1RFQU1fTUVNQkVSU0hJUC8wZmNmYTJiOC1hZGNjLTQ1ZWEtYTc4Mi1lNDYwNTkyZjgxZWY6MTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5\",\
                                 \  \"teamId\" : \"Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5\",\
                                 \  \"personId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                                 \  \"personEmail\" : \"johnny.chang@foomail.com\",\
                                 \  \"personDisplayName\" : \"John Andersen\",\
                                 \  \"personOrgId\" : \"Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE\",\
                                 \  \"isModerator\" : true,\
                                 \  \"created\" : \"2015-10-18T14:26:16.057Z\"\
                                 \}"
            teamMembership = TeamMembership { teamMembershipId                  = TeamMembershipId "Y2lzY29zcGFyazovL3VzL1RFQU1fTUVNQkVSU0hJUC8wZmNmYTJiOC1hZGNjLTQ1ZWEtYTc4Mi1lNDYwNTkyZjgxZWY6MTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5"
                                            , teamMembershipErrors              = Nothing
                                            , teamMembershipTeamId              = Just $ TeamId "Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5"
                                            , teamMembershipPersonId            = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                                            , teamMembershipPersonEmail         = Just $ Email "johnny.chang@foomail.com"
                                            , teamMembershipPersonDisplayName   = Just $ DisplayName "John Andersen"
                                            , teamMembershipPersonOrgId         = Just $ OrganizationId "Y2lzY29zcGFyazovL3VzL09SR0FOSVpBVElPTi85NmFiYzJhYS0zZGNjLTExZTUtYTE1Mi1mZTM0ODE5Y2RjOWE"
                                            , teamMembershipIsModerator         = Just True
                                            , teamMembershipCreated             = Just $ Timestamp "2015-10-18T14:26:16.057Z"
                                            }
            teamMembershipErrorJson = "{\
                                      \  \"id\": \"Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5\",\
                                      \  \"errors\": {\
                                      \    \"title\": {\
                                      \      \"code\": \"kms_failure\",\
                                      \      \"reason\": \"Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html\"\
                                      \    }\
                                      \  }\
                                      \}"
            teamMembershipError = TeamMembership { teamMembershipId                 = TeamMembershipId "Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5"
                                                 , teamMembershipErrors             = Just $ Errors
                                                     { errorsTitle                  = ErrorTitle
                                                         { errorTitleCode           = ErrorCode "kms_failure"
                                                         , errorTitleReason         = "Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html"
                                                         }
                                                     }
                                                 , teamMembershipTeamId             = Nothing
                                                 , teamMembershipPersonId           = Nothing
                                                 , teamMembershipPersonEmail        = Nothing
                                                 , teamMembershipPersonDisplayName  = Nothing
                                                 , teamMembershipPersonOrgId        = Nothing
                                                 , teamMembershipIsModerator        = Nothing
                                                 , teamMembershipCreated            = Nothing
                                                 }
            teamMembershipListJson = "{\"items\":[" <> teamMembershipJson <> "," <> teamMembershipErrorJson <> "]}"
            teamMembershipList = TeamMembershipList [ teamMembership, teamMembershipError ]

        it "can be unwrapped from TeamMembershipList" $ do
            unwrap teamMembershipList `shouldBe` [ teamMembership, teamMembershipError ]

        it "decodes Team Membership API response JSON" $ do
            eitherDecode teamMembershipJson `shouldBe` Right teamMembership
            (decode . encode) teamMembership `shouldBe` Just teamMembership

        it "decodes Team Membership list" $ do
            eitherDecode teamMembershipListJson `shouldBe` Right teamMembershipList
            (decode . encode) teamMembershipList `shouldBe` Just teamMembershipList

        it "encodes CreateTeamMembership to JSON" $ do
            let src = CreateTeamMembership
                      { createTeamMembershipTeamId      = TeamId "Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5"
                      , createTeamMembershipPersonId    = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                      , createTeamMembershipPersonEmail = Just $ Email "johnny.chang@foomail.com"
                      , createTeamMembershipIsModerator = Just True
                      }
                dst = "{\
                      \  \"teamId\" : \"Y2lzY29zcGFyazovL3VzL1RFQU0vMTNlMThmNDAtNDJmYy0xMWU2LWE5ZDgtMjExYTBkYzc5NzY5\",\
                      \  \"personId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                      \  \"personEmail\" : \"johnny.chang@foomail.com\",\
                      \  \"isModerator\" : true\
                      \}"
            eitherDecode dst `shouldBe` Right src
            (decode . encode) src `shouldBe` (decode dst :: Maybe CreateTeamMembership)

        it "encodes UpdateTeamMembership to JSON" $ do
            let src = UpdateTeamMembership { updateTeamMembershipIsModerator = False }
                dst = "{\"isModerator\":false}"
            encode src `shouldBe` dst
            (decode . encode) src `shouldBe` Just src

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
            roomErrorJson = "{\
                            \  \"id\": \"Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5\",\
                            \  \"errors\": {\
                            \    \"title\": {\
                            \      \"code\": \"kms_failure\",\
                            \      \"reason\": \"Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html\"\
                            \    }\
                            \  }\
                            \}"
            roomError = Room { roomId                   = RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5"
                             , roomErrors               = Just $ Errors
                                 { errorsTitle          = ErrorTitle
                                     { errorTitleCode   = ErrorCode "kms_failure"
                                     , errorTitleReason = "Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html"
                                     }
                                 }
                             , roomTitle                = Nothing
                             , roomType                 = Nothing
                             , roomIsLocked             = Nothing
                             , roomSipAddress           = Nothing
                             , roomLastActivity         = Nothing
                             , roomTeamId               = Nothing
                             , roomCreatorId            = Nothing
                             , roomCreated              = Nothing
                             }
            roomListJson = "{\"items\":[" <> roomJson <> "," <> roomErrorJson <> "]}"
            roomList = RoomList [ room, roomError ]

        it "can be unwrapped from RoomList" $ do
            unwrap roomList `shouldBe` [ room, roomError ]

        it "decodes Room API response JSON" $ do
            eitherDecode roomJson `shouldBe` Right room
            (decode . encode) room `shouldBe` Just room

        it "decodes Room list" $ do
            eitherDecode roomListJson `shouldBe` Right roomList
            (decode . encode) roomList `shouldBe` Just roomList

        it "encodes CreateRoom to JSON" $ do
            let src = CreateRoom { createRoomTitle  = RoomTitle "Project Unicorn - Sprint 0"
                                 , createRoomTeamId = Just $ TeamId "Y2lzY29zcGFyazovL3VzL1JPT00vNjRlNDVhZTAtYzQ2Yi0xMWU1LTlkZjktMGQ0MWUzNDIxOTcz"
                                 }
                dst = "{\
                      \  \"title\" : \"Project Unicorn - Sprint 0\",\
                      \  \"teamId\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vNjRlNDVhZTAtYzQ2Yi0xMWU1LTlkZjktMGQ0MWUzNDIxOTcz\"\
                      \}"
            eitherDecode dst `shouldBe` Right src
            (decode . encode) src `shouldBe` (decode dst :: Maybe CreateRoom)

        it "encodes UpdateRoom to JSON" $ do
            let src = UpdateRoom { updateRoomTitle =  RoomTitle "Project Unicorn - Sprint 0" }
                dst = "{\"title\":\"Project Unicorn - Sprint 0\"}"
            encode src `shouldBe` dst
            (decode . encode) src `shouldBe` Just src

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
            membershipErrorJson = "{\
                                  \  \"id\": \"Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5\",\
                                  \  \"errors\": {\
                                  \    \"title\": {\
                                  \      \"code\": \"kms_failure\",\
                                  \      \"reason\": \"Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html\"\
                                  \    }\
                                  \  }\
                                  \}"
            membershipError = Membership { membershipId                 = MembershipId "Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5"
                                         , membershipErrors             = Just $ Errors
                                             { errorsTitle              = ErrorTitle
                                                 { errorTitleCode       = ErrorCode "kms_failure"
                                                 , errorTitleReason     = "Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html"
                                                 }
                                             }
                                         , membershipRoomId             = Nothing
                                         , membershipPersonId           = Nothing
                                         , membershipPersonEmail        = Nothing
                                         , membershipPersonDisplayName  = Nothing
                                         , membershipPersonOrgId        = Nothing
                                         , membershipIsModerator        = Nothing
                                         , membershipIsMonitor          = Nothing
                                         , membershipCreated            = Nothing
                                         }
            membershipListJson = "{\"items\":[" <> membershipJson <> "," <> membershipErrorJson <> "]}"
            membershipList = MembershipList [ membership, membershipError ]

        it "can be unwrapped from MembershipList" $ do
            unwrap membershipList `shouldBe` [ membership, membershipError ]

        it "decodes Team Membership API response JSON" $ do
            eitherDecode membershipJson `shouldBe` Right membership
            (decode . encode) membership `shouldBe` Just membership

        it "decodes Team Membership list" $ do
            eitherDecode membershipListJson `shouldBe` Right membershipList
            (decode . encode) membershipList `shouldBe` Just membershipList

        it "encodes CreateMembership to JSON" $ do
            let src = CreateMembership
                      { createMembershipRoomId      = RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                      , createMembershipPersonId    = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY"
                      , createMembershipPersonEmail = Just $ Email "john.andersen@example.com"
                      , createMembershipIsModerator = Just True
                      }
                dst = "{\
                      \  \"roomId\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0\",\
                      \  \"personId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mNWIzNjE4Ny1jOGRkLTQ3MjctOGIyZi1mOWM0NDdmMjkwNDY\",\
                      \  \"personEmail\" : \"john.andersen@example.com\",\
                      \  \"isModerator\" : true\
                      \}"
            eitherDecode dst `shouldBe` Right src
            (decode . encode) src `shouldBe` (decode dst :: Maybe CreateMembership)

        it "encodes UpdateMembership to JSON" $ do
            let src = UpdateMembership { updateMembershipIsModerator = False }
                dst = "{\"isModerator\":false}"
            encode src `shouldBe` dst
            (decode . encode) src `shouldBe` Just src

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
            messageErrorJson = "{\
                               \  \"id\": \"Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5\",\
                               \  \"errors\": {\
                               \    \"title\": {\
                               \      \"code\": \"kms_failure\",\
                               \      \"reason\": \"Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html\"\
                               \    }\
                               \  }\
                               \}"
            messageError = Message { messageId                  = MessageId "Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5"
                                   , messageErrors              = Just $ Errors
                                       { errorsTitle            = ErrorTitle
                                           { errorTitleCode     = ErrorCode "kms_failure"
                                           , errorTitleReason   = "Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html"
                                           }
                                       }
                                   , messageRoomId              = Nothing
                                   , messageRoomType            = Nothing
                                   , messageToPersonId          = Nothing
                                   , messageToPersonEmail       = Nothing
                                   , messageText                = Nothing
                                   , messageHtml                = Nothing
                                   , messageFiles               = Nothing
                                   , messagePersonId            = Nothing
                                   , messagePersonEmail         = Nothing
                                   , messageCreated             = Nothing
                                   , messageMentionedPeople     = Nothing
                                   }
            messageListJson = "{\"items\":[" <> messageJson <> "," <> messageErrorJson <> "]}"
            messageList = MessageList [ message, messageError ]

        it "can be unwrapped from MessageList" $ do
            unwrap messageList `shouldBe` [ message, messageError ]

        it "decodes Message API response JSON" $ do
            eitherDecode messageJson `shouldBe` Right message
            (decode . encode) message `shouldBe` Just message

        it "decodes Message list" $ do
            eitherDecode messageListJson `shouldBe` Right messageList
            (decode . encode) messageList `shouldBe` Just messageList

        it "encodes CreateMessage to JSON" $ do
            let src = CreateMessage
                          { createMessageRoomId         = Just $ RoomId "Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                          , createMessageToPersonId     = Just $ PersonId "Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX"
                          , createMessageToPersonEmail  = Just $ Email "julie@example.com"
                          , createMessageText           = Just $ MessageText "PROJECT UPDATE - A new project plan has been published on Box: http://box.com/s/lf5vj. The PM for this project is Mike C. and the Engineering Manager is Jane W."
                          , createMessageMarkdown       = Just $ MessageMarkdown "**PROJECT UPDATE** A new project plan has been published [on Box](http://box.com/s/lf5vj). The PM for this project is <@personEmail:mike@example.com> and the Engineering Manager is <@personEmail:jane@example.com>."
                          , createMessageFiles          = Just $ [ FileUrl "http://www.example.com/images/media.png" ]
                          }
                dst = "{\
                      \  \"roomId\" : \"Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0\",\
                      \  \"toPersonId\" : \"Y2lzY29zcGFyazovL3VzL1BFT1BMRS9mMDZkNzFhNS0wODMzLTRmYTUtYTcyYS1jYzg5YjI1ZWVlMmX\",\
                      \  \"toPersonEmail\" : \"julie@example.com\",\
                      \  \"text\" : \"PROJECT UPDATE - A new project plan has been published on Box: http://box.com/s/lf5vj. The PM for this project is Mike C. and the Engineering Manager is Jane W.\",\
                      \  \"markdown\" : \"**PROJECT UPDATE** A new project plan has been published [on Box](http://box.com/s/lf5vj). The PM for this project is <@personEmail:mike@example.com> and the Engineering Manager is <@personEmail:jane@example.com>.\",\
                      \  \"files\" : [ \"http://www.example.com/images/media.png\" ]\
                      \}"
            eitherDecode dst `shouldBe` Right src
            (decode . encode) src `shouldBe` (decode dst :: Maybe CreateMessage)

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
            organizationErrorJson = "{\
                                    \  \"id\": \"Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5\",\
                                    \  \"errors\": {\
                                    \    \"title\": {\
                                    \      \"code\": \"kms_failure\",\
                                    \      \"reason\": \"Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html\"\
                                    \    }\
                                    \  }\
                                    \}"
            organizationError = Organization { organizationId           = OrganizationId "Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5"
                                             , organizationErrors       = Just $ Errors
                                                 { errorsTitle          = ErrorTitle
                                                     { errorTitleCode   = ErrorCode "kms_failure"
                                                     , errorTitleReason = "Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html"
                                                     }
                                                 }
                                             , organizationDisplayName  = Nothing
                                             , organizationCreated      = Nothing
                                             }
            organizationListJson = "{\"items\":[" <> organizationJson <> "," <> organizationErrorJson <> "]}"
            organizationList = OrganizationList [ organization, organizationError ]

        it "can be unwrapped from OrganizationList" $ do
            unwrap organizationList `shouldBe` [ organization, organizationError ]

        it "decodes Organization API response JSON" $ do
            eitherDecode organizationJson `shouldBe` Right organization
            (decode . encode) organization `shouldBe` Just organization

        it "decodes Organization list" $ do
            eitherDecode organizationListJson `shouldBe` Right organizationList
            (decode . encode) organizationList `shouldBe` Just organizationList

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
            licenseErrorJson = "{\
                               \  \"id\": \"Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5\",\
                               \  \"errors\": {\
                               \  \"title\": {\
                               \      \"code\": \"kms_failure\",\
                               \      \"reason\": \"Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html\"\
                               \    }\
                               \  }\
                               \}"
            licenseError = License { licenseId                  = LicenseId "Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5"
                                   , licenseErrors              = Just $ Errors
                                       { errorsTitle            = ErrorTitle
                                           { errorTitleCode     = ErrorCode "kms_failure"
                                           , errorTitleReason   = "Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html"
                                           }
                                       }
                                   , licenseName                = Nothing
                                   , licenseTotalUnits          = Nothing
                                   , licenseConsumedUnits       = Nothing
                                   }
            licenseListJson = "{\"items\":[" <> licenseJson <> "," <> licenseErrorJson <> "]}"
            licenseList = LicenseList [ license, licenseError ]

        it "can be unwrapped from LicenseList" $ do
            unwrap licenseList `shouldBe` [ license, licenseError ]

        it "decodes License API response JSON" $ do
            eitherDecode licenseJson `shouldBe` Right license
            (decode . encode) license `shouldBe` Just license

        it "decodes License list" $ do
            eitherDecode licenseListJson `shouldBe` Right licenseList
            (decode . encode) licenseList `shouldBe` Just licenseList

    describe "Role" $ do
        let roleJson = "{\
                       \  \"id\" : \"OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh\",\
                       \  \"name\" : \"Full Administrator\"\
                       \}"
            role = Role { roleId     = RoleId "OTZhYmMyYWEtM2RjYy0xMWU1LWExNTItZmUzNDgxOWNkYzlh"
                        , roleErrors = Nothing
                        , roleName   = Just $ RoleName "Full Administrator"
                        }
            roleErrorJson = "{\
                            \  \"id\": \"Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5\",\
                            \  \"errors\": {\
                            \    \"title\": {\
                            \      \"code\": \"kms_failure\",\
                            \      \"reason\": \"Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html\"\
                            \    }\
                            \  }\
                            \}"
            roleError = Role { roleId                   = RoleId "Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5"
                             , roleErrors               = Just $ Errors
                                 { errorsTitle          = ErrorTitle
                                     { errorTitleCode   = ErrorCode "kms_failure"
                                     , errorTitleReason = "Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html"
                                     }
                                 }
                             , roleName                 = Nothing
                             }
            roleListJson = "{\"items\":[" <> roleJson <> "]}"
            roleList = RoleList [ role ]

        it "can be unwrapped from RoleList" $ do
            unwrap roleList `shouldBe` [ role ]

        it "decodes Role API response JSON" $ do
            eitherDecode roleJson `shouldBe` Right role
            (decode . encode) role `shouldBe` Just role

        it "decodes Role list" $ do
            eitherDecode roleListJson `shouldBe` Right roleList
            (decode . encode) roleList `shouldBe` Just roleList

    describe "Webhook" $ do
        let webhookJson = "{\
                          \  \"id\" : \"96abc2aa-3dcc-11e5-a152-fe34819cdc9a\",\
                          \  \"name\" : \"My Awesome Webhook\",\
                          \  \"targetUrl\" : \"https://example.com/mywebhook\",\
                          \  \"resource\" : \"messages\",\
                          \  \"event\" : \"created\",\
                          \  \"filter\" : \"roomId=Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0\",\
                          \  \"secret\" : \"86dacc007724d8ea666f88fc77d918dad9537a15\",\
                          \  \"created\" : \"2015-10-18T14:26:16+00:00\"\
                          \}"
            webhook = Webhook { webhookId           = WebhookId "96abc2aa-3dcc-11e5-a152-fe34819cdc9a"
                              , webhookErrors       = Nothing
                              , webhookName         = Just $ WebhookName "My Awesome Webhook"
                              , webhookTargetUrl    = Just $ WebhookUrl "https://example.com/mywebhook"
                              , webhookResource     = Just $ WebhookResourceMessages
                              , webhookEvent        = Just $ WebhookEventCreated
                              , webhookFilter       = Just $ WebhookFilter "roomId=Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                              , webhookSecret       = Just $ WebhookSecret "86dacc007724d8ea666f88fc77d918dad9537a15"
                              , webhookCreated      = Just $ Timestamp "2015-10-18T14:26:16+00:00"
                              }
            webhookErrorJson = "{\
                               \  \"id\": \"Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5\",\
                               \  \"errors\": {\
                               \    \"title\": {\
                               \      \"code\": \"kms_failure\",\
                               \      \"reason\": \"Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html\"\
                               \    }\
                               \  }\
                               \}"
            webhookError = Webhook { webhookId                  = WebhookId "Y2lzY29zcGFyazovL3VzL1JPT00vNTIxN0EyMzYtNDQzQi00NThELTkzNjAtRDRFOTMyMTBBNUU5"
                                   , webhookErrors              = Just $ Errors
                                       { errorsTitle            = ErrorTitle
                                           { errorTitleCode     = ErrorCode "kms_failure"
                                           , errorTitleReason   = "Key management server failed to respond appropriately. For more information: https://developer.ciscospark.com/errors.html"
                                           }
                                       }
                                   , webhookName                = Nothing
                                   , webhookTargetUrl           = Nothing
                                   , webhookResource            = Nothing
                                   , webhookEvent               = Nothing
                                   , webhookFilter              = Nothing
                                   , webhookSecret              = Nothing
                                   , webhookCreated             = Nothing
                                   }
            webhookListJson = "{\"items\":[" <> webhookJson <> "," <> webhookErrorJson <> "]}"
            webhookList = WebhookList [ webhook, webhookError ]

        it "can be unwrapped from WebhookList" $ do
            unwrap webhookList `shouldBe` [ webhook, webhookError ]

        it "decodes Webhook API response JSON" $ do
            eitherDecode webhookJson `shouldBe` Right webhook
            (decode . encode) webhook `shouldBe` Just webhook

        it "decodes Webhook list" $ do
            eitherDecode webhookListJson `shouldBe` Right webhookList
            (decode . encode) webhookList `shouldBe` Just webhookList

        it "encodes CreateWebhook to JSON" $ do
            let src = CreateWebhook { createWebhookName         = WebhookName "My Awesome Webhook"
                                    , createWebhookTargetUrl    = WebhookUrl "https://example.com/mywebhook"
                                    , createWebhookResource     = WebhookResourceMessages
                                    , createWebhookEvent        = WebhookEventCreated
                                    , createWebhookFilter       = Just $ WebhookFilter "roomId=Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0"
                                    , createWebhookSecret       = Just $ WebhookSecret "86dacc007724d8ea666f88fc77d918dad9537a15"
                                    }
                dst = "{\
                      \  \"name\" : \"My Awesome Webhook\",\
                      \  \"targetUrl\" : \"https://example.com/mywebhook\",\
                      \  \"resource\" : \"messages\",\
                      \  \"event\" : \"created\",\
                      \  \"filter\" : \"roomId=Y2lzY29zcGFyazovL3VzL1JPT00vYmJjZWIxYWQtNDNmMS0zYjU4LTkxNDctZjE0YmIwYzRkMTU0\",\
                      \  \"secret\" : \"86dacc007724d8ea666f88fc77d918dad9537a15\"\
                      \}"
            eitherDecode dst `shouldBe` Right src
            (decode . encode) src `shouldBe` (decode dst :: Maybe CreateWebhook)

        it "encodes UpdateWebhook to JSON" $ do
            let src = UpdateWebhook { updateWebhookName         = WebhookName "My Awesome Webhook"
                                    , updateWebhookTargetUrl    = WebhookUrl "https://example.com/mywebhook"
                                    }
                dst = "{\"name\":\"My Awesome Webhook\",\"targetUrl\":\"https://example.com/mywebhook\"}"
            encode src `shouldBe` dst
            (decode . encode) src `shouldBe` Just src
