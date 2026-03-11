{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Conduit                    (mapM_C, runConduit, takeC, (.|))
import           Data.ByteString.Char8      as BC8 (pack)
import           Data.Default               (def)
import           Data.Semigroup             ((<>))
import           Data.Text                  as T (pack)
import           Network.HTTP.Client        (responseBody)
import           Options.Applicative
import           System.Environment         (lookupEnv)
import           System.IO                  (hPutStrLn, stderr)

import           Network.WebexTeams         hiding (streamTeamList)
import           Network.WebexTeams.Conduit

main :: IO ()
main = lookupEnv "WEBEXTEAMS_AUTH" >>= runIfEnvFound
  where
    runIfEnvFound Nothing   = hPutStrLn stderr "Missing WEBEXTEAMS_AUTH.  Set Webex Teams authorization string to WEBEXTEAMS_AUTH environment variable."
    runIfEnvFound (Just s)  = do
        opts <- execParser programOptions
        print opts
        run (Authorization $ BC8.pack s) opts

run :: Authorization -> Command -> IO ()
run auth (PersonListCommand count filter) =
    runConduit $ streamListWithFilter auth def filter .| takeC count .| mapM_C print

run auth (RoomListCommand count filter) =
    runConduit $ streamListWithFilter auth def filter .| takeC count .| mapM_C print

run auth (MembershipListCommand count filter) =
    runConduit $ streamListWithFilter auth def filter .| takeC count .| mapM_C print

run auth (MessageListCommand count filter) =
    runConduit $ streamListWithFilter auth def filter .| takeC count .| mapM_C print

run auth (TeamMembershipListCommand count filter) =
    runConduit $ streamListWithFilter auth def filter .| takeC count .| mapM_C print

run auth (TeamListCommand count) =
    runConduit $ streamTeamList auth def .| takeC count .| mapM_C print

run auth (PersonDetailCommand personId) =
    getDetail auth def personId >>= print . responseBody

run auth (RoomDetailCommand roomId) =
    getDetail auth def roomId >>= print . responseBody

run auth (MembershipDetailCommand membershipId) =
    getDetail auth def membershipId >>= print . responseBody

run auth (MessageDetailCommand messageId) =
    getDetail auth def messageId >>= print . responseBody

run auth (CreateMessageCommand roomId messageText) =
    createEntity auth def message >>= print . responseBody
      where
        message = CreateMessage (Just roomId) Nothing Nothing (Just messageText) Nothing Nothing

run auth (DeleteMessageCommand messageId) =
    deleteMessage auth def messageId >>= print . responseBody

run auth (TeamDetailCommand teamId) =
    getDetail auth def teamId >>= print . responseBody

run auth (TeamMembershipDetailCommand teamMembershipId) =
    getDetail auth def teamMembershipId >>= print . responseBody

run auth (CreateRoomCommand createRoom) =
    createEntity auth def createRoom >>= print . responseBody

run auth (DeleteRoomCommand roomId) =
    deleteRoom auth def roomId >>= print . responseBody

{-
    Command line parser
-}
data Command
    = PersonListCommand Int PersonFilter
    | PersonDetailCommand PersonId
    | RoomListCommand Int RoomFilter
    | RoomDetailCommand RoomId
    | CreateRoomCommand CreateRoom
    | DeleteRoomCommand RoomId
    | MembershipListCommand Int MembershipFilter
    | MembershipDetailCommand MembershipId
    | MessageListCommand Int MessageFilter
    | MessageDetailCommand MessageId
    | CreateMessageCommand RoomId MessageText
    | DeleteMessageCommand MessageId
    | TeamListCommand Int
    | TeamDetailCommand TeamId
    | TeamMembershipListCommand Int TeamMembershipFilter
    | TeamMembershipDetailCommand TeamMembershipId
    deriving (Show)


{-
    Common command line option parsers
-}
countParser :: Int -> Parser Int
countParser defaultCount = option auto
    (  long    "count"
    <> short   'c'
    <> metavar "MAX_ITEMS"
    <> value   (if defaultCount == def then maxBound else defaultCount)
    <> help "Maximum number of items to print"
    )

roomIdParser :: Parser RoomId
roomIdParser = RoomId . T.pack <$> strArgument
    (  metavar "ROOM_ID"
    <> help    "Identifier of a room")

emailOptParser :: Parser Email
emailOptParser = Email . T.pack <$> strOption
    (  long     "email"
    <> short    'e'
    <> metavar  "EMAIL_ADDRESS"
    <> help     "An email address to filter result"
    )

teamIdOptParser :: Parser TeamId
teamIdOptParser = TeamId . T.pack <$> strOption
    (  long     "team"
    <> short    't'
    <> metavar "TEAM_ID"
    <> help    "Identifier of a team"
    )

{-
    Person specific command line option parsers
-}
displayNameParser :: Parser DisplayName
displayNameParser = DisplayName . T.pack <$> strOption
    (  long     "name"
    <> short    'n'
    <> metavar  "DISPLAY_NAME"
    <> help     "Display name of the person to look up"
    )

organizationIdOptParser :: Parser OrganizationId
organizationIdOptParser = OrganizationId . T.pack <$> strOption
    (  long     "org"
    <> short    'o'
    <> metavar "ORGANIZATION_ID"
    <> help    "Identifier of a organization to be searched"
    )

personListOptParser :: Parser Command
personListOptParser = PersonListCommand <$> countParser def
                                        <*> (PersonFilter <$> optional emailOptParser
                                                          <*> optional displayNameParser
                                                          <*> optional organizationIdOptParser)

personDetailOptParser :: Parser Command
personDetailOptParser = PersonDetailCommand <$> (PersonId . T.pack <$> strArgument
                                                    (  metavar "PERSON_ID"
                                                    <> help    "Identifier of a person"
                                                    ))

{-
    Room specific command line option parsers
-}
roomTypeParser :: Parser (Maybe RoomType)
roomTypeParser
    =   flag Nothing (Just RoomTypeDirect)
            (  long  "direct"
            <> short 'd'
            <> help  "Filter only one-to-one space")
    <|> flag Nothing (Just RoomTypeGroup)
            (  long  "group"
            <> short 'g'
            <> help  "Filter only group space")

roomSortByParser :: Parser (Maybe RoomFilterSortBy)
roomSortByParser
    =   flag Nothing (Just RoomFilterSortById)
            (  long "sort-by-id"
            <> help "Sort by room ID")
    <|> flag Nothing (Just RoomFilterSortByLastActivity)
            (  long "sort-by-last-activity"
            <> help "Sort by most recent activity")
    <|> flag Nothing (Just RoomFilterSortByCreated)
            (  long "sort-by-created"
            <> help "Sort by most recentlly created")

roomTitleParser :: Parser RoomTitle
roomTitleParser = RoomTitle . T.pack <$> strArgument
    (  metavar  "ROOM_TITLE"
    <> help     "A user-friendly name for the room")

roomListOptParser :: Parser Command
roomListOptParser = RoomListCommand <$> countParser def
                                    <*> (RoomFilter <$> optional teamIdOptParser
                                                    <*> roomTypeParser
                                                    <*> roomSortByParser)

roomDetailOptParser :: Parser Command
roomDetailOptParser = RoomDetailCommand <$> roomIdParser

createRoomOptParser :: Parser Command
createRoomOptParser = CreateRoomCommand <$> (CreateRoom <$> roomTitleParser
                                                        <*> optional teamIdOptParser)
deleteRoomOptParser :: Parser Command
deleteRoomOptParser = DeleteRoomCommand <$> roomIdParser


{-
    Membership specific command line option parsers
-}
roomIdOptParser :: Parser RoomId
roomIdOptParser = RoomId . T.pack <$> strOption
    (  long     "room"
    <> short    'r'
    <> metavar "ROOM_ID"
    <> help    "Identifier of a room to be searched"
    )

personIdOptParser :: Parser PersonId
personIdOptParser = PersonId . T.pack <$> strOption
    (  long     "person"
    <> short    'p'
    <> metavar "PERSON_ID"
    <> help    "Identifier of a person to filter result"
    )

membershipListOptParser :: Parser Command
membershipListOptParser = MembershipListCommand <$> countParser def
                                                <*> (MembershipFilter <$> optional roomIdOptParser
                                                                      <*> optional personIdOptParser
                                                                      <*> optional emailOptParser)

membershipDetailOptParser :: Parser Command
membershipDetailOptParser = MembershipDetailCommand <$> (MembershipId . T.pack <$> strArgument
                                                            (  metavar "MEMBERSHIP_ID"
                                                            <> help    "Identifier of a membership"
                                                            ))

{-
    Message specific command line option parsers
-}
mentionedPeopleParser :: Parser (Maybe MentionedPeople)
mentionedPeopleParser
    =   flag Nothing (Just MentionedPeopleMe)
            (  long "me"
            <> help "List messages where the caller is mentioned"
            )
    <|> optional (MentionedPeople . PersonId . T.pack <$> strOption
                    (   long    "mentioned-person"
                    <>  short   'p'
                    <>  metavar "PERSON_ID"
                    <>  help    "List messages where the specified persionId is mentioned"
                    )
                 )

beforeOptParser :: Parser Timestamp
beforeOptParser = Timestamp . T.pack <$> strOption
    (  long     "before"
    <> short    'b'
    <> metavar  "TIMESTAMP"
    <> help     "List messages sent before a date and time, in ISO8601 format"
    )

beforeMessageOptParser :: Parser MessageId
beforeMessageOptParser = MessageId . T.pack <$> strOption
    (  long     "before-message"
    <> short    'm'
    <> metavar  "MESSAGE_ID"
    <> help     "List messages sent before a message, by ID"
    )

messageListOptParser :: Parser Command
messageListOptParser = MessageListCommand <$> countParser 10
                                          <*> (MessageFilter <$> roomIdParser
                                                             <*> mentionedPeopleParser
                                                             <*> optional beforeOptParser
                                                             <*> optional beforeMessageOptParser
                                                             )

messageIdParser :: Parser MessageId
messageIdParser = MessageId . T.pack <$> strArgument
    (  metavar "MESSAGE_ID"
    <> help    "Identifier of a message")

messageDetailOptParser :: Parser Command
messageDetailOptParser = MessageDetailCommand <$> messageIdParser

messageTextParser :: Parser MessageText
messageTextParser = MessageText . T.pack <$> strArgument
    (  metavar "MESSAGE_TEXT"
    <> help    "Body of message in text"
    )

createMessageOptParser :: Parser Command
createMessageOptParser = CreateMessageCommand <$> roomIdParser <*> messageTextParser

deleteMessageOptParser :: Parser Command
deleteMessageOptParser = DeleteMessageCommand <$> messageIdParser

{-
    Team specific command line option parsers
-}
teamIdParser :: Parser TeamId
teamIdParser = TeamId . T.pack <$> strArgument
    (  metavar "TEAM_ID"
    <> help    "Identifier of a team"
    )

teamListOptParser :: Parser Command
teamListOptParser = TeamListCommand <$> countParser def

teamDetailOptParser :: Parser Command
teamDetailOptParser = TeamDetailCommand <$> teamIdParser

{-
    Team membership specific command line option parsers
-}
teamMembershipListOptParser :: Parser Command
teamMembershipListOptParser = TeamMembershipListCommand <$> countParser def
                                                        <*> (TeamMembershipFilter <$> teamIdParser)

teamMembershipDetailOptParser :: Parser Command
teamMembershipDetailOptParser = TeamMembershipDetailCommand <$> (TeamMembershipId . T.pack <$> strArgument
                                                                    (  metavar "TEAM_MEMBERSHIP_ID"
                                                                    <> help    "Identifier of a team membership"
                                                                    ))

{-
    Top level parsers
-}
commandSubParser :: Parser Command
commandSubParser = hsubparser
    (  command "person-list" (info personListOptParser (progDesc "List people"))
    <> command "person-detail" (info personDetailOptParser (progDesc "Get detail for a person by ID"))
    <> command "room-list" (info roomListOptParser (progDesc "List belonging spaces"))
    <> command "room-detail" (info roomDetailOptParser (progDesc "Get detail for a team by ID"))
    <> command "create-room" (info createRoomOptParser (progDesc "Create a room"))
    <> command "delete-room" (info deleteRoomOptParser (progDesc "Delete a room"))
    <> command "membership-list" (info membershipListOptParser (progDesc "List memberships of authenticated user"))
    <> command "membership-detail" (info membershipDetailOptParser (progDesc "Get detail for a membership by ID"))
    <> command "message-list" (info messageListOptParser (progDesc "List messages in a room"))
    <> command "message-detail" (info messageDetailOptParser (progDesc "Get detail for a message by ID"))
    <> command "create-message" (info createMessageOptParser (progDesc "Send a message to a room"))
    <> command "delete-message" (info deleteMessageOptParser (progDesc "Delete a message"))
    <> command "team-list" (info teamListOptParser (progDesc "List belonging teams"))
    <> command "team-detail" (info teamDetailOptParser (progDesc "Get detail for a team by ID"))
    <> command "team-membership-list" (info teamMembershipListOptParser (progDesc "List team memberships of authenticated user"))
    <> command "team-membership-detail" (info teamMembershipDetailOptParser (progDesc "Get detail for a team membership by ID"))
    )

programOptions :: ParserInfo Command
programOptions = info (commandSubParser <**> helper)
    (  fullDesc
    <> progDesc "Sample porgram demonstrating how to use webex-teams-conduit"
    <> header   "webex-teams-conduit-exe -- Sample porgram demonstrating how to use webex-teams-conduit"
    )
