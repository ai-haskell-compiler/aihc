{-# LANGUAGE OverloadedStrings #-}

module GitHub.Types.Events where


import Control.Applicative
import Control.Monad

import Data.Aeson
import Data.Aeson.Types
import Data.Monoid
import Data.Text hiding (find)
import Data.Time
import Data.List

import GitHub.Types.Base
import GitHub.Types.Repository



-- | All events which can be produced by GitHub.
--
-- See https://developer.github.com/v3/activity/events/
data Event = Event
    { eventId :: !Text
    , eventActor :: !Actor
    , eventRepo :: !Repo
    , eventCreatedAt :: !UTCTime
    , eventPublic :: !Bool
    , eventPayload :: !Payload
    } deriving (Eq, Show)

instance FromJSON Event where
    parseJSON (Object o) = do
        eventType <- o .: "type"

        Event
            <$> o .: "id"
            <*> o .: "actor"
            <*> o .: "repo"
            <*> o .: "created_at"
            <*> o .: "public"
            <*> (eventPayloadParser eventType =<< o .: "payload")

    parseJSON _ = fail "Event"


data Actor = Actor
    { actorId :: !Integer
    , actorLogin :: !Text
    } deriving (Eq, Show)

instance FromJSON Actor where
    parseJSON (Object o) = Actor
        <$> o .: "id"
        <*> o .: "login"

    parseJSON _ = fail "Actor"


data Repo = Repo
    { repoId :: !Integer
    , repoName :: !Text
    } deriving (Eq, Show)

instance FromJSON Repo where
    parseJSON (Object o) = Repo
        <$> o .: "id"
        <*> o .: "name"

    parseJSON _ = fail "Repo"


data Payload
    = CommitCommentEventPayload            CommitCommentEvent
    | DeploymentEventPayload               DeploymentEvent
    | DeploymentStatusEventPayload         DeploymentStatusEvent
    | PushEventPayload                     PushEvent
    | IssuesEventPayload                   IssuesEvent
    | IssueCommentEventPayload             IssueCommentEvent
    | CreateEventPayload                   CreateEvent
    | PullRequestEventPayload              PullRequestEvent
    | PullRequestReviewCommentEventPayload PullRequestReviewCommentEvent
    | WatchEventPayload                    WatchEvent
    | DeleteEventPayload                   DeleteEvent
    | ForkEventPayload                     ForkEvent
    | ReleaseEventPayload                  ReleaseEvent
    | GollumEventPayload                   GollumEvent
    | MemberEventPayload                   MemberEvent
    | PublicEventPayload                   Value
    deriving (Eq, Show)


eventPayloadParsers :: [(Text, Text, Value -> Parser Payload)]
eventPayloadParsers =
    [ ( "CommitCommentEvent", "commit_comment"
      , fmap CommitCommentEventPayload . parseJSON)

    , ( "DeploymentEvent", "deployment"
      , fmap DeploymentEventPayload . parseJSON)

    , ( "DeploymentStatusEvent", "deployment_status"
      , fmap DeploymentStatusEventPayload . parseJSON)

    , ( "PushEvent", "push"
      , fmap PushEventPayload . parseJSON)

    , ( "IssuesEvent", "issues"
      , fmap IssuesEventPayload . parseJSON)

    , ( "IssueCommentEvent", "issue_comment"
      , fmap IssueCommentEventPayload . parseJSON)

    , ( "CreateEvent", "create"
      , fmap CreateEventPayload . parseJSON)

    , ( "PullRequestEvent", "pull_request"
      , fmap PullRequestEventPayload . parseJSON)

    , ( "PullRequestReviewCommentEvent", "pull_request_review_comment"
      , fmap PullRequestReviewCommentEventPayload . parseJSON)

    , ( "WatchEvent", "watch"
      , fmap WatchEventPayload . parseJSON)

    , ( "DeleteEvent", "delete"
      , fmap DeleteEventPayload . parseJSON)

    , ( "ForkEvent", "fork"
      , fmap ForkEventPayload . parseJSON)

    , ( "ReleaseEvent", "release"
      , fmap ReleaseEventPayload . parseJSON)

    , ( "GollumEvent", "gollum"
      , fmap GollumEventPayload . parseJSON)

    , ( "MemberEvent", "member"
      , fmap MemberEventPayload . parseJSON)

    , ( "PublicEvent", "public"
      , fmap PublicEventPayload . parseJSON)
    ]



eventPayloadParser :: Text -> Value -> Parser Payload
eventPayloadParser eventType x = case find (\(t, _, _) -> t == eventType) eventPayloadParsers of
    Nothing -> fail $ "eventPayloadParser: Unknown event type: " <> unpack eventType
    Just (_, _, p) -> p x

-- | Since the event type is included through different means (X-GitHub-Event
-- header, or inline in the JSON object), it's not possible to make 'Event'
-- an instance of 'FromJSON'. But if you know the type, you can use this
-- parser.
webhookPayloadParser :: Text -> Value -> Parser Payload
webhookPayloadParser eventType x =  case find (\(_, t, _) -> t == eventType) eventPayloadParsers of
    Nothing -> fail $ "webhookPayloadParser: Unknown event type: " <> unpack eventType
    Just (_, _, p) -> p x


------------------------------------------------------------------------------
-- CommitCommentEvent

data CommitCommentEvent = CommitCommentEvent
    { commitCommentEventComment :: Value
    } deriving (Eq, Show)

instance FromJSON CommitCommentEvent where
    parseJSON (Object x) = CommitCommentEvent
        <$> x .: "comment"

    parseJSON _ = fail "CommitCommentEvent"


------------------------------------------------------------------------------
-- DeploymentEvent

data DeploymentEvent = DeploymentEvent
    { deploymentEventDeployment  :: Deployment
      -- ^ The deployment.
    , deploymentEventRepository  :: Repository
      -- ^ The repository for which the deployment was created (UNDOCUMENTED).
    } deriving (Eq, Show)

instance FromJSON DeploymentEvent where
    parseJSON (Object x) = DeploymentEvent
        <$> x .: "deployment"
        <*> x .: "repository"

    parseJSON _ = fail "DeploymentEvent"


------------------------------------------------------------------------------
-- DeploymentStatusEvent

data DeploymentStatusEvent = DeploymentStatusEvent
    { deploymentStatusEventDeploymentStatus :: DeploymentStatus
      -- ^ The deployment status.
    , deploymentStatusEventDeployment       :: Deployment
      -- ^ The deployment which the status affects.
    , deploymentStatusEventRepository       :: Repository
      -- ^ The repository for which the deployment was created (UNDOCUMENTED).
    } deriving (Eq, Show)

instance FromJSON DeploymentStatusEvent where
    parseJSON (Object x) = DeploymentStatusEvent
        <$> x .: "deployment_status"
        <*> x .: "deployment"
        <*> x .: "repository"

    parseJSON _ = fail "DeploymentStatusEvent"


------------------------------------------------------------------------------
-- PushEvent

data PushEvent = PushEvent
    { pushEventSize :: !Int
    } deriving (Eq, Show)

instance FromJSON PushEvent where
    parseJSON (Object x) = PushEvent
        <$> x .: "size"

    parseJSON _ = fail "PushEvent"


------------------------------------------------------------------------------
-- IssuesEvent

data IssuesEvent = IssuesEvent
    { issuesEventAction :: !Text
    } deriving (Eq, Show)

instance FromJSON IssuesEvent where
    parseJSON (Object x) = IssuesEvent
        <$> x .: "action"

    parseJSON _ = fail "IssuesEvent"


------------------------------------------------------------------------------
-- IssuesEvent

data IssueCommentEvent = IssueCommentEvent
    { issueCommentEventAction :: !Text
    } deriving (Eq, Show)

instance FromJSON IssueCommentEvent where
    parseJSON (Object x) = IssueCommentEvent
        <$> x .: "action"

    parseJSON _ = fail "IssueCommentEvent"


------------------------------------------------------------------------------
-- IssuesEvent

data CreateEvent = CreateEvent
    { createEventRef :: !(Maybe Text)
    } deriving (Eq, Show)

instance FromJSON CreateEvent where
    parseJSON (Object x) = CreateEvent
        <$> x .: "ref"

    parseJSON _ = fail "CreateEvent"


------------------------------------------------------------------------------
-- PullRequestEvent

data PullRequestEvent = PullRequestEvent
    { pullRequestEventAction :: !Text
    } deriving (Eq, Show)

instance FromJSON PullRequestEvent where
    parseJSON (Object x) = PullRequestEvent
        <$> x .: "action"

    parseJSON _ = fail "PullRequestEvent"


------------------------------------------------------------------------------
-- PullRequestEvent

data PullRequestReviewCommentEvent = PullRequestReviewCommentEvent
    { pullRequestReviewCommentEventPullRequest :: !Value
    } deriving (Eq, Show)

instance FromJSON PullRequestReviewCommentEvent where
    parseJSON (Object x) = PullRequestReviewCommentEvent
        <$> x .: "pull_request"

    parseJSON _ = fail "PullRequestReviewCommentEvent"



------------------------------------------------------------------------------
-- WatchEvent

data WatchEvent = WatchEvent
    { watchEventAction :: !Text
    } deriving (Eq, Show)

instance FromJSON WatchEvent where
    parseJSON (Object x) = WatchEvent
        <$> x .: "action"

    parseJSON _ = fail "WatchEvent"


------------------------------------------------------------------------------
-- DeleteEvent

data DeleteEvent = DeleteEvent
    { deleteEventRef :: !Text
    } deriving (Eq, Show)

instance FromJSON DeleteEvent where
    parseJSON (Object x) = DeleteEvent
        <$> x .: "ref"

    parseJSON _ = fail "DeleteEvent"


------------------------------------------------------------------------------
-- ForkEvent

data ForkEvent = ForkEvent
    { forkEventForkee :: !Value
    } deriving (Eq, Show)

instance FromJSON ForkEvent where
    parseJSON (Object x) = ForkEvent
        <$> x .: "forkee"

    parseJSON _ = fail "ForkEvent"


------------------------------------------------------------------------------
-- ReleaseEvent

data ReleaseEvent = ReleaseEvent
    { releaseEventAction :: !Text
    } deriving (Eq, Show)

instance FromJSON ReleaseEvent where
    parseJSON (Object x) = ReleaseEvent
        <$> x .: "action"

    parseJSON _ = fail "ForkEvent"


------------------------------------------------------------------------------
-- GollumEvent

data GollumEvent = GollumEvent
    { gollumEventPages :: !Value
    } deriving (Eq, Show)

instance FromJSON GollumEvent where
    parseJSON (Object x) = GollumEvent
        <$> x .: "pages"

    parseJSON _ = fail "GollumEvent"


------------------------------------------------------------------------------
-- MemberEvent

data MemberEvent = MemberEvent
    { memberEventAction :: !Text
    } deriving (Eq, Show)

instance FromJSON MemberEvent where
    parseJSON (Object x) = MemberEvent
        <$> x .: "action"

    parseJSON _ = fail "MemberEvent"
