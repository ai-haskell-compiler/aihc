{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module API.Common where

import Data.Aeson hiding (Key)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Maybe
import Data.TreeDiff.Class
import Data.TreeDiff.Pretty
import qualified Data.Vector as Vec
import GHC.Generics
import GitLab
  ( ArchiveFormat (..),
    Artifact (Artifact),
    BoardIssue (BoardIssue),
    BoardIssueLabel (BoardIssueLabel),
    Branch (Branch),
    Change (Change),
    CommandsChanges (CommanandsChanges),
    Commit (Commit),
    CommitNote (CommitNote),
    CommitStats (CommitStats),
    CommitTodo (CommitTodo),
    Contributor (Contributor),
    DetailedStatus (DetailedStatus),
    Diff (Diff),
    DiffRefs (DiffRefs),
    Discussion (Discussion),
    EditIssueReq (EditIssueReq),
    Email (..),
    Epic (Epic),
    Event (..),
    EventActionName (..),
    EventTargetType (..),
    ExpirationPolicy (ExpirationPolicy),
    Group (Group),
    GroupShare (GroupShare),
    Identity (Identity),
    Issue (Issue),
    IssueBoard (IssueBoard),
    IssueCounts (IssueCounts),
    IssueStatistics (IssueStatistics),
    IssueStats (IssueStats),
    Job (Job),
    Key (..),
    License (License),
    Links (Links),
    Member (Member),
    MergeRequest (MergeRequest),
    Milestone (Milestone),
    MilestoneState (..),
    Namespace (Namespace),
    Note (Note),
    Owner (Ownwer),
    Permissions (Permissions),
    Pipeline (Pipeline),
    Project (Project),
    ProjectAvatar (ProjectAvatar),
    PushData (..),
    References (References),
    Release (Release),
    Repository (Repository),
    RepositoryFile (RepositoryFile),
    RepositoryFileBlame (RepositoryFileBlame),
    RepositoryFileSimple (RepositoryFileSimple),
    RepositoryStorage (RepositoryStorage),
    SamlIdentity (SamlIdentity),
    Starrer (Starrer),
    Statistics (Statistics),
    Tag (Tag),
    TaskCompletionStatus (TaskCompletionStatus),
    TestCase (TestCase),
    TestReport (TestReport),
    TestSuite (TestSuite),
    TimeEstimate (TimeEstimate),
    TimeStats (TimeStats),
    Todo (Todo),
    TodoAction (..),
    TodoProject (TodoProject),
    TodoState (..),
    TodoTarget (..),
    TodoTargetType (..),
    User (User),
    UserCount (..),
    UserPrefs (..),
    UserStatus (..),
    Version (Version),
    Visibility (..),
  )
import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.PrettyPrint.ANSI.Leijen as WL

removeNulls :: Value -> Maybe Value
removeNulls Null = Nothing
removeNulls (Array vec) = Just (Array (Vec.mapMaybe removeNulls vec))
removeNulls (String x) = Just $ String x
removeNulls (Number x) = Just $ Number x
removeNulls (Bool x) = Just $ Bool x
removeNulls (Object keyMap) = Just $ Object (KeyMap.mapMaybe removeNulls keyMap)

gitlabJsonParserTests :: (ToExpr a, FromJSON a, ToJSON a, Eq a, Show a) => String -> FilePath -> IO a -> IO a -> [TestTree]
gitlabJsonParserTests testPrefix jsonFilename parseFileF decodedCustomTypeF = do
  [ testCase
      (testPrefix <> "-decode-encode-decode")
      (decodeEncodeDecode parseFileF),
    testCase
      (testPrefix <> "-json-values-equal")
      (jsonValuesEqual jsonFilename decodedCustomTypeF)
    ]

decodeEncodeDecode :: (ToExpr a, FromJSON a, ToJSON a, Eq a, Show a) => IO a -> Assertion
decodeEncodeDecode parseFileF = do
  decodedFromFile <- parseFileF
  decodedAgain <- parseOne (encode decodedFromFile)
  (decodedFromFile == decodedAgain)
    @? showWL (ansiWlEditExprCompact (ediff decodedFromFile decodedAgain))

jsonValuesEqual :: (ToExpr a, FromJSON a, ToJSON a, Eq a, Show a) => FilePath -> IO a -> Assertion
jsonValuesEqual jsonFilename decodedCustomTypeF = do
  jsonValueFromFile <- parseValuesFromFile jsonFilename
  decodedCustomType <- decodedCustomTypeF
  let (Just jsonFromCustomType) = decode (encode decodedCustomType) :: Maybe Value
  (jsonValueFromFile == jsonFromCustomType)
    @? showWL (ansiWlEditExprCompact (ediff jsonValueFromFile jsonFromCustomType))

parseValuesFromFile :: String -> IO Value
parseValuesFromFile fname =
  fromJust . removeNulls . fromJust . decode <$> BSL.readFile fname

gitlabParseTestOne :: (ToExpr a, FromJSON a, Eq a, Show a) => a -> String -> Assertion
gitlabParseTestOne expectedHaskellValue filename = do
  raw <- BSL.readFile filename
  result <- parseOne raw
  (expectedHaskellValue == result)
    @? showWL (ansiWlEditExprCompact (ediff expectedHaskellValue result))

showWL :: WL.Doc -> String
showWL doc = WL.displayS (WL.renderSmart 0.4 80 doc) ""

gitlabParseTestMany :: (ToExpr a, FromJSON a, Eq a, Show a) => [a] -> String -> Assertion
gitlabParseTestMany expectedHaskellValue filename = do
  raw <- BSL.readFile filename
  result <- parseMany raw
  (expectedHaskellValue == result)
    @? showWL (ansiWlEditExprCompact (ediff expectedHaskellValue result))

parseOne :: FromJSON a => BSL.ByteString -> IO a
parseOne bs =
  case eitherDecode bs of
    Left err -> assertFailure err
    Right xs -> return xs

parseMany :: FromJSON a => BSL.ByteString -> IO [a]
parseMany bs =
  case eitherDecode bs of
    Left err -> assertFailure err
    Right xs -> return xs

-------------
-- Generic instances

deriving instance Generic ArchiveFormat

deriving instance Generic Member

deriving instance Generic SamlIdentity

deriving instance Generic Identity

deriving instance Generic Namespace

deriving instance Generic Links

deriving instance Generic Owner

deriving instance Generic Permissions

deriving instance Generic Project

deriving instance Generic License

deriving instance Generic ExpirationPolicy

deriving instance Generic RepositoryStorage

deriving instance Generic Statistics

deriving instance Generic User

deriving instance Generic MilestoneState

deriving instance Generic Milestone

deriving instance Generic TimeStats

deriving instance Generic Issue

deriving instance Generic Pipeline

deriving instance Generic DetailedStatus

deriving instance Generic Commit

deriving instance Generic CommitTodo

deriving instance Generic CommitStats

deriving instance Generic Tag

deriving instance Generic Release

deriving instance Generic Repository

deriving instance Generic Job

deriving instance Generic Artifact

deriving instance Generic Group

deriving instance Generic GroupShare

deriving instance Generic MergeRequest

deriving instance Generic TaskCompletionStatus

deriving instance Generic References

deriving instance Generic Change

deriving instance Generic DiffRefs

deriving instance Generic TodoAction

deriving instance Generic TodoTarget

deriving instance Generic TodoState

deriving instance Generic TodoProject

deriving instance Generic Todo

deriving instance Generic TodoTargetType

deriving instance Generic EditIssueReq

deriving instance Generic Discussion

deriving instance Generic CommitNote

deriving instance Generic Note

deriving instance Generic IssueBoard

deriving instance Generic BoardIssue

deriving instance Generic BoardIssueLabel

deriving instance Generic Visibility

deriving instance Generic TestSuite

deriving instance Generic TestCase

deriving instance Generic TimeEstimate

deriving instance Generic ProjectAvatar

deriving instance Generic Starrer

deriving instance Generic Branch

deriving instance Generic Diff

deriving instance Generic Epic

deriving instance Generic CommandsChanges

deriving instance Generic Contributor

deriving instance Generic RepositoryFile

deriving instance Generic RepositoryFileSimple

deriving instance Generic RepositoryFileBlame

deriving instance Generic Version

deriving instance Generic IssueStatistics

deriving instance Generic IssueStats

deriving instance Generic IssueCounts

deriving instance Generic TestReport

deriving instance Generic Email

deriving instance Generic Key

deriving instance Generic UserPrefs

deriving instance Generic UserStatus

deriving instance Generic UserCount

deriving instance Generic Event

deriving instance Generic EventActionName

deriving instance Generic EventTargetType

deriving instance Generic PushData

-------------
-- ToExpr instances

instance ToExpr ArchiveFormat

instance ToExpr Member

instance ToExpr SamlIdentity

instance ToExpr Identity

instance ToExpr Namespace

instance ToExpr Links

instance ToExpr Owner

instance ToExpr Permissions

instance ToExpr Project

instance ToExpr Statistics

instance ToExpr User

instance ToExpr Milestone

instance ToExpr MilestoneState

instance ToExpr TimeStats

instance ToExpr Issue

instance ToExpr Pipeline

instance ToExpr Commit

instance ToExpr CommitTodo

instance ToExpr CommitStats

instance ToExpr Tag

instance ToExpr Release

instance ToExpr Diff

instance ToExpr Repository

instance ToExpr Job

instance ToExpr Artifact

instance ToExpr Group

instance ToExpr GroupShare

instance ToExpr Branch

instance ToExpr RepositoryFile

instance ToExpr RepositoryFileSimple

instance ToExpr RepositoryFileBlame

instance ToExpr MergeRequest

instance ToExpr Todo

instance ToExpr TodoProject

instance ToExpr TodoAction

instance ToExpr TodoTarget

instance ToExpr TodoState

instance ToExpr Version

instance ToExpr EditIssueReq

instance ToExpr Discussion

instance ToExpr CommitNote

instance ToExpr Note

instance ToExpr IssueStatistics

instance ToExpr IssueStats

instance ToExpr IssueCounts

instance ToExpr IssueBoard

instance ToExpr BoardIssue

instance ToExpr BoardIssueLabel

instance ToExpr Visibility

instance ToExpr TestReport

instance ToExpr TestSuite

instance ToExpr TestCase

instance ToExpr TimeEstimate

instance ToExpr TaskCompletionStatus

instance ToExpr References

instance ToExpr Change

instance ToExpr DiffRefs

instance ToExpr DetailedStatus

instance ToExpr TodoTargetType

instance ToExpr License

instance ToExpr ExpirationPolicy

instance ToExpr RepositoryStorage

instance ToExpr Starrer

instance ToExpr ProjectAvatar

instance ToExpr Epic

instance ToExpr CommandsChanges

instance ToExpr Contributor

instance ToExpr Email

instance ToExpr Key

instance ToExpr UserPrefs

instance ToExpr UserStatus

instance ToExpr UserCount

instance ToExpr Event

instance ToExpr EventActionName

instance ToExpr EventTargetType

instance ToExpr PushData
