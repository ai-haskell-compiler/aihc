{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Web.RedHatBugzilla.Internal.Types
( BugId
, AttachmentId
, CommentId
, UserId
, EventId
, FlagId
, FlagType
, UserEmail
, Field (..)
, User (..)
, UserList (..)
, Flag (..)
, ExternalType (..)
, ExternalBug (..)
, Bug (..)
, BugList (..)
, BugIdList (..)
, Attachment (..)
, AttachmentList (..)
, Comment (..)
, CommentList (..)
, History (..)
, HistoryEvent (..)
, Change (..)
, Modification (..)
, fieldName
, searchFieldName
) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative (pure, (<$>), (<*>))
#endif
import Control.Monad (mzero)
import Data.Aeson
#if MIN_VERSION_aeson(1,0,0)
import Data.Aeson.Text
#else
import Data.Aeson.Encode
#endif
import Data.Aeson.Types
#if MIN_VERSION_aeson(2,0,0)
import Data.Aeson.Key
import qualified Data.Aeson.KeyMap as M
#else
import qualified Data.HashMap.Strict as M
#endif
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Read as TR
import qualified Data.Vector as V
import Data.Time.Clock (UTCTime(..))

type BugId        = Int
type AttachmentId = Int
type CommentId    = Int
type UserId       = Int
type EventId      = Int
type FlagId       = Int
type FlagType     = Int
type UserEmail    = T.Text

-- | A field which you can search by using 'Web.Bugzilla.searchBugs' or track
--   changes to using 'Web.Bugzilla.getHistory'. To get a human-readable name for
--   a field, use 'fieldName'.
data Field a where
  AliasField                    :: Field [T.Text]         -- Alias
  AssignedToField               :: Field UserEmail        -- Assignee
  AttachmentCreatorField        :: Field UserEmail        -- Attachment creator
  AttachmentDataField           :: Field T.Text           -- Attachment data
  AttachmentDescriptionField    :: Field T.Text           -- Attachment description
  AttachmentFilenameField       :: Field T.Text           -- Attachment filename
  AttachmentIsObsoleteField     :: Field Bool             -- Attachment is obsolete
  AttachmentIsPatchField        :: Field Bool             -- Attachment is patch
  AttachmentIsPrivateField      :: Field Bool             -- Attachment is private
  AttachmentMimetypeField       :: Field T.Text           -- Attachment mime type
  BlocksField                   :: Field Int              -- Blocks
  BugIdField                    :: Field Int              -- Bug ID
  CcField                       :: Field UserEmail        -- CC
  CcListAccessibleField         :: Field Bool             -- CC list accessible
  ClassificationField           :: Field T.Text           -- Classification
  CommentField                  :: Field T.Text           -- Comment
  CommentIsPrivateField         :: Field T.Text           -- Comment is private
  CommentTagsField              :: Field T.Text           -- Comment Tags
  CommenterField                :: Field UserEmail        -- Commenter
  ComponentField                :: Field [T.Text]         -- Component
  ContentField                  :: Field T.Text           -- Content
  CreationDateField             :: Field UTCTime          -- Creation date
  DaysElapsedField              :: Field Int              -- Days since bug changed
  DependsOnField                :: Field Int              -- Depends on
  EverConfirmedField            :: Field Bool             -- Ever confirmed
  FlagRequesteeField            :: Field UserEmail        -- Flag Requestee
  FlagSetterField               :: Field UserEmail        -- Flag Setter
  FlagsField                    :: Field T.Text           -- Flags
  GroupField                    :: Field T.Text           -- Group
  KeywordsField                 :: Field [T.Text]         -- Keywords
  ChangedField                  :: Field UTCTime          -- Changed
  CommentCountField             :: Field Int              -- Number of Comments
  OperatingSystemField          :: Field T.Text           -- OS
  HardwareField                 :: Field T.Text           -- Hardware
  PriorityField                 :: Field T.Text           -- Priority
  ProductField                  :: Field T.Text           -- Product
  QaContactField                :: Field UserEmail        -- QA Contact
  ReporterField                 :: Field UserEmail        -- Reporter
  ReporterAccessibleField       :: Field Bool             -- Reporter accessible
  ResolutionField               :: Field T.Text           -- Resolution
  RestrictCommentsField         :: Field Bool             -- Restrict Comments
  SeeAlsoField                  :: Field T.Text           -- See Also
  SeverityField                 :: Field T.Text           -- Severity
  StatusField                   :: Field T.Text           -- Status
  WhiteboardField               :: Field T.Text           -- Whiteboard
  SummaryField                  :: Field T.Text           -- Summary
  TagsField                     :: Field T.Text           -- Tags
  TargetMilestoneField          :: Field T.Text           -- Target Milestone
  TimeSinceAssigneeTouchedField :: Field Int              -- Time Since Assignee Touched
  BugURLField                   :: Field T.Text           -- URL
  VersionField                  :: Field T.Text           -- Version
  VotesField                    :: Field T.Text           -- Votes
  CustomField                   :: T.Text -> Field T.Text -- (Custom fields)

instance Eq (Field a) where
  (CustomField a) == (CustomField b) = a == b
  (CustomField _) == _               = False
  _ == (CustomField _)               = False
  a == b                             = searchFieldName a == searchFieldName b

instance Show (Field a) where
  show AliasField                    = "AliasField"
  show AssignedToField               = "AssignedToField"
  show AttachmentCreatorField        = "AttachmentCreatorField"
  show AttachmentDataField           = "AttachmentDataField"
  show AttachmentDescriptionField    = "AttachmentDescriptionField"
  show AttachmentFilenameField       = "AttachmentFilenameField"
  show AttachmentIsObsoleteField     = "AttachmentIsObsoleteField"
  show AttachmentIsPatchField        = "AttachmentIsPatchField"
  show AttachmentIsPrivateField      = "AttachmentIsPrivateField"
  show AttachmentMimetypeField       = "AttachmentMimetypeField"
  show BlocksField                   = "BlocksField"
  show BugIdField                    = "BugIdField"
  show CcField                       = "CcField"
  show CcListAccessibleField         = "CcListAccessibleField"
  show ClassificationField           = "ClassificationField"
  show CommentField                  = "CommentField"
  show CommentIsPrivateField         = "CommentIsPrivateField"
  show CommentTagsField              = "CommentTagsField"
  show CommenterField                = "CommenterField"
  show ComponentField                = "ComponentField"
  show ContentField                  = "ContentField"
  show CreationDateField             = "CreationDateField"
  show DaysElapsedField              = "DaysElapsedField"
  show DependsOnField                = "DependsOnField"
  show EverConfirmedField            = "EverConfirmedField"
  show FlagRequesteeField            = "FlagRequesteeField"
  show FlagSetterField               = "FlagSetterField"
  show FlagsField                    = "FlagsField"
  show GroupField                    = "GroupField"
  show KeywordsField                 = "KeywordsField"
  show ChangedField                  = "ChangedField"
  show CommentCountField             = "CommentCountField"
  show OperatingSystemField          = "OperatingSystemField"
  show HardwareField                 = "HardwareField"
  show PriorityField                 = "PriorityField"
  show ProductField                  = "ProductField"
  show QaContactField                = "QaContactField"
  show ReporterField                 = "ReporterField"
  show ReporterAccessibleField       = "ReporterAccessibleField"
  show ResolutionField               = "ResolutionField"
  show RestrictCommentsField         = "RestrictCommentsField"
  show SeeAlsoField                  = "SeeAlsoField"
  show SeverityField                 = "SeverityField"
  show StatusField                   = "StatusField"
  show WhiteboardField               = "WhiteboardField"
  show SummaryField                  = "SummaryField"
  show TagsField                     = "TagsField"
  show TargetMilestoneField          = "TargetMilestoneField"
  show TimeSinceAssigneeTouchedField = "TimeSinceAssigneeTouchedField"
  show BugURLField                   = "BugURLField"
  show VersionField                  = "VersionField"
  show VotesField                    = "VotesField"
  show (CustomField name)            = "CustomField " ++ show name

-- | Provides a human-readable name for a 'Field'.
fieldName :: Field a -> T.Text
fieldName AliasField                    = "Alias"
fieldName AssignedToField               = "Assigned to"
fieldName AttachmentCreatorField        = "Attachment creator"
fieldName AttachmentDataField           = "Attachment data"
fieldName AttachmentDescriptionField    = "Attachment description"
fieldName AttachmentFilenameField       = "Attachment filename"
fieldName AttachmentIsObsoleteField     = "Attachment is obsolete"
fieldName AttachmentIsPatchField        = "Attachment is patch"
fieldName AttachmentIsPrivateField      = "Attachment is private"
fieldName AttachmentMimetypeField       = "Attachment MIME type"
fieldName BlocksField                   = "Blocks"
fieldName BugIdField                    = "BugId"
fieldName CcField                       = "CC"
fieldName CcListAccessibleField         = "CC list is accessible"
fieldName ClassificationField           = "Classification"
fieldName CommentField                  = "Comment"
fieldName CommentIsPrivateField         = "Comment is private"
fieldName CommentTagsField              = "Comment tags"
fieldName CommenterField                = "Commenter"
fieldName ComponentField                = "Component"
fieldName ContentField                  = "Content"
fieldName CreationDateField             = "Creation date"
fieldName DaysElapsedField              = "Days elapsed"
fieldName DependsOnField                = "Depends on"
fieldName EverConfirmedField            = "Ever confirmed"
fieldName FlagRequesteeField            = "Flag requestee"
fieldName FlagSetterField               = "Flag setter"
fieldName FlagsField                    = "Flags"
fieldName GroupField                    = "Group"
fieldName KeywordsField                 = "Keywords"
fieldName ChangedField                  = "Changed"
fieldName CommentCountField             = "Comment count"
fieldName OperatingSystemField          = "Operating system"
fieldName HardwareField                 = "Hardware"
fieldName PriorityField                 = "Priority"
fieldName ProductField                  = "Product"
fieldName QaContactField                = "QA contact"
fieldName ReporterField                 = "Reporter"
fieldName ReporterAccessibleField       = "Reporter accessible"
fieldName ResolutionField               = "Resolution"
fieldName RestrictCommentsField         = "Restrict comments"
fieldName SeeAlsoField                  = "See also"
fieldName SeverityField                 = "Severity"
fieldName StatusField                   = "Status"
fieldName WhiteboardField               = "Whiteboard"
fieldName SummaryField                  = "Summary"
fieldName TagsField                     = "Tags"
fieldName TargetMilestoneField          = "Target milestone"
fieldName TimeSinceAssigneeTouchedField = "Time since assignee touched"
fieldName BugURLField                   = "Bug URL"
fieldName VersionField                  = "Version"
fieldName VotesField                    = "Votes"
fieldName (CustomField name)            = T.concat ["Custom field \"", T.pack (show name), "\""]

searchFieldName :: Field a -> T.Text
searchFieldName AliasField                    = "alias"
searchFieldName AssignedToField               = "assigned_to"
searchFieldName AttachmentCreatorField        = "attachments.submitter"
searchFieldName AttachmentDataField           = "attach_data.thedata"
searchFieldName AttachmentDescriptionField    = "attachments.description"
searchFieldName AttachmentFilenameField       = "attachments.filename"
searchFieldName AttachmentIsObsoleteField     = "attachments.isobsolete"
searchFieldName AttachmentIsPatchField        = "attachments.ispatch"
searchFieldName AttachmentIsPrivateField      = "attachments.isprivate"
searchFieldName AttachmentMimetypeField       = "attachments.mimetype"
searchFieldName BlocksField                   = "blocked"
searchFieldName BugIdField                    = "bug_id"
searchFieldName CcField                       = "cc"
searchFieldName CcListAccessibleField         = "cclist_accessible"
searchFieldName ClassificationField           = "classification"
searchFieldName CommentField                  = "longdesc"
searchFieldName CommentIsPrivateField         = "longdescs.isprivate"
searchFieldName CommentTagsField              = "comment_tag"
searchFieldName CommenterField                = "commenter"
searchFieldName ComponentField                = "component"
searchFieldName ContentField                  = "content"
searchFieldName CreationDateField             = "creation_ts"
searchFieldName DaysElapsedField              = "days_elapsed"
searchFieldName DependsOnField                = "dependson"
searchFieldName EverConfirmedField            = "everconfirmed"
searchFieldName FlagRequesteeField            = "requestees.login_name"
searchFieldName FlagSetterField               = "setters.login_name"
searchFieldName FlagsField                    = "flagtypes.name"
searchFieldName GroupField                    = "bug_group"
searchFieldName KeywordsField                 = "keywords"
searchFieldName ChangedField                  = "delta_ts"
searchFieldName CommentCountField             = "longdescs.count"
searchFieldName OperatingSystemField          = "op_sys"
searchFieldName HardwareField                 = "rep_platform"
searchFieldName PriorityField                 = "priority"
searchFieldName ProductField                  = "product"
searchFieldName QaContactField                = "qa_contact"
searchFieldName ReporterField                 = "reporter"
searchFieldName ReporterAccessibleField       = "reporter_accessible"
searchFieldName ResolutionField               = "resolution"
searchFieldName RestrictCommentsField         = "restrict_comments"
searchFieldName SeeAlsoField                  = "see_also"
searchFieldName SeverityField                 = "bug_severity"
searchFieldName StatusField                   = "bug_status"
searchFieldName WhiteboardField               = "status_whiteboard"
searchFieldName SummaryField                  = "short_desc"
searchFieldName TagsField                     = "tag"
searchFieldName TargetMilestoneField          = "target_milestone"
searchFieldName TimeSinceAssigneeTouchedField = "owner_idle_time"
searchFieldName BugURLField                   = "bug_file_loc"
searchFieldName VersionField                  = "version"
searchFieldName VotesField                    = "votes"
searchFieldName (CustomField name)            = name

-- | A Bugzilla user.
data User = User
  { userId       :: !UserId
  , userEmail    :: Maybe UserEmail
  , userName     :: T.Text
  , userRealName :: T.Text
  } deriving (Eq, Ord, Show)

instance FromJSON User where
  parseJSON (Object v) =
    User <$> v .: "id"
         <*> v .:? "email"
         <*> v .: "name"
         <*> v .: "real_name"
  parseJSON _ = mzero

newtype UserList = UserList [User]
                deriving (Eq, Show)

instance FromJSON UserList where
  parseJSON (Object v) = UserList <$> v .: "users"
  parseJSON _          = mzero

-- | Flags, which may be set on an attachment or on a bug directly.
data Flag = Flag
  { flagId               :: !FlagId
  , flagTypeId           :: !FlagType
  , flagName             :: T.Text
  , flagSetter           :: UserEmail
  , flagStatus           :: T.Text
  , flagCreationDate     :: UTCTime
  , flagModificationDate :: UTCTime
  , flagRequestee        :: Maybe UserEmail
  } deriving (Eq, Ord, Show)

instance FromJSON Flag where
  parseJSON (Object v) =
    Flag <$> v .: "id"
         <*> v .: "type_id"
         <*> v .: "name"
         <*> v .: "setter"
         <*> v .: "status"
         <*> v .: "creation_date"
         <*> v .: "modification_date"
         <*> v .:? "requestee"
  parseJSON _ = mzero

-- | An external bug type
data ExternalType = ExternalType
  { externalTypeDescription :: T.Text
  , externalTypeUrl         :: T.Text
  , externalTypeId          :: Int
  , externalTypeType        :: T.Text
  , externalTypeFullUrl     :: T.Text
  } deriving (Eq, Ord, Show)

instance FromJSON ExternalType where
  parseJSON (Object v) =
    ExternalType <$> v .: "description"
                 <*> v .: "url"
                 <*> v .: "id"
                 <*> v .: "type"
                 <*> v .: "full_url"
  parseJSON _ = mzero

-- | An external bug.
data ExternalBug = ExternalBug
  { externalDescription    :: T.Text
  , externalBzId           :: Int
  , externalPriority       :: T.Text
  , externalBugId          :: T.Text
  , externalStatus         :: T.Text
  , externalId             :: Int
  , externalType           :: ExternalType
  } deriving (Eq, Ord, Show)

instance FromJSON ExternalBug where
  parseJSON (Object v) =
    ExternalBug <$> v .: "ext_description"
                <*> v .: "ext_bz_id"
                <*> v .: "ext_priority"
                <*> v .: "ext_bz_bug_id"
                <*> v .: "ext_status"
                <*> v .: "id"
                <*> v .: "type"
  parseJSON _ = mzero

-- | A Bugzilla bug.
data Bug = Bug
  { bugId                  :: !BugId
  , bugAlias               :: Maybe [T.Text]
  , bugAssignedTo          :: UserEmail
  , bugAssignedToDetail    :: User
  , bugBlocks              :: [BugId]
  , bugCc                  :: [UserEmail]
  , bugCcDetail            :: [User]
  , bugClassification      :: T.Text
  , bugComponent           :: [T.Text]
  , bugCreationTime        :: UTCTime
  , bugCreator             :: UserEmail
  , bugCreatorDetail       :: User
  , bugDependsOn           :: [BugId]
  , bugDupeOf              :: Maybe BugId
  , bugFlags               :: Maybe [Flag]
  , bugGroups              :: [T.Text]
  , bugIsCcAccessible      :: Bool
  , bugIsConfirmed         :: Bool
  , bugIsCreatorAccessible :: Bool
  , bugIsOpen              :: Bool
  , bugKeywords            :: [T.Text]
  , bugLastChangeTime      :: UTCTime
  , bugOpSys               :: T.Text
  , bugPlatform            :: T.Text
  , bugPriority            :: T.Text
  , bugProduct             :: T.Text
  , bugQaContact           :: UserEmail
  , bugResolution          :: T.Text
  , bugSeverity            :: T.Text
  , bugStatus              :: T.Text
  , bugSummary             :: T.Text
  , bugTargetMilestone     :: T.Text
  , bugUrl                 :: T.Text
  , bugVersion             :: [T.Text]
  , bugWhiteboard          :: T.Text
  , bugCustomFields        ::
#if MIN_VERSION_aeson(2,0,0)
                              M.KeyMap T.Text
#else
                              M.HashMap T.Text T.Text
#endif
, bugExternalBugs        :: Maybe [ExternalBug]
  } deriving (Eq, Show)

instance FromJSON Bug where
  parseJSON (Object v) =
      Bug <$> v .: "id"
          <*> v .:? "alias"
          <*> v .: "assigned_to"
          <*> v .: "assigned_to_detail"
          <*> v .: "blocks"
          <*> v .: "cc"
          <*> v .: "cc_detail"
          <*> v .: "classification"
          <*> v .: "component"
          <*> v .: "creation_time"
          <*> v .: "creator"
          <*> v .: "creator_detail"
          <*> v .: "depends_on"
          <*> v .:? "dupe_of"
          <*> v .:? "flags"
          <*> v .: "groups"
          <*> v .: "is_cc_accessible"
          <*> v .: "is_confirmed"
          <*> v .: "is_creator_accessible"
          <*> v .: "is_open"
          <*> v .: "keywords"
          <*> v .: "last_change_time"
          <*> v .: "op_sys"
          <*> v .: "platform"
          <*> v .: "priority"
          <*> v .: "product"
          <*> v .: "qa_contact"
          <*> v .: "resolution"
          <*> v .: "severity"
          <*> v .: "status"
          <*> v .: "summary"
          <*> v .: "target_milestone"
          <*> v .: "url"
          <*> v .: "version"
          <*> v .: "whiteboard"
          <*> pure (customFields v)
          <*> v .:? "external_bugs"
  parseJSON _ = mzero

customFields :: Object ->
#if MIN_VERSION_aeson(2,0,0)
                M.KeyMap T.Text
#else
                M.HashMap T.Text T.Text
#endif
customFields = M.map stringifyCustomFields
             . M.filterWithKey filterCustomFields
  where
    stringifyCustomFields :: Value -> T.Text
    stringifyCustomFields (String t) = t
    stringifyCustomFields v          = T.concat
                                     . TL.toChunks
                                     . TLB.toLazyText
                                     . encodeToTextBuilder
                                     . toJSON
                                     $ v

    filterCustomFields k _ = "cf_" `T.isPrefixOf` toText k
#if !MIN_VERSION_aeson(2,0,0)
      where toText = id
#endif

newtype BugList = BugList [Bug]
               deriving (Eq, Show)

instance FromJSON BugList where
  parseJSON (Object v) = BugList <$> v .: "bugs"
  parseJSON _          = mzero

newtype BugIdList = BugIdList [BugId]
                 deriving (Eq, Show)

instance FromJSON BugIdList where
  parseJSON (Object v) = do
    bugs <- v .: "bugs"
    bugIds <- mapM (.: "id") bugs
    return $ BugIdList bugIds
  parseJSON _          = mzero

-- | An attachment to a bug.
data Attachment = Attachment
  { attachmentId             :: !AttachmentId
  , attachmentBugId          :: !BugId
  , attachmentFileName       :: T.Text
  , attachmentSummary        :: T.Text
  , attachmentCreator        :: UserEmail
  , attachmentIsPrivate      :: Bool
  , attachmentIsObsolete     :: Bool
  , attachmentIsPatch        :: Bool
  , attachmentFlags          :: [Flag]
  , attachmentCreationTime   :: UTCTime
  , attachmentLastChangeTime :: UTCTime
  , attachmentContentType    :: T.Text
  , attachmentSize           :: !Int
  , attachmentData           :: T.Text
  } deriving (Eq, Show)

instance FromJSON Attachment where
  parseJSON (Object v) =
    Attachment <$> v .: "id"
               <*> v .: "bug_id"
               <*> v .: "file_name"
               <*> v .: "summary"
               <*> v .: "creator"
               <*> (fromNumericBool <$> v .: "is_private")
               <*> (fromNumericBool <$> v .: "is_obsolete")
               <*> (fromNumericBool <$> v .: "is_patch")
               <*> v .: "flags"
               <*> v .: "creation_time"
               <*> v .: "last_change_time"
               <*> v .: "content_type"
               <*> v .: "size"
               <*> v .: "data"
  parseJSON _ = mzero

fromNumericBool :: Int -> Bool
fromNumericBool 0 = False
fromNumericBool _ = True

newtype AttachmentList = AttachmentList [Attachment]
                      deriving (Eq, Show)

instance FromJSON AttachmentList where
  parseJSON (Object v) = do
    attachmentsVal <- v .: "attachments"
    bugsVal <- v .: "bugs"
    case (attachmentsVal, bugsVal) of
      (Object (M.toList -> [(_, as)]), _) -> AttachmentList . (:[]) <$> parseJSON as
      (_, Object (M.toList -> [(_, as)])) -> AttachmentList <$> parseJSON as
      _                                   -> mzero
  parseJSON _ = mzero

-- | A bug comment. To display these the way Bugzilla does, you'll
-- need to call 'getUser' and use the 'userRealName' for each user.
data Comment = Comment
  { commentId           :: !CommentId
  , commentBugId        :: !BugId
  , commentAttachmentId :: Maybe AttachmentId
  , commentCount        :: !Int
  , commentText         :: T.Text
  , commentCreator      :: UserEmail
  , commentCreationTime :: UTCTime
  , commentIsPrivate    :: Bool
  } deriving (Eq, Show)

instance FromJSON Comment where
  parseJSON (Object v) =
    Comment <$> v .: "id"
            <*> v .: "bug_id"
            <*> v .: "attachment_id"
            <*> v .: "count"
            <*> v .: "text"
            <*> v .: "creator"
            <*> v .: "creation_time"
            <*> v .: "is_private"
  parseJSON _ = mzero

newtype CommentList = CommentList [Comment]
                   deriving (Eq, Show)

instance FromJSON CommentList where
  parseJSON (Object v) = do
    bugsVal <- v .: "bugs"
    case bugsVal of
      Object (M.toList -> [(_, cs)]) ->
        do comments <- withObject "comments" (.: "comments") cs
           withArray "comment list" (\a -> CommentList <$> parseJSON (addCount a)) comments
      _ -> mzero
  parseJSON _ = mzero

-- Note that we make the (possibly unwise) assumption that Bugzilla
-- returns the comments in order. If it turns out that's not true, we
-- can always sort by their 'id' to ensure correct results.
addCount :: V.Vector Value -> Value
addCount vs = Array $ V.zipWith addCount' (V.enumFromN 0 $ V.length vs) vs
 where
   addCount' :: Int -> Value -> Value
   addCount' c (Object v) = Object $ M.insert "count" (Number $ fromIntegral c) v
   addCount' _ v          = v

-- | History information for a bug.
data History = History
  { historyBugId   :: !BugId
  , historyEvents  :: [HistoryEvent]
  } deriving (Eq, Show)

instance FromJSON History where
  parseJSON (Object v) = do
    bugsVal <- v .: "bugs"
    case bugsVal of
      Array (V.toList -> [history]) ->
        withObject "history"
                   (\h -> History <$> h .: "id"
                                  <*> parseHistoryEvents h)
                   history
      _ -> mzero
  parseJSON _ = mzero

parseHistoryEvents :: Object -> Parser [HistoryEvent]
parseHistoryEvents h = do
  events <- h .: "history"
  withArray "event list" (parseJSON . addCount) events

-- | An event in a bug's history.
data HistoryEvent = HistoryEvent
  { historyEventId      :: EventId   -- ^ A sequential event id.
  , historyEventTime    :: UTCTime   -- ^ When the event occurred.
  , historyEventUser    :: UserEmail -- ^ Which user was responsible.
  , historyEventChanges :: [Change]  -- ^ All the changes which are
                                     --   part of this event.
  } deriving (Eq, Show)

instance FromJSON HistoryEvent where
  parseJSON (Object v) =
    HistoryEvent <$> v .: "count"
                 <*> v .: "when"
                 <*> v .: "who"
                 <*> v .: "changes"
  parseJSON _ = mzero

-- | A single change which is part of an event. Different constructors
--   are used according to the type of the field. The 'Modification'
--   describes the value of the field before and after the change.
data Change
  = TextFieldChange (Field T.Text) (Modification T.Text)
  | ListFieldChange (Field [T.Text]) (Modification [T.Text])
  | IntFieldChange (Field Int) (Modification Int)
  | TimeFieldChange (Field UTCTime) (Modification UTCTime)
  | BoolFieldChange (Field Bool) (Modification Bool)
    deriving (Eq, Show)

instance FromJSON Change where
  parseJSON (Object v) = do
    changedField <- v .: "field_name"
    case changedField of
      "alias"                  -> ListFieldChange AliasField <$> parseModification v
      "assigned_to"            -> TextFieldChange AssignedToField <$> parseModification v
      "attachments.submitter"  -> TextFieldChange AttachmentCreatorField <$> parseModification v
      "attach_data.thedata"    -> TextFieldChange AttachmentDataField <$> parseModification v
      "attachments.description"-> TextFieldChange AttachmentDescriptionField <$> parseModification v
      "attachments.filename"   -> TextFieldChange AttachmentFilenameField <$> parseModification v
      "attachments.isobsolete" -> BoolFieldChange AttachmentIsObsoleteField <$> parseModification v
      "attachments.ispatch"    -> BoolFieldChange AttachmentIsPatchField <$> parseModification v
      "attachments.isprivate"  -> BoolFieldChange AttachmentIsPrivateField <$> parseModification v
      "attachments.mimetype"   -> TextFieldChange AttachmentMimetypeField <$> parseModification v
      "blocks"                 -> IntFieldChange BlocksField <$> parseModification v
      "bug_id"                 -> IntFieldChange BugIdField <$> parseModification v
      "cc"                     -> TextFieldChange CcField <$> parseModification v
      "is_cc_accessible"       -> BoolFieldChange CcListAccessibleField <$> parseModification v
      "classification"         -> TextFieldChange ClassificationField <$> parseModification v
      "component"              -> ListFieldChange ComponentField <$> parseModification v
      "content"                -> TextFieldChange ContentField <$> parseModification v
      "creation_time"          -> TimeFieldChange CreationDateField <$> parseModification v
      "days_elapsed"           -> IntFieldChange DaysElapsedField <$> parseModification v
      "depends_on"             -> IntFieldChange DependsOnField <$> parseModification v
      "everconfirmed"          -> BoolFieldChange EverConfirmedField <$> parseModification v
      "flagtypes.name"         -> TextFieldChange FlagsField <$> parseModification v
      "bug_group"              -> TextFieldChange GroupField <$> parseModification v
      "keywords"               -> ListFieldChange KeywordsField <$> parseModification v
      "op_sys"                 -> TextFieldChange OperatingSystemField <$> parseModification v
      "platform"               -> TextFieldChange HardwareField <$> parseModification v
      "priority"               -> TextFieldChange PriorityField <$> parseModification v
      "product"                -> TextFieldChange ProductField <$> parseModification v
      "qa_contact"             -> TextFieldChange QaContactField <$> parseModification v
      "reporter"               -> TextFieldChange ReporterField <$> parseModification v
      "reporter_accessible"    -> BoolFieldChange ReporterAccessibleField <$> parseModification v
      "resolution"             -> TextFieldChange ResolutionField <$> parseModification v
      "restrict_comments"      -> BoolFieldChange RestrictCommentsField <$> parseModification v
      "see_also"               -> TextFieldChange SeeAlsoField <$> parseModification v
      "severity"               -> TextFieldChange SeverityField <$> parseModification v
      "status"                 -> TextFieldChange StatusField <$> parseModification v
      "whiteboard"             -> TextFieldChange WhiteboardField <$> parseModification v
      "summary"                -> TextFieldChange SummaryField <$> parseModification v
      "tag"                    -> TextFieldChange TagsField <$> parseModification v
      "target_milestone"       -> TextFieldChange TargetMilestoneField <$> parseModification v
      "url"                    -> TextFieldChange BugURLField <$> parseModification v
      "version"                -> TextFieldChange VersionField <$> parseModification v
      "votes"                  -> TextFieldChange VotesField <$> parseModification v
      name                     -> TextFieldChange (CustomField name) <$> parseModification v
  parseJSON _ = mzero

-- | A description of how a field changed during a 'HistoryEvent'.
data (Eq a, Show a) => Modification a = Modification
  { modRemoved      :: Maybe a
  , modAdded        :: Maybe a
  , modAttachmentId :: Maybe AttachmentId
  } deriving (Eq, Show)

parseModification :: (FromJSON a, Eq b, Show b, ToModification a b) => Object -> Parser (Modification b)
parseModification v = Modification <$> (toMod =<< v .: "removed")
                                   <*> (toMod =<< v .: "added")
                                   <*> v .:? "attachment_id"

class ToModification a b | b -> a where toMod :: a -> Parser (Maybe b)
instance ToModification T.Text T.Text where toMod = return . Just
instance ToModification UTCTime UTCTime where toMod = return . Just

instance ToModification T.Text Int where
  toMod v | v == "" = return Nothing
          | otherwise = case TR.decimal v of
                          Left _       -> mzero
                          Right (i, _) -> return $ Just i

instance ToModification T.Text Bool where
  toMod v | v == "0"  = return $ Just False
          | v == "1"  = return $ Just True
          | otherwise = mzero

instance ToModification T.Text [T.Text] where
  toMod v = return . Just $ T.splitOn ", " v
