{-# LANGUAGE OverloadedStrings #-}

-- | This package is designed to provide an easy-to-use, typesafe
--   interface to querying Bugzilla from Haskell.
--
--   A modified version of Web.Bugzilla to support
--   the list fields in Red Hat's modified bugzilla API.
--
--   A very simple program using this package might look like this:
--
-- >   let session = anonymousSession "https://bugzilla.redhat.com"
-- >       user = "me@example.org"
-- >       query = AssignedToField .==. user .&&.
-- >               FlagRequesteeField .==. user .&&.
-- >               (FlagsField `contains` "review" .||. FlagsField `contains` "feedback")
-- >   bugs <- searchBugs session query
-- >   mapM_ (putStrLn . show . bugSummary) bugs
--
--   There's a more in-depth demo program included with the
--   source code to this package.

module Web.RedHatBugzilla
( -- * Connecting to Bugzilla
  apikeySession
, anonymousSession

, BugzillaServer
, BugzillaSession (..)
, BugzillaApiKey (..)

  -- * Querying Bugzilla
, searchBugs
, searchBugsAll
, searchBugs'
, searchBugsWithLimit
, searchBugsAllWithLimit
, searchBugsWithLimit'
, getBug
, getBugAll
, getAttachment
, getAttachments
, getComments
, getHistory
, searchUsers
, getUser
, getUserById
, newBzRequest
, sendBzRequest
, intAsText

, Request
, BugId
, AttachmentId
, CommentId
, UserId
, EventId
, FlagId
, FlagType
, UserEmail
, Field (..)
, User (..)
, Flag (..)
, Bug (..)
, ExternalBug (..)
, ExternalType (..)
, Attachment (..)
, Comment (..)
, History (..)
, HistoryEvent (..)
, Change (..)
, Modification (..)
, fieldName

, BugzillaException (..)
) where

import Control.Exception (throw)
import Data.Aeson (FromJSON)
import qualified Data.Text as T

import Web.RedHatBugzilla.Internal.Network
import Web.RedHatBugzilla.Internal.Search
import Web.RedHatBugzilla.Internal.Types

-- | Creates a 'BugzillaSession' using the provided api key.
apikeySession :: BugzillaServer -> BugzillaApiKey -> BugzillaSession
apikeySession = ApiKeySession

-- | Creates an anonymous 'BugzillaSession'. Note that some content
--   will be hidden by Bugzilla when you make queries in this state.
anonymousSession :: BugzillaServer -> BugzillaSession
anonymousSession = AnonymousSession

intAsText :: Int -> T.Text
intAsText = T.pack . show

-- | Searches Bugzilla and returns a list of 'Bug's. The 'SearchExpression'
-- can be constructed using the operators in "Web.Bugzilla.Search".
searchBugs :: BugzillaSession -> SearchExpression -> IO [Bug]
searchBugs session search = do
  BugList bugs <- doSearchBugs session search Nothing Nothing
  return bugs

-- | Similar to 'searchBugs', but return _all fields.
searchBugsAll :: BugzillaSession -> SearchExpression -> IO [Bug]
searchBugsAll session search = do
  BugList bugs <- doSearchBugs session search (Just "_all") Nothing
  return bugs

-- | Like 'searchBugs', but returns a list of 'BugId's. You can
-- retrieve the 'Bug' for each 'BugId' using 'getBug'. The combination
-- of 'searchBugs'' and 'getBug' is much less efficient than
-- 'searchBugs'. 'searchBugs'' is suitable for cases where you won't need to call
-- 'getBug' most of the time - for example, polling to determine whether the
-- set of bugs returned by a query has changed.
searchBugs' :: BugzillaSession -> SearchExpression -> IO [BugId]
searchBugs' session search = do
  BugIdList bugs <- doSearchBugs session search (Just "id") Nothing
  return bugs

doSearchBugs :: FromJSON a => BugzillaSession -> SearchExpression -> Maybe T.Text -> Maybe (Int, Int) -> IO a
doSearchBugs session search includeField limits = do
  let fieldsQuery = case includeField of
        Nothing -> []
        Just field -> [("include_fields", Just field)]
      limitQuery = case limits of
        Nothing -> []
        Just (limit, offset) -> [("limit", Just $ intAsText limit),
                                 ("offset", Just $ intAsText offset)]
      searchQuery = evalSearchExpr search
      req = newBzRequest session ["bug"] (limitQuery ++ fieldsQuery ++ searchQuery)
  sendBzRequest req

-- | Search Bugzilla and returns a limited number of results. You can
--   call this repeatedly and use 'offset' to retrieve the results of
--   a large query incrementally. Note that most Bugzillas won't
--   return all of the results for a very large query by default, but
--   you can request this by calling 'searchBugsWithLimit' with 0 for
--   the limit.
searchBugsWithLimit :: BugzillaSession
                    -> Int  -- ^ The maximum number of results to return.
                    -> Int  -- ^ The offset from the first result to start from.
                    -> SearchExpression
                    -> IO [Bug]
searchBugsWithLimit session limit offset search = do
  BugList bugs <- doSearchBugs session search Nothing (Just (limit, offset))
  return bugs

-- | Similar to 'searchBugsWithLimit', but return _all fields.
searchBugsAllWithLimit :: BugzillaSession
                       -> Int  -- ^ The maximum number of results to return.
                       -> Int  -- ^ The offset from the first result to start from.
                       -> SearchExpression
                       -> IO [Bug]
searchBugsAllWithLimit session limit offset search = do
  BugList bugs <- doSearchBugs session search (Just "_all") (Just (limit, offset))
  return bugs

-- | Like 'searchBugsWithLimit', but returns a list of 'BugId's. See
-- 'searchBugs'' for more discussion.
searchBugsWithLimit' :: BugzillaSession
                     -> Int  -- ^ The maximum number of results to return.
                     -> Int  -- ^ The offset from the first result to start from.
                     -> SearchExpression
                     -> IO [BugId]
searchBugsWithLimit' session limit offset search = do
  BugIdList bugs <- doSearchBugs session search (Just "id") (Just (limit, offset))
  return bugs

-- | Retrieve a bug by bug number.
getBug :: BugzillaSession -> BugId -> IO (Maybe Bug)
getBug session bid = getBugIncludeFields session bid []

-- | Retrieve all bug field by bug number
getBugAll :: BugzillaSession -> BugId -> IO (Maybe Bug)
getBugAll session bid = getBugIncludeFields session bid ["_all"]

-- | Retrieve a bug by bug number with fields
getBugIncludeFields :: BugzillaSession -> BugId -> [T.Text] -> IO (Maybe Bug)
getBugIncludeFields session bid includeFields = do
  let req = newBzRequest session ["bug", intAsText bid] query
  (BugList bugs) <- sendBzRequest req
  case bugs of
    [bug] -> return $ Just bug
    []    -> return Nothing
    _     -> throw $ BugzillaUnexpectedValue
                     "Request for a single bug returned multiple bugs"
  where
    query = map (\f -> ("include_fields", Just f)) includeFields

-- | Retrieve a bug by attachment number.
getAttachment :: BugzillaSession -> AttachmentId -> IO (Maybe Attachment)
getAttachment session aid = do
  let req = newBzRequest session ["bug", "attachment", intAsText aid] []
  (AttachmentList as) <- sendBzRequest req
  case as of
    [a] -> return $ Just a
    []  -> return Nothing
    _   -> throw $ BugzillaUnexpectedValue
                   "Request for a single attachment returned multiple attachments"

-- | Get all attachments for a bug.
getAttachments :: BugzillaSession -> BugId -> IO [Attachment]
getAttachments session bid = do
  let req = newBzRequest session ["bug", intAsText bid, "attachment"] []
  (AttachmentList as) <- sendBzRequest req
  return as

-- | Get all comments for a bug.
getComments :: BugzillaSession -> BugId -> IO [Comment]
getComments session bid = do
  let req = newBzRequest session ["bug", intAsText bid, "comment"] []
  (CommentList as) <- sendBzRequest req
  return as

-- | Get the history for a bug.
getHistory :: BugzillaSession -> BugId -> IO History
getHistory session bid = do
  let req = newBzRequest session ["bug", intAsText bid, "history"] []
  sendBzRequest req

-- | Search user names and emails using a substring search.
searchUsers :: BugzillaSession -> T.Text -> IO [User]
searchUsers session text = do
  let req = newBzRequest session ["user"] [("match", Just text)]
  (UserList users) <- sendBzRequest req
  return users

-- | Get a user by email.
getUser :: BugzillaSession -> UserEmail -> IO (Maybe User)
getUser session user = do
  let req = newBzRequest session ["user", user] []
  (UserList users) <- sendBzRequest req
  case users of
    [u] -> return $ Just u
    []  -> return Nothing
    _   -> throw $ BugzillaUnexpectedValue
                   "Request for a single user returned multiple users"

-- | Get a user by user ID.
getUserById :: BugzillaSession -> UserId -> IO (Maybe User)
getUserById session uid = do
  let req = newBzRequest session ["user", intAsText uid] []
  (UserList users) <- sendBzRequest req
  case users of
    [u] -> return $ Just u
    []  -> return Nothing
    _   -> throw $ BugzillaUnexpectedValue
                   "Request for a single user returned multiple users"
