{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Monad
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime)
import System.Environment (getArgs)
import System.IO

import Web.RedHatBugzilla
import Web.RedHatBugzilla.Search

main :: IO ()
main = dispatch Nothing =<< getArgs

dispatch :: Maybe BugzillaServer -> [String] -> IO ()
dispatch Nothing ("--server" : server : as) = dispatch (Just $ T.pack server) as
dispatch s ["--assigned-to", user]          = withBz s $ doAssignedTo (T.pack user)
dispatch s ["--assigned-to-brief", user]    = withBz s $ doAssignedToBrief (T.pack user)
dispatch s ["--requests", user]             = withBz s $ doRequests (T.pack user)
dispatch s ["--history", bug, n]            = withBz s $ doHistory (read bug) (read n)
dispatch _ _                                = usage

usage :: IO ()
usage = hPutStrLn stderr "Connection options:"
     >> hPutStrLn stderr "  --server [domain name] - REQUIRED. The Bugzilla server to access."
     >> hPutStrLn stderr ""
     >> hPutStrLn stderr "Bugzilla queries:"
     >> hPutStrLn stderr "  --assigned-to [user email] - List bugs assigned to the user."
     >> hPutStrLn stderr "  --assigned-to [user email] - List bugs assigned to the user."
     >> hPutStrLn stderr "  --requests [user email]    - List requests for the user."
     >> hPutStrLn stderr "  --history [bug number] [n] - List the most recent 'n' changes to the bug."

withBz :: Maybe BugzillaServer -> (BugzillaSession -> IO ())
       -> IO ()
withBz mServer f = do
  let server = case mServer of
                 Just s  -> s
                 Nothing -> error "Please specify a server with '--server'"
  f $ anonymousSession server


doAssignedTo :: UserEmail -> BugzillaSession -> IO ()
doAssignedTo user session = do
    let search = AssignedToField .==. user
    bugs <- searchBugs session search
    mapM_ showBug bugs
  where
    showBug Bug {..} = putStrLn $ show bugId ++ ": " ++ show bugSummary
                                 ++ " [" ++ T.unpack bugStatus ++ ": " ++ T.unpack bugResolution ++ "] Updated: "
                                 ++ show bugLastChangeTime

doAssignedToBrief :: UserEmail -> BugzillaSession -> IO ()
doAssignedToBrief user session = do
    let search = AssignedToField .==. user
    bugs <- searchBugs' session search
    mapM_ print bugs

doRequests :: UserEmail -> BugzillaSession -> IO ()
doRequests user session = do
    let needinfoSearch = FlagRequesteeField .==. user .&&. FlagsField `contains` "needinfo"
    needinfoBugs <- searchBugs session needinfoSearch
    mapM_ showNeedinfo needinfoBugs

    let reviewSearch = FlagRequesteeField .==. user .&&.
                       (FlagsField `contains` "review" .||. FlagsField `contains` "feedback")
    reviewBugs <- map bugId <$> searchBugs session reviewSearch
    forM_ reviewBugs $ \rBugId -> do
      attachments <- getAttachments session rBugId
      mapM_ showReview $ filter (any hasReviewFlag . attachmentFlags) attachments
      mapM_ showFeedback $ filter (any hasFeedbackFlag . attachmentFlags) attachments

  where
    showNeedinfo Bug {..} = do
      let flags = filter hasNeedinfoFlag $ fromMaybe [] bugFlags
      forM_ flags $ \flag ->
        putStrLn $ "[NEEDINFO] " ++ show bugId ++ ": " ++ show bugSummary
                ++ " (" ++ show (flagSetter flag) ++ " " ++ show (flagCreationDate flag) ++ ")"

    showReview Attachment {..} =
      putStrLn $ "[REVIEW] " ++ show attachmentBugId ++ ": " ++ show attachmentSummary
              ++ " (" ++ show attachmentCreator ++ " " ++ show attachmentCreationTime ++ ")"

    showFeedback Attachment {..} =
      putStrLn $ "[FEEDBACK] " ++ show attachmentBugId ++ ": " ++ show attachmentSummary
              ++ " (" ++ show attachmentCreator ++ " " ++ show attachmentCreationTime ++ ")"

    hasNeedinfoFlag f = flagRequestee f == Just user && flagName f == "needinfo"
    hasReviewFlag f   = flagRequestee f == Just user && flagName f == "review"
    hasFeedbackFlag f = flagRequestee f == Just user && flagName f == "feedback"

doHistory :: BugId -> Int -> BugzillaSession -> IO ()
doHistory bug count session = do
    comments <- getComments session bug
    history <- getHistory session bug
    recentEventsRev <- takeRecent count (reverse comments)
                                        (reverse $ historyEvents history)
    mapM_ putStrLn (reverse recentEventsRev)
  where
    takeRecent :: Int -> [Comment] -> [HistoryEvent] -> IO [String]
    takeRecent 0 _ _ = return []
    takeRecent n (c:cs) (e:es)
      | commentCreationTime c `diffUTCTime` historyEventTime e >= 0 = (:) <$> showComment c
                                                                          <*> takeRecent (n - 1) cs (e:es)
      | otherwise                                                   = (:) <$> showEvent e
                                                                          <*> takeRecent (n - 1) (c:cs) es
    takeRecent n cs@(_:_) [] = mapM showComment $ take n cs
    takeRecent n [] es@(_:_) = mapM showEvent $ take n es
    takeRecent _ [] []       = return []

    -- FIXME: showComment and showEvent will call getUser for the same
    -- user over and over again. You should never do this in a real application.
    showComment Comment {..} = do
      user <- getUser session commentCreator
      let commentUserRealName = maybe commentCreator userRealName user
      let commentUserEmail = fromMaybe commentCreator $ userEmail =<< user
      return $ "(Comment " ++ show commentCount ++ ") " ++ T.unpack commentUserRealName
            ++ " <" ++ T.unpack commentUserEmail ++ "> " ++ show commentCreationTime
            ++ "\n" ++ (unlines . map ("  " ++) . lines . T.unpack $ commentText)

    showEvent HistoryEvent {..} = do
      user <- getUser session historyEventUser
      let eventUserRealName = maybe historyEventUser userRealName user
      let eventUserEmail = fromMaybe historyEventUser $ userEmail =<< user
      return $ "(Event " ++ show historyEventId ++ ") " ++ T.unpack eventUserRealName
            ++ " <" ++ T.unpack eventUserEmail ++ ">\n"
            ++ concatMap showChange historyEventChanges

    showChange (TextFieldChange f (Modification r a aid)) = showChange' f r a aid
    showChange (ListFieldChange f (Modification r a aid)) = showChange' f r a aid
    showChange (IntFieldChange f (Modification r a aid))  = showChange' f r a aid
    showChange (TimeFieldChange f (Modification r a aid)) = showChange' f r a aid
    showChange (BoolFieldChange f (Modification r a aid)) = showChange' f r a aid

    showChange' f r a aid = "  " ++ showField f ++ ": "
                         ++ showMod r ++ " -> " ++ showMod a
                         ++ showAid aid ++ "\n"

    showField = T.unpack . fieldName

    showMod :: Show a => Maybe a -> String
    showMod (Just v) = show v
    showMod Nothing  = "___"

    showAid :: Maybe AttachmentId -> String
    showAid (Just aid) = " (Attachment " ++ show aid ++ ")"
    showAid Nothing    = ""
