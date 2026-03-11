-- hdav.hs: WebDAV client
-- Copyright © 2012-2020  Clint Adams
--
-- vim: softtabstop=4:shiftwidth=4:expandtab
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

import Paths_DAV (version)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.UTF8 as BU
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>),(<*>), pure)
#endif
import Control.Applicative (optional)
import Control.Monad (liftM2, unless)
import Control.Monad.IO.Class (MonadIO)
import Data.Version (showVersion)
import Data.Monoid ((<>))

import Network.Socket (withSocketsDo)
import Network.URI (normalizePathSegments)

import Network.Protocol.HTTP.DAV (DAVT, evalDAVT, setCreds, setDepth, setUserAgent, getPropsM, getContentM, putContentM, putPropsM, delContentM, moveContentM, mkCol, Depth(..), caldavReportM, withLockIfPossible, withLockIfPossibleForDelete)

import Options.Applicative.Builder (argument, command, help, idm, info, long, metavar, option, prefs, progDesc, showHelpOnError, str, strOption, subparser, auto)
import Options.Applicative.Extra (customExecParser)
import Options.Applicative.Types (Parser)

import System.Console.Haskeline (runInputT, getPassword)
import qualified System.Console.Haskeline as SCH

import Text.XML (renderLBS, def)

data Options = Options {
    url :: String
  , url2 :: String
  , username :: Maybe String
  , password :: Maybe String
  , username2 :: Maybe String
  , password2 :: Maybe String
}

data Command = Copy Options | Move Options | Delete Options | MakeCollection Options | GetProps Options (Maybe Depth) | Put FilePath Options | CaldavReport Options

oneUUP :: Parser Options
oneUUP = Options
    <$> argument str ( metavar "URL" )
    <*> pure ""
    <*> (optional $ strOption
        ( long "username"
       <> metavar "USERNAME"
       <> help "username for URL" ))
    <*> (optional $ strOption
        ( long "password"
       <> metavar "PASSWORD"
       <> help "password for URL" ))
    <*> pure Nothing
    <*> pure Nothing

twoUUP :: Parser Options
twoUUP = Options
    <$> argument str ( metavar "SOURCEURL" )
    <*> argument str ( metavar "TARGETURL" )
    <*> (optional $ strOption
        ( long "source-username"
       <> metavar "USERNAME"
       <> help "username for source URL" ))
    <*> (optional $ strOption
        ( long "source-password"
       <> metavar "PASSWORD"
       <> help "password for source URL" ))
    <*> (optional $ strOption
        ( long "target-username"
       <> metavar "USERNAME"
       <> help "username for target URL" ))
    <*> (optional $ strOption
        ( long "target-password"
       <> metavar "PASSWORD"
       <> help "password for target URL" ))

twoUoneUP :: Parser Options
twoUoneUP = Options
    <$> argument str ( metavar "SOURCEURL" )
    <*> argument str ( metavar "TARGETURL" )
    <*> (optional $ strOption
        ( long "username"
       <> metavar "USERNAME"
       <> help "username for URL" ))
    <*> (optional $ strOption
        ( long "password"
       <> metavar "PASSWORD"
       <> help "password for URL" ))
    <*> pure Nothing
    <*> pure Nothing

doCopy :: Options -> IO ()
doCopy o = do
     (sourceUsername, sourcePassword) <- ingestCreds ("source URL " ++ sourceurl) (username o) (password o)
     (targetUsername, targetPassword) <- ingestCreds ("target URL " ++ targeturl) (username2 o) (password2 o)
     (p, b) <- runDAV sourceurl $ setCreds sourceUsername sourcePassword >> setDepth (Just Depth0) >> withLockIfPossible True (liftM2 (,) getPropsM getContentM)
     runDAV targeturl $ setCreds targetUsername targetPassword >> withLockIfPossible False (putContentM b >> putPropsM p)
     where
         sourceurl = url o
         targeturl = url2 o

doDelete :: Options -> IO ()
doDelete o = do
    (u, p) <- ingestCreds ("URL " ++ url o) (username o) (password o)
    runDAV (url o) $ setCreds u p >> withLockIfPossibleForDelete False delContentM

doMove :: Options -> IO ()
doMove o = do
    (u, p) <- ingestCreds ("URL " ++ url o) (username o) (password o)
    runDAV (url o) $ setCreds u p >> moveContentM (BU.fromString $ url2 o)

doMakeCollection :: Options -> IO ()
doMakeCollection o = do
    (u, p) <- ingestCreds ("URL " ++ url o) (username o) (password o)
    go (url o) (u, p)
  where
     go url' (u, p) = do
       ok <- makeCollection url' (u, p)
       unless ok $ do
         go (parent url') (u, p)
         ok' <- makeCollection url' (u, p)
         unless ok' $
           error $ "failed creating " ++ url'

     parent url' = reverse $ dropWhile (== '/')$ reverse $
        normalizePathSegments (url' ++ "/..")

     makeCollection url' (u, p) = runDAV url' $ setCreds u p >> mkCol

doGetProps :: Options -> Maybe Depth -> IO ()
doGetProps o md = do
    (u, p) <- ingestCreds ("URL " ++ url o) (username o) (password o)
    doc <- runDAV (url o) $ setCreds u p >> setDepth md >> getPropsM
    BL.putStr (renderLBS def doc)

doPut :: FilePath -> Options -> IO ()
doPut file o = do
    (u, p) <- ingestCreds ("URL " ++ url o) (username o) (password o)
    bs <- BL.readFile file
    runDAV (url o) $ setCreds u p >> putContentM (Nothing, bs)

doReport :: Options -> IO ()
doReport o = do
    (u, p) <- ingestCreds ("URL " ++ url o) (username o) (password o)
    doc <- runDAV (url o) $ setCreds u p >> setDepth (Just Depth1) >> caldavReportM
    BL.putStr (renderLBS def doc)

dispatch :: Command -> IO ()
dispatch (Copy o) = doCopy o
dispatch (Move o) = doMove o
dispatch (Delete o) = doDelete o
dispatch (MakeCollection o) = doMakeCollection o
dispatch (GetProps o md) = doGetProps o md
dispatch (Put f o) = doPut f o
dispatch (CaldavReport o) = doReport o

main :: IO ()
main = withSocketsDo $ do
    putStrLn $ concat [
      "hDAV version ", showVersion version, ", Copyright (C) 2012-2016  Clint Adams\n",
      "hDAV comes with ABSOLUTELY NO WARRANTY.\n",
      "This is free software, and you are welcome to redistribute it\n",
      "under certain conditions.\n"]

    customExecParser (prefs showHelpOnError) (info cmd idm) >>= dispatch

cmd :: Parser Command
cmd = subparser
  ( command "copy" (info ( Copy <$> twoUUP ) ( progDesc "Copy props and data from one location to another" ))
 <> command "delete" (info ( Delete <$> oneUUP ) ( progDesc "Delete props and data" ))
 <> command "getprops" (info ( GetProps <$> oneUUP <*> (optional depth))  ( progDesc "Fetch props and output them to stdout" ))
 <> command "makecollection" (info ( MakeCollection <$> oneUUP ) ( progDesc "Make a new collection" ))
 <> command "move" (info ( Move <$> twoUoneUP ) ( progDesc "Move props and data from one location to another in the same DAV space" ))
 <> command "put" (info ( Put <$> argument str ( metavar "FILE" ) <*> oneUUP )  ( progDesc "Put file to URL" ))

 <> command "caldav-report" (info ( CaldavReport <$> oneUUP )  ( progDesc "Get CalDAV report" ))
  )
 where
   depth = option auto ( long "depth" <> metavar "DEPTH" <> help "depth" ) :: Parser Depth

runDAV :: MonadIO m => String -> DAVT m a -> m a
runDAV u f = evalDAVT u (setUserAgent (BU.fromString $ "hDAV/" ++ showVersion version) >> f) >>= \x -> case x of
    Left e -> error e
    Right r -> return r

ingestCreds :: String -> Maybe String -> Maybe String -> IO (B.ByteString, B.ByteString)
ingestCreds _ Nothing _ = return (B.empty, B.empty)
ingestCreds _ (Just u) (Just p) = return (BU.fromString u, BU.fromString p)
ingestCreds q (Just u) Nothing = do
    mnewp <- runInputT SCH.defaultSettings $ getPassword (Just '*') (concat ["Password for ", u, " at ", q, ": "])
    return (BU.fromString u, maybe B.empty BU.fromString mnewp)
