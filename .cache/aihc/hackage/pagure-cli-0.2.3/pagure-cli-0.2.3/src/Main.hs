{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative (
#if !MIN_VERSION_base(4,8,0)
                            (<$>), (<*>),
#endif
#if !MIN_VERSION_simple_cmd_args(0,1,3)
                            (<|>)
#endif
                           )
import Control.Monad.Extra (unless, when, whenJust)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Types
#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.KeyMap as M
#else
import qualified Data.HashMap.Lazy as M
#endif
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Vector as V
import Data.Yaml (encode)
import Network.HTTP.Query ((+/+))
import SimpleCmd (error', (+-+))
import SimpleCmdArgs
import Fedora.Pagure

import Paths_pagure_cli (version)

main :: IO ()
main =
  simpleCmdArgs (Just version) "Pagure client" "Simple pagure CLI" $
  subcommands
  [ Subcommand "list" "list projects" $
    listProjects
    <$> serverOpt
    <*> countOpt
    <*> nohideOpt
    <*> formatOpt
    <*> forksOpt
    <*> optional namespaceOpt
    <*> optional packagerOpt
    <*> optional (strArg "PATTERN")
  , Subcommand "user" "list user repos" $
    userRepos
    <$> serverOpt
    <*> countOpt
    <*> switchWith 'f' "forks" "List user's forks"
    <*> strArg "USER"
  , Subcommand "group" "list group repos" $
    groupProjects
    <$> serverOpt
    <*> switchWith 'c' "count" "Count projects"
    <*> strArg "GROUP"
  , Subcommand "project" "show project details" $
    projectInfo
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "PROJECT"
  , Subcommand "branches" "list project branches" $
    repoBranches
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "REPO"
  , Subcommand "git-url" "show project repo's git urls" $
    gitUrl
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "REPO"
  , Subcommand "contributors" "show project repo's contributors" $
    contributors
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "REPO"
  , Subcommand "issues" "list project issues" $
    projectIssues
    <$> serverOpt
    <*> countOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "REPO"
    <*> switchWith 'A' "all" "list Open and Closed issues"
    <*> optional (strOptionWith 'a' "author" "AUTHOR" "Filter issues by creator")
    <*> optional (strOptionWith 'S' "since" "Y-M-D" "Filter issues updated after date")
    <*> optional (strOptionWith 't' "title" "pattern" "Filter issues by title")
  , Subcommand "issue" "show project issue" $
    projectIssue
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "REPO"
    <*> argumentWith auto "ISSUE"
  , Subcommand "users" "list users" $
    users
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "PATTERN"
  , Subcommand "username" "fullname of user" $
    username
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "USERNAME"
  , Subcommand "userinfo" "show user details" $
    userInfo
    <$> serverOpt
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "USERNAME"
  , Subcommand "groups" "list groups" $
    groups
    <$> serverOpt
    <*> countOpt
    <*> nohideOpt
    <*> formatOpt
    <*> optional (strArg "PATTERN")
  , Subcommand "groupinfo" "show group details" $
    groupInfo
    <$> serverOpt
    <*> switchWith 'p' "projects" "List projects"
    <*> nohideOpt
    <*> formatOpt
    <*> strArg "GROUP"
  ]
  where
    countOpt = switchWith 'c' "count" "Show number only"
    nohideOpt = switchWith 'e' "show-empty" "Display empty lists and objects"
    formatOpt = flagWith' FormatJson 'j' "json" "Output JSON" <|> flagWith FormatDefault FormatYaml 'y' "yaml" "YAML output"
    namespaceOpt = strOptionWith 'n' "namespace" "NAMESPACE" "Specify project repo namespace"
    packagerOpt = Owner <$> ownerOpt <|> Committer <$> usernameOpt
    usernameOpt = strOptionWith 'u' "username" "USERNAME" "Projects to which username can commit"
    ownerOpt = strOptionWith 'o' "owner" "OWNER" "Projects with certain owner"
    serverOpt = strOptionalWith 's' "server" "SERVER" "Pagure server" srcFedoraprojectOrg
    forksOpt = flagWith' OnlyForks 'F' "only-forks" "Only list forks" <|>
               flagWith NoForks IncludeForks 'f' "include-forks" "Include forks [default: ignore forks]"

data OutputFormat = FormatDefault | FormatJson | FormatYaml

data Packager = Owner String | Committer String

data Forks = NoForks | IncludeForks | OnlyForks

srcFedoraprojectOrg :: String
srcFedoraprojectOrg = "src.fedoraproject.org"

defaultPrinter :: Bool -> OutputFormat -> (Object -> IO ()) -> Object -> IO ()
defaultPrinter _ FormatDefault pr = pr
defaultPrinter nohide fmt _ = yamlPrinter nohide fmt

listProjects :: String -> Bool -> Bool -> OutputFormat -> Forks -> Maybe String
             -> Maybe Packager -> Maybe String -> IO ()
listProjects server count nohide format forks mnamespace mpackager mpattern = do
  unless (count || isJust mpackager || isJust mpattern) $
    error' "Please give a package pattern, --count, or --owner/--username"
  let path = "projects"
      params = makeKey "short" "1" ++ fork ++ packager ++ maybeKey "namespace" mnamespace ++ maybeKey "pattern" mpattern
  pages <- queryPagureCountPaged server count path params ("pagination", "page")
  let projects = concatMap (lookupKey' "projects") pages
  mapM_ (defaultPrinter nohide format printProject) projects
  where
    -- (!orphan only works on pagure >=0.29)
    packager = case mpackager of
      Nothing -> boolKey "owner" (server == srcFedoraprojectOrg) "!orphan"
      Just (Owner o) -> makeKey "owner" o
      Just (Committer c) -> makeKey "username" c

    fork = case forks of
      NoForks -> makeKey "fork" "0"
      IncludeForks -> []
      OnlyForks -> makeKey "fork" "1"

    printProject :: Object -> IO ()
    printProject project = do
      let key' = if isJust mnamespace then "name" else "fullname"
      whenJust (lookupKey key' project) T.putStrLn

-- FIXME duplicates subset of listProjects
userRepos :: String -> Bool -> Bool -> String -> IO ()
userRepos server count forks user =
  if count then do
    let path = "user" +/+ user
    mcnt <- queryPagureCount server path [] $
            if forks then "forks_pagination" else "repos_pagination"
    print $
      fromMaybe
      (error' ("number of" +-+ (if forks then "forks" else "repos") +-+ "could not be determined"))
      mcnt
    else do
    repos <- (if forks then pagureUserForks else pagureUserRepos) server user
    mapM_ T.putStrLn repos

boolKey :: String -> Bool -> String -> Query
boolKey _ False _ = []
boolKey k True val = makeKey k val

-- FIXME limit max number of issues
projectIssues :: String -> Bool -> Bool -> OutputFormat -> String -> Bool
              -> Maybe String -> Maybe String -> Maybe String -> IO ()
projectIssues server count nohide format repo allstatus mauthor msince mpat = do
  let path = repo +/+ "issues"
      params = [makeItem "status" "all" | allstatus] ++
               maybeKey "author" mauthor ++ maybeKey "since" msince
  pages <- queryPagureCountPaged server count path params ("pagination", "page")
  mapM_ (defaultPrinter nohide format printIssues) pages
  where
    printIssues :: Object -> IO ()
    printIssues result = do
      let issues = lookupKey' "issues" result :: [Object]
      mapM_ printIssue issues

    printIssue :: Object -> IO ()
    printIssue issue = do
      let mfields = parseIssue issue
      case mfields of
        Nothing -> putStrLn "parsing issue failed"
        Just (id',title,status) ->
          when (isNothing mpat || T.pack (fromJust mpat) `T.isInfixOf` title) $
          putStrLn $ "https://" ++ server +/+ repo +/+ "issue" +/+ show id' +-+ "(" ++ T.unpack status ++ "):" +-+ T.unpack title

    parseIssue :: Object -> Maybe (Integer, Text, Text)
    parseIssue =
      parseMaybe $ \obj -> do
        id' <- obj .: "id"
        title <- obj .: "title"
        status <- obj .: "status"
        return (id',title,status)

repoBranches :: String -> Bool -> OutputFormat -> String -> IO ()
repoBranches server nohide format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo +/+ "git/branches"
  eres <- queryPagureSingle server path []
  either error' (defaultPrinter nohide format (printKeyList "branches")) eres

users :: String -> Bool -> OutputFormat -> String -> IO ()
users server nohide format pat = do
  let path = "users"
      params = makeKey "pattern" pat
  res <- queryPagure server path params
  defaultPrinter nohide format (printKeyList "users") res

username :: String -> Bool -> OutputFormat -> String -> IO ()
username server nohide format user = do
  let path = "user" +/+ user
  eres <- queryPagureSingle server path $ makeKey "per_page" "1"
  case eres of
    Left err -> error' err
    Right res -> defaultPrinter nohide format printName res
  where
    printName res =
      case lookupKey "user" res >>= lookupKey "fullname" of
        Nothing -> error' "User fullname not found"
        Just fn -> T.putStrLn fn

groups :: String -> Bool -> Bool -> OutputFormat -> Maybe String -> IO ()
groups server count nohide format mpat = do
  let path = "groups"
      params = maybeKey "pattern" mpat
  pages <- queryPagureCountPaged server count path params ("pagination", "page")
  mapM_ (defaultPrinter nohide format (printKeyList "groups")) pages

printKeyList :: String -> Object -> IO ()
printKeyList key' res =
  mapM_ T.putStrLn (lookupKey' (T.pack key') res :: [Text])

gitUrl :: String -> Bool -> OutputFormat -> String -> IO ()
gitUrl server nohide format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo +/+ "git/urls"
  res <- queryPagure server path []
  defaultPrinter nohide format printURLs res
  where
    printURLs result =
      mapM_ T.putStrLn $
      M.elems $ localLookupKey "urls" result

    localLookupKey :: String -> Object ->
#if MIN_VERSION_aeson(2,0,0)
                      M.KeyMap Text
#else
                      M.HashMap Text Text
#endif
    localLookupKey = lookupKey' . T.pack

contributors :: String -> Bool -> OutputFormat -> String -> IO ()
contributors server nohide format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo +/+ "contributors"
  res <- queryPagure server path []
  yamlPrinter nohide format res

yamlPrinter :: Bool -> OutputFormat -> Object -> IO ()
yamlPrinter nohide format =
  case format of
    FormatDefault -> yamlPrinter nohide FormatYaml
    FormatJson -> BL.putStrLn . encodePretty . hideEmpty
    FormatYaml -> B.putStrLn . encode . hideEmpty
  where
    hideEmpty = if nohide then id else cleanObject

cleanObject :: Object -> Object
cleanObject = M.mapMaybe go
  where
    go :: Value -> Maybe Value
    -- drop empty lists
    go (Array v) | V.null v = Nothing
                 | otherwise = Just $ Array (V.map (wrap go) v)
                 where
                   wrap f (Object o) = Object (M.mapMaybe f o)
                   wrap f (Array a)  = Array (V.map (wrap f) a)
                   wrap _ x          = x
    -- drop empty objects
    go (Object o) =
        let cleaned = M.mapMaybe go o
        in if M.null cleaned then Nothing else Just (Object cleaned)
    go other = Just other

projectInfo :: String -> Bool -> OutputFormat -> String -> IO ()
projectInfo server nohide format repo = do
  let namespace =
        if server == srcFedoraprojectOrg && '/' `notElem` repo
        then "rpms/" else ""
      path = namespace ++ repo
  eval <- pagureProjectInfo server path
  either error' (yamlPrinter nohide format) eval

projectIssue :: String -> Bool -> OutputFormat -> String -> Int -> IO ()
projectIssue server nohide format repo issue = do
  eval <- pagureProjectIssueInfo server repo issue
  either error' (yamlPrinter nohide format) eval

userInfo :: String -> Bool -> OutputFormat -> String -> IO ()
userInfo server nohide format user = do
  eval <- pagureUserInfo server user []
  either error' (yamlPrinter nohide format) eval

groupInfo :: String -> Bool -> Bool -> OutputFormat -> String -> IO ()
groupInfo server projects nohide format group = do
  let params = [makeItem "projects" "1" | projects]
  eval <- pagureGroupInfo server group params
  either error' (yamlPrinter nohide format) eval

-- FIXME support acl parameter (admin, commit or ticket)
groupProjects :: String -> Bool -> String -> IO ()
groupProjects server count group =
  pagureGroupRepos server count group >>=
  mapM_ T.putStrLn
