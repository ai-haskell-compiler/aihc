{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- SPDX-License-Identifier: GPL-2.0-or-later

module Main (main) where

#if !MIN_VERSION_simple_cmd_args(0,1,3)
import Control.Applicative ((<|>))
#endif
import Control.Monad.Extra
import Data.Bifunctor
import qualified Data.ByteString.Char8 as B
import Data.List.Extra
import Data.Maybe
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Data.List as L
import Network.HTTP.Types
import Options.Applicative (fullDesc, header, progDescDoc,
#if !MIN_VERSION_simple_cmd_args(0,1,4)
                            many
#endif
  )
import SimpleCmd (cmd_)
import SimpleCmdArgs
import System.Directory
import System.IO

import Bugzilla
import Common
import Fields
import Help
import ParseArg
import Paths_rhbzquery
import User

data QueryMode = BugList | ListFields | ListOperators | CreateBug
               | QueryPage | APIQuery | Reverse
  deriving Eq

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  simpleCmdArgsWithMods (Just version) (fullDesc <> header "Bugzilla query tool" <> progDescDoc (Just detailedHelp)) $
    run <$>
    switchWith 'n' "dryrun" "Do not open url" <*>
    switchWith 'm' "mine" "My bugs" <*>
    strOptionalWith 's' "server" "SERVER" ("Bugzilla server [default: " ++ brc ++ "]") brc <*>
    (flagWith' ListFields 'l' "list-fields" "List query FIELDs" <|>
     flagWith' ListOperators 'o' "list-operators" "List op search operator types" <|>
     flagWith' CreateBug 'f' "file" "File a bug" <|>
     flagWith' QueryPage 'q' "query" "Open advanced query page" <|>
     flagWith' Reverse 'r' "reverse" "Convert url query to args" <|>
     flagWith BugList APIQuery 'w' "api" "Web API query") <*>
    -- FIXME should really use some and many
    many (strArg argHelp)
  where
    run :: Bool -> Bool -> String -> QueryMode -> [String] -> IO ()
    run _ _ _ ListFields _ = do
      mapM_ putStrLn allBzFields
      putStrLn $ "\nAliases: " ++ unwords (map fst fieldAliases)
      putStrLn "(also cf_ prefixes are optional)"

    run _ _ _ ListOperators _ =
      mapM_ putStrLn $ map showOpHelp operators ++
        ["", "content~ uses matches", "content!~ uses notmatches"]
    run _ _ _ Reverse [arg] =
      let args = words $ map ampersandSpace $ removeURLPrefix arg
      -- FIXME drop component= and uses aliases (summary), etc
      in putStrLn $ unwords $ map renderStatus (filter (not . ("list_id=" `isPrefixOf`)) args)
      where
        ampersandSpace c =
          if c == '&' then ' ' else c
        removeURLPrefix url =
          if '?' `elem` url then tail (dropWhile (/= '?') url) else url
        renderStatus s =
          if "bug_status=" `isPrefixOf` s
          then lower $ dropPrefix "bug_status=" s
          else s
    run _ _ _ _ [] = error' "please give an argument or --help"
    run dryrun mine server mode args = do
      user <-
        if mine
        then do
          mail <- getRhBzUser
          return [ArgParameter "assigned_to" Equals mail]
        else return []
      let argtypes = mapMaybe readBzQueryArg args
          url =
            let query =
                  case mode of
                    CreateBug -> L.nub $ concatMap argToSimpleField argtypes
                    _ ->
                      let status = [ArgParameter "bug_status" Equals "__open__" | not (hasStatusSet argtypes)]
                          advanced = [ArgParameter "format" Equals "advanced" | mode == QueryPage]
                      in L.nub $ numberMetaFields $ advanced ++ status ++ user ++ argtypes
            in queryURL server mode query
      B.putStrLn url
      unless dryrun $ do
        whenJustM (findExecutable "xdg-open") $ \xdgOpen ->
          cmd_ xdgOpen [B.unpack url]

    queryURL :: String -> QueryMode -> [(BzFields,String)] -> B.ByteString
    queryURL server mode query =
      "https://" <> B.pack server <> "/" <>
      case mode of
        APIQuery -> "rest/bug"
        BugList -> "buglist.cgi"
        CreateBug -> "enter_bug.cgi"
        QueryPage -> "query.cgi"
        ListFields -> "query.cgi" -- unused
        ListOperators -> "query.cgi" -- unused
        Reverse -> "query.cgi" -- unused
      <> renderQuery True (bzQuery query)

    hasStatusSet :: [ArgType] -> Bool
    hasStatusSet [] = False
    hasStatusSet (ArgStatusAll:_) = True
    hasStatusSet ((ArgStatusBefore _):_) = True
    hasStatusSet ((ArgStatusAfter _):_) = True
    hasStatusSet ((ArgParameter "bug_status" _ _):_) = True
    hasStatusSet ((ArgParameter "status" _ _):_) = True
    hasStatusSet (_:rest) = hasStatusSet rest

    numberMetaFields :: [ArgType] -> [(BzFields,String)]
    numberMetaFields =
      snd . foldr (\arg (i,flds) -> let (i',fld) = argToFields i arg in (i', fld ++ flds)) (0,[])

    bzQuery :: [(BzFields,String)] -> Query
    bzQuery = map (bimap (B.pack . show) (Just . B.pack))
