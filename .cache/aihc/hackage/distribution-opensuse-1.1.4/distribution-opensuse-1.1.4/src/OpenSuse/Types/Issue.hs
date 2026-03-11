{-# LANGUAGE DeriveGeneric #-}

module OpenSuse.Types.Issue
  ( Issue(..), parseIssue, parseCve, parseBsc, showIssue
  , isCve, isBsc
  )
  where

import OpenSuse.Prelude

import Data.Aeson
import Data.List ( isSuffixOf )
import Data.Text ( unpack )
import Text.Read

data Issue = Bsc Natural
           | Cve Natural Natural
  deriving (Show, Eq, Ord, Generic)

instance Hashable Issue
instance Binary Issue
instance NFData Issue

parseIssue :: String -> Issue
parseIssue ('C':'V':'E':'-':cve) = parseCve cve
parseIssue ('b':'s':'c':'#':bsc) = parseBsc bsc
parseIssue bsc                   = parseBsc bsc

showIssue :: Issue -> String
showIssue (Cve y n) = "CVE-" ++ show y ++ "-" ++ show n
showIssue (Bsc n) = "bsc#" ++ show n

parseCve :: String -> Issue
parseCve cve = case break (=='-') cve of
                 (y,'-':n) -> Cve (safeRead "cve-year" y) (safeRead "cve-number" n)
                 _         -> error ("malformed CVE: " ++ show cve)

parseBsc :: String -> Issue     -- TODO: https://gitlab.suse.de/l3ms/smelt/issues/184
parseBsc bsc = Bsc (safeRead "bsc number" (stripFixmeSuffix bsc))

instance FromJSON Issue where
  parseJSON = withText "Issue ID" (return . parseIssue . unpack)

instance FromJSONKey Issue where
  fromJSONKey = FromJSONKeyText (parseIssue . unpack)

safeRead :: Read a => String -> String -> a
safeRead ctx x = fromMaybe (error ("invalid " ++ ctx ++ ": " ++ show x)) (readMaybe x)

stripFixmeSuffix :: String -> String
stripFixmeSuffix x
  | "_FIXME" `isSuffixOf` x = reverse . drop 6 . reverse $ x
  | otherwise               = x

isCve :: Issue -> Bool
isCve (Cve _ _) = True
isCve _ = False

isBsc :: Issue -> Bool
isBsc (Bsc _) = True
isBsc _ = False
