module MajorVer (
  MajorVer(..),
  readCompactMajor,
  readMajor,
  showMajor,
  showCompact,
  snapMajorVer,
  MajorVerAlias(..),
  eitherReadMajorAlias
  )
where

import Data.Char (isDigit)
import Data.List.Extra
import Numeric.Natural (Natural)
import SimpleCmd (error', (+-+))
import Text.Read (readMaybe)

-- FIXME allow specific snapshots?
data MajorVer = LTS Natural | Nightly
  deriving (Eq, Ord)

maybeReadMajor :: String -> Maybe MajorVer
maybeReadMajor "nightly" = Just Nightly
maybeReadMajor ver
  | "nightly-" `isPrefixOf` ver =
      case dropPrefix "nightly-" ver of
        ds ->
          if null ds
          then error' $ "bad major:" +-+ ver
          else
            if all (\d -> isDigit d || d == '-') ds
            then Just Nightly
            else error' $ "bad major version:" +-+ ver
maybeReadMajor ver
  | "lts" `isPrefixOf` ver =
      case dropPrefix "-" (dropPrefix "lts" ver) of
        vs ->
          if null vs
          then error' $ "bad major:" +-+ ver
          else
            if all (\d -> isDigit d || d == '.') vs
            then Just (LTS $ read $ takeWhile isDigit vs)
            else error' $ "bad major version:" +-+ ver
maybeReadMajor "ghc" = Nothing
maybeReadMajor ver
  | "ghc" `isPrefixOf` ver =
      case dropPrefix "-" $ dropPrefix "ghc" ver of
        "9.10" -> Just (LTS 24)
        "9.8" -> Just (LTS 23)
        "9.6" -> Just (LTS 22)
        "9.4" -> Just (LTS 21)
        "9.2" -> Just (LTS 20)
        "9.0" -> Just (LTS 19)
        "8.10" -> Just (LTS 18)
        "8.8" -> Just (LTS 16)
        "8.6" -> Just (LTS 14)
        "8.4" -> Just (LTS 12)
        "8.2" -> Just (LTS 11)
        "8.0" -> Just (LTS 9)
        "7.10" -> Just (LTS 6)
        "7.8" -> Just (LTS 2)
        ds ->
          if all (\d -> isDigit d || d == '.') ds
          then error' $ "bad major version:" +-+ ver
          else Nothing
  | otherwise = Nothing

-- readMajor "lts-16"
readMajor :: String -> MajorVer
readMajor "nightly" = Nightly
readMajor ver =
  case maybeReadMajor ver of
    Just s -> s
    Nothing ->
      error' $! "couldn't parse " ++ ver ++ " (expected lts*, nightly*, ghc-X.Y or ghcX.Y)"

-- readCompactMajor "lts16"
-- Should we support stack-lts.yaml or even stack-<minor>.yaml?
readCompactMajor :: String -> Maybe MajorVer
readCompactMajor "nightly" = Just Nightly
readCompactMajor ver =
  if "lts" `isPrefixOf` ver then
    case readMaybe (dropPrefix "lts" ver) of
      Just major -> Just (LTS major)
      Nothing -> error' $! "couldn't parse compact " ++ ver ++  " (expected ltsXX)"
  else Nothing

showMajor :: MajorVer -> String
showMajor Nightly = "nightly"
showMajor (LTS ver) = "lts-" ++ show ver

showCompact :: MajorVer -> String
showCompact = filter (/= '-') . showMajor

snapMajorVer :: String -> MajorVer
snapMajorVer snap =
  case breakOn "." snap of
    (major,_suf) -> readMajor major

---- MajorVerAlias

data MajorVerAlias = LatestLTS | MajorVer MajorVer

maybeReadMajorAlias ::  String -> Maybe MajorVerAlias
maybeReadMajorAlias "lts" = Just LatestLTS
maybeReadMajorAlias v = MajorVer <$> maybeReadMajor v

eitherReadMajorAlias :: String -> Either String MajorVerAlias
eitherReadMajorAlias cs =
  case maybeReadMajorAlias cs of
    Just s -> Right s
    _ -> Left cs
