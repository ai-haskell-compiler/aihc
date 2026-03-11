{-# LANGUAGE CPP #-}

module URL (
  (+//+),
  URL(..),
  renderUrl,
  FileDir(..),
  removeSubpath
) where

import Data.List.Extra (dropSuffix, dropWhileEnd)

newtype URL = URL String
  deriving Eq

data FileDir = File | Dir
  deriving Eq

renderUrl :: FileDir -> URL -> String
renderUrl Dir (URL url) = trailingSlash url
renderUrl File (URL url) = url

foldSlash :: [String] -> String
foldSlash = foldr (+/+) ""

infixr 5 +//+
(+//+) :: URL -> [String] -> URL
URL base +//+ rel =
  URL $ base +/+ foldSlash rel

-- piecesToPath :: [String] -> String
-- piecesToPath =

-- unsafeURI :: T.Text -> URI
-- unsafeURI u =
--   case mkURI u of
--     Nothing -> error $ "failed to parse url: " ++ show u
--     Just uri -> uri

-- from http-directory
infixr 5 +/+
(+/+) :: String -> String -> String
s +/+ t =
  case (s,t) of
    ("",_) -> t
    (_,"") -> s
    (_,_) -> dropWhileEnd (== '/') s ++ '/' : dropWhile (== '/') t

removeSubpath :: [String] -> String -> String
removeSubpath path url =
  let url' = (if last url == '/' then init else id) url
  in dropSuffix (foldSlash path) url'

-- from http-directory
trailingSlash :: String -> String
trailingSlash "" = ""
trailingSlash str =
  if last str == '/' then str else str ++ "/"
