{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Hakyll.Convert.OutputFormat (validOutputFormat, formatPath) where

import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hakyll.Convert.Common
import System.FilePath

validOutputFormat :: T.Text -> Bool
validOutputFormat format
  | T.null format = False
  | otherwise =
      case formatPath format def of
        Just _ -> True
        Nothing -> False

formatPath :: T.Text -> DistilledPost -> Maybe T.Text
formatPath format post = T.concat <$> helper [] format
  where
    helper acc input =
      case T.uncons input of
        Just ('%', rest) ->
          case T.uncons rest of
            Just (ch, rest2) ->
              if ch `M.member` acceptableFormats
                then
                  let formatter = acceptableFormats M.! ch
                   in helper ((formatter post) : acc) rest2
                else Nothing
            Nothing -> Nothing
        Just (ch, rest) -> helper ((T.singleton ch) : acc) rest
        Nothing -> Just $ reverse acc

-- When adding a new format, don't forget to update the help message in
-- tools/hakyll-convert.hs
acceptableFormats :: M.Map Char (DistilledPost -> T.Text)
acceptableFormats =
  M.fromList
    [ -- this lets users put literal percent sign in the format)
      ('%', const $ T.singleton '%'),
      ('o', fmtOriginalPath), -- original filepath, like 2016/01/02/blog-post.html
      ('s', fmtSlug), -- original slug, i.e. "blog-post" from the example above
      ('y', fmtYear2), -- publication year, 2 digits
      ('Y', fmtYear4), -- publication year, 4 digits
      ('m', fmtMonth), -- publication month
      ('d', fmtDay), -- publication day
      ('H', fmtHour), -- publication hour
      ('M', fmtMinute), -- publication minute
      ('S', fmtSecond) -- publication second
    ]

fmtOriginalPath :: DistilledPost -> T.Text
fmtOriginalPath post =
  T.pack
    . dropTrailingSlash
    . dropExtensions
    $ chopUri
    $ T.unpack (dpUri post)
  where
    dropTrailingSlash = reverse . dropWhile (== '/') . reverse
    dropDomain path =
      -- carelessly assumes we can treat URIs like filepaths
      joinPath $
        drop 1 $ -- drop the domain
          splitPath path
    chopUri (dropPrefix "http://" -> ("", rest)) = dropDomain rest
    chopUri (dropPrefix "https://" -> ("", rest)) = dropDomain rest
    chopUri u =
      error $
        "We've wrongly assumed that blog post URIs start with http:// or https://, but we got: " ++ u

    dropPrefix :: (Eq a) => [a] -> [a] -> ([a], [a])
    dropPrefix (x : xs) (y : ys) | x == y = dropPrefix xs ys
    dropPrefix left right = (left, right)

fmtSlug :: DistilledPost -> T.Text
fmtSlug post =
  T.reverse
    . (T.takeWhile (/= '/'))
    . T.reverse
    $ fmtOriginalPath post

fmtDate :: String -> DistilledPost -> T.Text
fmtDate format = T.pack . (formatTime defaultTimeLocale format) . dpDate

fmtYear2 :: DistilledPost -> T.Text
fmtYear2 = fmtDate "%y"

fmtYear4 :: DistilledPost -> T.Text
fmtYear4 = fmtDate "%Y"

fmtMonth :: DistilledPost -> T.Text
fmtMonth = fmtDate "%m"

fmtDay :: DistilledPost -> T.Text
fmtDay = fmtDate "%d"

fmtHour :: DistilledPost -> T.Text
fmtHour = fmtDate "%H"

fmtMinute :: DistilledPost -> T.Text
fmtMinute = fmtDate "%M"

fmtSecond :: DistilledPost -> T.Text
fmtSecond = fmtDate "%S"
