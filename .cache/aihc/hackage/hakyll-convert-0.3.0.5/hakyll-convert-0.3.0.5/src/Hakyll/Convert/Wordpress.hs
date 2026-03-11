{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Convert.Wordpress (readPosts, distill) where

import Data.Maybe
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, parseTimeM, rfc822DateFormat)
import Data.XML.Types (Element (..), Name (..), elementChildren, elementText)
import Hakyll.Convert.Common
import Text.RSS.Import
import Text.RSS.Syntax
import qualified Text.XML as XML

-- | Returns only public posts
readPosts :: FilePath -> IO (Maybe [RSSItem])
readPosts f = do
  doc <- XML.readFile (XML.def :: XML.ParseSettings) f
  let root = XML.toXMLElement $ XML.documentRoot doc
  return $ fmap select (elementToRSS root)
  where
    select = filter isPublished . rssItems . rssChannel

isPublished :: RSSItem -> Bool
isPublished i = "publish" `elem` getStatus i

distill :: Bool -> RSSItem -> DistilledPost
distill extractComments item =
  DistilledPost
    { dpTitle = rssItemTitle item,
      dpBody = body,
      dpUri = link,
      dpTags = tags,
      dpCategories = categories,
      dpDate = date
    }
  where
    body =
      if extractComments
        then
          T.intercalate
            "\n"
            [ content,
              "",
              "<h3 id='hakyll-convert-comments-title'>Comments</h3>",
              comments
            ]
        else content
    link = fromMaybe "" (rssItemLink item)
    content = T.unlines (map strContent contentTags)
    categories = rssCategoriesOfType "category"
    tags = rssCategoriesOfType "post_tag"
    contentTags = concatMap (findElements contentTag) (rssItemOther item)
    rssCategoriesOfType ty =
      [ rssCategoryValue c
        | c <- rssItemCategories item,
          rssCategoryDomain c == Just ty
      ]
    contentTag =
      Name
        { nameLocalName = "encoded",
          nameNamespace = Just "http://purl.org/rss/1.0/modules/content/",
          namePrefix = Just "content"
        }
    comments = T.intercalate "\n" $ map formatComment commentTags
    commentTags = rssItemOther item >>= findElements commentTag
    commentTag = wordpressTag "comment"
    --
    date = case parseTime' =<< rssItemPubDate item of
      Nothing -> fromJust $ parseTime' "Thu, 01 Jan 1970 00:00:00 UTC"
      Just d -> d
    parseTime' d =
      parseTimeM True defaultTimeLocale rfc822DateFormat (T.unpack d)

-- ---------------------------------------------------------------------
-- helpers
-- ---------------------------------------------------------------------

formatComment :: Element -> T.Text
formatComment commentElement =
  T.intercalate
    "\n"
    [ "<div class='hakyll-convert-comment'>",
      T.concat
        [ "<p class='hakyll-convert-comment-date'>",
          "On ",
          pubdate,
          ", ",
          author,
          " wrote:",
          "</p>"
        ],
      "<div class='hakyll-convert-comment-body'>",
      comment,
      "</div>",
      "</div>"
    ]
  where
    pubdate = fromMaybe "unknown date" $ findField "comment_date"
    author = fromMaybe "unknown author" $ findField "comment_author"
    comment = fromMaybe "" $ findField "comment_content"
    findField name =
      strContent <$> findChild (wordpressTag name) commentElement

wordpressTag :: T.Text -> Name
wordpressTag name =
  Name
    { nameLocalName = name,
      nameNamespace = Just "http://wordpress.org/export/1.2/",
      namePrefix = Just "wp"
    }

getStatus :: RSSItem -> [T.Text]
getStatus item =
  map strContent statusTags
  where
    statusTags = concatMap (findElements (wordpressTag "status")) (rssItemOther item)

-- | Find all non-nested elements which are named `name`, starting with `root`.
-- ("Non-nested" means we don't search sub-elements of an element that's named
-- `name`.)
findElements :: Name -> Element -> [Element]
findElements name element =
  if elementName element == name
    then [element]
    else concatMap (findElements name) (elementChildren element)

-- | Find first immediate child of `root` which is named `name`.
findChild :: Name -> Element -> Maybe Element
findChild name element =
  let subelements = elementChildren element
      matching = filter (\child -> elementName child == name) subelements
   in listToMaybe matching

-- | The contents of the element (ignoring non-text sub-elements).
strContent :: Element -> T.Text
strContent element = T.concat $ elementText element
