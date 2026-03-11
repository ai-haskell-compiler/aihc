{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Hakyll.Convert.Blogger (FullPost (..), readPosts, distill) where

import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.XML.Types (Element (..), Name (..), Node (..), elementChildren)
import Hakyll.Convert.Common
import Text.Atom.Feed
import Text.Atom.Feed.Import
import qualified Text.XML as XML

-- | A post and its comments
data FullPost = FullPost
  { fpPost :: Entry,
    fpComments :: [Entry],
    fpUri :: T.Text
  }
  deriving (Show)

-- | An entry is assumed to be either a post, or a comment.
--   If it's a post, it should be associated with the URI
--   that visitors would use to read the post (on the old blog)
--   If it's a comment, it should be the URI for the corresponding
--   post.
data BloggerEntry
  = Post {beUri_ :: T.Text, beEntry :: Entry}
  | Comment {beUri_ :: T.Text, beEntry :: Entry}
  | Orphan {beEntry :: Entry}
  deriving (Show)

beUri :: BloggerEntry -> Maybe T.Text
beUri (Orphan _) = Nothing
beUri (Post u _) = Just u
beUri (Comment u _) = Just u

-- ---------------------------------------------------------------------
-- Feed to helper type
-- ---------------------------------------------------------------------

-- | Returns only published posts
readPosts :: FilePath -> IO (Maybe [FullPost])
readPosts f = do
  doc <- XML.readFile (XML.def :: XML.ParseSettings) f
  let root = XML.toXMLElement $ XML.documentRoot doc
  return $ fmap (extractPosts . feedEntries) $ elementFeed $ deleteDrafts root

-- has to be done on the XML level as our atom lib doesn't understand
-- the blogger-specific XML for drafts
deleteDrafts :: Element -> Element
deleteDrafts e =
  e {elementNodes = filter isInnocent (elementNodes e)}
  where
    isInnocent (NodeElement element) = not (isDraft element)
    isInnocent _ = True

isDraft :: Element -> Bool
isDraft e =
  not . null $ findElements draft e
  where
    draft =
      Name
        { nameLocalName = "draft",
          nameNamespace = Just "http://purl.org/atom/app#",
          namePrefix = Just "app"
        }

-- | Warning: this silently ignores orphans, templates, settings
extractPosts :: [Entry] -> [FullPost]
extractPosts entries =
  map toFullPost blocks
  where
    toFullPost (uri, blockEntries) =
      FullPost
        { fpPost = post,
          fpComments = comments,
          fpUri = uri
        }
      where
        post = case [e | Post _ e <- blockEntries] of
          [] -> huh "Block of entries with no post?!"
          [p] -> p
          _ps -> huh "Block of entries with more than one post?!"
        comments = [e | Comment _ e <- blockEntries]
        huh msg = error . unlines $ msg : map (txtToString . entryTitle . beEntry) blockEntries
    blocks = [(u, xs) | (Just u, xs) <- blocks_] -- drop orphans
    blocks_ =
      buckets beUri $
        map identifyEntry $
          filter isInteresting entries

-- | Contains actual meat (posts, comments; but not eg. templates)
isInteresting :: Entry -> Bool
isInteresting e =
  not $ any isBoring cats
  where
    isBoring c = any (\t -> isBloggerCategoryOfType t c) ["settings", "template"]
    cats = entryCategories e

-- | Tag an entry from the blogger feed as either being a post,
--   a comment, or an "orphan" (a comment without an associated post)
identifyEntry :: Entry -> BloggerEntry
identifyEntry e =
  if isPost e
    then case getLink "self" `mplus` getLink "alternate" of
      Just l -> Post (postUrl l) e
      Nothing -> entryError e oopsSelf
    else case getLink "alternate" of
      Just l -> Comment (postUrl l) e
      Nothing -> Orphan e
  where
    isPost = any (isBloggerCategoryOfType "post") . entryCategories
    postUrl = T.takeWhile (/= '?') . replaceSchema . linkHref
    replaceSchema url = maybe url (T.append "https://") (T.stripPrefix "http://" url)
    getLink ty = case filter (isLink ty) $ entryLinks e of
      [] -> Nothing
      [x] -> Just x
      _xs -> entryError e (oopsLink ty)
    isLink ty l = linkRel l == Just (Right ty) && linkType l == Just "text/html"
    oopsSelf = "Was expecting blog posts to have a self link"
    oopsLink = T.append "Was expecting entries have at most one link of type "

isBloggerCategory :: Category -> Bool
isBloggerCategory =
  (== Just "http://schemas.google.com/g/2005#kind")
    . catScheme

isBloggerCategoryOfType ::
  -- | \"comment\", \"post\", etc
  T.Text ->
  Category ->
  Bool
isBloggerCategoryOfType ty c =
  isBloggerCategory c
    && catTerm c == T.append "http://schemas.google.com/blogger/2008/kind#" ty

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

distill :: Bool -> FullPost -> DistilledPost
distill extractComments fp =
  DistilledPost
    { dpBody = body fpost,
      dpUri = fpUri fp,
      dpTitle = title fpost,
      dpTags = tags fpost,
      dpCategories = [],
      dpDate = date fpost
    }
  where
    fpost = fpPost fp
    fcomments = fpComments fp
    --
    body post =
      let article = fromContent $ entryContent post
          comments = T.intercalate "\n" $ map formatComment fcomments
       in if extractComments
            then
              T.intercalate
                "\n"
                [ article,
                  "",
                  "<h3 id='hakyll-convert-comments-title'>Comments</h3>",
                  comments
                ]
            else article

    fromContent (Just (HTMLContent x)) = x
    fromContent _ = error "Hakyll.Convert.Blogger.distill expecting HTML"

    formatComment c =
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
        pubdate = fromMaybe "unknown date" (entryPublished c)
        author = T.unwords $ map personName (entryAuthors c)
        comment = fromContent $ entryContent c
    --
    title p = case txtToString (entryTitle p) of
      "" -> Nothing
      t -> Just (T.pack t)
    tags =
      map catTerm
        . filter (not . isBloggerCategory)
        . entryCategories
    date x = case parseTime' =<< entryPublished x of
      Nothing -> fromJust $ parseTime' "1970-01-01T00:00:00Z"
      Just d -> d
    parseTime' d =
      msum $
        map
          (\f -> parseTimeM True defaultTimeLocale f (T.unpack d))
          [ "%FT%T%Q%z", -- with time zone
            "%FT%T%QZ" -- zulu time
          ]

-- ---------------------------------------------------------------------
-- odds and ends
-- ---------------------------------------------------------------------

entryError :: forall a. Entry -> T.Text -> a
entryError e msg =
  error $ (T.unpack msg) ++ " [on entry " ++ (T.unpack (entryId e)) ++ "]\n" ++ show e

buckets :: (Ord b) => (a -> b) -> [a] -> [(b, [a])]
buckets f = M.toList . M.fromListWith (++) . (map (\x -> (f x, [x])))

-- | Find all non-nested elements which are named `name`, starting with `root`.
-- ("Non-nested" means we don't search sub-elements of an element that's named
-- `name`.)
findElements :: Name -> Element -> [Element]
findElements name element =
  if elementName element == name
    then [element]
    else concatMap (findElements name) (elementChildren element)
