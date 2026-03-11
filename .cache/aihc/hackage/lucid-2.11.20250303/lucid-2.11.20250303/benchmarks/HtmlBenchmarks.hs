-- | This is a collection of HTML benchmarks for BlazeMarkup.

--
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings, ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
module HtmlBenchmarks where

import           Data.Monoid (Monoid,mappend,mempty)
import           Data.Text (Text)
import qualified Data.Text as T
-- import qualified Data.Text.Lazy.Builder as B

import qualified Prelude as P
import           Prelude hiding (div, id)

-- import BenchmarkUtils
import           Lucid
import           Lucid.Base
-- import qualified BenchmarkUtils as H

-- | Description of an HTML benchmark
--
data HtmlBenchmark = forall a. HtmlBenchmark
    String          -- ^ Name.
    (a -> Html ())  -- ^ Rendering function.
    a               -- ^ Data.
    (Html ())       -- ^ Longer description.

-- | List containing all benchmarks.
--
benchmarks :: [HtmlBenchmark]
benchmarks =
    [ HtmlBenchmark "bigTable" bigTable bigTableData $
        let h = toHtml $ show $ length bigTableData
            w = toHtml $ show $ length $ P.head bigTableData
        in "Rendering of a big (" >> h >> "x" >> w >> ") HTML table"
    , HtmlBenchmark "basic" basic basicData
        "A simple, small basic template with a few holes to fill in"
    , HtmlBenchmark "wideTree" wideTree wideTreeData $
        "A very wide tree (" >> toHtml (show (length wideTreeData)) >> " elements)"
    , HtmlBenchmark "wideTreeEscaping" wideTree wideTreeEscapingData $ do
        "A very wide tree (" >> toHtml (show (length wideTreeEscapingData)) >> " elements)"
        " with lots of escaping"
    , HtmlBenchmark "deepTree" deepTree deepTreeData $ do
        "A really deep tree (" >> toHtml (show deepTreeData) >> " nested templates)"
    , HtmlBenchmark "manyAttributes" manyAttributes manyAttributesData $ do
        "A single element with " >> toHtml (show (length manyAttributesData))
        "  distinct attributes."
    , HtmlBenchmark "duplicateAttributes" duplicateAttributes duplicateAttributesData $ do
        "A single element with a single attribute and " >> toHtml (show (length duplicateAttributesData))
        " values."
    , HtmlBenchmark "customAttribute" customAttributes customAttributesData $
        "Creating custom attributes (middle ground between manyAttributes and duplicateAttributes)"

    ]

rows :: Int
rows = 1000

bigTableData :: [[Int]]
bigTableData = replicate rows [1..10]
{-# NOINLINE bigTableData #-}

basicData :: (Text, Text, [Text])
basicData = ("Just a test", "joe", items)
{-# NOINLINE basicData #-}

items :: [Text]
items = map (("Number " `mappend`) . T.pack . show) [1 :: Int .. 14]
{-# NOINLINE items #-}

wideTreeData :: [Text]
wideTreeData = take 5000 $
    cycle ["λf.(λx.fxx)(λx.fxx)", "These old days", "Foobar", "lol", "x ∈ A"]
{-# NOINLINE wideTreeData #-}

wideTreeEscapingData :: [Text]
wideTreeEscapingData = take 1000 $
    cycle ["<><>", "\"lol\"", "<&>", "'>>'"]
{-# NOINLINE wideTreeEscapingData #-}

deepTreeData :: Int
deepTreeData = 1000
{-# NOINLINE deepTreeData #-}

manyAttributesData :: [(T.Text, T.Text)]
manyAttributesData = zipWith mk [0 ..] wideTreeData where
    mk :: Int -> T.Text -> (T.Text, T.Text)
    mk i val = (T.pack ("attr" ++ show i), val)

duplicateAttributesData :: [Text]
duplicateAttributesData = wideTreeData

customAttributesData :: [(Text, Text)]
customAttributesData = zip wideTreeData wideTreeData

-- | Render the argument matrix as an HTML table.
--
bigTable :: [[Int]]  -- ^ Matrix.
         -> Html ()     -- ^ Result.
bigTable t = table_ (mapM_ row t)

row :: [Int] -> Html ()
row r = tr_ (mapM_ (td_ . toHtml . show) r)

-- | Render a simple HTML page with some data.
--
basic :: (Text, Text, [Text])  -- ^ (Title, User, Items)
      -> Html ()                        -- ^ Result.
basic (title', user, items') = html_ $ do
    head_ $ title_ $ toHtml title'
    body_ $ do
        with div_ [id_ "header"] $ (h1_ $ toHtml title')
        p_ $ do "Hello, "; toHtml user; "!"
        p_ $ "Hello, me!"
        p_ $ "Hello, world!"
        h2_ $ "loop"
        ol_ $ mapM_ (li_ . toHtml) items'
        with div_ [id_ "footer"] mempty

-- | A benchmark producing a very wide but very shallow tree.
--
wideTree :: [Text]  -- ^ Text to create a tree from.
         -> Html ()      -- ^ Result.
wideTree = div_ . mapM_ ((with p_ [id_ "foo"]) . toHtml)

-- | Create a very deep tree.
--
deepTree :: Int   -- ^ Depth of the tree.
         -> Html ()  -- ^ Result.
deepTree 0 = "foo"
deepTree n = p_ $ table_ $ tr_ $ td_ $ div_ $ deepTree (n - 1)

-- | Create an element with many attributes.
--
manyAttributes :: [(T.Text, T.Text)]  -- ^ List of attribute values.
               -> Html ()      -- ^ Result.
manyAttributes as = img_ (map (\(key, val) -> makeAttribute key val) as)

duplicateAttributes :: [Text]  -- ^ List of attribute values.
               -> Html ()      -- ^ Result.
duplicateAttributes as = img_ (map id_ as)

customAttributes :: [(Text, Text)]  -- ^ List of attribute name, value pairs
                 -> Html ()                -- ^ Result
customAttributes xs =
  img_ (map (\(key,val) -> makeAttribute key val) xs)
