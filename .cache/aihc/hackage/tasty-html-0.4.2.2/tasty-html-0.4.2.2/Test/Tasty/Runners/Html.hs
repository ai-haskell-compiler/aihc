{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Run a 'Tasty.TestTree' and produce an HTML file summarising the test results.
module Test.Tasty.Runners.Html
  ( HtmlPath(..)
  , htmlRunner
  ) where

import Control.Applicative (Const(..))
import Control.Monad ((>=>), unless, when)
import Control.Monad.Trans.Class (lift)
import Control.Concurrent.STM (atomically, readTVar)
import qualified Control.Concurrent.STM as STM(retry)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum(Sum,getSum))
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.ByteString as B
import Control.Monad.State (StateT, evalStateT, liftIO)
import qualified Control.Monad.State as State (get, modify)
import Data.Functor.Compose (Compose(Compose,getCompose))
import qualified Data.IntMap as IntMap
import Data.Proxy (Proxy(..))
import Data.Tagged (Tagged(..))
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import Test.Tasty.Runners
  ( Ingredient(TestReporter)
  , Status(Done)
  , StatusMap
  , Traversal(Traversal,getTraversal)
  )
import Test.Tasty.Providers (IsTest, TestName)
import qualified Test.Tasty.Runners as Tasty
import qualified Test.Tasty.Ingredients as Tasty
import Test.Tasty.Options as Tasty
import Text.Blaze.Html5 (Markup, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Printf (printf)

import Paths_tasty_html (getDataFileName)

-- * Exported

-- | Path where the HTML will be rendered.
newtype HtmlPath = HtmlPath FilePath deriving (Typeable)

-- | HTML 'Option' for the HTML 'Ingredient'.
instance IsOption (Maybe HtmlPath) where
  defaultValue = Nothing
  parseValue = Just . Just . HtmlPath
  optionName = Tagged "html"
  optionHelp = Tagged "A file path to store the test results in HTML"

-- | Path where external assets will be looked up
newtype AssetsPath = AssetsPath FilePath deriving (Typeable)

-- | Assets 'Option' for the HTML 'Ingredient'.
instance IsOption (Maybe AssetsPath) where
    defaultValue = Nothing
    parseValue = Just . Just . AssetsPath
    optionName = Tagged "assets"
    optionHelp = Tagged "Directory where HTML assets will be looked up. \
                        \If not given the assets will be inlined within the \
                        \HTML file."

{-| To run tests using this ingredient, use 'Tasty.defaultMainWithIngredients',
    passing 'htmlRunner' as one possible ingredient. This ingredient will run
    tests if you pass the @--html@ command line option. For example,
    @--html=results.html@ will run all the tests and generate @results.html@ as
    output.

    Note enabling @--html@ will ignore all ingredients following 'htmlRunner'.
    If you want to produce the HTML report /in addition/ to other outputs, you can
    use 'Tasty.composeReporters', as in

    > main = defaultMainWithIngredients ingredients tests
    >   where ingredients = [ listingTests, htmlRunner `composeReporters` consoleTestReporter ]

-}
htmlRunner :: Ingredient
htmlRunner = TestReporter optionDescription $ \options testTree -> do
  HtmlPath htmlPath <- lookupOption options
  let mAssetsPath = lookupOption options
  return $ \statusMap -> do
    Const summary <- flip evalStateT 0 $ getCompose $ getTraversal $
      Tasty.foldTestTree
        Tasty.trivialFold { Tasty.foldSingle = runTest statusMap
                          , Tasty.foldGroup  = runGroup
                          }
        options
        testTree

    return $ \time -> do
      generateHtml summary time htmlPath mAssetsPath
      return $ getSum (summaryFailures summary) == 0

 where
  optionDescription = [ Option (Proxy :: Proxy (Maybe HtmlPath))
                      , Option (Proxy :: Proxy (Maybe AssetsPath))
                      ]

-- Silence unused import warning
_onlyUsedByHaddock :: ()
_onlyUsedByHaddock = ()
  where _ = Tasty.composeReporters


-- * Internal

-- ** Types
--
{-| Includes the number of successful and failed tests and the 'Markup' to
    render the results of a test run.
-}
data Summary = Summary { summaryFailures :: Sum Int
                       , summarySuccesses :: Sum Int
                       , htmlRenderer :: Markup
                       } deriving (Generic)

instance Semigroup Summary where
  (<>) = mappenddefault

instance Monoid Summary where
  mempty = memptydefault
  mappend = (<>)

-- | A 'Traversal' composed of a 'Summary' and a test count.
type SummaryTraversal = Traversal (Compose (StateT Int IO) (Const Summary))

-- ** Test folding

-- | To be used for an individual test when when folding the final 'TestTree'.
runTest :: IsTest t
        => StatusMap -> OptionSet -> TestName -> t -> SummaryTraversal
runTest statusMap _ testName _ = Traversal $ Compose $ do
  ix <- State.get

  result <- lift $ atomically $ do
    status <- readTVar $
      fromMaybe (error "Attempted to lookup test by index outside bounds") $
      IntMap.lookup ix statusMap

    case status of
      -- If the test is done, return the result
      Done result -> return result
      -- Otherwise the test has either not been started or is currently
      -- executing
      _ -> STM.retry

  -- Generate HTML for the test
  msg <- liftIO . Tasty.formatMessage . Tasty.resultDescription $ result
  let time = Tasty.resultTime result
      summary = if Tasty.resultSuccessful result
                then mkSuccess testName time msg
                else mkFailure testName time msg

  Const summary <$ State.modify (+1)

-- | To be used for a 'TestGroup' when folding the final 'TestTree'.
runGroup :: OptionSet -> TestName -> [SummaryTraversal] -> SummaryTraversal
runGroup _opts groupName children = Traversal $ Compose $ do
  Const soFar <- getCompose $ getTraversal $ mconcat children

  let successful = summaryFailures soFar == Sum 0
  let grouped = testGroupMarkup groupName successful $ treeMarkup $ htmlRenderer soFar

  return $ Const soFar { htmlRenderer = grouped }

-- ** HTML

-- | Generates the final HTML report.
generateHtml :: Summary  -- ^ Test summary.
             -> Tasty.Time -- ^ Total run time.
             -> FilePath -- ^ Where to write.
             -> Maybe AssetsPath
             -> IO ()
generateHtml summary time htmlPath mAssetsPath = do

  prologue <- case mAssetsPath of
    Nothing -> includeStyle "data/tasty.css"
    Just (AssetsPath path) -> pure $ H.link ! A.rel "stylesheet" ! A.href (H.toValue $ path <> "/" <> "tasty.css")
  epilogue <- case mAssetsPath of
    Nothing -> includeScript "data/tasty.js"
    Just (AssetsPath path) -> pure $ H.script ! A.src (H.toValue $ path <> "/" <> "tasty.js") $ mempty

  TIO.writeFile htmlPath $
    renderHtml $
      H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
          H.meta ! A.charset "utf-8"
          H.meta ! A.name "viewport"
                 ! A.content "width=device-width, initial-scale=1.0"
          H.title "Tasty Test Results"
          prologue

          case mAssetsPath of
            Nothing -> mempty
            Just (AssetsPath _) -> mempty

        H.body $ do
          H.h1 "Tasty Test Results"
          if summaryFailures summary > Sum 0
            then failureBanner
            else successBanner
          H.button ! A.id "expand-all" ! A.class_ "hidden" $ "Expand all"

          H.toMarkup $ treeMarkup $ htmlRenderer summary
          epilogue
  where
    getRead = getDataFileName >=> B.readFile

    includeScript = getRead >=> \bs ->
      return . H.unsafeByteString $ "<script>" <> bs <> "</script>"

    includeStyle path = do
      bs <- getRead path
      pure $ H.style $ H.unsafeByteString bs

    failureBanner = H.div ! A.id "status-banner" ! A.class_ "fail" $ do
      H.toMarkup . getSum $ summaryFailures summary
      " out of " :: Markup
      H.toMarkup tests
      " tests failed" :: Markup
      H.span $ H.toMarkup $ formatTime time

    successBanner = H.div ! A.id "status-banner" ! A.class_ "pass" $ do
      "All " :: Markup
      H.toMarkup tests
      " tests passed" :: Markup
      H.span $ H.toMarkup $ formatTime time

    tests = getSum $ summaryFailures summary <> summarySuccesses summary

-- | Set the 'htmlRenderer' of a 'Summary' with the given 'Markup'.
mkSummary :: Markup -> Summary
mkSummary contents = mempty { htmlRenderer = contents }

-- | Create an HTML 'Summary' with a test success.
mkSuccess :: TestName
          -> Tasty.Time
          -> String -- ^ Description for the test.
          -> Summary
mkSuccess name time desc = summary { summarySuccesses = Sum 1 }
  where
    summary = mkSummary $ testItemMarkup name True time desc

-- | Create an HTML 'Summary' with a test failure.
mkFailure :: TestName
          -> Tasty.Time
          -> String -- ^ Description for the test.
          -> Summary
mkFailure name time desc = summary { summaryFailures = Sum 1 }
  where
    summary = mkSummary $ testItemMarkup name False time desc

-- | Markup representing the branching of a /tree/.
treeMarkup :: Markup -> Markup
treeMarkup = H.ul

-- | Markup for a test group.
testGroupMarkup :: TestName -> Bool -> Markup -> Markup
testGroupMarkup groupName successful body =
  H.li $ do
    H.h4 ! className $ do
      H.toMarkup groupName
      H.span ! A.class_ "expand" $ " (click to expand)"
    body
    H.div ! A.class_ "ellipsis" $ H.preEscapedText "&#x2026;" -- "…"

  where
    className :: H.Attribute
    className = classNames $ ["group"] <> ["fail" | not successful]

classNames :: [String] -> H.Attribute
classNames = A.class_ . H.toValue . unwords

-- | Markup for a single test.
testItemMarkup :: TestName
               -> Bool
               -> Tasty.Time
               -> String
               -> Markup
testItemMarkup testName successful time desc = do
  H.li ! className $ do
    -- for check mark / cross
    H.div ! A.class_ "mark" $ H.preEscapedText $
      if successful
      then "&#x2713;" -- "✓"
      else "&#x2715;" -- "✕"
    H.div $ do
      H.h5 $ do
        H.toMarkup testName
        when (time >= 0.01) $
          H.span $ H.toMarkup (formatTime time)

      unless (null desc) $
        H.pre $ H.small $ H.toMarkup desc

  where
    className :: H.Attribute
    className = classNames $ ["item"] <> ["fail" | not successful]

formatTime :: Tasty.Time -> String
formatTime = printf " (%.2fs)"

-- vim: textwidth=79 shiftwidth=2
