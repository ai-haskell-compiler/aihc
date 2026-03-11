{-# LANGUAGE OverloadedStrings, FlexibleInstances, DeriveDataTypeable, TypeFamilies, CPP, NamedFieldPuns #-}
-- | Write hspec tests that are webdriver tests, automatically managing the webdriver sessions.
--
-- This module re-exports functions from "Test.Hspec" and "Test.WebDriver.Commands" and it is
-- intended that you just import @Test.Hspec.WebDriver@.  If you need to import @Test.Hspec@ or
-- @Test.WebDriver@, you should do so using a qualified import.
--
-- >{-# LANGUAGE OverloadedStrings #-}
-- >module XKCD where
-- >
-- >import Test.Hspec.WebDriver
-- >
-- >allBrowsers :: [Capabilities]
-- >allBrowsers = [firefoxCaps, chromeCaps, ieCaps]
-- >
-- >browsersExceptIE :: [Capabilities]
-- >browsersExceptIE = [firefoxCaps, chromeCaps]
-- >
-- >main :: IO ()
-- >main = hspec $
-- >    describe "XKCD Tests" $ do
-- >
-- >        session "for 327" $ using allBrowsers $ do
-- >            it "opens the page" $ runWD $
-- >                openPage "http://www.xkcd.com/327/"
-- >            it "checks hover text" $ runWD $ do
-- >                e <- findElem $ ByCSS "div#comic > img"
-- >                e `shouldBeTag` "img"
-- >                e `shouldHaveAttr` ("title", "Her daughter is named Help I'm trapped in a driver's license factory.")
-- >
-- >        parallel $ session "for 303" $ using browsersExceptIE $ do
-- >            it "opens the page" $ runWD $
-- >                openPage "http://www.xkcd.com/303/"
-- >            it "checks the title" $ runWD $ do
-- >                e <- findElem $ ById "ctitle"
-- >                e `shouldBeTag` "div"
-- >                e `shouldHaveText` "Compiling"
--
-- The above code assumes selenium-server-standalone is running on @127.0.0.1:4444@ at path
-- @\/wd\/hub@ (this is the default).
module Test.Hspec.WebDriver(
  -- * Webdriver Example
    WdExample(..)
  , WdOptions (..)
  , runWD
  , runWDOptions
  , runWDWith
  , runWDWithOptions
  , pending
  , pendingWith
  , example

  -- * Webdriver Sessions
  , session
  , sessionWith
  , inspectSession
  , using
  , WdTestSession

  -- * Default Capabilities
  , firefoxCaps
  , chromeCaps
  , ieCaps
  , operaCaps
  , iphoneCaps
  , ipadCaps
  , androidCaps

  -- * Expectations
  , shouldBe
  , shouldBeTag
  , shouldHaveText
  , shouldHaveAttr
  , shouldReturn
  , shouldThrow

  -- * Re-exports from "Test.Hspec"
  , hspec
  , Spec
  , SpecWith
  , describe
  , context
  , it
  , specify
  , parallel
  , runIO

  -- * Re-exports from "Test.WebDriver"
  , WD
  , Capabilities
  , module Test.WebDriver.Commands
) where

import Control.Concurrent.MVar (MVar, takeMVar, putMVar, newEmptyMVar)
import Control.Exception (SomeException(..))
import Control.Exception.Lifted (try, Exception, onException, throwIO)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.State (state, evalState, execState)
import Data.Default (Default(..))
import Data.IORef (newIORef, writeIORef, readIORef)
import qualified Data.Text as T
import Data.Typeable (Typeable, cast)
import Test.HUnit (assertEqual, assertFailure)
import qualified Data.Aeson as A

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), Applicative)
import Data.Traversable (traverse)
#endif

import qualified Test.Hspec as H
import Test.Hspec hiding (shouldReturn, shouldBe, shouldSatisfy, shouldThrow, pending, pendingWith, example)
import Test.Hspec.Core.Spec (Result(..), ResultStatus(..))
import Test.Hspec.Core.Spec (Item(..), Example(..), SpecTree, Tree(..), fromSpecList, runSpecM)

import Test.WebDriver (WD, Capabilities)
import qualified Test.WebDriver as W
import Test.WebDriver.Commands
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Capabilities as W
import qualified Test.WebDriver.Session as W

-- | The state passed between examples inside the mvars.
data SessionState multi = SessionState {
    -- | The already created sessions
    stSessionMap :: [(multi, W.WDSession)]
    -- | True if the previous example had an error
  , stPrevHadError :: Bool
    -- | True if the previous example was aborted with 'inspectSession'
  , stPrevAborted :: Bool
    -- | Create a new session
  , stCreateSession :: IO W.WDSession
}

-- | Internal state for webdriver test sessions.
data WdTestSession multi = WdTestSession {
    wdTestOpen :: IO (SessionState multi)
  , wdTestClose :: SessionState multi -> IO ()
}

-- | A webdriver example.
--
-- The webdriver action of type @'WD' ()@ should interact with the webpage using commands from
-- "Test.WebDriver.Commands" (which is re-exported from this module) and then use the
-- <#g:4 expectations> in this module.  It is possible to split up the spec of a single page into multiple
-- examples where later examples start with the web browser state from the end of the previous
-- example.  This is helpful to keep each individual example small and allows the entire spec to be
-- described at the beginning with pending examples.
--
-- The way this works is that you combine examples into a session using 'session' or 'sessionWith'.
-- A webdriver session is then threaded through all examples in a session so that a later example in
-- the session can rely on the webbrowser state as set up by the previous example.  The type system
-- enforces that every webdriver example must be located within a call to 'session' or
-- 'sessionWith'.  Indeed, a 'WdExample' produces a @'SpecWith' ('WdTestSession' multi)@ which can
-- only be converted to 'Spec' using 'session' or 'sessionWith'.  The reason for the 'WdPending'
-- constructor is so that a pending example can be specified with type @'SpecWith' ('WdTestSession'
-- multi)@ so it can compose with the other webdriver examples.
--
-- The type @multi@ is used when testing multiple sessions at once (e.g. to test multiple
-- interacting users), otherwise it is @()@. Values of this type are used to determine which browser
-- session the example should be executed against.  A new session is created every time a new value
-- of type @multi@ is seen.  Note that the type system enforces that every example within the
-- session has the same type @multi@.
data WdExample multi = WdExample multi WdOptions (WD ()) | WdPending (Maybe String)

-- | Optional options that can be passed to 'runWDOptions'.
data WdOptions = WdOptions {
  -- | As soon as an example fails, skip all remaining tests in the session.  Defaults to True.
  skipRemainingTestsAfterFailure :: Bool
  }

instance Default WdOptions where
  def = WdOptions { skipRemainingTestsAfterFailure = True }

-- | A shorthand for constructing a 'WdExample' from a webdriver action when you are only testing a
-- single browser session at once.  See the XKCD example at the top of the page.
runWD :: WD () -> WdExample ()
runWD = WdExample () def

-- | A version of runWD that accepts some custom options
runWDOptions :: WdOptions -> WD () -> WdExample ()
runWDOptions = WdExample ()

-- | Create a webdriver example, specifying which of the multiple sessions the example should be
-- executed against.  I suggest you create an enumeration for multi, for example:
--
-- >data TestUser = Gandolf | Bilbo | Legolas
-- >    deriving (Show, Eq, Enum, Bounded)
-- >
-- >runUser :: TestUser -> WD () -> WDExample TestUser
-- >runUser = runWDWith
-- >
-- >spec :: Spec
-- >spec = session "tests some page" $ using [firefoxCaps] $ do
-- >    it "does something with Gandolf" $ runUser Gandolf $ do
-- >        openPage ...
-- >    it "does something with Bilbo" $ runUser Bilbo $ do
-- >        openPage ...
-- >    it "goes back to the Gandolf session" $ runUser Gandolf $ do
-- >        e <- findElem ....
-- >        ...
--
-- In the above code, two sessions are created and the examples will go back and forth between the
-- two sessions.  Note that a session for Legolas will only be created the first time he shows up in
-- a call to @runUser@, which might be never.  To share information between the sessions (e.g. some
-- data that Gandolf creates that Bilbo should expect), the best way I have found is to use IORefs
-- created with 'runIO' (wrapped in a utility module).
runWDWith :: multi -> WD () -> WdExample multi
runWDWith multi = WdExample multi def

-- | A version of runWDWith that accepts some custom options
runWDWithOptions :: multi -> WdOptions -> WD () -> WdExample multi
runWDWithOptions = WdExample

-- | A pending example.
pending :: WdExample multi
pending = WdPending Nothing

-- | A pending example with a message.
pendingWith :: String -> WdExample multi
pendingWith = WdPending . Just

-- | A version of 'H.example' which lifts an @IO ()@ to a webdriver example (so it can be composed
-- with other webdriver examples).  In the case of multiple sessions, it doesn't really matter which
-- session the expectation is executed against, so a default value is used.  In the case of single
-- sessions, the type is @WdExample ()@.
example :: Default multi => Expectation -> WdExample multi
example = WdExample def def . liftIO

-- | Combine the examples nested inside this call into a webdriver session or multiple sessions.
-- For each of the capabilities in the list, the examples are executed one at a time in depth-first
-- order and so later examples can rely on the browser state created by earlier examples.  These
-- passes through the examples are independent for different capabilities.  Note that when using
-- 'parallel', the examples within a single pass still execute serially.  Different passes through
-- the examples will be executed in parallel.  The sessions are managed as follows:
--
-- * In the simplest case when @multi@ is @()@, before the first example is executed a new webdriver
-- session with the given capabilities is created.  The examples are then executed in depth-first
-- order, and the session is then closed when either an exception occurs or the examples complete.
-- (The session can be left open with 'inspectSession').
--
-- * More generally, as the examples are executed, each time a new value of type @multi@ is seen, a
-- new webdriver session with the capabilities is automatically created.  Later examples will
-- continue with the session matching their value of @multi@.
--
-- This function uses the default webdriver host (127.0.0.1), port (4444), and basepath
-- (@\/wd\/hub@).
session :: String -> ([Capabilities], SpecWith (WdTestSession multi)) -> Spec
session msg (caps, spec) = sessionWith W.defaultConfig msg (caps', spec)
  where
    caps' = map f caps
    f c = case A.toJSON (W.browser c) of
      A.String b -> (c, T.unpack b)
      _ -> (c, show c) -- this should not be the case, every browser toJSON is a string

-- | A variation of 'session' which allows you to specify the webdriver configuration.  Note that
-- the capabilities in the 'W.WDConfig' will be ignored, instead the capabilities will come from the
-- list of 'Capabilities' passed to 'sessionWith'.
--
-- In addition, each capability is paired with a descriptive string which is passed to hspec to
-- describe the example.  By default, 'session' uses the browser name as the description.  'sessionWith'
-- supports a more detailed description so that in the hspec output you can distinguish between
-- capabilities that share the same browser but differ in the details, for example capabilities with and
-- without javascript.
sessionWith :: W.WDConfig -> String -> ([(Capabilities, String)], SpecWith (WdTestSession multi)) -> Spec
sessionWith cfg msg (caps, spec) = spec'
    where
        procT c = procTestSession cfg (W.getCaps c) spec
        spec' = case caps of
                    [] -> it msg $ H.pendingWith "No capabilities specified"
                    [(c,cDscr)] -> describe (msg ++ " using " ++ cDscr) $ procT c
                    _ -> describe msg $ mapM_ (\(c,cDscr) -> describe ("using " ++ cDscr) $ procT c) caps

-- | A synonym for constructing pairs that allows the word @using@ to be used with 'session' so that the session
-- description reads like a sentance.
--
-- >allBrowsers :: [Capabilities]
-- >allBrowsers = [firefoxCaps, chromeCaps, ieCaps]
-- >
-- >browsersExceptIE :: [Capabilities]
-- >browsersExceptIE = [firefoxCaps, chromeCaps]
-- >
-- >mobileBrowsers :: [Capabilities]
-- >mobileBrowsers = [iphoneCaps, ipadCaps, androidCaps]
-- >
-- >myspec :: Spec
-- >myspec = do
-- >  session "for the home page" $ using allBrowsers $ do
-- >    it "loads the page" $ runWD $ do
-- >        ...
-- >    it "scrolls the carosel" $ runWD $ do
-- >        ...
-- >  session "for the users page" $ using browsersExceptIE $ do
-- >    ...
using :: [caps] -> SpecWith (WdTestSession multi) -> ([caps], SpecWith (WdTestSession multi))
using = (,)

-- | Default capabilities which can be used in the list passed to 'using'.  I suggest creating a
-- top-level definition such as @allBrowsers@ and @browsersWithoutIE@ such as in the XKCD example at
-- the top of the page, so that you do not specify the browsers in the individual spec.
firefoxCaps, chromeCaps, ieCaps, operaCaps, iphoneCaps, ipadCaps, androidCaps :: Capabilities
firefoxCaps = W.defaultCaps { W.browser = W.firefox }
chromeCaps = W.defaultCaps { W.browser = W.chrome }
ieCaps = W.defaultCaps { W.browser = W.ie }
operaCaps = W.defaultCaps { W.browser = W.opera }
iphoneCaps = W.defaultCaps { W.browser = W.iPhone }
ipadCaps = W.defaultCaps { W.browser = W.iPad }
androidCaps = W.defaultCaps { W.browser = W.android }

data AbortSession = AbortSession
    deriving (Show, Typeable)
instance Exception AbortSession

-- | Abort the session without closing the session.
--
-- Normally, 'session' will automatically close the session either when the tests complete without
-- error or when any of the tests within the session throws an error.  When developing the test
-- suite, this can be annoying since closing the session causes the browser window to close.
-- Therefore, while developing the test suite, you can insert a call to 'inspectSession'.  This will
-- immedietly halt the session (all later tests will fail) but will not close the session so that
-- the browser window stays open.
inspectSession :: WD ()
inspectSession = throwIO AbortSession

-- | 'H.shouldBe' lifted into the 'WD' monad.
shouldBe :: (Show a, Eq a) => a -> a -> WD ()
x `shouldBe` y = liftIO $ x `H.shouldBe` y

-- | Asserts that the given element matches the given tag.
shouldBeTag :: Element -> T.Text -> WD ()
e `shouldBeTag` name = do
    t <- tagName e
    liftIO $ assertEqual ("tag of " ++ show e) name t

-- | Asserts that the given element has the given text.
shouldHaveText :: Element -> T.Text -> WD ()
e `shouldHaveText` txt = do
    t <- getText e
    liftIO $ assertEqual ("text of " ++ show e) txt t

-- | Asserts that the given elemnt has the attribute given by @(attr name, value)@.
shouldHaveAttr :: Element -> (T.Text, T.Text) -> WD ()
e `shouldHaveAttr` (a, txt) = do
    t <- attr e a
    liftIO $ assertEqual ("attribute " ++ T.unpack a ++ " of " ++ show e) (Just txt) t

-- | Asserts that the action returns the expected result.
shouldReturn :: (Show a, Eq a) => WD a -> a -> WD ()
action `shouldReturn` expected = action >>= (\a -> liftIO $ a `H.shouldBe` expected)

-- | Asserts that the action throws an exception.
shouldThrow :: (Show e, Eq e, Exception e) => WD a -> e -> WD ()
shouldThrow w expected = do
    r <- try w
    case r of
        Left err -> err `shouldBe` expected
        Right _ -> liftIO $ assertFailure $ "did not get expected exception " ++ show expected

--------------------------------------------------------------------------------
-- Internal Test Runner
--------------------------------------------------------------------------------

-- | Create a WdTestSession.
createTestSession :: W.WDConfig -> [MVar (SessionState multi)] -> Int -> WdTestSession multi
createTestSession cfg mvars n = WdTestSession open close
    where
        open | n == 0 = return $ SessionState [] False False create
             | otherwise = takeMVar (mvars !! n)

        create = do
            s <- W.mkSession cfg
#if MIN_VERSION_webdriver(0,7,0)
            W.runWD s $ createSession $ W.wdCapabilities cfg
#else
            W.runWD s $ createSession [] $ W.wdCapabilities cfg
#endif

        close st | length mvars - 1 == n = mapM_ ((`W.runWD` closeSession) . snd) $ stSessionMap st
                 | otherwise = putMVar (mvars !! (n + 1)) st

-- | Convert a single test item to a generic item by providing it with the WdTestSession.
procSpecItem :: W.WDConfig -> [MVar (SessionState multi)] -> Int -> Item (WdTestSession multi) -> Item ()
procSpecItem cfg mvars n item = item { itemExample = \p act progress -> itemExample item p (act . act') progress }
    where
        act' f () = f (createTestSession cfg mvars n)

-- | Convert a spec tree of test items to a spec tree of generic items by creating a single session for
-- the entire tree.
procTestSession :: W.WDConfig -> Capabilities -> SpecWith (WdTestSession multi) -> Spec
procTestSession cfg cap s = do
    (mvars, trees) <- runIO $ do
#if MIN_VERSION_hspec_core(2,10,0)
        (_, trees) <- runSpecM s
#else
        trees <- runSpecM s
#endif
        let cnt = countItems trees
        mvars <- replicateM cnt newEmptyMVar
        return (mvars, trees)

    fromSpecList $ mapWithCounter (procSpecItem cfg {W.wdCapabilities = cap} mvars) trees

instance Eq multi => Example (WdExample multi) where
    type Arg (WdExample multi) = WdTestSession multi
    evaluateExample (WdPending msg) _ _ _ = return $ Result "" (Pending Nothing msg)
    evaluateExample (WdExample multi (WdOptions {skipRemainingTestsAfterFailure}) wd) _ act _ = do
        prevHadError <- newIORef False
        aborted <- newIORef False

        act $ \testsession -> do

            tstate <- wdTestOpen testsession

            msess <- case (lookup multi $ stSessionMap tstate,
                           (stPrevHadError tstate || stPrevAborted tstate) && skipRemainingTestsAfterFailure) of
                (_, True) -> return Nothing
                (Just s, False) -> return $ Just s
                (Nothing, False) ->
                    Just <$> stCreateSession tstate
                        `onException` wdTestClose testsession tstate { stPrevHadError = True }

            case msess of
                Just wdsession -> W.runWD wdsession $ do
                    -- run the example
                    macterr <- try wd
                    case macterr of
                        Right () -> do
                            -- pass current session on to the next test
                            wdsession' <- W.getSession
                            let smap = (multi, wdsession') : filter ((/=multi) . fst) (stSessionMap tstate)
                            liftIO $ wdTestClose testsession tstate { stSessionMap = smap }

                        Left acterr@(SomeException actex) ->
                            case cast actex of
                                Just AbortSession -> do
                                    -- pass empty list on to the next test so the session is not closed
                                    liftIO $ wdTestClose testsession tstate { stSessionMap = [], stPrevAborted = True }
                                    liftIO $ writeIORef aborted True
                                Nothing -> do
                                    liftIO $ wdTestClose testsession tstate { stPrevHadError = True }
                                    throwIO acterr

                _ -> do
                    -- on error, just pass along the session and error
                    writeIORef prevHadError $ stPrevHadError tstate
                    writeIORef aborted $ stPrevAborted tstate
                    wdTestClose testsession tstate

        merr <- readIORef prevHadError
        mabort <- readIORef aborted
        return $ case (merr, mabort) of
            (True, _) -> Result "" (Pending Nothing (Just "Previous example had an error"))
            (_, True) -> Result "" (Pending Nothing (Just "Session has been aborted"))
            _ -> Result "" Success

--------------------------------------------------------------------------------
--- Utils
--------------------------------------------------------------------------------

#if MIN_VERSION_hspec_core(2,10,0)
traverseSpec :: Applicative f => (Item a -> f (Item b)) -> [SpecTree a] -> f [SpecTree b]
traverseSpec = traverse . traverse

#else
-- | Traverse a spec allowing the type to change
traverseTree :: Applicative f => (Item a -> f (Item b)) -> SpecTree a -> f (SpecTree b)
traverseTree f (Leaf i) = Leaf <$> f i
traverseTree f (Node msg ss) = Node msg <$> traverse (traverseTree f) ss
#if MIN_VERSION_hspec_core(2,8,0)
traverseTree f (NodeWithCleanup loc c ss) = NodeWithCleanup loc c' <$> traverse (traverseTree f) ss
#else
traverseTree f (NodeWithCleanup c ss) = NodeWithCleanup c' <$> traverse (traverseTree f) ss
#endif
    where
        c' _b = c undefined -- this undefined is OK since we do not export the definition of WdTestSession
                            -- so the user cannot do anything with the passed in value to 'afterAll'

traverseSpec :: Applicative f => (Item a -> f (Item b)) -> [SpecTree a] -> f [SpecTree b]
traverseSpec f = traverse (traverseTree f)
#endif


-- | Process the items in a depth-first walk, passing in the item counter value.
mapWithCounter :: (Int -> Item a -> Item b) -> [SpecTree a] -> [SpecTree b]
mapWithCounter f s = flip evalState 0 $ traverseSpec go s
    where
        go item = state $ \cnt -> (f cnt item, cnt+1)

countItems :: [SpecTree a] -> Int
countItems s = flip execState 0 $ traverseSpec go s
    where
        go item = state $ \cnt -> (item, cnt+1)
