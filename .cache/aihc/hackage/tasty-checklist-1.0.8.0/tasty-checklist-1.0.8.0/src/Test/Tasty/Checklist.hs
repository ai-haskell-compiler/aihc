{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | This package provides the ability to run a Checklist of several
-- "checks" during a single test.  A "bad" check does not immediately
-- result in a test failure; at the end of the test (passed or failed
-- due to primary testing), all failed checks are reported (and any
-- failed checks will result in an overall test failure at the end.
--
-- This type of checking can be very useful when needing to test
-- various aspects of an operation that is complex to setup, has
-- multiple effects, or where the checks are related such that knowing
-- about the multiple failures makes debugging easier.
--
-- An alternative approach is to have some sort of common preparation
-- code and use a separate test for each item.  This module simply
-- provides a convenient method to collate related items under the
-- aegis of a single test.
--
-- This package also provides the 'checkValues' function which can be
-- used to check a number of derived values from a single input value
-- via a checklist.  This can be used to independently verify a number
-- of record fields of a data structure or to validate related
-- operations performed from a single input.
--
-- See the documentation for 'check' and 'checkValues' for examples of
-- using this library.  The tests in the source package also provide
-- additional examples of usage.

module Test.Tasty.Checklist
  (
    -- * Checklist testing context
    withChecklist
  , CanCheck
  -- * Performing or Disabling checks
  , check
  , discardCheck
  -- * Type-safe multi-check specifications
  -- $checkValues
  -- $setup
  , checkValues
  , DerivedVal(Val, Got, Observe)
  -- * Error reporting
  , CheckResult
  , ChecklistFailures
  -- * Displaying tested values
  , TestShow(testShow)
  , testShowList
  , multiLineDiff
  )
where

import           Control.Exception ( evaluate )
import           Control.Monad ( join, unless )
import           Control.Monad.Catch
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Data.IORef
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Parameterized.Context as Ctx
import           Data.Text ( Text )
import qualified Data.Text as T
import           System.IO ( hFlush, hPutStrLn, stdout, stderr )


-- | The ChecklistFailures exception is thrown if any checks have
-- failed during testing.

data ChecklistFailures = ChecklistFailures Text [CheckResult]

-- | The internal 'CheckResult' captures the failure information for a check

data CheckResult = CheckFailed CheckName (Maybe InputAsText) FailureMessage
                 | CheckMessage Text

newtype CheckName = CheckName { checkName :: Text }
newtype InputAsText = InputAsText { inputAsText :: Text } deriving (Eq, Ord)
newtype FailureMessage = FailureMessage { failureMessage :: Text }

instance Exception ChecklistFailures

instance Show CheckResult where
  show (CheckFailed what onValue msg) =
    let chknm = if length (T.lines (checkName what)) > 1
                then "check: " <> T.unpack (checkName what)
                else "check '" <> T.unpack (checkName what) <> "'"
        chkmsg = if T.null (failureMessage msg)
                 then ""
                 else " with: " <> T.unpack (failureMessage msg)
                      -- n.b. msg might be carefully crafted to preceed chkval
        chkval = case onValue of
          Nothing -> ""
          Just i -> "\n" <> indent <> "using:"
                    <> indent <> T.unpack (inputAsText i)
    in if or [ length chknm > 100
             , length chkmsg > 100
             , length chkval > 100
             ]
       then replicate 50 '#' <> "\n\n" <> indent
            <> "Failed " <> chknm <> chkmsg <> chkval
       else "Failed " <> chknm <> chkmsg <> chkval
  show (CheckMessage txt) = "-- " <> T.unpack txt

instance Show ChecklistFailures where
  show (ChecklistFailures topMsg fails) =
    let isMessage = \case
          CheckMessage _ -> True
          _ -> False
        checkCnt = length $ filter (not . isMessage) fails
    in "ERROR: " <> T.unpack topMsg <> "\n  "
       <> show checkCnt <> " checks failed in this checklist:\n  -"
       <> List.intercalate "\n  -" (show <$> fails)

indent :: String
indent = "        "

indenT :: Text
indenT = T.pack indent

-- | A convenient Constraint to apply to functions that will perform
-- checks (i.e. call 'check' one or more times)

type CanCheck = (?checker :: IORef [CheckResult])


-- | This should be used to wrap the test that contains checks.  This
-- initializes the environment needed for the checks to run, and on
-- exit from the test, reports any (and all) failed checks as a test
-- failure.

withChecklist :: (MonadIO m, MonadMask m)
              => Text -> (CanCheck => m a) -> m a
withChecklist topMsg t = do
  checks <- liftIO $ newIORef mempty
  r <- (let ?checker = checks in t)
       `onException` (liftIO $
                       do cs <- List.reverse <$> readIORef checks
                          unless (null cs) $ do
                            hFlush stdout
                            hPutStrLn stderr ""
                            let pfx = "        WARN "
                            mapM_ (hPutStrLn stderr . (pfx <>) . show) cs
                            hFlush stderr
                     )

  -- If t failed, never get here:
  liftIO $ do
    collected <- List.reverse <$> readIORef checks
    unless (null collected) $
      throwM (ChecklistFailures topMsg collected)
  return r

-- | This is used to run a check within the code.  The first argument
-- is the "name" of this check, the second is a function that takes a
-- value and returns 'True' if the value is OK, or 'False' if the
-- value fails the check.  The last argument is the value to check.
--
-- >>> :set -XOverloadedStrings
-- >>> import Test.Tasty
-- >>> import Test.Tasty.HUnit
-- >>> :{
-- >>> defaultMain $ testCase "odd numbers" $ withChecklist "odds" $ do
-- >>>  let three = 3 :: Int
-- >>>  check "three is odd" odd three
-- >>>  check "two is odd" odd (2 :: Int)
-- >>>  check "7 + 3 is odd" odd $ 7 + three
-- >>>  check "7 is odd" odd (7 :: Int)
-- >>> :}
-- odd numbers: FAIL
--   Exception: ERROR: odds
--     2 checks failed in this checklist:
--     -Failed check 'two is odd' with: 2
--     -Failed check '7 + 3 is odd' with: 10
-- ...
-- <BLANKLINE>
-- 1 out of 1 tests failed (...s)
-- *** Exception: ExitFailure 1
--
-- Any check failures are also printed to stdout (and omitted from the
-- above for clarity).  This is so that those failures are reported
-- even if a more standard test assertion is used that prevents
-- completion of the checklist.  Thus, if an @assertEqual "values"
-- three 7@ had been added to the above, that would have been the only
-- actual (and immediate) fail for the test, but any failing 'check's
-- appearing before that @assertEqual@ would still have printed.

check :: (CanCheck, TestShow a, MonadIO m)
      => Text -> (a -> Bool) -> a -> m ()
check = checkShow testShow Nothing

checkShow :: (CanCheck, MonadIO m)
          => (a -> String)
          -> Maybe InputAsText
          -> Text -> (a -> Bool) -> a -> m ()
checkShow showit failInput what eval val = do
  r <- liftIO $ evaluate (eval val)
  unless r $ do
    let failtxt = FailureMessage $ T.pack $ showit val
    let chk = CheckFailed (CheckName what) failInput failtxt
    liftIO $ modifyIORef ?checker (chk:)


-- | Sometimes checks are provided in common testing code, often in
-- setup/preparation for the main tests.  In some cases, the check is
-- not applicable for that particular test.  This function can be used
-- to discard any pending failures for the associated named check.
--
-- This is especially useful when a common code block is used to
-- perform a set of checks: if a few of the common checks are not
-- appropriate for the current situation, 'discardCheck' can be used
-- to throw away the results of those checks by matching on the check
-- name.

discardCheck :: (CanCheck, MonadIO m) => Text -> m ()
discardCheck what = do
  let isCheck n (CheckFailed n' _ _) = n == checkName n'
      isCheck _ (CheckMessage _) = False
  liftIO $ modifyIORef ?checker (filter (not . isCheck what))

----------------------------------------------------------------------

-- $checkValues
--
-- Implementing a number of discrete 'check' calls can be tedious,
-- especially when they are validating different aspects of the same
-- result value.  To facilitate this, the 'checkValues' function can
-- be used along with a type-safe list of checks to perform.
--
-- To demonstrate this, first consider the following sample program,
-- which has code that generates a complex @Struct@ value, along with
-- tests for various fields in that @Struct@.

-- $setup
-- >>> :set -XPatternSynonyms
-- >>> :set -XOverloadedStrings
-- >>>
-- >>> import Data.Parameterized.Context ( pattern Empty, pattern (:>) )
-- >>> import Test.Tasty.Checklist
-- >>> import Test.Tasty
-- >>> import Test.Tasty.HUnit
-- >>>
-- >>> :{
-- >>> data Struct = MyStruct { foo :: Int, bar :: Char, baz :: String }
-- >>>
-- >>> instance Show Struct where
-- >>>    show s = baz s <> " is " <> show (foo s) <> [bar s]
-- >>> instance TestShow Struct where testShow = show
-- >>>
-- >>> someFun :: Int -> Struct
-- >>> someFun n = MyStruct (n * 6)
-- >>>               (if n * 6 == 42 then '!' else '?')
-- >>>               "The answer to the universe"
-- >>>
-- >>> oddAnswer :: Struct -> Bool
-- >>> oddAnswer = odd . foo
-- >>>
-- >>> test = testCase "someFun result" $
-- >>>    withChecklist "results for someFun" $
-- >>>    someFun 3 `checkValues`
-- >>>         (Empty
-- >>>         :> Val "foo" foo 42
-- >>>         :> Val "baz field" baz "The answer to the universe"
-- >>>         :> Val "shown" show "The answer to the universe is 42!"
-- >>>         :> Val "odd answer" oddAnswer False
-- >>>         :> Got "even answer" (not . oddAnswer)
-- >>>         :> Val "double-checking foo" foo 42
-- >>>         )
-- >>> :}
--
-- This code will be used below to demonstrate various advanced
-- checklist capabilities.

-- | The 'checkValues' is a checklist that tests various values that
-- can be derived from the input value.  The input value is provided,
-- along with an 'Data.Parameterized.Context.Assignment' list of
-- extraction functions and the expected result value (and name) of
-- that extraction.  Each extraction is performed as a check within
-- the checklist.
--
-- This is convenient to gather together a number of validations on a
-- single datatype and represent them economically.
--
-- One example is testing the fields of a record structure, given the
-- above code:
--
-- >>> defaultMain test
-- someFun result: FAIL
--   Exception: ERROR: results for someFun
--     3 checks failed in this checklist:
--     --- Input for below: The answer to the universe is 18?
--     -Failed check: foo
--             expected:    42
--             failed with: 18
--     -Failed check: shown
--             expected:    "The answer to the universe is 42!"
--             failed with: "The answer to the universe is 18?"
--     -Failed check: double-checking foo
--             expected:    42
--             failed with: 18
-- ...
-- <BLANKLINE>
-- 1 out of 1 tests failed (...s)
-- *** Exception: ExitFailure 1
--
-- In this case, several of the values checked were correct, but more
-- than one was wrong.  Helpfully, this test output lists /all/ the
-- wrong answers for the single input provided.

checkValues :: CanCheck
            => TestShow dType
            => dType -> Ctx.Assignment (DerivedVal dType) idx ->  IO ()
checkValues got expF = do
  join $ evaluate <$> Ctx.traverseWithIndex_ (chkValue got) expF
  -- All the checks are evaluating against the same 'got' value; normally a check
  -- reports the value that caused it to fail but that could get repetitious.
  -- This groups check failures by their input and removes the input from each
  -- checkfailure, instead starting the group with a CheckMessage describing the
  -- input for that entire group.
  let groupByInp chks =
        let gmap = foldr insByInp mempty chks
            insByInp = \case
              c@(CheckFailed _ mbi _) -> Map.insertWith (<>) mbi [c]
              CheckMessage _ -> id -- regrouping, ignore any previous groups
            addGroup (mbi,gchks) =
              let newChks = dropInput <$> gchks
                  dropInput (CheckFailed nm _ fmsg) =
                    if Just (failureMessage fmsg) == (inputAsText <$> mbi)
                    then CheckFailed nm Nothing
                         $ FailureMessage "<< ^^ above input ^^ >>"
                    else CheckFailed nm Nothing fmsg
                  dropInput i@(CheckMessage _) = i
                  dispInput i = if T.length (inputAsText i) > 100
                                then inputAsText i <> "\n\n"
                                else inputAsText i
                  grpTitle = maybe "<no input identified>"
                             (("Input for below: " <>) . dispInput)
                             mbi
              in (<> (newChks <> [CheckMessage grpTitle]))
        in foldr addGroup mempty $ Map.toList gmap

  liftIO $ modifyIORef ?checker groupByInp


chkValue :: CanCheck
         => TestShow dType
         => dType -> Ctx.Index idx valType -> DerivedVal dType valType -> IO ()
chkValue got _idx =
  let ti = Just $ InputAsText $ T.pack $ testShow got
  in \case
    (Val txt fld v) ->
      let msg =
            if T.length txt > 100
            then txt <> "\n\n" <> expline <> "\n\n" <> failedpfx
            else txt <> "\n" <> expline <> failedpfx
          expline = indenT <> "  expected:    " <> tv <> "\n"
          failedpfx = indenT <> "  failed"
          tv = T.pack (testShow v)
      in checkShow testShow ti msg (v ==) $ fld got
    (Observe txt fld v observationReport) ->
      let msg = txt <> " observation failure"
      in checkShow (observationReport v) ti msg (v ==) $ fld got
    (Got txt fld) -> checkShow testShow ti txt id $ fld got

-- | Each entry in the 'Data.Parameterized.Context.Assignment' list
-- for 'checkValues' should be one of these 'DerivedVal' values.
--
-- The @i@ type parameter is the input type, and the @d@ is the value
-- derived from that input type.

data DerivedVal i d where

  -- | Val allows specification of a description string, an extraction
  -- function, and the expected value to be extracted.  The
  -- 'checkValues' function will add a Failure if the expected value is
  -- not obtained.
  Val :: (TestShow d, Eq d) => Text -> (i -> d) -> d -> DerivedVal i d

  -- | Got allows specification of a description string and an
  -- extraction function.  The 'checkValues' function will add a
  -- Failure if the extraction result is False.
  --
  -- > Val "what" f True === Got "what" f
  --
  Got :: Text -> (i -> Bool) -> DerivedVal i Bool

  -- | Observe performs the same checking as Val except the TestShow
  -- information for the actual and expected values are not as useful
  -- (e.g. they are lengthy, multi-line, or gibberish) so instead this
  -- allows the specification of a function that will take the
  -- supplied expected value and the result of the extraction function
  -- (the actual), respectively, and generate its own description of
  -- the failure.
  --
  Observe :: (Eq d) => Text -> (i -> d) -> d -> (d -> d -> String) -> DerivedVal i d

----------------------------------------------------------------------

-- | The 'TestShow' class is defined to provide a way for the various
-- data objects tested by this module to be displayed when tests fail.
-- The default 'testShow' will use a 'Show' instance, but this can be
-- overridden if there are alternate ways to display a particular
-- object (e.g. pretty-printing, etc.)

class TestShow v where
  testShow :: v -> String
  default testShow :: Show v => v -> String
  testShow = show

-- Some TestShow instances using Show for regular datatypes
instance TestShow ()
instance TestShow Bool
instance TestShow Int
instance TestShow Integer
instance TestShow Float
instance TestShow Char
instance TestShow String

instance (TestShow a, TestShow b) => TestShow (a,b) where
  testShow (a,b) = "(" <> testShow a <> ", " <> testShow b <> ")"
instance (TestShow a, TestShow b, TestShow c) => TestShow (a,b,c) where
  testShow (a,b,c) = "(" <> testShow a <> ", " <> testShow b <> ", " <> testShow c <> ")"

-- | A helper function for defining a testShow for lists of items.
--
-- > instance TestShow [Int] where testShow = testShowList

testShowList :: TestShow v => [v] -> String
testShowList  l = "[ " <> (List.intercalate ", " (testShow <$> l)) <> " ]"


-- | The multiLineDiff is another helper function that can be used to
-- format a line-by-line difference display of two Text
-- representations.  This is provided as a convenience function to
-- help format large text regions for easier comparison.

multiLineDiff :: T.Text -> T.Text -> String
multiLineDiff expected actual =
  let dl (e,a) = if e == a then db e else de " ↱" e <> "\n    " <> da " ↳" a
      db b = "|        > " <> b
      de m e = "|" <> m <> "expect> " <> e
      da m a = "|" <> m <> "actual> " <> a
      el = visible <$> T.lines expected
      al = visible <$> T.lines actual
      visible = T.replace " " "␠"
                . T.replace "\n" "␤"
                . T.replace "\t" "␉"
                . T.replace "\012" "␍"
      addnum :: Int -> T.Text -> T.Text
      addnum n l = let nt = T.pack (show n)
                       nl = T.length nt
                   in T.take (4 - nl) "    " <> nt <> l
      ll = T.pack . show . length
      tl = T.pack . show . T.length
      banner = "MISMATCH between "
               <> ll el <> "l/" <> tl expected <> "c expected and "
               <> ll al <> "l/" <> tl actual <> "c actual"
      diffReport = fmap (uncurry addnum) $
                   zip [1..] $ concat $
                   -- Highly simplistic "diff" output assumes
                   -- correlated lines: added or removed lines just
                   -- cause everything to shown as different from that
                   -- point forward.
                   [ fmap dl $ zip el al
                   , fmap (de "∌ ") $ drop (length al) el
                   , fmap (da "∹ ") $ drop (length el) al
                   ]
                   -- n.b. T.lines seems to consume trailing whitespace before
                   -- newlines as well.  This will show any of this whitespace
                   -- difference on the last line, but not for other lines with
                   -- whitespace.
                   <> if el == al
                      then let maxlen = max (T.length expected) (T.length actual)
                               end x = T.drop (maxlen - 5) x
                           in [ [ de "∌ ending " $ visible $ end expected ]
                              , [ da "∹ ending " $ visible $ end actual ]
                              ]
                      else mempty
      details = banner : diffReport
  in if expected == actual then "<no difference>" else T.unpack (T.unlines details)
