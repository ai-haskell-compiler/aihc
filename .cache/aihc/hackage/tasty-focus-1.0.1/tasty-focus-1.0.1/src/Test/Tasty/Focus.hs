-- | Simple focus mechanism for @tasty@, similar to @hspec@.
-- Mark the root of your test tree with 'withFocus'.
-- Then, if any of the subtrees of your test suite are marked with 'focus', only those test trees will be run.
--
-- @
-- main = defaultMain . 'withFocus' $
--   testGroup "tests"
--     [ fooTests
--     , testGroup "subgroup"
--       [ 'focus' barTests
--       , bazTests
-- 	  ]
-- 	, quuxTests
--    ]
-- @

module Test.Tasty.Focus
  ( withFocus,
    focus,
  )
where

import Data.Monoid
import Data.Tagged
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Runners

data Focused = Focused | NotFocused

instance IsOption Focused where
  defaultValue = NotFocused
  parseValue _ = Nothing
  optionName = Tagged "focused"
  optionHelp = Tagged "focused"

anyFocused :: TestTree -> Bool
anyFocused = getAny . foldTestTree tfold mempty
  where
    tfold = trivialFold {foldSingle = \opts _ _ -> Any (focusedOpts opts)}
    focusedOpts opts = case lookupOption opts of
      Focused -> True
      NotFocused -> False

-- | Intended to be used at the root of your test suite.
--   If any of the subtrees are focused, filter out all non-focused subtrees.
--   If there are no focused subtrees, return the entire tree.
withFocus :: TestTree -> TestTree
withFocus tree = if anyFocused tree then go tree else tree
  where
    go (PlusTestOptions f t) = case lookupOption (f mempty) of
      NotFocused -> TestGroup "ignored" []
      Focused -> PlusTestOptions f t
    go (TestGroup n t) = TestGroup n (fmap go . filter anyFocused $ t)
    go (SingleTest n t) = SingleTest n t
    go (WithResource s k) = WithResource s (go . k)
    go (AskOptions f) = AskOptions (go . f)
    go (After d e t) = After d e (go t)

-- | Marks the tree as focused, as long as none of its subtrees are focused.
--
-- This funcion is marked as deprecated so that @-Werror@ will catch it if you accidentally leave tests focused.
focus :: TestTree -> TestTree
focus tree =
  if anyFocused tree
    then tree
    else testGroup "focused" [PlusTestOptions (setOption Focused) tree]

{-# WARNING focus "Focusing tests... don't forget to re-enable your entire test suite!" #-}
