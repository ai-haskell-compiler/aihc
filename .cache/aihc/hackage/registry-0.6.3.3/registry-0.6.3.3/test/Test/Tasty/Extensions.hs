{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
--
-- Registry      : Test.Tasty.Extensions
-- Description : Tasty / Hedgehog / HUnit integration
--
-- This module unifies property based testing with Hedgehog
-- and one-off tests.
module Test.Tasty.Extensions
  ( module Hedgehog,
    module Tasty,
    gotException,
    groupByModuleName,
    minTestsOk,
    noShrink,
    prop,
    run,
    runOnly,
    test,
    withSeed,
  )
where

import Data.MultiMap
import GHC.Stack
import Hedgehog hiding (test)
import Hedgehog.Gen as Hedgehog hiding (discard, print)
import Hedgehog.Internal.Property (PropertyName (..))
import Protolude hiding (empty, toList, (.&.))
import System.Environment
import Test.Tasty as Tasty
import Test.Tasty.Hedgehog as Tasty
import Test.Tasty.Options as Tasty
import Test.Tasty.Providers as Tasty (singleTest)
import Test.Tasty.Runners as Tasty (TestTree (..), foldSingle, foldTestTree, trivialFold)
import qualified Prelude

-- | Create a Tasty test from a Hedgehog property
prop :: HasCallStack => TestName -> PropertyT IO () -> TestTree
prop name p =
  let aModuleName = getModuleName
   in withFrozenCallStack $
        localOption (ModuleName (toS aModuleName)) $
          testPropertyNamed name (PropertyName name) (Hedgehog.property p)

-- | Create a Tasty test from a Hedgehog property called only once
test :: HasCallStack => TestName -> PropertyT IO () -> TestTree
test name p = withFrozenCallStack (minTestsOk 1 . noShrink $ prop name p)

gotException :: forall a. (HasCallStack, Show a) => a -> PropertyT IO ()
gotException a = withFrozenCallStack $ do
  res <- liftIO (try (evaluate a) :: IO (Either SomeException a))
  case res of
    Left _ -> assert True
    Right _ -> annotateShow ("excepted an exception" :: Text) >> assert False

-- * Parameters

minTestsOk :: Int -> TestTree -> TestTree
minTestsOk n = localOption (HedgehogTestLimit (Just (toEnum n :: TestLimit)))

noShrink :: TestTree -> TestTree
noShrink = localOption (HedgehogShrinkLimit (Just (0 :: ShrinkLimit)))

withSeed :: Prelude.String -> TestTree -> TestTree
withSeed seed tree =
  case parseValue seed of
    Nothing -> prop ("cannot parse seed " <> seed) failure
    Just (s :: HedgehogReplay) -> localOption s tree

-- * GROUPING

-- | Extract the ModuleName option value for a given test and
--   group all the tests with that option into the same test group
groupByModuleName :: TestTree -> TestTree
groupByModuleName testTree =
  let grouped =
        assocs $
          foldTestTree
            ( trivialFold
                { foldSingle = \os n t ->
                    let (ModuleName aModuleName) = lookupOption os :: ModuleName
                     in insert (toS aModuleName) (setOptionSet os $ singleTest n t) empty
                }
            )
            mempty
            testTree
   in TestGroup "All" (uncurry TestGroup <$> grouped)

instance (Ord k) => Semigroup (MultiMap k v) where
  (<>) m1 m2 = fromList (toList m1 <> toList m2)

instance (Ord k) => Monoid (MultiMap k v) where
  mempty = empty
  mappend = (<>)

-- | This is unfortunate. Due to the API for `foldTestTree` in Tasty
--   giving back the current `OptionSet` applicable to a single test
--   it is not possible to re-set those option values on that test
--   without listing them exhaustively. This means
--   that if other options are set on tests in that file, they need to be
--   added in that function
setOptionSet :: OptionSet -> TestTree -> TestTree
setOptionSet os =
  localOption (lookupOption os :: HedgehogTestLimit)
    . localOption (lookupOption os :: HedgehogShrinkLimit)
    . localOption (lookupOption os :: HedgehogReplay)

getModuleName :: HasCallStack => Prelude.String
getModuleName =
  case getCallStack callStack of
    ((_, loc) : _) -> srcLocModule loc
    _other -> "root"

-- | Option describing the current module name
newtype ModuleName = ModuleName Text deriving (Eq, Show)

-- | The option triggering the database tests is called 'postgres' for compatibility reasons
instance IsOption ModuleName where
  defaultValue = ModuleName "root"
  parseValue = fmap ModuleName . safeRead
  optionName = pure "module-name"
  optionHelp = pure "internal option used to group tests into the same module"
  optionCLParser = mkFlagCLParser mempty (ModuleName "root")

-- * GHCi run functions

run :: Runnable t => t -> IO ()
run tests = runIt tests >>= defaultMain . groupByModuleName

runOnly :: Runnable t => Text -> t -> IO ()
runOnly p tests = do
  setEnv "TASTY_PATTERN" (toS p)
  run tests `finally` unsetEnv "TASTY_PATTERN"

class Runnable t where
  runIt :: t -> IO TestTree

instance Runnable (IO TestTree) where
  runIt t = t

instance Runnable TestTree where
  runIt = pure
