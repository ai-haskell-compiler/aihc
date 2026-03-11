{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Test.Data.Registry.Make.TweakingSpec where

import Data.Registry
import Data.Registry.Internal.Types
import Protolude
import Test.Tasty.Extensions

-- | Modification of stored values
test_tweak = test "created values can be modified prior to being stored" $ do
  c1 <- liftIO $
    do
      let r =
            fun newAppUsingConfig1
              <: fun newUseConfig1
              <: val (Config 1)

      let r' = tweak (\(UseConfig1 _) -> UseConfig1 (Config 10)) r
      pure (printAppConfig (make @AppUsingConfig1 r'))

  c1 === Config 10

newtype AppUsingConfig1 = AppUsingConfig1 {printAppConfig :: Config}

newAppUsingConfig1 config1 = AppUsingConfig1 {printAppConfig = printConfig1 config1}

newtype Config = Config Int deriving (Eq, Show)

newtype UseConfig1 = UseConfig1 {printConfig1 :: Config}

newUseConfig1 config = UseConfig1 {printConfig1 = config}

-- * =========

test_tweak_non_lossy = test "a modified value must not lose its context, specialization or dependencies" $ do
  (a, stats) <- liftIO $ do
    let r =
          tweak (\(B (C _)) -> B (C 3))
            . specialize @A @C (val $ C 2)
            $ fun A
              <: fun B
              <: val (C 1)
    pure (make @A r, makeStatistics @A r)

  -- The specialized value was 2 but after tweaking it is 3
  a === A (B (C 3))

  -- Get the value for the type C
  let cValue = findMostRecentValue @C stats

  annotateShow stats
  annotateShow cValue
  annotateShow (findCreatedValues @C stats)
  isJust (valueContext =<< cValue) === True
  isJust (valueSpecialization =<< cValue) === True

  -- this seems weird but a value is in the list of its dependencies
  (not . null) (valueDependencies <$> cValue) === True

newtype A = A B deriving (Eq, Show)

newtype B = B C deriving (Eq, Show)

newtype C = C Int deriving (Eq, Show)
