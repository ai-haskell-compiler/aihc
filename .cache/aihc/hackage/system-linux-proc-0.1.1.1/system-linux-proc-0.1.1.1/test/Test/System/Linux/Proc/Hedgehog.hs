module Test.System.Linux.Proc.Hedgehog
  ( assertShow
  , predicate
  , predicate2
  , propertyWithTests
  ) where

import           GHC.Stack (HasCallStack, withFrozenCallStack)

import           Hedgehog (Property, PropertyT, TestLimit,
                    diff, property, success, withTests)
import           Hedgehog.Internal.Property (MonadTest, eval, failWith)

import           Text.Show.Pretty (ppShow)

assertShow :: (MonadTest m, HasCallStack, Show a) => a -> Bool -> m ()
assertShow a b =
  if b then
    success
  else
    withFrozenCallStack $ failWith Nothing (ppShow a)

predicate :: (MonadTest m, HasCallStack, Show a) => a -> (a -> Bool) -> m ()
predicate a prd = do
  ok <- withFrozenCallStack $ eval (prd a)
  if ok then
    success
  else
    withFrozenCallStack $ failWith Nothing (ppShow a)

predicate2 :: (MonadTest m, HasCallStack, Show a) => a -> a -> (a -> a -> Bool) -> m ()
predicate2 a b prd =
  withFrozenCallStack $ diff a prd b

propertyWithTests :: TestLimit -> PropertyT IO () -> Property
propertyWithTests n = withTests n . property
