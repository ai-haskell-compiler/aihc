{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
-- |
-- Introductory documentation: <https://github.com/hspec/hspec-expectations#readme>
module Test.Hspec.Expectations.Lifted (

-- * Setting expectations
  expectationFailure
, shouldBe
, shouldSatisfy
, shouldStartWith
, shouldEndWith
, shouldContain
, shouldMatchList
, shouldReturn

, shouldNotBe
, shouldNotSatisfy
, shouldNotContain
, shouldNotReturn

-- * Re-exports
, HasCallStack
) where

import           Control.Monad.IO.Class
import qualified Test.Hspec.Expectations as E
import           Test.Hspec.Expectations (HasCallStack)

infix 1 `shouldBe`, `shouldSatisfy`, `shouldStartWith`, `shouldEndWith`, `shouldContain`, `shouldMatchList`, `shouldReturn`
infix 1 `shouldNotBe`, `shouldNotSatisfy`, `shouldNotContain`, `shouldNotReturn`

liftIO2 :: MonadIO m => (a -> b -> IO r) -> a -> b -> m r
liftIO2 action a = liftIO . action a

expectationFailure :: (HasCallStack, MonadIO m) => String -> m ()
expectationFailure = liftIO . E.expectationFailure

-- |
-- @actual \`shouldBe\` expected@ sets the expectation that @actual@ is equal
-- to @expected@.
shouldBe :: (HasCallStack, MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldBe = liftIO2 E.shouldBe

-- |
-- @v \`shouldSatisfy\` p@ sets the expectation that @p v@ is @True@.
shouldSatisfy :: (HasCallStack, MonadIO m, Show a) => a -> (a -> Bool) -> m ()
shouldSatisfy = liftIO2 E.shouldSatisfy

-- |
-- @list \`shouldStartWith\` prefix@ sets the expectation that @list@ starts with @prefix@,
shouldStartWith :: (HasCallStack, MonadIO m, Show a, Eq a) => [a] -> [a] -> m ()
shouldStartWith = liftIO2 E.shouldStartWith

-- |
-- @list \`shouldEndWith\` suffix@ sets the expectation that @list@ ends with @suffix@,
shouldEndWith :: (HasCallStack, MonadIO m, Show a, Eq a) => [a] -> [a] -> m ()
shouldEndWith = liftIO2 E.shouldEndWith

-- |
-- @list \`shouldContain\` sublist@ sets the expectation that @sublist@ is contained,
-- wholly and intact, anywhere in @list@.
shouldContain :: (HasCallStack, MonadIO m, Show a, Eq a) => [a] -> [a] -> m ()
shouldContain = liftIO2 E.shouldContain

-- |
-- @xs \`shouldMatchList\` ys@ sets the expectation that @xs@ has the same
-- elements that @ys@ has, possibly in another order
shouldMatchList :: (HasCallStack, MonadIO m, Show a, Eq a) => [a] -> [a] -> m ()
shouldMatchList = liftIO2 E.shouldMatchList

-- |
-- @action \`shouldReturn\` expected@ sets the expectation that @action@
-- returns @expected@.
shouldReturn :: (HasCallStack, MonadIO m, Show a, Eq a) => m a -> a -> m ()
shouldReturn action expected = action >>= liftIO . (`E.shouldBe` expected)

-- |
-- @actual \`shouldNotBe\` notExpected@ sets the expectation that @actual@ is not
-- equal to @notExpected@
shouldNotBe :: (HasCallStack, MonadIO m, Show a, Eq a) => a -> a -> m ()
shouldNotBe = liftIO2 E.shouldNotBe

-- |
-- @v \`shouldNotSatisfy\` p@ sets the expectation that @p v@ is @False@.
shouldNotSatisfy :: (HasCallStack, MonadIO m, Show a) => a -> (a -> Bool) -> m ()
shouldNotSatisfy = liftIO2 E.shouldNotSatisfy

-- |
-- @list \`shouldNotContain\` sublist@ sets the expectation that @sublist@ is not
-- contained anywhere in @list@.
shouldNotContain :: (HasCallStack, MonadIO m, Show a, Eq a) => [a] -> [a] -> m ()
shouldNotContain = liftIO2 E.shouldNotContain

-- |
-- @action \`shouldNotReturn\` notExpected@ sets the expectation that @action@
-- does not return @notExpected@.
shouldNotReturn :: (HasCallStack, MonadIO m, Show a, Eq a) => m a -> a -> m ()
shouldNotReturn action expected = action >>= liftIO . (`E.shouldNotBe` expected)
