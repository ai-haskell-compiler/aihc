{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.QuickCheck.IO where

import qualified Control.Exception as E
import           Test.HUnit.Lang
import           Test.QuickCheck.Property

instance Testable Assertion where
  property = propertyIO
#if !MIN_VERSION_QuickCheck(2,9,0)
  exhaustive _ = True
#endif

propertyIO :: Assertion -> Property
propertyIO action = ioProperty $ do
  (action >> return succeeded) `E.catch` \ e ->
    return failed {theException = Just (E.toException e), reason = formatAssertion e}
  where
    formatAssertion e = case e of
#if MIN_VERSION_HUnit(1,3,0)
      HUnitFailure _ err ->
#else
      HUnitFailure err ->
#endif
#if MIN_VERSION_HUnit(1,5,0)
        formatFailureReason err
#else
        err
#endif
