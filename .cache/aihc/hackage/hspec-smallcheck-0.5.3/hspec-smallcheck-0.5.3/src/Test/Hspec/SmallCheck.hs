{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Hspec.SmallCheck (property) where

import           Prelude ()
import           Test.Hspec.SmallCheck.Compat

import           Data.IORef
import           Test.Hspec.Core.Spec
import           Test.SmallCheck
import           Test.SmallCheck.Drivers
import qualified Test.HUnit.Lang as HUnit
import           Control.Exception (try)
import           Data.Maybe
import           Data.CallStack

import qualified Test.Hspec.SmallCheck.Types as T

property :: Testable IO a => a -> Property IO
property = test

srcLocToLocation :: SrcLoc -> Location
srcLocToLocation loc = Location {
  locationFile = srcLocFile loc
, locationLine = srcLocStartLine loc
, locationColumn = srcLocStartCol loc
}

instance Testable IO (IO ()) where
  test action = monadic $ do
    r <- try action
    return $ case r of
      Right () -> test True
      Left e -> case e of
        HUnit.HUnitFailure loc reason -> test . failure $ case reason of
          HUnit.Reason s -> T.Reason s
          HUnit.ExpectedButGot prefix expected actual -> T.ExpectedActual (fromMaybe "" prefix) expected actual
          where
            failure :: T.Reason -> Either String String
            failure = Left . show . T.Failure (srcLocToLocation <$> loc)

instance Example (Property IO) where
  type Arg (Property IO) = ()
  evaluateExample p c _ reportProgress = do
    counter <- newIORef 0
    let hook _ = do
          modifyIORef counter succ
          n <- readIORef counter
          reportProgress (n, 0)
    r <- smallCheckWithHook (fromMaybe 5 $ paramsSmallCheckDepth c) hook p
    return . Result "" $ case r of
      Just e -> case T.parseResult (ppFailure e) of
        (m, Just (T.Failure loc reason)) -> Failure loc $ case reason of
          T.Reason err -> Reason (fromMaybe "" $ T.concatPrefix m err)
          T.ExpectedActual prefix expected actual -> ExpectedButGot (T.concatPrefix m prefix) expected actual
        (m, Nothing) -> Failure Nothing (Reason m)
      Nothing -> Success
