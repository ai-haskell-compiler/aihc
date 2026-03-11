{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestLib where

-- https://hackage.haskell.org/package/ansi-terminal
import qualified System.Console.ANSI as Term

-- https://hackage.haskell.org/package/base
import Control.Monad.IO.Class (MonadIO(liftIO))
import GHC.Stack (HasCallStack)

-- https://hackage.haskell.org/package/HMock
import Test.HMock (makeMockable)

-- https://hackage.haskell.org/package/tasty-hunit
import qualified Test.Tasty.HUnit as HUnit

-- (redact)
import Redact.Monad.Handle (MonadHandle)
import qualified Redact.Monad.Terminal as RMT
import Redact.Monad.Terminal (MonadTerminal)

------------------------------------------------------------------------------

makeMockable [t|MonadHandle|]

makeMockable [t|MonadTerminal|]

------------------------------------------------------------------------------

redactSGRs :: [Term.SGR]
redactSGRs = RMT.redactSGRs Term.Red Term.Vivid

------------------------------------------------------------------------------

assertSuccess
  :: (HasCallStack, MonadIO m, Show e)
  => Either e ()
  -> m ()
assertSuccess = \case
    Right () -> pure ()
    Left err ->
      liftIO . HUnit.assertFailure $ "unexpected error: " ++ show err
