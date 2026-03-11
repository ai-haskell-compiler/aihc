{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Haskell.TH.TestUtils.QMode (
  MockedMode (..),
  QMode (..),
  IsMockedMode (..),
) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift)
import System.IO.Unsafe (unsafePerformIO)

data MockedMode = FullyMocked | FullyMockedWithIO | NotMocked

class IsMockedMode (mode :: MockedMode) where
  type TestQResult mode a

  runResult :: Q a -> TestQResult mode a
  fmapResult :: (a -> b) -> TestQResult mode a -> TestQResult mode b

instance IsMockedMode 'FullyMocked where
  type TestQResult 'FullyMocked a = a
  runResult = unsafePerformIO . runQ
  fmapResult = ($)

instance IsMockedMode 'FullyMockedWithIO where
  type TestQResult 'FullyMockedWithIO a = IO a
  runResult = runQ
  fmapResult = fmap

instance IsMockedMode 'NotMocked where
  type TestQResult 'NotMocked a = Q a
  runResult = id
  fmapResult = fmap

{- Configuring TestQ -}

data QMode (mode :: MockedMode) where
  -- | All Q actions are mocked and IO actions are disallowed.
  MockQ :: QMode 'FullyMocked
  -- | Same as MockQ, except IO actions are passed through.
  -- Useful if your TH code, for example, reads files with runIO.
  MockQAllowIO :: QMode 'FullyMockedWithIO
  -- | No mocking is done.
  -- Useful for running Q as normal, but you need to get error messages.
  AllowQ :: QMode 'NotMocked

deriving instance Show (QMode mode)
deriving instance Lift (QMode mode)
