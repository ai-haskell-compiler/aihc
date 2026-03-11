{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      :  Language.Haskell.TH.TestUtils
Maintainer  :  Brandon Chinn <brandonchinn178@gmail.com>
Stability   :  experimental
Portability :  portable

This module defines utilites for testing Template Haskell code.
-}
module Language.Haskell.TH.TestUtils (
  -- * Configuring TestQ
  QState (..),
  MockedMode (..),
  QMode (..),
  ReifyInfo (..),
  loadNames,
  unmockedState,

  -- * Running TestQ
  runTestQ,
  runTestQErr,
  tryTestQ,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as Except
import qualified Control.Monad.Trans.Reader as Reader
import qualified Control.Monad.Trans.State as State
import Data.Maybe (fromMaybe)
import Language.Haskell.TH (Name, Q, runIO, runQ)
import Language.Haskell.TH.Syntax (Quasi (..), mkNameU)

import Language.Haskell.TH.TestUtils.QMode
import Language.Haskell.TH.TestUtils.QState

runTestQ :: forall mode a. (IsMockedMode mode) => QState mode -> Q a -> TestQResult mode a
runTestQ state = fmapResult' (either error id) . tryTestQ state
  where
    fmapResult' = fmapResult @mode @(Either String a) @a

runTestQErr :: forall mode a. (IsMockedMode mode, Show a) => QState mode -> Q a -> TestQResult mode String
runTestQErr state = fmapResult' (either id (error . mkMsg)) . tryTestQ state
  where
    fmapResult' = fmapResult @mode @(Either String a) @String
    mkMsg a = "Unexpected success: " ++ show a

tryTestQ :: forall mode a. (IsMockedMode mode) => QState mode -> Q a -> TestQResult mode (Either String a)
tryTestQ state = runResult @mode . runTestQMonad . runQ
  where
    runTestQMonad =
      Except.runExceptT
        . (`State.evalStateT` initialInternalState)
        . (`Reader.runReaderT` state)
        . unTestQ

    initialInternalState =
      InternalState
        { lastErrorReport = Nothing
        , newNameCounter = 0
        }

data InternalState = InternalState
  { lastErrorReport :: Maybe String
  , newNameCounter :: Int
  }

newtype TestQ (mode :: MockedMode) a = TestQ
  { unTestQ ::
      Reader.ReaderT
        (QState mode)
        ( State.StateT
            InternalState
            ( Except.ExceptT
                String
                Q
            )
        )
        a
  }
  deriving (Functor, Applicative, Monad)

{- TestQ stack: ReaderT -}

getState :: TestQ mode (QState mode)
getState = TestQ Reader.ask

getMode :: TestQ mode (QMode mode)
getMode = mode <$> getState

lookupReifyInfo :: (ReifyInfo -> a) -> Name -> TestQ mode a
lookupReifyInfo f name = do
  QState{reifyInfo} <- getState
  case lookup name reifyInfo of
    Just info -> return $ f info
    Nothing -> error $ "Cannot reify " ++ show name ++ " (did you mean to add it to reifyInfo?)"

{- TestQ stack: StateT -}

getLastError :: TestQ mode (Maybe String)
getLastError = TestQ . lift $ State.gets lastErrorReport

storeLastError :: String -> TestQ mode ()
storeLastError msg = TestQ . lift $ State.modify (\state -> state{lastErrorReport = Just msg})

getAndIncrementNewNameCounter :: TestQ mode Int
getAndIncrementNewNameCounter = TestQ . lift $ State.state $ \state ->
  let n = newNameCounter state
   in (n, state{newNameCounter = n + 1})

{- TestQ stack: ExceptT -}

throwError :: String -> TestQ mode a
throwError = TestQ . lift . lift . Except.throwE

catchError :: TestQ mode a -> (String -> TestQ mode a) -> TestQ mode a
catchError (TestQ action) handler = TestQ $ catchE' action (unTestQ . handler)
  where
    catchE' = Reader.liftCatch (State.liftCatch Except.catchE)

{- TestQ stack: Q -}

liftQ :: Q a -> TestQ mode a
liftQ = TestQ . lift . lift . lift

{- Instances -}

instance MonadIO (TestQ mode) where
  liftIO = liftQ . runIO

instance MonadFail (TestQ mode) where
  fail msg = do
    -- The implementation of 'fail' for Q will send the message to qReport before calling 'fail'.
    -- Check to see if qReport put any message in the state and throw that message if so.
    lastMessage <- getLastError
    throwError $ fromMaybe msg lastMessage

-- | A helper to override Quasi methods when mocked and passthrough when not.
use :: Override mode a -> TestQ mode a
use Override{..} = do
  mode <- getMode
  case (mode, whenMocked) of
    (AllowQ, _) -> liftQ whenAllowed
    (_, DoInstead testQ) -> testQ
    (_, Unsupported label) -> error $ "Cannot run '" ++ label ++ "' with TestQ"

data Override mode a = Override
  { whenAllowed :: Q a
  , whenMocked :: WhenMocked mode a
  }

data WhenMocked mode a
  = DoInstead (TestQ mode a)
  | Unsupported String

instance Quasi (TestQ mode) where
  {- IO -}

  qRunIO io =
    getMode >>= \case
      MockQ -> error "IO actions not allowed"
      _ -> liftIO io

  {- Error handling + reporting -}

  qRecover handler action = action `catchError` const handler

  qReport False msg =
    use
      Override
        { whenAllowed = qReport False msg
        , whenMocked = DoInstead $ return ()
        }
  qReport True msg = storeLastError msg

  {- Names -}

  qNewName name =
    use
      Override
        { whenAllowed = qNewName name
        , whenMocked = DoInstead $ mkNameU name . fromIntegral <$> getAndIncrementNewNameCounter
        }

  qLookupName b name =
    use
      Override
        { whenAllowed = qLookupName b name
        , whenMocked = DoInstead $ do
            QState{knownNames} <- getState
            return $ lookup name knownNames
        }

  {- ReifyInfo -}

  qReify name =
    use
      Override
        { whenAllowed = qReify name
        , whenMocked = DoInstead $ lookupReifyInfo reifyInfoInfo name
        }

  qReifyFixity name =
    use
      Override
        { whenAllowed = qReifyFixity name
        , whenMocked = DoInstead $ lookupReifyInfo reifyInfoFixity name
        }

  qReifyRoles name =
    use
      Override
        { whenAllowed = qReifyRoles name
        , whenMocked =
            DoInstead $
              lookupReifyInfo reifyInfoRoles name >>= \case
                Nothing -> error $ "No roles associated with " ++ show name
                Just roles -> return roles
        }

  qReifyType name =
    use
      Override
        { whenAllowed = qReifyType name
        , whenMocked = DoInstead $ lookupReifyInfo reifyInfoType name
        }

  {- Currently unsupported -}

  qReifyInstances name types =
    use
      Override
        { whenAllowed = qReifyInstances name types
        , whenMocked = Unsupported "qReifyInstances"
        }
  qReifyAnnotations annlookup =
    use
      Override
        { whenAllowed = qReifyAnnotations annlookup
        , whenMocked = Unsupported "qReifyAnnotations"
        }
  qReifyModule mod' =
    use
      Override
        { whenAllowed = qReifyModule mod'
        , whenMocked = Unsupported "qReifyModule"
        }
  qReifyConStrictness name =
    use
      Override
        { whenAllowed = qReifyConStrictness name
        , whenMocked = Unsupported "qReifyConStrictness"
        }
  qLocation =
    use
      Override
        { whenAllowed = qLocation
        , whenMocked = Unsupported "qLocation"
        }
  qAddDependentFile fp =
    use
      Override
        { whenAllowed = qAddDependentFile fp
        , whenMocked = Unsupported "qAddDependentFile"
        }
  qAddTopDecls decls =
    use
      Override
        { whenAllowed = qAddTopDecls decls
        , whenMocked = Unsupported "qAddTopDecls"
        }
  qAddModFinalizer q =
    use
      Override
        { whenAllowed = qAddModFinalizer q
        , whenMocked = Unsupported "qAddModFinalizer"
        }
  qGetQ =
    use
      Override
        { whenAllowed = qGetQ
        , whenMocked = Unsupported "qGetQ"
        }
  qPutQ a =
    use
      Override
        { whenAllowed = qPutQ a
        , whenMocked = Unsupported "qPutQ"
        }
  qIsExtEnabled ext =
    use
      Override
        { whenAllowed = qIsExtEnabled ext
        , whenMocked = Unsupported "qIsExtEnabled"
        }
  qExtsEnabled =
    use
      Override
        { whenAllowed = qExtsEnabled
        , whenMocked = Unsupported "qExtsEnabled"
        }
  qAddCorePlugin plugin =
    use
      Override
        { whenAllowed = qAddCorePlugin plugin
        , whenMocked = Unsupported "qAddCorePlugin"
        }
  qAddTempFile suffix =
    use
      Override
        { whenAllowed = qAddTempFile suffix
        , whenMocked = Unsupported "qAddTempFile"
        }
  qAddForeignFilePath lang fp =
    use
      Override
        { whenAllowed = qAddForeignFilePath lang fp
        , whenMocked = Unsupported "qAddForeignFilePath"
        }

#if MIN_VERSION_template_haskell(2,18,0)
  qPutDoc loc doc = use Override
    { whenAllowed = qPutDoc loc doc
    , whenMocked = Unsupported "qPutDoc"
    }

  qGetDoc loc = use Override
    { whenAllowed = qGetDoc loc
    , whenMocked = Unsupported "qGetDoc"
    }
#endif

#if MIN_VERSION_template_haskell(2,19,0)
  qGetPackageRoot =
    use
      Override
        { whenAllowed = qGetPackageRoot
        , whenMocked = Unsupported "qGetPackageRoot"
        }
#endif
