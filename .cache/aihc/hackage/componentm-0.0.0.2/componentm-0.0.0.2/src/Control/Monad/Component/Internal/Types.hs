{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.Monad.Component.Internal.Types
  ( ComponentError (..)
  , ComponentBuildError (..)
  , ComponentM (..)
  , Build (..)
  , BuildResult (..)
  , TeardownResult
  , ComponentEvent (..)
  , buildTableToOrderedList
  , buildTableToTeardown
  ) where

import           RIO
import qualified RIO.HashMap               as M.Hash
import qualified RIO.Set                   as S
import           RIO.Time                  (NominalDiffTime)

import           Data.Text.Prettyprint.Doc (Pretty, pretty, (<+>))
import qualified Data.Text.Prettyprint.Doc as Pretty

import           Control.Monad.Catch       (MonadThrow (..))
import           Data.Graph                (graphFromEdges', topSort)

import           Control.Teardown          (Teardown, TeardownResult,
                                            newTeardown)

import           Text.Show.Pretty          (ppShow)

--------------------------------------------------------------------------------

-- | Exception thrown by the 'runComponentM' family of functions
data ComponentError
    -- | Failure raised when the Application Callback given to a 'runComponentM'
    -- function throws an exception
  = ComponentRuntimeFailed
    {
      -- | Exception that was originally thrown by the Application Callback
      componentErrorOriginalException :: !SomeException
      -- | Result from the execution allocated resources teardown
    , componentErrorTeardownResult    :: !TeardownResult
    }
    -- | Failure raised when execution of 'ComponentM' throws an exception
  | ComponentBuildFailed
    {
      -- | Exceptions thrown by 'IO' sub-routines used when constructing
      -- 'ComponentM' values (e.g. 'buildComponent')
      componentErrorBuildErrors    :: ![ComponentBuildError]
      -- | Result from the execution allocated resources teardown
    , componentErrorTeardownResult :: !TeardownResult
    }
  deriving (Generic, Show)

instance Exception ComponentError

instance Pretty ComponentError where
  pretty err =
    case err of
      ComponentBuildFailed errList teardownResult ->
        "Application failed on initialization, following are the exceptions that made it failed:"
        <> Pretty.hardline
        <> Pretty.hardline
        <> Pretty.indent 2 (Pretty.vsep $ map (\buildErr -> "* " <> pretty buildErr <> Pretty.hardline) errList)
        <> Pretty.hardline
        <> "Following, we have the information of application resources cleanup:"
        <> Pretty.hardline
        <> Pretty.hardline
        <> pretty teardownResult

      ComponentRuntimeFailed runtimeErr teardownResult ->
        "Application failed at runtime, following is the exception that made the app failed:"
        <> Pretty.hardline
        <> Pretty.hardline
        <> Pretty.indent 2 (pretty $ ppShow runtimeErr)
        <> Pretty.hardline
        <> "Following, we have the information of application resources cleanup:"
        <> Pretty.hardline
        <> pretty teardownResult

-- | Exception raised on the execution of 'IO' sub-routines used when
-- constructing 'ComponentM' values (e.g. 'buildComponent')
data ComponentBuildError
  -- | Failure thrown when using the same component key on a Component composition
  = DuplicatedComponentKeyDetected !Description
  -- | Failure thrown when the allocation sub-routine of a Component fails with an exception
  | ComponentAllocationFailed !Description !SomeException
  -- | Failure thrown when calling the 'throwM' when composing 'ComponentM' values
  | ComponentErrorThrown !SomeException
  -- | Failure thrown when calling 'liftIO' fails with an exception
  | ComponentIOLiftFailed !SomeException
  deriving (Generic, Show)

instance Exception ComponentBuildError

instance Pretty ComponentBuildError where
  pretty err =
    case err of
      DuplicatedComponentKeyDetected desc ->
        "DuplicateComponentKeyDetected" <+> pretty (show desc) <+> "- please, make sure that component names are unique"

      ComponentAllocationFailed desc componentErr ->
        "ComponentAllocationFailed" <+> pretty (show desc) <+> "- the following error was reported:"
        <> Pretty.nest 2 (Pretty.hardline
                          <> "|" <> Pretty.hardline
                          <> Pretty.nest 4 ("`-" <+> pretty (ppShow componentErr)))


      ComponentErrorThrown thrownErr ->
        "ComponentErrorThrown - the following error was thrown using the `throwM` function:"
        <> Pretty.nest 2 (Pretty.hardline
                          <> "|" <> Pretty.hardline
                          <> Pretty.nest 4 ("`-" <+> pretty (ppShow thrownErr)))

      ComponentIOLiftFailed ioErr ->
        "ComponentIOLiftFailed - the following error was thrown from an `IO` operation invoked via `liftIO`:"
        <> Pretty.nest 2 (Pretty.hardline
                          <> "|" <> Pretty.hardline
                          <> Pretty.nest 4 ("`-" <+> pretty (ppShow ioErr)))

type Description = Text

-- | Contains metadata about the build of a resource from a 'ComponentM' value
data Build
  = Build {
      -- | Name of the component built
      componentDesc     :: !Description
      -- | Cleanup sub-routine of the component built
    , componentTeardown :: !Teardown
      -- | Elasped time in the allocation of a component resource
    , buildElapsedTime  :: !NominalDiffTime
      -- | Error thrown in the allocation of a component resource
    , buildFailure      :: !(Maybe SomeException)
      -- | What other components this build depends on
    , buildDependencies :: !(Set Description)
    }
  deriving (Generic)

instance Pretty Build where
  pretty Build {componentDesc, buildElapsedTime, buildFailure} =
    let
      statusSymbol :: Text
      statusSymbol = if isJust buildFailure then "✘" else "✓"

      errorInfo =
        if isJust buildFailure then
          [
            Pretty.hardline
          , Pretty.pipe <+> pretty (ppShow buildFailure)
          ]
        else
          []

    in
      Pretty.hang 2
          $ Pretty.hsep
          $ [ pretty statusSymbol
            , pretty componentDesc
            , Pretty.parens (pretty $ show buildElapsedTime)
            ] <> errorInfo


instance Display Build where
  display = displayShow . pretty

type BuildTable = HashMap Description Build

-- | Wraps a collection of 'Build' records
newtype BuildResult
  = BuildResult { toBuildList :: [Build] }

instance Pretty BuildResult where
  pretty (BuildResult builds) =
      "Following, we have the information of the application resources initialization:"
      <> Pretty.hardline
      <> Pretty.hardline
      <> Pretty.vsep (map pretty builds)
      <> Pretty.hardline

instance Display BuildResult where
  display buildResult =
    displayShow $ pretty buildResult

-- | An event record used to trace the execution of an application
-- initialization and teardown
data ComponentEvent
  = ComponentBuilt !BuildResult
  | ComponentReleased !TeardownResult
  | ComponentErrorDetected !ComponentError

instance Pretty ComponentEvent where
  pretty ev =
    case ev of
      ComponentBuilt buildResult ->
        Pretty.hardline
        <> "# Application Initialized"
        <> Pretty.hardline
        <> Pretty.hardline
        <> pretty buildResult
        <> Pretty.hardline

      ComponentReleased teardownResult ->
        Pretty.hardline
        <> "# Application Finished"
        <> Pretty.hardline
        <> Pretty.hardline
        <> pretty teardownResult
        <> Pretty.hardline

      ComponentErrorDetected err ->
        Pretty.hardline
        <> "# Application Failed"
        <> Pretty.hardline
        <> Pretty.hardline
        <> pretty err
        <> Pretty.hardline

instance Display ComponentEvent where
  display = displayShow . pretty

--------------------

-- | Represents the construction of a Component in your application, components
-- may be composed using a 'Monad' or 'Applicative' interface.
newtype ComponentM a
  = ComponentM (IO (Either ([ComponentBuildError], BuildTable)
                           (a, BuildTable)))

--------------------

instance Functor ComponentM where
  fmap f (ComponentM action) =
    ComponentM $ do
      result <- action
      return $! case result of
        Left err ->
          Left err
        Right (a, builds) ->
          Right (f a, builds)

--------------------

validateKeyDuplication
  :: Monad m
  => (HashMap Text v -> HashMap Text v -> HashMap Text v)
  -> HashMap Text v
  -> HashMap Text v
  -> m
       ( Either
           ([ComponentBuildError], HashMap Text v)
           (HashMap Text v)
       )
validateKeyDuplication mergeFn a b =
  case M.Hash.keys $ M.Hash.intersection a b of
    []   -> return $ Right (mergeFn a b)
    keys -> do
      let errors = map DuplicatedComponentKeyDetected keys
      return (Left (errors, M.Hash.union a b))

instance Applicative ComponentM where
  pure a = ComponentM $
    return $ Right (a, M.Hash.empty)

  (<*>) (ComponentM cf) (ComponentM ca) = ComponentM $ do
    -- NOTE: We do not handle IO errors here because they are being managed in
    -- the leafs; we don't expose the constructor of ComponentM
    let validateKeys =
          validateKeyDuplication M.Hash.union

    (rf, ra) <- concurrently cf ca
    case (rf, ra) of
      (Right (f, depsF), Right (a, depsA)) ->
        validateKeys depsF depsA >>= \case
          Right deps  -> return $ Right (f a, deps)
          Left (errors, deps) -> return $ Left (errors, deps)


      (Right (_, depsF), Left (errA, depsA)) ->
        validateKeys depsF depsA >>= \case
          Right deps -> return $ Left (errA, deps)
          Left (errors, deps) -> return $ Left (errA <> errors, deps)

      (Left (errF, depsF), Right (_, depsA)) ->
        validateKeys depsF depsA >>= \case
          Right deps -> return $ Left (errF, deps)
          Left (errors, deps) -> return $ Left (errF <> errors, deps)

      (Left (errF, depsF), Left (errA, depsA)) ->
        validateKeys depsF depsA >>= \case
          Right deps -> return $ Left (errF <> errA, deps)
          Left (errors, deps) -> return $ Left (errF <> errA <> errors, deps)

--------------------

appendDependency :: Description -> Build -> Build
appendDependency depDesc build =
  build { buildDependencies = S.insert depDesc (buildDependencies build) }

appendDependencies :: BuildTable -> BuildTable -> BuildTable
appendDependencies fromBuildTable toBuildTable =
  let appendDependenciesToBuild build =
        foldr appendDependency build (M.Hash.keys fromBuildTable)
  in
    -- First, we add all keys from fromBuildTable to all entries of toBuildTable
    -- Then, we join both fromBuildTable and toBuildTable
      M.Hash.map appendDependenciesToBuild toBuildTable
        & M.Hash.union fromBuildTable

instance Monad ComponentM where
  return = pure
  (>>=) (ComponentM ma) f = ComponentM $ do
    let validateKeys =
          validateKeyDuplication appendDependencies

    resultA <- ma
    case resultA of
      Left (errA, depsA) ->
        return $ Left (errA, depsA)

      Right (a, depsA) -> do
        let (ComponentM mb) = f a
        resultB <- mb

        case resultB of
          Left (errB, depsB) ->
            validateKeys depsA depsB >>= \case
              Right deps -> return $ Left (errB, deps)
              Left (errors, deps) -> return $ Left (errB <> errors, deps)

          Right (b, depsB) ->
            validateKeys depsA depsB >>= \case
              Right deps -> return $ Right (b, deps)
              Left (errors, deps) -> return $ Left (errors, deps)

instance MonadThrow ComponentM where
  throwM e =
    ComponentM
      $ return
      $ Left ([ComponentErrorThrown $ toException e], M.Hash.empty)

instance MonadIO ComponentM where
  liftIO action = ComponentM $ do
    eresult <- try action
    case eresult of
      Left err -> return $ Left ([ComponentIOLiftFailed err], M.Hash.empty)
      Right a  -> return $ Right (a, M.Hash.empty)


--------------------------------------------------------------------------------

buildTableToOrderedList :: BuildTable -> [Build]
buildTableToOrderedList buildTable =
  let buildGraphEdges :: [(Build, Description, [Description])]
      buildGraphEdges = M.Hash.foldrWithKey
        (\k build acc -> (build, k, S.toList $ buildDependencies build) : acc)
        []
        buildTable

      (componentGraph, lookupBuild) = graphFromEdges' buildGraphEdges
  in  map
        (\buildIndex -> let (build, _, _) = lookupBuild buildIndex in build)
        (topSort componentGraph)

buildTableToTeardown :: Text -> BuildTable -> IO Teardown
buildTableToTeardown appName buildTable = newTeardown
  appName
  (map componentTeardown $ buildTableToOrderedList buildTable)
