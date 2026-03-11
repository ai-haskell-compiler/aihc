{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Cauldron
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import Data.Foldable qualified
import Data.Function ((&))
import Data.IORef
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Monoid
import Data.Proxy
import Data.Set qualified
import Data.Typeable (typeRep)
import Test.Tasty
import Test.Tasty.HUnit

type Text = String

type M = WriterT [Text] IO

-- | And initialization action which some beans might register.
newtype Initializer = Initializer {runInitializer :: M ()}
  deriving (Semigroup, Monoid) via (Ap M ())

newtype Logger m = Logger
  { logMessage :: Text -> m ()
  }

makeLogger :: M (Initializer, Logger M)
makeLogger = do
  tell ["logger constructor"]
  pure
    ( Initializer do tell ["logger init"],
      Logger \message -> tell [message]
    )

data Repository m = Repository
  { findById :: Int -> m (Maybe Text),
    store :: Int -> Text -> m ()
  }

makeRepository :: Logger M -> M (Initializer, Repository M)
makeRepository Logger {logMessage} = do
  mapRef <- liftIO do newIORef @(Map Int Text) mempty
  pure
    ( Initializer do logMessage "repo init invoking logger",
      Repository
        { findById = \key -> do
            logMessage "findById"
            m <- liftIO do readIORef mapRef
            pure do Map.lookup key m,
          store = \key v -> do
            logMessage "store"
            liftIO do modifyIORef mapRef do Map.insert key v
        }
    )

data Weird m = Weird
  { weirdOp :: m (),
    anotherWeirdOp :: m ()
  }

makeWeird :: Logger M -> M (Weird M)
makeWeird _ = do
  tell ["weird constructor"]
  pure
    Weird
      { weirdOp = tell ["weirdOp"],
        anotherWeirdOp = tell ["another weirdOp"]
      }

data Lonely m = Lonely {soLonely :: m ()}

makeLonely :: Lonely M
makeLonely = do
  Lonely {soLonely = tell ["so lonely"]}

-- | Note that the patter-match on the self-dependency must be lazy, or else a
-- nasty, difficult to diagnose infinite loop will happen!
makeSelfInvokingWeird :: Weird M -> M (Weird M)
makeSelfInvokingWeird ~Weird {weirdOp = selfWeirdOp} = do
  tell ["self-invoking weird constructor"]
  pure
    Weird
      { weirdOp = tell ["weirdOp 2"],
        anotherWeirdOp = do
          tell ["another weirdOp 2"]
          selfWeirdOp
      }

weirdDeco :: Text -> Weird M -> Weird M
weirdDeco txt Weird {weirdOp, anotherWeirdOp} =
  Weird
    { weirdOp = do
        tell ["deco for weirdOp " <> txt]
        weirdOp,
      anotherWeirdOp = do
        tell ["deco for anotherWeirdOp " <> txt]
        anotherWeirdOp
    }

cauldron :: Cauldron M
cauldron =
    [ recipe @(Logger M) $ eff $ pure makeLogger,
      recipe @(Repository M) $ eff $ wire makeRepository,
      recipe @(Initializer, Repository M) $ val_ $ wire (,)
    ]

cauldronMissingDep :: Cauldron M
cauldronMissingDep =
  cauldron
    & delete (typeRep (Proxy @(Logger M)))

cauldronDoubleDutyBean :: Cauldron M
cauldronDoubleDutyBean =
  cauldron
    & insert @Initializer (val $ pure (Initializer (pure ())))

cauldronWithCycle :: Cauldron M
cauldronWithCycle =
  cauldron
    & insert @(Logger M)
      (eff $ wire \(_ :: Repository M) -> makeLogger)

cauldronX1 :: Cauldron M
cauldronX1 =
    [ recipe @(Logger M) $ eff $ pure makeLogger,
      recipe @(Weird M) $ eff $ wire makeWeird -- overwritten
    ]

cauldronX2 :: Cauldron M
cauldronX2 =
    [ recipe @(Repository M) $ eff $ do
        action <- wire makeRepository
        pure do
          (initializer, repo) <- action
          pure (initializer, repo),
      recipe @(Weird M)
        Recipe
          { bare = eff $ wire makeSelfInvokingWeird,
            decos =
              fromDecoList
                [ val $ wire (weirdDeco "inner"),
                  val $ wire (weirdDeco "outer")
                ]
          },
      recipe @Result $ val_ do wire Result
    ]

data Result = Result Initializer (Repository M) (Weird M)

cauldronLonely :: Cauldron M
cauldronLonely =
    [ recipe @(Lonely M) $ val $ pure makeLonely
    ]

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "lookup" do
        case cauldron & Cauldron.lookup @(Logger M) (\_ _ -> ()) of
          Just () -> pure ()
          Nothing -> assertFailure "bean not found",
      testCase "value" do
        (_, traces) <- case cook' [cauldron] of
          Left _ -> assertFailure "could not wire"
          Right beansAction -> runWriterT do
            boiledBeans <- beansAction
            let (Initializer {runInitializer}, Repository {findById, store}) = boiledBeans
            runInitializer
            store 1 "foo"
            _ <- findById 1
            pure ()
        assertEqual
          "traces"
          [ "logger constructor",
            "logger init",
            "repo init invoking logger",
            "store",
            "findById"
          ]
          traces,
      testCase "value sequential" do
        ((), traces) <- case cook @Result allowSelfDeps (mconcat [cauldronX1, cauldronX2]) of
          Left _ -> assertFailure "could not wire"
          Right beansAction -> do
            runWriterT do
              boiledBeans <- beansAction
              let Result (Initializer {runInitializer}) (Repository {findById, store}) (Weird {anotherWeirdOp}) = boiledBeans
              runInitializer
              store 1 "foo"
              _ <- findById 1
              anotherWeirdOp
              pure ()
        assertEqual
          "traces"
          ( sort
              [ -- "weird constructor", -- not happens, because overwritten
                -- the order of the traces here is a bit too overspecified. several orders could be valid.
                "logger constructor",
                "self-invoking weird constructor",
                "logger init",
                "repo init invoking logger",
                "store",
                "findById",
                -- the deco is applied! The outer the deco, the earliest is invoked.
                "deco for anotherWeirdOp outer",
                "deco for anotherWeirdOp inner",
                "another weirdOp 2",
                "deco for weirdOp outer",
                "deco for weirdOp inner",
                -- note that the self-invocation used the method from 'makeSelfInvokingWeird'
                "weirdOp 2"
              ]
          )
          (sort traces),
      -- case getDependencyGraph cauldronNonEmpty of
      --  dg2  -> do
      --    let adj2 = toAdjacencyMap dg2
      --    unless (hasVertex (PrimaryBean (typeRep (Proxy @(Logger M)))) adj2) do
      --      assertFailure "cauldron 2 doesn't have the fully built logger from cauldron 1 in its dep graph"
      --    when (hasVertex (BarePrimaryBean (typeRep (Proxy @(Logger M)))) adj2) do
      --      assertFailure "cauldron 2 has the bare undecorated logger from cauldron 1 in its dep graph, despite not depending on it directly"
      --  pure ()

      testCase "value nested" do
        ((), traces) <- case ( do
                                 constructorX2 <- nest allowSelfDeps [cauldronX2]
                                 cook @Result
                                   allowSelfDeps
                                   (mconcat [ cauldronX1,
                                     Cauldron.recipe @Result constructorX2
                                   ])
                             ) of
          Left _ -> assertFailure "could not wire"
          Right beansAction -> do
            runWriterT do
              boiledBeans <- beansAction
              let Result (Initializer {runInitializer}) (Repository {findById, store}) (Weird {anotherWeirdOp}) = boiledBeans
              runInitializer
              store 1 "foo"
              _ <- findById 1
              anotherWeirdOp
              pure ()
        assertEqual
          "traces"
          ( sort
              [ -- the order of the traces here is a bit too overspecified. several orders could be valid.
                "logger constructor",
                "self-invoking weird constructor",
                "weird constructor", -- note that this is present. Overwritten by nested, but still built
                -- The absence of the logger init is because we are only getting the aggregate beans from the nested.
                -- "logger init",
                "repo init invoking logger",
                "store",
                "findById",
                -- the deco is applied! The outer the deco, the earliest is invoked.
                "deco for anotherWeirdOp outer",
                "deco for anotherWeirdOp inner",
                "another weirdOp 2",
                "deco for weirdOp outer",
                "deco for weirdOp inner",
                -- note that the self-invocation used the method from 'makeSelfInvokingWeird'
                "weirdOp 2"
              ]
          )
          (sort traces),
      -- case getDependencyGraph cauldronNonEmpty of
      --  dg2  -> do
      --    let adj2 = toAdjacencyMap dg2
      --    unless (hasVertex (PrimaryBean (typeRep (Proxy @(Logger M)))) adj2) do
      --      assertFailure "cauldron 2 doesn't have the fully built logger from cauldron 1 in its dep graph"
      --    when (hasVertex (BarePrimaryBean (typeRep (Proxy @(Logger M)))) adj2) do
      --      assertFailure "cauldron 2 has the bare undecorated logger from cauldron 1 in its dep graph, despite not depending on it directly"
      --  pure ()

      testCase "lonely beans get built" do
        (_, _) <- case cook allowSelfDeps cauldronLonely of
          Left _ -> assertFailure "could not wire"
          Right beansAction -> runWriterT do
            boiledBeans <- beansAction
            let Lonely {soLonely} = boiledBeans
            soLonely
            pure ()
        pure (),
      testCase "cauldron missing dep" do
        case cook' [cauldronMissingDep] of
          Left (MissingDependenciesError missingDeps)
            | [MissingDependencies _ tr missingSet] <- Data.Foldable.toList missingDeps,
              tr == typeRep (Proxy @(Repository M)) && missingSet == Data.Set.singleton (typeRep (Proxy @(Logger M))) ->
                pure ()
          _ -> assertFailure "missing dependency not detected"
        pure (),
      testCase "cauldron with double duty bean" do
        case cook' [cauldronDoubleDutyBean] of
          Left (DoubleDutyBeansError _) -> pure ()
          _ -> assertFailure "double duty beans not detected"
        pure (),
      testCase "cauldron with cycle" do
        case cook' [cauldronWithCycle] of
          Left (DependencyCycleError (DependencyCycle vs)) ->
            -- Why not a cycle of length 3? Because there also are bare versions for each bean.
            assertEqual "cycle of the expected length" 4 (Data.Foldable.length vs)
          _ -> assertFailure "dependency cycle not detected"
        pure ()
    ]
  where
    cook' = cook allowSelfDeps

main :: IO ()
main = defaultMain tests
