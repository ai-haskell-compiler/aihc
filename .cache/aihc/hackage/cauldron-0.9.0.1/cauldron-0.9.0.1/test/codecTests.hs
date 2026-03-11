{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NoFieldSelectors #-}

module Main (main) where

import Cauldron
import Cauldron.Builder
import Control.Exception (throwIO)
import Data.Foldable qualified
import Data.Function ((&))
import Data.Functor.Identity
import Data.Monoid
import Test.Tasty
import Test.Tasty.HUnit

data Foo
  = EndFoo
  | FooToBar Bar
  deriving stock (Show)

data Bar
  = EndBar
  | BarToFoo Foo
  | BarToBaz Baz
  deriving stock (Show)

data Baz
  = EndBaz
  | BazToFoo Foo
  deriving stock (Show)

newtype Serializer a = Serializer {runSerializer :: a -> String}

makeFooSerializer :: Serializer Bar -> Serializer Foo
makeFooSerializer Serializer {runSerializer = runBar} =
  Serializer
    { runSerializer = \case
        EndFoo -> ".EndFoo"
        FooToBar bar -> ".FooToBar" ++ runBar bar
    }

makeBarSerializer :: Serializer Foo -> Serializer Baz -> Serializer Bar
makeBarSerializer Serializer {runSerializer = runFoo} Serializer {runSerializer = runBaz} =
  Serializer
    { runSerializer = \case
        EndBar -> ".EndBar"
        BarToFoo foo -> ".BarToFoo" ++ runFoo foo
        BarToBaz baz -> ".BarToBar" ++ runBaz baz
    }

makeBazSerializer :: Serializer Foo -> Serializer Baz
makeBazSerializer Serializer {runSerializer = runFoo} =
  Serializer
    { runSerializer = \case
        EndBaz -> ".EndBaz"
        BazToFoo foo -> ".BazToFoo" ++ runFoo foo
    }

cauldron :: Cauldron Identity
cauldron =
  mconcat
    [ recipe @(Serializer Foo) $ val $ wire makeFooSerializer,
      recipe @(Serializer Bar) $ val $ wire makeBarSerializer,
      recipe @(Serializer Baz) $ val $ wire makeBazSerializer
    ]

builder :: Builder Identity ()
builder = mdo
  foo <- _val_ $ makeFooSerializer <$> bar
  bar <- _val_ $ makeBarSerializer <$> foo <*> baz
  baz <- _val_ $ makeBazSerializer <$> foo
  pure ()

builder2 :: Builder Identity ()
builder2 = mdo
  _ <- _val_ $ wire makeFooSerializer
  _ <- _val_ $ wire makeBarSerializer
  _ <- _val_ $ wire makeBazSerializer
  pure ()

builder3 :: Builder Identity ()
builder3 = mdo
  foo <- add $ val_ $ wire makeFooSerializer
  _ <- add $ val_ $ makeBarSerializer <$> foo <*> baz
  baz <- add $ val_ $ wire makeBazSerializer
  pure ()

builderDupErr :: Builder Identity ()
builderDupErr = mdo
  foo1 <- _val_ $ makeFooSerializer <$> bar
  foo2 <- _val_ $ makeFooSerializer <$> bar
  bar <- _val_ $ makeBarSerializer <$> foo1 <*> baz
  baz <- _val_ $ makeBazSerializer <$> foo2
  _ <- _val_ $ makeBazSerializer <$> foo2
  pure ()

newtype Acc = Acc Int
  deriving stock (Show)
  deriving stock (Eq)
  deriving (Semigroup, Monoid) via Sum Int

newtype Bcc = Bcc Int
  deriving stock (Show)
  deriving stock (Eq)
  deriving (Semigroup, Monoid) via Sum Int

cauldronAccums1 :: Cauldron Identity
cauldronAccums1 =
  mconcat
    [ recipe @(Serializer Foo) $ val $ wire $ \sb -> (Acc 5, makeFooSerializer sb),
      recipe @(Serializer Bar) $ val $ wire $ \sf sb -> (Acc 3, makeBarSerializer sf sb),
      recipe @(Serializer Baz) $ val $ wire $ \sf -> (Acc 7, makeBazSerializer sf)
    ]

cauldronAccums2 :: Cauldron Identity
cauldronAccums2 =
  mconcat
    [ recipe @(Serializer Foo) $ val $ wire $ \(_ :: Acc) sb -> makeFooSerializer sb,
      recipe @(Serializer Bar) $ val $ wire $ \sf sb -> (Acc 3, makeBarSerializer sf sb),
      recipe @(Serializer Baz) $ val $ wire $ \sf -> (Acc 7, makeBazSerializer sf)
    ]

cauldronAccumsOops1 :: Cauldron Identity
cauldronAccumsOops1 =
  mconcat
    [ recipe @(Serializer Foo) $ val $ wire $ \(_ :: Acc) sb -> (Acc 5, makeFooSerializer sb),
      recipe @(Serializer Bar) $ val $ wire $ \sf sb -> (Acc 3, makeBarSerializer sf sb),
      recipe @(Serializer Baz) $ val $ wire $ \sf -> (Acc 7, makeBazSerializer sf)
    ]

cauldronAccumsOops2 :: Cauldron Identity
cauldronAccumsOops2 =
  mconcat
    [ recipe @(Serializer Foo) $ val $ wire $ \(_ :: Acc) sb -> (Bcc 5, makeFooSerializer sb),
      recipe @(Serializer Bar) $ val $ wire $ \(_ :: Bcc) sf sb -> (Acc 5, makeBarSerializer sf sb),
      recipe @(Serializer Baz) $ val $ wire $ \sf -> (Acc 7, makeBazSerializer sf)
    ]

tests :: TestTree
tests =
  testGroup
    "All"
    [ testCase "successful cyclic wiring" do makeBasicTest cauldron,
      testCase "successful cyclic wiring - builder" do
        c <- builder & execBuilder & either throwIO pure
        makeBasicTest c,
      testCase "successful cyclic wiring - builder 2" do
        c <- builder2 & execBuilder & either throwIO pure
        makeBasicTest c,
      testCase "successful cyclic wiring - builder 3" do
        c <- builder3 & execBuilder & either throwIO pure
        makeBasicTest c,
      testCase "should fail builder exec" do
        builderDupErr & execBuilder & \case
          Left _ -> do
            -- appendFile "/tmp/foo.txt" $ prettyDuplicateBeans err
            pure ()
          Right _ -> assertFailure "Builder should have failed with duplicate beans error",
      testCase "should fail cycle wiring" do
        Data.Foldable.for_ @[] [("forbid", forbidDepCycles), ("selfdeps", allowSelfDeps)] \(name, fire) ->
          case cook @(Serializer Foo) fire cauldron of
            Left (DependencyCycleError _) -> pure ()
            Left _ -> assertFailure $ "Unexpected error when wiring" ++ name
            Right _ -> assertFailure $ "Unexpected success when wiring" ++ name,
      testCase "cyclic wiring with accums" do
        Data.Foldable.for_ @[]
          [ ("normal", cauldronAccums1, Acc 15),
            ("someConsume", cauldronAccums2, Acc 10)
          ]
          \(name, c, expected) ->
            case cook @Acc allowDepCycles c of
              Left _err -> do
                -- putStrLn $ prettyRecipeError err
                assertFailure $ "could not wire " ++ name
              Right (Identity acc) ->
                assertEqual "experted result" expected acc,
      testCase "wiring with accums" do
        Data.Foldable.for_ @[]
          [ ("aggcyle", cauldronAccumsOops1),
            ("indirectagg", cauldronAccumsOops2)
          ]
          \(name, c) ->
            case cook @(Serializer Foo) allowDepCycles c of
              Left (DependencyCycleError _) -> assertFailure $ "We should be able to wire cycles with accs"
              Left _ -> assertFailure $ "Unexpected error when wiring" ++ name
              Right _ -> pure ()
    ]
  where
    makeBasicTest :: Cauldron Identity -> IO ()
    makeBasicTest theCauldron =
      case cook allowDepCycles theCauldron of
        Left _ -> do
          -- putStrLn $ prettyRecipeError err
          assertFailure "could not wire"
        Right (Identity (Serializer {runSerializer})) -> do
          let value = FooToBar (BarToFoo (FooToBar (BarToBaz EndBaz)))
          assertEqual "experted result" ".FooToBar.BarToFoo.FooToBar.BarToBar.EndBaz" (runSerializer value)

main :: IO ()
main = defaultMain tests
