{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Monad (join)
import Control.Monad.Trans.State.Strict (State, state, evalState)
import Data.Functor.Const (Const(Const))
import Data.Functor.Identity (Identity(Identity), runIdentity)
import Data.Kind
import GHC.Exts (Constraint)
import GHC.Generics (Generic)
import qualified GHC.Generics as G
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Runners as Tasty
import Test.Tasty.QuickCheck ((===))
import qualified Test.Tasty.QuickCheck as QC
import qualified Text.Read

import Flay

--------------------------------------------------------------------------------

-- | Just making sure TypeApplications works as expected.
_test_flay_TypeApplications
  :: Flayable Trivial s t f g
  => Flay Trivial s t (f :: k -> Type) (g :: k -> Type)
_test_flay_TypeApplications = flay @_ @Trivial

-- | Just making sure TypeApplications works as expected.
--
-- Note: This triggers a warning about '-Wsimplifiable-class-constraints'.
-- I'm not sure how to prevent it.
_test_flay1_TypeApplications
  :: Flayable1 Trivial r
  => Flay Trivial (r f) (r g) (f :: k -> Type) (g :: k -> Type)
_test_flay1_TypeApplications = flay1 @Trivial

--------------------------------------------------------------------------------

data Foo f = Foo (f Int) (f Bool)
  deriving (Generic)

flayFoo :: (c Int, c Bool) => Flay c (Foo f) (Foo g) f g
flayFoo h (Foo a b) = Foo <$> h Dict a <*> h Dict b

-- This one is actually free:
--   instance FieldsF c Foo => Flayable c (Foo f) (Foo g) f g

deriving instance (Eq (f Int), Eq (f Bool)) => Eq (Foo f)
-- Testing 'Fields' here as well.
deriving instance Fields Show (Foo f) => Show (Foo f)

instance (QC.Arbitrary (f Int), QC.Arbitrary (f Bool)) => QC.Arbitrary (Foo f) where
  arbitrary = Foo <$> QC.arbitrary <*> QC.arbitrary

-- example values
foo1 :: Applicative m => Foo m
foo1 = Foo (pure 0) (pure False)

foo2 :: Applicative m => Foo m
foo2 = Foo (pure 1) (pure True)

--------------------------------------------------------------------------------

data Bar f = Bar (f Int) Int
  deriving (Generic)

flayBar :: c Int => Flay c (Bar f) (Bar g) f g
flayBar h (Bar a b) = Bar <$> h Dict a <*> pure b

-- | Checking 'FieldsF' here as well.
instance FieldsF c Bar => Flayable c (Bar f) (Bar g) f g where flay = flayBar

deriving instance Eq (f Int) => Eq (Bar f)
deriving instance Show (f Int) => Show (Bar f)

instance QC.Arbitrary (f Int) => QC.Arbitrary (Bar f) where
  arbitrary = Bar <$> QC.arbitrary <*> QC.arbitrary

--------------------------------------------------------------------------------

data Qux f
  = Qux1 (f Int) Int
  | Qux2 { q2a :: (f Int), q2b :: (f Int) }
  | Qux3 (Foo f)
  deriving (Generic)

flayQux :: (c Int, c Bool) => Flay c (Qux f) (Qux g) f g
flayQux h (Qux1 a b) = Qux1 <$> h Dict a <*> pure b
flayQux h (Qux2 a b) = Qux2 <$> h Dict a <*> h Dict b
flayQux h (Qux3 a) = Qux3 <$> flayFoo h a

instance (c Int , c Bool) => Flayable c (Qux f) (Qux g) f g where
  flay h (Qux1 fa b) = Qux1 <$> h Dict fa <*> pure b
  flay h (Qux2 fa fb) = Qux2 <$> h Dict fa <*> h Dict fb
  flay h (Qux3 foo) = Qux3 <$> flay h foo

deriving instance (Eq (f Int), Eq (Foo f)) => Eq (Qux f)
deriving instance (Show (f Int), Show (Foo f)) => Show (Qux f)

instance (QC.Arbitrary (f Int), QC.Arbitrary (f Bool)) => QC.Arbitrary (Qux f) where
  arbitrary = QC.oneof [ Qux1 <$> QC.arbitrary <*> QC.arbitrary
                       , Qux2 <$> QC.arbitrary <*> QC.arbitrary
                       , Qux3 <$> QC.arbitrary ]

--------------------------------------------------------------------------------

data Zoo
  = Zoo0
  | Zoo1 Int
  | Zoo2 Int Int
  deriving (Generic, Eq, Show)

instance QC.Arbitrary Zoo where
  arbitrary = QC.oneof [ pure Zoo0
                       , pure Zoo1 <*> QC.arbitrary
                       , pure Zoo2 <*> QC.arbitrary <*> QC.arbitrary ]

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt

tt :: Tasty.TestTree
tt = Tasty.testGroup "main"
  [ QC.testProperty "Flayable: Foo: identity law" $
      QC.forAll QC.arbitrary $ \(foo :: Foo Maybe) ->
         Identity foo === flay @_ @Trivial (const pure) foo
  , QC.testProperty "Flayable: Bar: identity law" $
      QC.forAll QC.arbitrary $ \(bar :: Bar Maybe) ->
         Identity bar === flay @_ @Trivial (const pure) bar
  , QC.testProperty "Flayable: Qux: identity law" $
      QC.forAll QC.arbitrary $ \(qux :: Qux Maybe) ->
         Identity qux === flay @_ @Trivial (const pure) qux
  , QC.testProperty "collectShow: Foo: flayFoo" $
      QC.forAll QC.arbitrary $ \foo@(Foo (Identity a) (Identity b)) ->
         [show a, show b] === collectShow' flayFoo foo
  , QC.testProperty "collectShow: Foo: flay" $
      QC.forAll QC.arbitrary $ \foo@(Foo (Identity a) (Identity b)) ->
         [show a, show b] === collectShow' flay foo
  , QC.testProperty "collectShow: Foo: flay1" $
      QC.forAll QC.arbitrary $ \foo@(Foo (Identity a) (Identity b)) ->
         [show a, show b] === collectShow' flay1 foo
  , QC.testProperty "collectShow: Bar: flayBar" $
      QC.forAll QC.arbitrary $ \bar@(Bar (Identity a) _) ->
         [show a] === collectShow' flayBar bar
  , QC.testProperty "collectShow: Bar: flay" $
      QC.forAll QC.arbitrary $ \bar@(Bar (Identity a) _) ->
         [show a] === collectShow' flay bar
  , QC.testProperty "collectShow: Bar: flay1" $
      QC.forAll QC.arbitrary $ \bar@(Bar (Identity a) _) ->
         [show a] === collectShow' flay1 bar
  , QC.testProperty "pump/flay/dump: Zoo" $
      QC.forAll QC.arbitrary $ \(zoo0 :: Zoo, i0 :: Int) ->
         let pzoo0 :: Pump Zoo Identity
             pzoo0 = pump Identity zoo0
             h :: Dict (Read a) -> Identity a -> State Int (Maybe a)
             h Dict _ = state (\i -> (Text.Read.readMaybe (show i), i+1))
             spzoo1 :: State Int (Pump Zoo Maybe)
             spzoo1 = flay1 h pzoo0
             pzoo1 :: Pump Zoo Maybe
             pzoo1 = evalState spzoo1 i0
             yzoo1 :: Maybe Zoo
             yzoo1 = dump id pzoo1
         in yzoo1 === Just (case zoo0 of Zoo0 -> Zoo0
                                         Zoo1{} -> Zoo1 i0
                                         Zoo2{} -> Zoo2 i0 (i0+1))
  ]


collectShow'
  :: Flay Show s t Identity (Const ()) -> s -> [String]
collectShow' fl = collect' fl (\Dict (Identity a) -> [show a])
