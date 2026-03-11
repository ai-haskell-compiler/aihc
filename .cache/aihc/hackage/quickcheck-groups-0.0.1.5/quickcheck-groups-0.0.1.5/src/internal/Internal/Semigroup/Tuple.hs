{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{- HLINT ignore "Redundant bracket" -}

-- |
-- Copyright: © 2022–2025 Jonathan Knowles
-- License: Apache-2.0
--
module Internal.Semigroup.Tuple
    where

import Data.Functor
    ( (<&>) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import GHC.Generics
    ( Generic )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , applyArbitrary2
    , applyArbitrary3
    , applyArbitrary4
    , choose
    , genericShrink
    , oneof
    , shuffle
    , suchThatMap
    )
import Text.Show.Pretty
    ( ppShow )

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup.Foldable as F1

--------------------------------------------------------------------------------
-- Variables
--------------------------------------------------------------------------------

data Variable = A | B | C | D
    deriving (Bounded, Enum, Eq, Ord, Show)

bindVariable :: BindingSet s -> Variable -> s
bindVariable BindingSet {bindingForA} A = bindingForA
bindVariable BindingSet {bindingForB} B = bindingForB
bindVariable BindingSet {bindingForC} C = bindingForC
bindVariable BindingSet {bindingForD} D = bindingForD

--------------------------------------------------------------------------------
-- Variable sums
--------------------------------------------------------------------------------

newtype VariableSum = VariableSum (NonEmpty Variable)
    deriving (Eq, Ord, Semigroup)

instance Arbitrary VariableSum where
    arbitrary = genVariableSum

instance Show VariableSum where
    show (VariableSum vs) = F1.intercalate1 " <> " $ show <$> vs

a = VariableSum (A :| [])
b = VariableSum (B :| [])
c = VariableSum (C :| [])
d = VariableSum (D :| [])

genVariableSum :: Gen VariableSum
genVariableSum =
    VariableSum <$> genVariableList `suchThatMap` NE.nonEmpty
  where
    genVariableList :: Gen [Variable]
    genVariableList = do
        itemCount <- choose (1, 4)
        take itemCount <$> shuffle universe

bindVariableSum :: BindingSet s -> VariableSum -> NonEmpty s
bindVariableSum tuple (VariableSum selectors) =
    bindVariable tuple <$> selectors

evalVariableSum :: Semigroup s => BindingSet s -> VariableSum -> s
evalVariableSum = (F1.fold1 .) . bindVariableSum

showVariableSum :: Show s => (BindingSet s) -> VariableSum -> String
showVariableSum tuple =
    F1.intercalateMap1 " <> " show . bindVariableSum tuple

--------------------------------------------------------------------------------
-- Binding sets (for variables)
--------------------------------------------------------------------------------

data BindingSet s = BindingSet
    { bindingForA :: s
    , bindingForB :: s
    , bindingForC :: s
    , bindingForD :: s
    }
    deriving (Eq, Generic, Ord)

instance Show s => Show (BindingSet s) where
    show (BindingSet va vb vc vd) = mconcat
        [ "BindingSet {"
        , "a = " <> show va <> ", "
        , "b = " <> show vb <> ", "
        , "c = " <> show vc <> ", "
        , "d = " <> show vd
        , "}"
        ]

instance Arbitrary s => Arbitrary (BindingSet s) where
    arbitrary = genBindingSet
    shrink = shrinkBindingSet

genBindingSet :: Arbitrary s => Gen (BindingSet s)
genBindingSet = applyArbitrary4 BindingSet

shrinkBindingSet :: Arbitrary s => BindingSet s -> [BindingSet s]
shrinkBindingSet = genericShrink

--------------------------------------------------------------------------------
-- Tuples
--------------------------------------------------------------------------------

data Tuple1 s = Tuple1 VariableSum (BindingSet s)
    deriving (Eq, Ord)

data Tuple2 s = Tuple2 VariableSum VariableSum (BindingSet s)
    deriving (Eq, Ord)

data Tuple3 s = Tuple3 VariableSum VariableSum VariableSum (BindingSet s)
    deriving (Eq, Ord)

instance Arbitrary a => Arbitrary (Tuple1 a) where
    arbitrary = genTuple1
    shrink = shrinkTuple1

instance Arbitrary a => Arbitrary (Tuple2 a) where
    arbitrary = genTuple2
    shrink = shrinkTuple2

instance Arbitrary a => Arbitrary (Tuple3 a) where
    arbitrary = genTuple3
    shrink = shrinkTuple3

instance (Show s, Semigroup s) => Show (Tuple1 s) where
    show = showTuple1

instance (Show s, Semigroup s) => Show (Tuple2 s) where
    show = showTuple2

instance (Show s, Semigroup s) => Show (Tuple3 s) where
    show = showTuple3

genTuple1 :: Arbitrary a => Gen (Tuple1 a)
genTuple1 = applyArbitrary2 Tuple1

genTuple2 :: forall a. Arbitrary a => Gen (Tuple2 a)
genTuple2 = oneof [genRandom, genHandChosen]
  where
    genRandom :: Gen (Tuple2 a)
    genRandom = applyArbitrary3 Tuple2

    genHandChosen :: Gen (Tuple2 a)
    genHandChosen = oneof $ fmap (arbitrary <&>)
        [ -- All identical:
          Tuple2 a a
        , -- All different:
          Tuple2 a b
          -- Shared common prefix:
        , Tuple2 (a <> b) (a <> c)
          -- Shared common suffix:
        , Tuple2 (a <> c) (b <> c)
          -- Shared common overlap (left to right):
        , Tuple2 (a <> b) (b <> c)
          -- Shared common overlap (right to left):
        , Tuple2 (c <> b) (b <> a)
          -- Append to the RHS (from left to right):
        , Tuple2 (a) (a <> b)
          -- Append to the RHS (from right to left):
        , Tuple2 (a <> b) (a)
          -- Append to the LHS (from left to right):
        , Tuple2 (b) (a <> b)
          -- Append to the LHS (from right to left):
        , Tuple2 (a <> b) (b)
        ]

genTuple3 :: forall a. Arbitrary a => Gen (Tuple3 a)
genTuple3 = oneof [genRandom, genHandChosen]
  where
    genRandom :: Gen (Tuple3 a)
    genRandom = applyArbitrary4 Tuple3

    genHandChosen :: Gen (Tuple3 a)
    genHandChosen = oneof $ fmap (arbitrary <&>)
        [ -- All identical:
          Tuple3 a a a
          -- All different:
        , Tuple3 a b c
          -- Shared common prefix:
        , Tuple3 (a <> b) (a <> c) (a <> d)
          -- Shared common suffix:
        , Tuple3 (a <> d) (b <> d) (c <> d)
          -- Append to the RHS (from left to right):
        , Tuple3 (a) (a <> b) (a <> b <> c)
          -- Append to the RHS (from right to left):
        , Tuple3 (a <> b <> c) (a <> b) (a)
          -- Append to the LHS (from left to right):
        , Tuple3 (c) (b <> c) (a <> b <> c)
          -- Append to the LHS (from right to left):
        , Tuple3 (a <> b <> c) (b <> c) (c)
        ]

evalTuple1 :: Semigroup s => Tuple1 s -> s
evalTuple1 (Tuple1 c1 t) =
    ( evalVariableSum t c1
    )

evalTuple2 :: Semigroup s => Tuple2 s -> (s, s)
evalTuple2 (Tuple2 c1 c2 t) =
    ( evalVariableSum t c1
    , evalVariableSum t c2
    )

evalTuple3 :: Semigroup s => Tuple3 s -> (s, s, s)
evalTuple3 (Tuple3 c1 c2 c3 t) =
    ( evalVariableSum t c1
    , evalVariableSum t c2
    , evalVariableSum t c3
    )

showTuple1 :: (Semigroup a, Show a) => Tuple1 a -> String
showTuple1 (evalTuple1 -> va) = unlines
    [ mempty, "a:", showWrap va
    ]

showTuple2 :: (Semigroup a, Show a) => Tuple2 a -> String
showTuple2 (evalTuple2 -> (va, vb)) = unlines
    [ mempty, "a:", showWrap va
    , mempty, "b:", showWrap vb
    ]

showTuple3 :: (Semigroup a, Show a) => Tuple3 a -> String
showTuple3 (evalTuple3 -> (va, vb, vc)) = unlines
    [ mempty, "a:", showWrap va
    , mempty, "b:", showWrap vb
    , mempty, "c:", showWrap vc
    ]

shrinkTuple1 :: Arbitrary a => Tuple1 a -> [Tuple1 a]
shrinkTuple1 (Tuple1 c1 t) = Tuple1 c1 <$> shrink t

shrinkTuple2 :: Arbitrary a => Tuple2 a -> [Tuple2 a]
shrinkTuple2 (Tuple2 c1 c2 t) = Tuple2 c1 c2 <$> shrink t

shrinkTuple3 :: Arbitrary a => Tuple3 a -> [Tuple3 a]
shrinkTuple3 (Tuple3 c1 c2 c3 t) = Tuple3 c1 c2 c3 <$> shrink t

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

showWrap :: Show a => a -> String
showWrap x
    | singleLineMaxLengthExceeded =
        multiLine
    | otherwise =
        singleLine
  where
    multiLine = ppShow x
    singleLine = show x
    singleLineMaxLength = 80
    singleLineMaxLengthExceeded = F.length singleLine > singleLineMaxLength

universe :: (Bounded a, Enum a) => [a]
universe = [minBound .. maxBound]
