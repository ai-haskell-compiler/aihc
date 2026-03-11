{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
  This module provides a way to generically obtain every possible value of
  a type, provided the generic representation of the type is compatible.

  Probably the main reason this is useful is, unlike the builtin Haskell
  @deriving Enum@ capability, this package understands non-nullary data
  constructors. So you can, for instance, enumerate something like

  > data Foo = Foo Bool Bool deriving (Generic)

  This module does not provide a way to manually provide an enumeration
  by instantiating a type class. Enumerations __must__ be obtained
  generically.  Therefore, it is not enough that your type be an instance
  of 'Generic'. Any types which it references must also be instances of
  'Generic'.


  In GHCI:

  > λ: :set +m
  > λ: :set prompt "λ: "
  > λ: :set prompt-cont "λ.. "
  > λ: :set -XDeriveGeneric
  > λ:
  > λ: :{
  > λ.. data Foo
  > λ..   = A Bar
  > λ..   | B
  > λ..   | C Bool
  > λ..   deriving (Show, Generic)
  > λ..
  > λ.. data Bar
  > λ..   = X
  > λ..   | Y
  > λ..   | Z
  > λ..   deriving (Show, Generic)
  > λ.. :}
  > λ:
  > λ: enumeration :: [Foo]
  > [A X,A Y,A Z,B,C False,C True]
  > λ:
-}
module Data.Enumeration.Generic (
  enumeration,
  predMay,
  succMay,

  HasPredecessor,
  HasSuccessor,
  HasFirst,
) where


import GHC.Generics ((:*:)((:*:)), (:+:)(L1, R1), Generic(Rep, from,
  to), K1(K1), M1(M1), U1(U1), R)


{- | Generically produce the predecessor. -}
class GenericPred a where
  gPred :: a p -> Maybe (a p)
instance (GenericPred typ) => GenericPred (M1 a b typ) where
  gPred (M1 a) = M1 <$> gPred a
instance (GenericLast l, GenericPred l, GenericPred r) => GenericPred (l :+: r) where
  gPred = \case
    L1 a -> L1 <$> gPred a
    R1 a ->
      case gPred a of
        Nothing -> Just (L1 gLastOf)
        Just b -> Just (R1 b)
instance GenericPred U1 where
  gPred U1 = Nothing
instance (Generic typ, GenericPred (Rep typ)) => GenericPred (K1 R typ) where
  gPred (K1 a) = K1 . to <$> gPred (from a)


{- | Generically produce the successor. -}
class GenericSucc a where
  gSucc :: a p -> Maybe (a p)
instance (GenericSucc typ) => GenericSucc (M1 a b typ) where
  gSucc (M1 a) = M1 <$> gSucc a
instance (GenericSucc l, GenericSucc r, GenericFirst r) => GenericSucc (l :+: r) where
  gSucc = \case
    L1 a ->
      case gSucc a of
        Nothing -> Just (R1 gFirstOf)
        Just b -> Just (L1 b)
    R1 a -> R1 <$> gSucc a
instance GenericSucc U1 where
  gSucc U1 = Nothing
instance (Generic typ, GenericSucc (Rep typ)) => GenericSucc (K1 R typ) where
  gSucc (K1 a) = K1 . to <$> gSucc (from a)
instance (GenericSucc r, GenericFirst r, GenericSucc l) => GenericSucc (l :*: r) where
  gSucc (l :*: r) =
    case gSucc r of
      Nothing -> (:*: gFirstOf) <$> gSucc l
      Just r2 -> Just (l :*: r2)


{- | Generically produce the last value. -}
class GenericLast a where
  gLastOf :: a p
instance GenericLast U1 where
  gLastOf = U1
instance (GenericLast r) => GenericLast (l :+: r) where
  gLastOf = R1 gLastOf
instance (GenericLast typ) => GenericLast (M1 a b typ) where
  gLastOf = M1 gLastOf
instance (Generic typ, GenericLast (Rep typ)) => GenericLast (K1 R typ) where
  gLastOf = K1 (to gLastOf)


{- | Generically produce the first value. -}
class GenericFirst a where
  gFirstOf :: a p
instance (GenericFirst typ) => GenericFirst (M1 a b typ) where
  gFirstOf = M1 gFirstOf
instance (GenericFirst l) => GenericFirst (l :+: r) where
  gFirstOf = L1 gFirstOf
instance (GenericFirst l, GenericFirst r) => GenericFirst (l :*: r) where
  gFirstOf = gFirstOf :*: gFirstOf
instance (Generic typ, GenericFirst (Rep typ)) => GenericFirst (K1 R typ) where
  gFirstOf = K1 (to gFirstOf)
instance GenericFirst U1 where
  gFirstOf = U1


{- | Produce a list of every possible value. -}
enumeration :: (Generic a, HasFirst (Rep a), HasSuccessor (Rep a)) => [a]
enumeration =
    to <$> go (Just gFirstOf)
  where
    go = \case
      Nothing -> []
      Just a -> a:go (gSucc a)


{- | Return the preceding value, if there is one. -}
predMay :: (Generic a, HasPredecessor (Rep a)) => a -> Maybe a
predMay a = to <$> gPred (from a)


{- | Return the succeeding value, if there is one. -}
succMay :: (Generic a, HasSuccessor (Rep a)) => a -> Maybe a
succMay a = to <$> gSucc (from a)


{-
  The purpose of these constraint aliases is so that we can avoid
  documenting the Generic* type classes, while still exporting enough
  symbols so that the user can copy the type signatures of the functions
  exported by this module, and use them in their own code.
-}
type HasPredecessor = GenericPred
type HasSuccessor = GenericSucc
type HasFirst = GenericFirst
