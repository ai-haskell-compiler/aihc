module Essentials
  (
    {- * Function            -} ($), (&),
    {- * Category            -} id, (.), (>>>), (<<<),
    {- * Functor             -} fmap, (<$>), (<&>), (<$), ($>), void,
    {- * Applicative         -} pure, (<*>), (<**>), (<*), (*>),
    {- * Monad               -} (>>=), (=<<), (>=>), (<=<),
    {- * Boole               -} Bool (False, True), otherwise,
    {- * Comparison          -} (==), (/=), (<), (>), (<=), (>=),
    {- * Monoid              -} (<>), mempty,
    {- * Traversal           -} traverse, traverse_,
    {- * Maybe               -} Maybe (Nothing, Just), maybe,
    {- * Void                -} Void, absurd,
    {- * Identity            -} Identity (Identity, runIdentity),
    {- * Const               -} Const (Const, getConst),
    {- * Type classes        -} Semigroup, Monoid, Eq, Ord, Enum, Bounded, Show,
    {- * Constructor classes -} Functor, Applicative, Monad, Foldable, Traversable,
    {- * Type                -} Type,
    {- * Undefined           -} undefined,
  )
  where

import Control.Applicative   (Applicative, pure, (*>), (<*), (<*>), (<**>))
import Control.Category      (id, (.), (>>>), (<<<))
import Control.Monad         (Monad, (<=<), (=<<), (>=>), (>>=))
import Data.Bool             (Bool (False, True), otherwise)
import Data.Eq               (Eq, (/=), (==))
import Data.Foldable         (Foldable, traverse_)
import Data.Function         (($), (&))
import Data.Functor          (Functor, fmap, void, ($>), (<$), (<$>), (<&>))
import Data.Functor.Const    (Const (Const, getConst))
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.Kind             (Type)
import Data.Maybe            (Maybe (Just, Nothing), maybe)
import Data.Monoid           (Monoid, mempty)
import Data.Ord              (Ord, (<), (>), (<=), (>=))
import Data.Semigroup        (Semigroup, (<>))
import Data.Traversable      (Traversable, traverse)
import Data.Void             (Void, absurd)
import Prelude               (Bounded, Enum, undefined)
import Text.Show             (Show)
