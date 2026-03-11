{-# language CPP #-}

-- |Reexports from @base@.
module Incipit.Base (
  module Control.Applicative,
  module Control.Arrow,
  module Control.Category,
  module Control.Concurrent.MVar,
  module Control.Exception,
  module Control.Monad,
  module Control.Monad.Fail,
  module Control.Monad.IO.Class,
  module Data.Bifunctor,
  module Data.Bits,
  module Data.Bool,
  module Data.Char,
  module Data.Coerce,
  module Data.Either,
  module Data.Eq,
  module Data.Function,
  module Data.Functor,
  module Data.Functor.Compose,
  module Data.Functor.Contravariant,
  module Data.Functor.Identity,
  module Data.Int,
  module Data.Kind,
  module Data.List,
  module Data.List.NonEmpty,
  module Data.Maybe,
  module Data.Monoid,
  module Data.Ord,
  module Data.Proxy,
  module Data.Semigroup,
  module Data.Traversable,
  module Data.Tuple,
  module Data.Typeable,
#if __GLASGOW_HASKELL__ >= 904
  module Data.Type.Equality,
#endif
  module Data.Void,
  module Data.Word,
  module GHC.Base,
  module GHC.Enum,
  module GHC.Err,
  module GHC.Float,
  module GHC.Generics,
  module GHC.Num,
  module GHC.OverloadedLabels,
  module GHC.Real,
  module GHC.Show,
  module GHC.Stack,
  module GHC.TypeLits,
  module Incipit.Fixed,
  module Incipit.Foldable,
  module Incipit.Fractional,
  module Incipit.Integral,
  module Numeric.Natural,
  module System.IO,
  module Text.Show,
) where

import Control.Applicative (
  Alternative (..),
  Applicative (..),
  Const (..),
  ZipList (..),
  liftA2,
  liftA3,
  optional,
  (<**>),
  )
import Control.Arrow ((&&&))
import Control.Category ((<<<), (>>>))
import Control.Concurrent.MVar
import Control.Exception (Exception, SomeException (..))
import Control.Monad (
  Monad ((>>), (>>=)),
  MonadPlus (..),
  filterM,
  forever,
  guard,
  join,
  mfilter,
  replicateM,
  replicateM_,
  unless,
  when,
  zipWithM,
  zipWithM_,
  (<$!>),
  (<=<),
  (=<<),
  (>=>),
  )
import Control.Monad.Fail (MonadFail (..))
import Control.Monad.IO.Class
import Data.Bifunctor (Bifunctor (..))
import Data.Bits (toIntegralSized, xor)
import Data.Bool (Bool (..), bool, not, otherwise, (&&), (||))
import Data.Char (Char, chr)
import Data.Coerce (Coercible, coerce)
import Data.Either
import Data.Eq (Eq (..))
import Data.Function (const, fix, flip, id, on, ($), (&), (.))
import Data.Functor (Functor (..), void, ($>), (<$>), (<&>))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Contravariant (Contravariant (..), (>$<))
import Data.Functor.Identity (Identity (..))
import Data.Int (Int, Int16, Int32, Int64, Int8)
import Data.Kind (Constraint, Type)
import Data.List (
  break,
  drop,
  dropWhile,
  filter,
  genericDrop,
  genericLength,
  genericReplicate,
  genericSplitAt,
  genericTake,
  group,
  inits,
  intercalate,
  intersperse,
  isPrefixOf,
  iterate,
  map,
  permutations,
  repeat,
  replicate,
  reverse,
  scanl,
  scanl',
  scanl1,
  scanr,
  scanr1,
  sort,
  sortBy,
  sortOn,
  span,
  splitAt,
  subsequences,
  tails,
  take,
  takeWhile,
  transpose,
  uncons,
  unfoldr,
  unzip,
  unzip3,
  zip,
  zip3,
  zipWith,
  (++),
  )
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Maybe hiding (fromJust)
import Data.Monoid (Monoid (..))
import Data.Ord (Down (..), Ord (..), Ordering (..), comparing)
import Data.Proxy (Proxy (..))
import Data.Semigroup (Semigroup (..))
import Data.Traversable (Traversable (..), for, forM, mapAccumL, mapAccumR)
import Data.Tuple (curry, fst, snd, swap, uncurry)
#if __GLASGOW_HASKELL__ >= 904
import Data.Type.Equality (type (~))
#endif
import Data.Typeable (Typeable)
import Data.Void (Void)
import Data.Word (Word, Word16, Word32, Word64, Word8)
import GHC.Base (maxInt, minInt, ord, seq, ($!))
import GHC.Enum (Bounded (..), Enum (..))
import GHC.Err (error, undefined)
import GHC.Float (Double (..), Float (..))
import GHC.Generics (Generic)
import GHC.Num (Integer, Num (..), subtract)
import GHC.OverloadedLabels (IsLabel (..))
import GHC.Real (
  Fractional (fromRational),
  Integral (toInteger),
  Ratio,
  Rational,
  Real (..),
  RealFrac (..),
  denominator,
  even,
  fromIntegral,
  gcd,
  lcm,
  numerator,
  odd,
  realToFrac,
  (^),
  (^^),
  )
import GHC.Show (Show)
import GHC.Stack (HasCallStack, withFrozenCallStack)
import GHC.TypeLits hiding (ErrorMessage (Text))
import Numeric.Natural (Natural)
import System.IO (FilePath, IO, print, putStr, putStrLn)
import Text.Show (showParen, showString, showsPrec)

import Incipit.Fixed
import Incipit.Foldable
import Incipit.Fractional
import Incipit.Integral hiding (safeOp)
