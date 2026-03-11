{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables,
             GeneralizedNewtypeDeriving, DeriveDataTypeable #-}

-- TODO: Can we pattern match on functions?
-- What about functions of several arguments? Can we have interleaved
-- pattern matching? Do we need to use currying to achieve this? What
-- limitations does that lead to?
-- TODO: getMatches: What happens with infinite input? Hmm... We do want the
-- possibility of non-termination, right?
-- TODO: getMatches: Frequencies?
-- TODO: match: Document limitations. Can functions be handled?

-- |
-- Module      :  Test.ChasingBottoms.ContinuousFunctions
-- Copyright   :  (c) Nils Anders Danielsson 2005-2022
-- License     :  See the file LICENCE.
--
-- Maintainer  :  http://www.cse.chalmers.se/~nad/
-- Stability   :  experimental
-- Portability :  non-portable (GHC-specific)
--
-- Note: /This module is unfinished and experimental. However, I do not think that I will ever finish it, so I have released it in its current state. The documentation below may not be completely correct. The source code lists some things which should be addressed./
--
-- A framework for generating possibly non-strict, partial,
-- continuous functions.
--
-- The functions generated using the standard QuickCheck 'Arbitrary'
-- instances are all strict. In the presence of partial and infinite
-- values testing using only strict functions leads to worse coverage
-- than if more general functions are used, though.
--
-- Using 'isBottom' it is relatively easy to generate possibly
-- non-strict functions that are, in general, not monotone. For
-- instance, using
--
-- > type Cogen a = forall b. a -> Gen b -> Gen b
-- >
-- > integer :: Gen Integer
-- > integer = frequency [ (1, return bottom), (10, arbitrary) ]
-- >
-- > coBool :: CoGen Bool
-- > coBool b | isBottom b = variant 0
-- > coBool False          = variant 1
-- > coBool True           = variant 2
-- >
-- > function :: Cogen a -> Gen b -> Gen (a -> b)
-- > function coGen gen = promote (\a -> coGen a gen)
--
-- we can generate possibly non-strict functions from 'Bool' to
-- 'Integer' using @function coBool integer@. There is a high
-- likelihood that the functions generated are not monotone, though.
-- The reason that we can get non-monotone functions in a language
-- like Haskell is that we are using the impure function 'isBottom'.
--
-- Sometimes using possibly non-monotone functions is good enough,
-- since that set of functions is a superset of the continuous
-- functions. However, say that we want to test that @x 'O.<=!' y@
-- implies that @f x 'O.<=!' f y@ for all functions @f@ (whenever the
-- latter expression returns a total result). This property is not
-- valid in the presence of non-monotone functions.
--
-- By avoiding 'isBottom' and, unlike the standard 'coarbitrary'
-- functions, deferring some pattern matches, we can generate
-- continuous, possibly non-strict functions. There are two steps
-- involved in generating a continuous function using the framework
-- defined here.
--
-- (1) First the argument to the function is turned into a
--     'PatternMatch'. A 'PatternMatch' wraps up the pattern match on
--     the top-level constructor of the argument, plus all further
--     pattern matches on the children of the argument. Just like when
--     'coarbitrary' is used a pattern match is represented as a
--     generator transformer. The difference here is that there is not
--     just one transformation per input, but one transformation per
--     constructor in the input. 'PatternMatch'es can be constructed
--     generically using 'match'.
--
-- (2) Then the result is generated, almost like for a normal
--     'Arbitrary' instance. However, for each constructor generated a
--     subset of the transformations from step 1 are applied. This
--     transformation application is wrapped up in the function
--     'transform'.
--
-- The net result of this is that some pattern matches are performed
-- later, or not at all, so functions can be lazy.
--
-- Here is an example illustrating typical use of this framework:
--
-- > data Tree a
-- >   = Branch (Tree a) (Tree a)
-- >   | Leaf a
-- >     deriving (Show, Typeable, Data)
-- >
-- > finiteTreeOf :: MakeResult a -> MakeResult (Tree a)
-- > finiteTreeOf makeResult = sized' tree
-- >   where
-- >   tree size = transform $
-- >     if size == 0 then
-- >       baseCase
-- >      else
-- >       frequency' [ (1, baseCase)
-- >                  , (1, liftM2 Branch tree' tree')
-- >                  ]
-- >     where
-- >     tree' = tree (size `div` 2)
-- >
-- >     baseCase =
-- >       frequency' [ (1, return bottom)
-- >                  , (2, liftM Leaf makeResult)
-- >                  ]
--
-- Note the use of 'transform'. To use this function to generate
-- functions of type @Bool -> Tree Integer@ we can use
--
-- > forAll (functionTo (finiteTreeOf flat)) $
-- >   \(f :: Bool -> Tree Integer) ->
-- >     ...

module Test.ChasingBottoms.ContinuousFunctions
  ( -- * Basic framework
    function
  , functionTo
  , PatternMatch(..)
  , GenTransformer
  , MakePM
  , MakeResult
  , transform
    -- * Liftings of some QuickCheck functionality
  , lift'
  , arbitrary'
  , choose'
  , elements'
  , oneof'
  , frequency'
  , sized'
  , resize'
    -- * Generic @MakePM@
  , match
    -- * Some @MakeResult@s
  , flat
  , finiteListOf
  , infiniteListOf
  , listOf
  ) where

import Test.QuickCheck
  hiding ( (><)
         , listOf
         , infiniteListOf
         , function
         )
import Test.QuickCheck.Arbitrary (CoArbitrary(..))
import Test.QuickCheck.Gen.Unsafe (promote)
import Data.Sequence as Seq
import Data.Foldable as Seq (foldr)
import Prelude as P hiding (concat)
import Test.ChasingBottoms.IsBottom
import Control.Monad
import Control.Monad.Reader
import Control.Applicative
import Control.Arrow
import System.Random
import Data.Generics
import qualified Data.List as L

import qualified Test.ChasingBottoms.SemanticOrd as O

------------------------------------------------------------------------
-- Generation of functions

-- | Generator for continuous, not necessarily strict functions.
-- Functions are generated by first generating pattern matches, and
-- then generating a result.

function :: MakePM a -> MakeResult b -> Gen (a -> b)
function makePM makeResult =
   promote $ \a -> run makeResult (singleton $ makePM a)

-- | 'functionTo' specialises 'function':
--
-- @
--  'functionTo' = 'function' 'match'
-- @

functionTo :: Data a => MakeResult b -> Gen (a -> b)
functionTo = function match

------------------------------------------------------------------------
-- Pattern matching

-- | 'PatternMatch' packages up the possible outcomes of a pattern
-- match in a style suitable for generating functions. A pattern match
-- is a generator ('Gen') transformer based on the top-level
-- constructor, and a sequence of 'PatternMatch'es based on the
-- children of that constructor.

data PatternMatch
  = PatternMatch { apply :: GenTransformer
                   -- ^ A generator transformer, in the style of 'coarbitrary'.
                 , more :: Seq PatternMatch
                   -- ^ Further pattern matches made possible by this
                   -- match.
                 }

-- | The type of a generator transformer.

type GenTransformer = forall a. Gen a -> Gen a

-- | This newtype is (currently) necessary if we want to use
-- 'GenTransformer' as an argument to a type constructor.

newtype GenTransformer' = GenT GenTransformer

-- | The type of a 'PatternMatch' generator.

type MakePM a = a -> PatternMatch

------------------------------------------------------------------------
-- Generic MakePM

-- These functions provided inspiration for the generic one below.

matchFlat :: CoArbitrary a => MakePM a
matchFlat a = PatternMatch { apply = coarbitrary a, more = Seq.empty }

data Tree a
   = Branch (Tree a) (Tree a)
   | Leaf a
     deriving (Show, Typeable, Data)

matchTree :: MakePM a -> MakePM (Tree a)
matchTree match t = PatternMatch { apply = toVariant t, more = moreT t }
  where
  toVariant (Branch {}) = variant 1
  toVariant (Leaf {})   = variant 0

  moreT (Branch l r) = fromList [matchTree match l, matchTree match r]
  moreT (Leaf x)     = singleton (match x)

-- | Generic implementation of 'PatternMatch' construction.

match :: forall a. Data a => MakePM a
match x = PatternMatch
            { apply = toVariant x
            , more  = more x
            }
  where
  toVariant :: forall a b. Data a => a -> Gen b -> Gen b
  toVariant x = case constrRep (toConstr x) of
    AlgConstr n   -> variant (n - 1)  -- n >= 1.
    IntConstr i   -> coarbitrary i
    FloatConstr d -> coarbitrary d
    CharConstr s  -> nonBottomError "match: Encountered CharConstr."

  more :: forall a. Data a => a -> Seq PatternMatch
  more = gmapQr (<|) Seq.empty match

------------------------------------------------------------------------
-- MakeResult monad

-- | Monad for generating results given previously generated pattern
-- matches.
--
-- A @'MakeResult' a@ should be implemented almost as other generators for
-- the type @a@, with the difference that 'transform' should be
-- used wherever the resulting function should be allowed to pattern
-- match (typically for each constructor emitted). See example above.

-- Note that we do not want to export a 'MonadReader' instance, so we
-- cannot define one...

newtype MakeResult a
  = MR { unMR :: ReaderT PatternMatches Gen a }
    deriving (Functor, Applicative, Monad)

type PatternMatches = Seq PatternMatch

-- | Lowering of a 'MakeResult'.

run :: MakeResult a -> PatternMatches -> Gen a
run mr pms = runReaderT (unMR mr) pms

-- | Lifting of a 'Gen'.

lift' :: Gen a -> MakeResult a
lift' gen = MR $ lift gen

-- | Returns the 'PatternMatches' in scope.

getPMs :: MakeResult PatternMatches
getPMs = MR ask

withPMs :: (PatternMatches -> Gen a) -> MakeResult a
withPMs f = do
  pms <- getPMs
  lift' $ f pms

-- | 'transform' makes sure that the pattern matches get to influence
-- the generated value. See 'MakeResult'.

transform :: MakeResult a -> MakeResult a
transform makeResult = withPMs $ \pms -> do
  (GenT trans, keep) <- getMatches pms
  trans (run makeResult keep)

-- | Extracts some pattern matches to trigger right away. These
-- triggered pattern matches may result in new pattern matches which
-- may in turn also be triggered, and so on.

getMatches :: Seq PatternMatch -> Gen (GenTransformer', Seq PatternMatch)
getMatches pms = do
  -- Throw away pattern matches with probability 0.1.
  (_, pms') <- partition' 9 pms
  -- Use pattern matches with probability 0.33.
  (use, keep) <- partition' 2 pms'
  let transform = compose $ fmap (\pm -> apply pm) use
      further = concat $ fmap more use
  if Seq.null further then
    return (GenT transform, keep)
   else do
    (GenT transform', keep') <- getMatches further
    return (GenT (transform . transform'), keep >< keep')

------------------------------------------------------------------------
-- Sequence helpers

-- | Concatenates arguments.

concat :: Seq (Seq a) -> Seq a
concat = Seq.foldr (><) Seq.empty

-- | Composes arguments.

compose :: Seq (a -> a) -> a -> a
compose = Seq.foldr (.) id

-- | Partitions a 'Seq'. The first argument (a positive integer) is
-- the relative probability with which elements end up in the second
-- part compared to the first one.

partition' :: Int -> Seq a -> Gen (Seq a, Seq a)
partition' freq ss = case viewl ss of
  EmptyL  -> return (Seq.empty, Seq.empty)
  x :< xs -> do
    (ys, zs) <- partition' freq xs
    frequency [ (1,    return (x <| ys, zs))
              , (freq, return (ys, x <| zs))
              ]

------------------------------------------------------------------------
-- Lifting of QuickCheck's Gen monad

-- | Lifting of 'arbitrary'.

arbitrary' :: Arbitrary a => MakeResult a
arbitrary' = lift' arbitrary

-- | Lifting of 'choose'.

choose' :: Random a => (a, a) -> MakeResult a
choose' = lift' . choose

-- | Lifting of 'elements'.

elements' :: [a] -> MakeResult a
elements' = lift' . elements

-- | Lifting of 'oneof'.

oneof' :: [MakeResult a] -> MakeResult a
oneof' mrs = withPMs $ \pms -> oneof $ map (\mr -> run mr pms) mrs

-- | Lifting of 'frequency'.

frequency' :: [(Int, MakeResult a)] -> MakeResult a
frequency' freqs = withPMs $ \pms ->
  frequency $ map (id *** flip run pms) freqs

-- | Lifting of 'sized'.

sized' :: (Int -> MakeResult a) -> MakeResult a
sized' mr = withPMs $ \pms -> sized (\size -> run (mr size) pms)

-- | Lifting of 'resize'.

resize' :: Int -> MakeResult a -> MakeResult a
resize' n mr = withPMs $ \pms -> resize n (run mr pms)

------------------------------------------------------------------------
-- Some predefined generators

-- | An implementation of @'MakeResult' a@ which is suitable when @a@
-- is flat and has an 'Arbitrary' instance. Yields bottoms around 10%
-- of the time.

flat :: Arbitrary a => MakeResult a
flat = transform $
  frequency' [ (1, return bottom)
             , (9, arbitrary')
             ]

-- | This 'MakeResult' yields finite partial lists.

finiteListOf :: MakeResult a -> MakeResult [a]
finiteListOf makeResult = sized' list
    where
    list size = transform $
      if size == 0 then
        baseCase
       else
        frequency' [ (1, baseCase)
                   , (9, liftM2 (:) makeResult (list (size - 1)))
                   ]

    baseCase =
      frequency' [(1, return bottom), (1, return [])]

-- | This 'MakeResult' yields infinite partial lists.

infiniteListOf :: MakeResult a -> MakeResult [a]
infiniteListOf makeResult = transform $
  liftM2 (:) makeResult (infiniteListOf makeResult)

-- | This 'MakeResult' yields finite or infinite partial lists.

listOf :: MakeResult a -> MakeResult [a]
                    -- Not really necessary to have a transform here...
listOf makeResult = transform $
   oneof' [ finiteListOf makeResult
          , infiniteListOf makeResult
          ]

------------------------------------------------------------------------
-- Failed attempt at a generic implementation of MakeResult

-- Main problem: Getting the frequencies right. Lists are very short
-- right now.

-- Other problem: Int and Float.

-- Further remark: We need finite and infinite versions of this
-- function.

makeResult :: forall a. Data a => MakeResult a
makeResult = transform (frequency' $ (1, return bottom) : others)
  where
  others = case dataTypeRep (dataTypeOf (undefined :: a)) of
             AlgRep constrs ->
               map (handle (L.genericLength constrs)) constrs
             IntRep         -> [(9, cast' (arbitrary' :: MakeResult Integer))]
             FloatRep       -> [(9, cast' (arbitrary' :: MakeResult Double))]
             CharRep        -> nonBottomError "makeResult: CharRep."
             NoRep          -> nonBottomError "makeResult: NoRep."

  handle noConstrs con =
    (freq, fromConstrM makeResult con :: MakeResult a)
    where noArgs = glength (fromConstr con :: a)
          -- Aim for at most 10% bottoms (on average).
          freq = 1 `max` ceiling (9 / noConstrs)

  cast' gen = flip fmap gen $ \x -> case cast x of
    Just x' -> x'
    Nothing -> nonBottomError $
                 "makeResult: Cannot handle Int and Float." ++
                 " Use Integer or Double instead."
