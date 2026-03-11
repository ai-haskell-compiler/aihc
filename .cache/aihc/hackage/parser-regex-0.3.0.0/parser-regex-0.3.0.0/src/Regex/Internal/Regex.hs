{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | This is an internal module. You probably don't need to import this.
--
module Regex.Internal.Regex
  ( RE(..)
  , Many(..)

  , token
  , anySingle
  , single
  , satisfy

  , foldlMany
  , foldlManyMin
  , manyr
  , optionalMin
  , someMin
  , manyMin
  , atLeast
  , atMost
  , betweenCount
  , atLeastMin
  , atMostMin
  , betweenCountMin
  , sepBy
  , sepBy1
  , endBy
  , endBy1
  , sepEndBy
  , sepEndBy1
  , chainl1
  , chainr1
  , toFind
  , toFindMany

  , fmap'
  , liftA2'
  , foldlMany'
  , foldlManyMin'
  ) where

import Control.Applicative (Alternative(..))
import qualified Control.Applicative as Ap
import Control.DeepSeq (NFData(..), NFData1(..), rnf1)
import Control.Monad (void)
import Data.Functor.Classes (Eq1(..), Ord1(..), Show1(..), showsUnaryWith)
import Data.Semigroup (Semigroup(..))
import qualified Data.Foldable as F
import qualified Data.Traversable as T

import Regex.Internal.Solo (Solo, mkSolo)

---------------------------------
-- RE and constructor functions
---------------------------------

-- | A regular expression. Operates on a sequence of elements of type @c@ and
-- capable of parsing into an @a@.
--
-- A @RE@ is a Functor, Applicative, and Alternative.
--
-- * 'pure': Succeed without consuming input.
-- * 'liftA2', '<*>', '*>', '<*': Sequential composition.
-- * 'empty': Fail.
-- * '<|>': Alternative composition. Left-biased, i.e. the result of parsing
--   using @a \<|> b@ is the result of parsing using @a@ if it succeeds,
--   otherwise it is the result of parsing using @b@ if it succeeds,
--   otherwise parsing fails.
-- * 'many': Zero or more. @many a@ parses multiple @a@s sequentially. Biased
--   towards matching more. Use 'manyMin' for a bias towards matching less.
--   Also see the section "Looping parsers".
-- * 'some': One or more. @some a@ parses multiple @a@s sequentially. Biased
--   towards matching more. Use 'someMin' for a bias towards matching less.
--
-- In addition to expected Functor, Applicative, and Alternative laws,
-- @RE@ obeys these Applicative-Alternative laws:
--
-- @
-- a \<*> empty = empty
-- empty \<*> a = empty
-- (a \<|> b) \<*> c = (a \<*> c) \<|> (b \<*> c)
-- a \<*> (b \<|> c) = (a \<*> b) \<|> (a \<*> c)
-- @
--
-- Note that, because of bias, it is /not true/ that @a \<|> b = b \<|> a@.
--
-- /Performance tip/: Prefer the smaller of equivalent regexes, i.e. prefer
-- @(a \<|> b) \<*> c@ over @(a \<*> c) \<|> (b \<*> c)@.
--

-- See Note [Functions returning Solo]
data RE c a where
  RToken  :: !(c -> Maybe a) -> RE c a
  RFmap   :: !(a1 -> Solo a) -> !(RE c a1) -> RE c a
  RFmap_  :: a -> !(RE c a1) -> RE c a
  RPure   :: a -> RE c a
  RLiftA2 :: !(a1 -> a2 -> Solo a) -> !(RE c a1) -> !(RE c a2) -> RE c a
  REmpty  :: RE c a
  RAlt    :: !(RE c a) -> !(RE c a) -> RE c a
  RFoldGr :: !(a -> a1 -> Solo a) -> a -> !(RE c a1) -> RE c a
  RFoldMn :: !(a -> a1 -> Solo a) -> a -> !(RE c a1) -> RE c a
  RMany   :: !(a1 -> Solo a) -> !(a2 -> Solo a) -> !(a2 -> a1 -> Solo a2) -> !a2 -> !(RE c a1) -> RE c a -- Greedy

instance Functor (RE c) where
  fmap f = RFmap (\x -> mkSolo (f x))
  {-# INLINE fmap #-}

  (<$) = RFmap_

fmap' :: (a -> b) -> RE c a -> RE c b
fmap' f = RFmap (\x -> mkSolo $! f x)
{-# INLINE fmap' #-}

instance Applicative (RE c) where
  pure = RPure

  liftA2 f = RLiftA2 (\x y -> mkSolo (f x y))
  {-# INLINE liftA2 #-}

  re1 *> re2 = RLiftA2 (\_ y -> mkSolo y) (void re1) re2

  re1 <* re2 = RLiftA2 (\x _ -> mkSolo x) re1 (void re2)

liftA2' :: (a1 -> a2 -> b) -> RE c a1 -> RE c a2 -> RE c b
liftA2' f = RLiftA2 (\x y -> mkSolo $! f x y)
{-# INLINE liftA2' #-}

instance Alternative (RE c) where
  empty = REmpty
  (<|>) = RAlt
  some re = liftA2' (:) re (many re)
  many = fmap reverse . foldlMany' (flip (:)) []

-- | @(<>)@ = @liftA2 (<>)@
instance Semigroup a => Semigroup (RE c a) where
  (<>) = Ap.liftA2 (<>)
  sconcat = fmap sconcat . T.sequenceA
  {-# INLINE sconcat #-}

-- | @mempty@ = @pure mempty@
instance Monoid a => Monoid (RE c a) where
  mempty = pure mempty
  mconcat = fmap mconcat . T.sequenceA
  {-# INLINE mconcat #-}
-- Use the underlying type's sconcat/mconcat because it may be more efficient
-- than the default right-associative definition.
-- stimes is not defined here since there is no way to delegate to the stimes
-- of a.

-- | Parse a @c@ into an @a@ if the given function returns @Just@.
token :: (c -> Maybe a) -> RE c a
token = RToken

-- | Zero or more. Biased towards matching more.
--
-- Also see the section "Looping parsers".
manyr :: RE c a -> RE c (Many a)
manyr =
  RMany
    (\xs -> mkSolo (Repeat xs))
    (\xs -> mkSolo (Finite (reverse xs)))
    (\xs x -> mkSolo (x:xs))
    []

-- | Parse many occurences of the given @RE@. Biased towards matching more.
--
-- Also see the section "Looping parsers".
foldlMany :: (b -> a -> b) -> b -> RE c a -> RE c b
foldlMany f = RFoldGr (\z x -> mkSolo (f z x))
{-# INLINE foldlMany #-}

foldlMany' :: (b -> a -> b) -> b -> RE c a -> RE c b
foldlMany' f !z = RFoldGr (\z' x -> mkSolo $! f z' x) z
{-# INLINE foldlMany' #-}

-- | Parse many occurences of the given @RE@. Minimal, i.e. biased towards
-- matching less.
foldlManyMin :: (b -> a -> b) -> b -> RE c a -> RE c b
foldlManyMin f = RFoldMn (\z x -> mkSolo (f z x))
{-# INLINE foldlManyMin #-}

foldlManyMin' :: (b -> a -> b) -> b -> RE c a -> RE c b
foldlManyMin' f !z = RFoldMn (\z' x -> mkSolo $! f z' x) z
{-# INLINE foldlManyMin' #-}

-- | Parse a @c@ if it satisfies the given predicate.
satisfy :: (c -> Bool) -> RE c c
satisfy p = token (\c -> if p c then Just c else Nothing)
{-# INLINE satisfy #-}

-- | Parse the given @c@.
single :: Eq c => c -> RE c c
single !c = satisfy (c==)

-- | Parse any @c@.
anySingle :: RE c c
anySingle = token Just

---------
-- Many
---------

-- | A repeating value or a finite list.
data Many a
  = Repeat a   -- ^ A single value repeating indefinitely
  | Finite [a] -- ^ A finite list
  deriving (Eq, Show)

instance Ord a => Ord (Many a) where
  compare (Repeat x) (Repeat y) = compare x y
  compare xs ys = compare (F.toList xs) (F.toList ys)

instance Eq1 Many where
  liftEq f m1 m2 = case (m1,m2) of
    (Repeat x, Repeat y) -> f x y
    (Finite xs, Finite ys) -> liftEq f xs ys
    _ -> False

instance Ord1 Many where
  liftCompare f m1 m2 = case (m1,m2) of
    (Repeat x, Repeat y) -> f x y
    _ -> liftCompare f (F.toList m1) (F.toList m2)

instance Show1 Many where
  liftShowsPrec sp sl p m = case m of
    Repeat x -> showsUnaryWith sp "Repeat" p x
    Finite xs -> showParen (p > 10) $
      showString "Finite" . showChar ' ' . sl xs

instance Functor Many where
  fmap f m = case m of
    Repeat x -> Repeat (f x)
    Finite xs -> Finite (map f xs)

instance F.Foldable Many where
  foldr f z m = case m of
    Repeat x -> let r = f x r in r
    Finite xs -> foldr f z xs

  foldl' f z m = case m of
    Repeat _ -> error "Foldable Many: Repeat: foldl'"
    Finite xs -> F.foldl' f z xs

  foldl f z m = case m of
    Repeat _ -> error "Foldable Many: Repeat: foldl"
    Finite xs -> foldl f z xs

  toList m = case m of
    Repeat x -> repeat x
    Finite xs -> xs

instance NFData a => NFData (Many a) where
  rnf = rnf1

instance NFData1 Many where
  liftRnf f m = case m of
    Repeat x -> f x
    Finite xs -> liftRnf f xs

----------------
-- Combinators
----------------

-- | Zero or one. Minimal, i.e. biased towards zero.
--
-- @Use Control.Applicative.'optional'@ for the same but biased towards one.
optionalMin :: RE c a -> RE c (Maybe a)
optionalMin re = pure Nothing <|> Just <$> re

-- | One or more. Minimal, i.e. biased towards matching less.
someMin :: RE c a -> RE c [a]
someMin re = liftA2' (:) re (manyMin re)

-- | Zero or more. Minimal, i.e. biased towards matching less.
manyMin :: RE c a -> RE c [a]
manyMin = fmap reverse . foldlManyMin' (flip (:)) []

-- | At least n times. Biased towards matching more.
atLeast :: Int -> RE c a -> RE c [a]
atLeast n re = replicateAppendM (max n 0) re (many re)

-- | At most n times. Biased towards matching more.
atMost :: Int -> RE c a -> RE c [a]
atMost n = betweenCount (0,n)

-- | Between m and n times (inclusive). Biased towards matching more.
betweenCount :: (Int, Int) -> RE c a -> RE c [a]
betweenCount (l,h) re
  | l' > h = empty
  | otherwise = replicateAppendM l' re (go (h - l'))
  where
    l' = max l 0
    go 0 = pure []
    go n = liftA2' (:) re (go (n-1)) <|> pure []

-- | At least n times. Minimal, i.e. biased towards matching less.
atLeastMin :: Int -> RE c a -> RE c [a]
atLeastMin n re = replicateAppendM (max n 0) re (manyMin re)

-- | At most n times. Minimal, i.e. biased towards matching less.
atMostMin :: Int -> RE c a -> RE c [a]
atMostMin n = betweenCountMin (0,n)

-- | Between m and n times (inclusive). Minimal, i.e. biased towards matching
-- less.
betweenCountMin :: (Int, Int) -> RE c a -> RE c [a]
betweenCountMin (l,h) re
  | l' > h = empty
  | otherwise = replicateAppendM l' re (go (h - l'))
  where
    l' = max l 0
    go 0 = pure []
    go n = pure [] <|> liftA2' (:) re (go (n-1))

-- n0 must be >= 0
replicateAppendM :: Int -> RE c a -> RE c [a] -> RE c [a]
replicateAppendM n0 re re1 = go n0
  where
    go 0 = re1
    go n = liftA2' (:) re (go (n-1))

-- | @r \`sepBy\` sep@ parses zero or more occurences of @r@, separated by
-- @sep@. Biased towards matching more.
sepBy :: RE c a -> RE c sep -> RE c [a]
sepBy re sep = sepBy1 re sep <|> pure []

-- | @r \`sepBy1\` sep@ parses one or more occurences of @r@, separated by
-- @sep@. Biased towards matching more.
sepBy1 :: RE c a -> RE c sep -> RE c [a]
sepBy1 re sep = liftA2' (:) re (many (sep *> re))

-- | @r \`endBy\` sep@ parses zero or more occurences of @r@, separated and
-- ended by @sep@. Biased towards matching more.
endBy :: RE c a -> RE c sep -> RE c [a]
endBy re sep = many (re <* sep)

-- | @r \`endBy1\` sep@ parses one or more occurences of @r@, separated and
-- ended by @sep@. Biased towards matching more.
endBy1 :: RE c a -> RE c sep -> RE c [a]
endBy1 re sep = some (re <* sep)

-- | @r \`sepEndBy\` sep@ parses zero or more occurences of @r@, separated and
-- optionally ended by @sep@. Biased towards matching more.
sepEndBy :: RE c a -> RE c sep -> RE c [a]
sepEndBy re sep = sepEndBy1 re sep <|> pure []

-- | @r \`sepEndBy1\` sep@ parses one or more occurences of @r@, separated and
-- optionally ended by @sep@. Biased towards matching more.
sepEndBy1 :: RE c a -> RE c sep -> RE c [a]
sepEndBy1 re sep = sepBy1 re sep <* Ap.optional sep

-- | @chainl1 r op@ parses one or more occurences of @r@, separated by @op@.
-- The result is obtained by left associative application of all functions
-- returned by @op@ to the values returned by @p@. Biased towards matching more.
chainl1 :: RE c a -> RE c (a -> a -> a) -> RE c a
chainl1 re op = Ap.liftA2 (flip id) re rest
  where
    rest = foldlMany (flip (.)) id (Ap.liftA2 flip op re)

-- | @chainr1 r op@ parses one or more occurences of @r@, separated by @op@.
-- The result is obtained by right associative application of all functions
-- returned by @op@ to the values returned by @p@. Biased towards matching more.
chainr1 :: RE c a -> RE c (a -> a -> a) -> RE c a
chainr1 re op = Ap.liftA2 id rest re
  where
    rest = foldlMany (.) id (Ap.liftA2 (flip id) re op)

-- | Results in the first occurence of the given @RE@. Fails if no occurence
-- is found.
toFind :: RE c a -> RE c a
toFind re = manyMin anySingle *> re <* many anySingle

-- | Results in all non-overlapping occurences of the given @RE@. Always
-- succeeds.
toFindMany :: RE c a -> RE c [a]
toFindMany re =
  reverse <$>
  foldlMany' (flip ($)) [] ((:) <$> re <|> id <$ anySingle)

----------
-- Notes
----------

-- Note [Functions returning Solo]
-- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
-- We use `-> Solo a` functions in RFmap, RLiftA2, RFold, RMany to get better
-- control of the evaluation and avoid wastefully creating thunks.
--
-- Under normal circumstances, GHC does a good job of avoiding such thunks using
-- demand analysis. However, for a function stored in a RE, there is no way to
-- know its demand characteristics, making such optimizations impossible.
--
-- So, Solo is a way to get some /manual/ control over evaluation. For functions
-- where we want to avoid thunks we use strict combinators, e.g. `liftA2' (:)`,
-- so that forcing the Solo forces the result. Where we don't want to force the
-- result, we use lazy combinators which simply put the thunk in the Solo, e.g.
-- `fmap reverse`.
--
-- On GHC, Solo is implemented as an unboxed 1-tuple at no extra cost. It does
-- have a cost on non-GHC however.
--
-- An alternative is to store the strictness of the function alongside it, as a
-- Bool for instance, and force the result when applying the function if it is
-- strict. In fact, this is how it was originally implemented here. This method
-- adds memory costs to the RE and makes the code a little more complicated. The
-- current setup also might incur a memory cost if an unknown function has to
-- be wrapped in a function which returns Solo. But in practice the function is
-- usually statically known and wrapping function gets simplified.
