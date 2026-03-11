-- | This module exports base types and functions. You can use these to define
-- functions to work on arbitrary sequence types.
--
-- If you want to work with @Text@ or @String@, import and use "Regex.Text" or
-- "Regex.List" instead.
--
module Regex.Base
  (
    -- * @RE@ and @Parser@
    R.RE
  , P.Parser

    -- * Compile
  , P.compile
  , P.compileBounded

    -- * Parse
    -- $parse
  , P.ParserState
  , P.prepareParser
  , P.stepParser
  , P.finishParser
  , P.Foldr
  , P.parseFoldr
  , P.parseNext

    -- * @RE@s and combinators
  , R.token
  , R.anySingle
  , R.single
  , R.satisfy
  , R.foldlMany
  , R.foldlManyMin
  , R.Many(..)
  , R.manyr
  , R.optionalMin
  , R.someMin
  , R.manyMin
  , R.atLeast
  , R.atMost
  , R.betweenCount
  , R.atLeastMin
  , R.atMostMin
  , R.betweenCountMin
  , R.sepBy
  , R.sepBy1
  , R.endBy
  , R.endBy1
  , R.sepEndBy
  , R.sepEndBy1
  , R.chainl1
  , R.chainr1
  , R.toFind
  , R.toFindMany

    -- * Strict combinators
    -- $strict

  , R.fmap'
  , R.liftA2'
  , R.foldlMany'
  , R.foldlManyMin'

    -- * Additional information

    -- ** Recursive definitions
    -- $recursive-definitions

    -- ** Laziness
    -- $laziness

    -- ** Looping parsers
    -- $looping-parsers

    -- ** Performance
    -- $performance
  ) where

import qualified Regex.Internal.Regex as R
import qualified Regex.Internal.Parser as P

-- $parse
--
-- The functions @prepareParser@, @stepParser@, and @finishParser@ grant
-- a large amount of control over the parsing process, making it possible to
-- parse in a resumable or even branching manner.
--
-- @parseFoldr@ and @parseNext@ may be more convenient to use, depending on the
-- sequence to parse.
--

-- $strict
--
-- These combinators force the result before continuing parsing. But beware!
-- If that particular parse ends up failing, the work done will have been for
-- nothing. This can blow up the complexity of parsing. For instance,
-- @fmap' sum (many digit)@ is \(O(n^2)\).
--
-- These functions are intended to be used when the work done in forcing the
-- result is guaranteed to be cheaper than creating a thunk, saving memory and
-- time.
-- For instance, @liftA2' (:)@ is a good usage, since @(:)@ does a small amount
-- of work and a thunk is avoided. As another example, @liftA2' ((+) \@Int)@ is
-- /not/ a good usage, because @(+)@ is strict and forces its arguments,
-- performing an arbitrary amount of work. However, it is okay to use
-- @liftA2' ((+) \@Int)@ if it is known for certain that its arguments will be
-- in WHNF.
--
-- __WARNING__: If you are not sure whether to use these function,
-- /don't use these functions/. Simply use @fmap@, @liftA2@, @foldlMany@ or
-- @foldlManyMin@ instead.
--

-- $recursive-definitions
--
-- It is not possible to define a @RE@ recursively. If it were permitted, it
-- would be capable of parsing more than
-- [regular languages](https://en.wikipedia.org/wiki/Regular_language).
-- Unfortunately, there is no good way\* to make it impossible to write such
-- a regex in the first place. So it must be avoided by the programmer. As an
-- example, avoid this:
--
-- @
-- re :: RE Int [Int]
-- re = liftA2 (:) (single 1) re \<|> [] \<$ single 0  -- diverges!
-- @
--
-- Instead, use appropriate combinators from this module:
--
-- @
-- re = many (single 1) <* single 0
-- @
--
-- For the same reason, be cautious when using combinators from the other
-- packages on @RE@s. Make sure that they do not attempt to construct a
-- recursive @RE@.
--
-- If you find that your regex is impossible to write without recursion,
-- you are attempting to parse a non-regular language! You need a more powerful
-- parser than what this library has to offer.
--
-- \*[Unlifted datatypes](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/primitives.html#unlifted-datatypes)
-- can be used for this but they are too inconvenient to work with.
--

-- $laziness
--
-- Parsing is lazy in the result value, i.e. the @a@ in @RE c a@ or
-- @Parser c a@. In fact, for the algorithm used in this library, this laziness
-- is essential for good runtime complexity. However, there is little reason
-- to be lazy in other aspects, such as the elements of the sequence, @c@, or
-- the functions and regexes used in combinators. Functions are strict in such
-- arguments.
--
-- @
-- -- Lazy in the result
-- parseFoldr foldr (compile (pure ⊥)) [] = Just ⊥
-- parseFoldr foldr (compile (fmap (\\_ -> ⊥) (single 1))) [1] = Just ⊥
--
-- -- Strict in places like
-- single ⊥ = ⊥
-- fmap ⊥ r = ⊥
-- liftA2 f r ⊥ = ⊥
-- @
--

-- $looping-parsers
--
-- What should be the result of parsing an empty sequence with
-- @(many (pure ()))@?
--
-- Since @many r@ parses @r@ as many times as possible, and @pure ()@ succeeds
-- without consuming input, the result should arguably be the infinite list
-- @repeat ()@. Similarly, parsing an empty sequence with
-- @(foldlMany f z (pure ()))@ should diverge. Note that this applies to not
-- just @pure x@, but any regex that can succeed without consuming input, such
-- as @many x@, @manyMin x@, etc.
--
-- This library considers that such an outcome is not desirable in practice. It
-- would be surprising to get an infinite structure from a parser. So, in the
-- case that @many@ succeeds an infinite number of times, this library treats it
-- as succeeding /zero/ times.
--
-- By this rule, @(many (pure ()))@ on an empty sequence parses as @[]@ and
-- @(foldlMany f z (pure ()))@ parses as @z@.
--
-- This behavior makes it impossible to distinguish between zero parses and
-- infinite parses. To address this, an alternate combinator 'Regex.List.manyr'
-- is provided. This parses into a 'Regex.List.Many', a type that clearly
-- indicates if parsing succeeded without consuming input into an infinite list,
-- or if it succeeded a finite number of times.
--

-- $performance
--
-- This section describes some performance characteristics of this library,
-- without requiring a dive into the source code.
--
-- Parsing with a @RE@ is done in two distinct steps.
--
-- 1. A @RE@ is compiled to a @Parser@, which is a
-- [nondeterministic finite automaton](https://en.wikipedia.org/wiki/Nondeterministic_finite_automaton)
-- (NFA), in \(O(m)\) time. \(m\) here is the size of the @RE@, which is the
-- number of nodes in its internal tree representation. The resulting @Parser@
-- has \(O(m)\) size.
-- 2. The @Parser@ is run on a sequence in \(O(mn \log m)\) time, where \(n\) is
-- the length of the sequence. This assumes that each @(c -> Maybe a)@ function
-- used to parse individual elements takes \(O(1)\) time.
--
-- /Performance tip/: Use @(\<$)@ over @(\<$>)@, and @(\<*)@\/@(*>)@ over
-- @liftA2@\/@(\<*>)@ when ignoring the result of a @RE@. Knowing the result is
-- ignored allows compiling to a faster parser.
--
-- Memory usage for parsing is \(O(nm)\), but
--
-- * If the result of a @RE@ is ignored using @(\<$)@, @(\<*)@, or @(*>)@, only
--   \(O(m)\) memory is required.
--
-- This applies even as subcomponents. So, any subcomponent @RE@ of a larger
-- @RE@ that is only recognizing a section of the list is cheaper in terms of
-- memory.
--
