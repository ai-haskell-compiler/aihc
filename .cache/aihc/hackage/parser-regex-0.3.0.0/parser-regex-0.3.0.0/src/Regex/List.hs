-- | This module offers regexes, combinators, and operations to work with the
-- list type (@[]@), and also specifically 'String's, which are lists of
-- 'Char's.
--
module Regex.List
  (
    -- * @RE@s
    R.RE
  , R.token
  , R.satisfy
  , R.single
  , R.anySingle
  , L.list
  , L.manyList
  , L.someList
  , L.manyListMin
  , L.someListMin

    -- * @Char@ @RE@s
  , L.charIgnoreCase
  , L.oneOfChar
  , L.stringIgnoreCase
  , L.manyStringOf
  , L.someStringOf
  , L.manyStringOfMin
  , L.someStringOfMin

    -- * Numeric @Char@ @RE@s
  , L.naturalDec
  , L.integerDec
  , L.naturalHex
  , L.integerHex
  , L.wordRangeDec
  , L.intRangeDec
  , L.wordRangeHex
  , L.intRangeHex
  , L.wordDecN
  , L.wordHexN

    -- * Combinators
  , R.foldlMany
  , R.foldlManyMin
  , L.toMatch
  , L.withMatch
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

    -- * Combinators in @base@
    -- $combase

    -- * Compile and parse
  , L.reParse
  , P.Parser
  , P.compile
  , P.compileBounded
  , L.parse
  , L.parseSure

    -- * List operations
  , L.find
  , L.findAll
  , L.splitOn
  , L.replace
  , L.replaceAll

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
import qualified Regex.Internal.List as L


-- $combase
--
-- Various combinators are available in @base@ that work with @RE@s, by virtue
-- of @RE@ being @Applicative@ and @Alternative@.
-- Since this package does not attempt to redefine or re-export such
-- combinators, you need to import and use them. Commonly used combinators
-- are:
--
-- * "Control.Applicative": @liftA2@, @\<|>@, @empty@, @many@, @some@,
--   @optional@
-- * "Control.Monad": @void@, @replicateM@, @replicateM_@
-- * "Data.Foldable": @traverse_@, @for_@, @sequenceA_@, @asum@
-- * "Data.Traversable": @traverse@, @for@, @sequenceA@
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
-- re :: RE Char [String]
-- re = liftA2 (:) (list "ha") re \<|> [] \<$ list "!"  -- diverges!
-- @
--
-- Instead, use appropriate combinators from this module:
--
-- @
-- re = many (list "ha") <* list "!"
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
-- reParse (pure ⊥) "" = Just ⊥
-- reParse (fmap (\\_ -> ⊥) (char \'a\')) "a" = Just ⊥
--
-- -- Strict in places like
-- single ⊥ = ⊥
-- fmap ⊥ r = ⊥
-- liftA2 f r ⊥ = ⊥
-- @
--

-- $looping-parsers
--
-- What should be the result of @reParse (many (pure ())) ""@?
--
-- Since @many r@ parses @r@ as many times as possible, and @pure ()@ succeeds
-- without consuming input, the result should arguably be the infinite list
-- @repeat ()@. Similarly, @reParse (foldlMany f z (pure ())) ""@ should
-- diverge. Note that this applies to not just @pure x@, but any regex that
-- can succeed without consuming input, such as @many x@, @manyMin x@, etc.
--
-- This library considers that such an outcome is not desirable in practice. It
-- would be surprising to get an infinite structure from a parser. So, in the
-- case that @many@ succeeds an infinite number of times, this library treats it
-- as succeeding /zero/ times.
--
-- By this rule, @reParse (many (pure ())) ""@ parses as @[]@ and
-- @reParse (foldlMany f z (pure ())) ""@ parses as @z@.
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
-- 2. The @Parser@ is run on a list in \(O(mn \log m)\) time, where \(n\) is
-- the length of the list. This assumes that each @(c -> Maybe a)@ function
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
