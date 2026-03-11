-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Regex.Base.RegexLike
-- Copyright   :  (c) Chris Kuklewicz 2006
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Maintainer  :  Andreas Abel
-- Stability   :  stable
-- Portability :  non-portable (MPTC+FD)
--
-- Classes and instances for Regex matching.
--
-- All the /classes/ are declared here, and some common type aliases, and
-- the 'MatchResult' data type.
--
-- The only /instances/ here are for 'Extract' 'String', 'Extract' 'SB.ByteString',
-- and 'Extract' 'ST.Text'. There are no data values.  The 'RegexContext'
-- instances are in "Text.Regex.Base.Context", except for ones which
-- run afoul of a repeated variable (@'RegexContext' regex a a@), which
-- are defined in each modules' String and ByteString modules.
-----------------------------------------------------------------------------

module Text.Regex.Base.RegexLike (
  -- ** Type aliases
  MatchOffset,
  MatchLength,
  MatchArray,
  MatchText,
  -- ** Data types
  MatchResult(..),
  -- ** Classes
  RegexOptions(..),
  RegexMaker(..),
  RegexLike(..),
  RegexContext(..),
  Extract(..),
  AllSubmatches(..),AllTextSubmatches(..),AllMatches(..),AllTextMatches(..)
  ) where

import Prelude
  (Int, Bool, String
  , (.), (+)
  , error, id, return, snd
  , drop, fmap, length, map, take
  , toEnum
  )


import Control.Monad.Fail (MonadFail)
import Data.Array(Array,(!))
import Data.Maybe(Maybe,isJust,maybe)
import qualified Data.ByteString as SB (take,drop,empty,ByteString)
import qualified Data.ByteString.Lazy as LB (take,drop,empty,ByteString)
import qualified Data.Sequence as S(take,drop,empty,Seq)
import qualified Data.Text as ST (take,drop,empty,Text)
import qualified Data.Text.Lazy as LT (take,drop,empty,Text)

-- | 0 based index from start of source, or (-1) for unused
type MatchOffset = Int

-- | non-negative length of a match
type MatchLength = Int

-- | 0 based array, with 0th index indicating the full match.  If the
-- full match location is not available, represent as (0,0).
type MatchArray = Array Int (MatchOffset,MatchLength)
type MatchText source = Array Int (source,(MatchOffset,MatchLength))

-- | This is the same as the type from JRegex.
data MatchResult a = MR {
    mrBefore :: a,
    mrMatch  :: a,
    mrAfter  :: a,
    mrSubList :: [a],
    mrSubs   :: Array Int a
}


-- | Rather than carry them around separately, the options for how to
-- execute a regex are kept as part of the regex.  There are two types
-- of options.  Those that can only be specified at compilation time
-- and never changed are @compOpt@.  Those that can be changed later and
-- affect how matching is performed are @execOpt@.  The actually types
-- for these depend on the backend.
--
class RegexOptions regex compOpt execOpt
  | regex   -> compOpt execOpt
  , compOpt -> regex execOpt
  , execOpt -> regex compOpt
  where

  -- | No options set at all in the backend.
  blankCompOpt   :: compOpt

  -- | No options set at all in the backend.
  blankExecOpt   :: execOpt

  -- | Reasonable options (extended, caseSensitive, multiline regex).
  defaultCompOpt :: compOpt

  -- | Reasonable options (extended, caseSensitive, multiline regex).
  defaultExecOpt :: execOpt

  -- | Forget old flags and use new ones.
  setExecOpts    :: execOpt -> regex -> regex

  -- | Retrieve the current flags.
  getExecOpts    :: regex -> execOpt


-- | @RegexMaker@ captures the creation of the compiled regular
-- expression from a source type and an option type.  Methods 'makeRegexM' and
-- 'makeRegexM' report parse errors using 'MonadError', usually (@Either
-- String regex@).
--
-- The 'makeRegex' function has a default implementation that depends
-- on 'makeRegexOpts' and uses 'defaultCompOpt' and 'defaultExecOpt'.
-- Similarly for 'makeRegexM' and 'makeRegexOptsM'.
--
-- There are also default implementations for 'makeRegexOpts' and
-- 'makeRegexOptsM' in terms of each other.  So a minimal instance
-- definition needs to only define one of these, hopefully
-- 'makeRegexOptsM'.
--
class (RegexOptions regex compOpt execOpt) => RegexMaker regex compOpt execOpt source
  | regex -> compOpt execOpt, compOpt -> regex execOpt, execOpt -> regex compOpt where

  -- | Use the 'defaultCompOpt' and 'defaultExecOpt'.
  makeRegex :: source -> regex

  -- | Specify your own options.
  makeRegexOpts :: compOpt -> execOpt -> source -> regex

  -- | Use the 'defaultCompOpt' and 'defaultExecOpt', reporting errors with 'fail'.
  makeRegexM :: (MonadFail m) => source -> m regex

  -- | Specify your own options, reporting errors with fail
  makeRegexOptsM :: (MonadFail m) => compOpt -> execOpt -> source -> m regex

  makeRegex = makeRegexOpts defaultCompOpt defaultExecOpt
  makeRegexM = makeRegexOptsM defaultCompOpt defaultExecOpt
  makeRegexOpts c e s = maybe (error "makeRegexOpts failed") id (makeRegexOptsM c e s)
  makeRegexOptsM c e s = return (makeRegexOpts c e s)


-- | RegexLike is parametrized on a regular expression type and a
-- source type to run the matching on.
--
-- There are default implementations: 'matchTest' and 'matchOnceText' use
-- 'matchOnce'; 'matchCount' and 'matchAllText' use 'matchAll'.
-- Conversely, 'matchOnce' uses
-- 'matchOnceText' and 'matchAll' uses 'matchAllText'. So a minimal complete
-- instance need to provide at least ('matchOnce' or 'matchOnceText') and
-- ('matchAll' or 'matchAllText').  Additional definitions are often
-- provided where they will increase efficiency.
--
-- > [ c | let notVowel = makeRegex "[^aeiou]" :: Regex, c <- ['a'..'z'], matchTest notVowel [c]  ]
-- >
-- > "bcdfghjklmnpqrstvwxyz"
--
-- The strictness of these functions is instance dependent.
--
class (Extract source) => RegexLike regex source where

  -- | This returns the first match in the source (it checks the whole
  -- source, not just at the start). This returns an array of
  -- (offset,length) index pairs for the match and captured
  -- substrings.  The offset is 0-based.  A (-1) for an offset means a
  -- failure to match.  The lower bound of the array is 0, and the 0th
  -- element is the (offset,length) for the whole match.
  matchOnce  :: regex -> source -> Maybe MatchArray

  -- | @matchAll@ returns a list of matches.  The matches are in order
  -- and do not overlap. If any match succeeds but has 0 length then
  -- this will be the last match in the list.
  matchAll   :: regex -> source -> [MatchArray]

  -- | @matchCount@ returns the number of non-overlapping matches
  -- returned by @matchAll@.
  matchCount :: regex -> source -> Int

  -- | @matchTest@ returns @True@ if there is a match somewhere in the
  -- source (it checks the whole source not just at the start).
  matchTest  :: regex -> source -> Bool

  -- | This is @matchAll@ with the actual subsections of the source
  -- instead of just the (offset,length) information.
  matchAllText  :: regex -> source -> [MatchText source]

  -- | This can return a tuple of three items: the source before the
  -- match, an array of the match and captured substrings (with their
  -- indices), and the source after the match.
  matchOnceText :: regex -> source -> Maybe (source, MatchText source, source)

  matchAll regex source = map (fmap snd) (matchAllText regex source)
  matchOnce regex source = fmap (\(_,mt,_) -> fmap snd mt) (matchOnceText regex source)
  matchTest regex source = isJust (matchOnce regex source)
  matchCount regex source = length (matchAll regex source)
  matchOnceText regex source =
    fmap (\ma -> let (o,l) = ma ! 0
                 in (before o source
                    ,fmap (\ol -> (extract ol source,ol)) ma
                    ,after (o+l) source))
         (matchOnce regex source)
  matchAllText regex source =
    map (fmap (\ol -> (extract ol source,ol)))
        (matchAll regex source)


-- | @RegexContext@ is the polymorphic interface to do matching.  Since
-- 'target' is polymorphic you may need to supply the type explicitly
-- in contexts where it cannot be inferred.
--
-- The monadic 'matchM' version uses 'fail' to report when the 'regex'
-- has no match in 'source'.  Two examples:
--
-- Here the contest 'Bool' is inferred:
--
-- > [ c | let notVowel = makeRegex "[^aeiou]" :: Regex, c <- ['a'..'z'], match notVowel [c]  ]
-- >
-- > "bcdfghjklmnpqrstvwxyz"
--
-- Here the context @[String]@ must be supplied:
--
-- > let notVowel = (makeRegex "[^aeiou]" :: Regex )
-- > in do { c <- ['a'..'z'] ; matchM notVowel [c] } :: [String]
-- >
-- > ["b","c","d","f","g","h","j","k","l","m","n","p","q","r","s","t","v","w","x","y","z"]
--
class (RegexLike regex source) => RegexContext regex source target where
  match :: regex -> source -> target
  matchM :: (MonadFail m) => regex -> source -> m target


-- | Extract allows for indexing operations on 'String' or 'ByteString'.
--
class Extract source where

  -- | @before@ is a renamed 'take'.
  before :: Int -> source -> source

  -- | @after@ is a renamed 'drop'.
  after :: Int -> source -> source

  -- | When there is no match, this can construct an empty data value.
  empty :: source

  -- | @extract@ takes an offset and length, and has this default implementation:
  --
  -- @
  --   extract (off, len) source = before len (after off source)
  -- @
  extract :: (Int,Int) -> source -> source
  extract (off,len) source = before len (after off source)

instance Extract String where
  before =  take; after = drop; empty = []

instance Extract SB.ByteString where
  before = SB.take; after = SB.drop; empty = SB.empty

instance Extract LB.ByteString where
  before = LB.take . toEnum; after = LB.drop . toEnum; empty = LB.empty

instance Extract (S.Seq a) where
  before = S.take; after = S.drop; empty = S.empty

-- | @since 0.94.0.0
instance Extract ST.Text where
  before = ST.take; after = ST.drop; empty = ST.empty

-- | @since 0.94.0.0
instance Extract LT.Text where
  before = LT.take . toEnum; after = LT.drop . toEnum; empty = LT.empty

-- | Used in results of 'RegexContext' instances.
newtype AllSubmatches f b = AllSubmatches {getAllSubmatches :: (f b)}

-- | Used in results of 'RegexContext' instances.
newtype AllTextSubmatches f b = AllTextSubmatches {getAllTextSubmatches :: (f b)}

-- | Used in results of 'RegexContext' instances.
newtype AllMatches f b = AllMatches {getAllMatches :: (f b)}

-- | Used in results of 'RegexContext' instances.
newtype AllTextMatches f b = AllTextMatches {getAllTextMatches :: (f b) }
