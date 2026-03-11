module Math.OEIS
  ( -- * Example usage
    -- $sample

    -- * Lookup functions
    getSequenceByID,    lookupSequenceByID
  , extendSequence,     lookupSequence
  , getSequenceByID_IO, lookupSequenceByID_IO
  , extendSequence_IO,  lookupSequence_IO
  , searchSequence_IO,  lookupOEIS
  , searchSequences_IO, lookupSequences_IO

    -- * Data structures
  , SequenceData
  , Language(..), Keyword(..)
  , OEISSequence(..)
  ) where

--------------------------------------------------------------------------------

import Control.Applicative ((<$>))
import Prelude
import Data.Char        (isDigit, isSpace)
import Data.List        (isPrefixOf, tails, find)
import Data.Maybe       (listToMaybe, fromMaybe)
import Network.URI      (escapeURIString, isAllowedInURI)
import System.IO.Unsafe (unsafePerformIO)

import Math.OEIS.Internal
import Math.OEIS.Types

--------------------------------------------------------------------------------

-- | Interpret a string as a OEIS request, and return the results as Strings.
lookupOEIS :: String -> IO [String]
lookupOEIS a = do
         let a'  = commas . reverse . dropWhile isSpace . reverse . dropWhile isSpace $ a
         x <- searchSequence_IO a'
         case x of
            Nothing -> return ["Sequence not found."]
            Just s  -> return [description s, show $ sequenceData s]
 where commas []                     = []
       commas (x:' ':xs) | isDigit x = x : ',' : commas xs
       commas (x:xs)                 = x : commas xs

-- | Look up a sequence in the OEIS using its search function.
searchSequence_IO :: String -> IO (Maybe OEISSequence)
searchSequence_IO x = listToMaybe <$> searchSequences_IO x

-- | Look up sequences in the OEIS using its search function (returns up to
-- 10 results).
searchSequences_IO :: String -> IO [OEISSequence]
searchSequences_IO x = getOEIS (baseSearchURI ++) (escapeURIString isAllowedInURI x)

-- | Look up a sequence in the OEIS by its catalog number. Generally this would
-- be its A-number, but M-numbers (from the /Encyclopedia of Integer
-- Sequences/) and N-numbers (from the /Handbook of Integer Sequences/) can be
-- used as well.
--
-- Note that the result is not in the 'IO' monad, even though the
-- implementation requires looking up information via the Internet. There are
-- no side effects to speak of, and from a practical point of view the function
-- is referentially transparent (OEIS A-numbers could change in theory, but
-- it's extremely unlikely).
--
-- Examples:
--
-- > Prelude Math.OEIS> getSequenceByID "A000040"    -- the prime numbers
-- > Just [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47...
-- >
-- > Prelude Math.OEIS> getSequenceByID "nosuch"     -- no such sequence!
-- > Nothing

getSequenceByID :: String -> Maybe SequenceData
getSequenceByID = unsafePerformIO . getSequenceByID_IO

-- | The same as 'getSequenceByID', but with a result in the 'IO' monad.
getSequenceByID_IO :: String -> IO (Maybe SequenceData)
getSequenceByID_IO x = fmap (fmap sequenceData) (lookupSequenceByID_IO x)

-- | Look up a sequence by ID number, returning a data structure containing the
-- entirety of the information the OEIS has on the sequence.
--
-- The standard disclaimer about not being in the 'IO' monad applies.
--
-- Examples:
--
-- > Prelude Math.OEIS> description `fmap` lookupSequenceByID "A000040"
-- > Just "The prime numbers."
-- >
-- > Prelude Math.OEIS> keywords `fmap` lookupSequenceByID "A000105"
-- > Just [Nonn,Hard,Nice,Core]

lookupSequenceByID :: String -> Maybe OEISSequence
lookupSequenceByID = unsafePerformIO . lookupSequenceByID_IO

-- | The same as 'lookupSequenceByID', but in the 'IO' monad.
lookupSequenceByID_IO :: String -> IO (Maybe OEISSequence)
lookupSequenceByID_IO x = listToMaybe <$> getOEIS idSearchURI x

-- | Extend a sequence by using it as a lookup to the OEIS, taking the first
-- sequence returned as a result, and using it to augment the original
-- sequence.
--
-- Note that @xs@ is guaranteed to be a prefix of @extendSequence xs@. If the
-- matched OEIS sequence contains any elements prior to those matching @xs@,
-- they will be dropped. In addition, if no matching sequences are found, @xs@
-- will be returned unchanged.
--
-- The result is not in the 'IO' monad even though the implementation requires
-- looking up information via the Internet. There are no side effects, and
-- practically speaking this function is referentially transparent
-- (technically, results may change from time to time when the OEIS database is
-- updated; this is slightly more likely than the results of 'getSequenceByID'
-- changing, but still unlikely enough to be essentially a non-issue. Again,
-- purists may use 'extendSequence_IO').
--
-- Examples:
--
-- > Prelude Math.OEIS> extendSequence [5,7,11,13,17]
-- > [5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71...
--
-- > Prelude Math.OEIS> extendSequence [2,4,8,16,32]
-- > [2,4,8,16,32,64,128,256,512,1024,2048,4096,8192...
--
-- > Prelude Math.OEIS> extendSequence [9,8,7,41,562]   -- nothing matches
-- > [9,8,7,41,562]
extendSequence :: SequenceData -> SequenceData
extendSequence = unsafePerformIO . extendSequence_IO

-- | The same as 'extendSequence', but in the 'IO' monad.
extendSequence_IO :: [Integer] -> IO [Integer]
extendSequence_IO [] = return []
extendSequence_IO xs = do
    oeis <- lookupSequence_IO xs
    return $ case oeis of
      Nothing -> xs
      Just s  -> extend xs (sequenceData s)

-- | @extend xs ext@ returns the maximal suffix of @ext@ of which @xs@ is a
-- prefix, or @xs@ if @xs@ is not a prefix of any suffixes of @ext@. It is
-- guaranteed that
--
-- > forall xs ext. xs `isPrefixOf` (extend xs ext)
extend :: SequenceData -> SequenceData -> SequenceData
extend xs ext = fromMaybe xs . find (xs `isPrefixOf`) $ tails ext

-- | Find a matching sequence in the OEIS database, returning a data structure
-- containing the entirety of the information the OEIS has on the sequence.
--
-- The standard disclaimer about not being in the 'IO' monad applies.
lookupSequence :: SequenceData -> Maybe OEISSequence
lookupSequence = unsafePerformIO . lookupSequence_IO

-- | The same as 'lookupSequence', but in the 'IO' monad.
lookupSequence_IO :: SequenceData -> IO (Maybe OEISSequence)
lookupSequence_IO x = listToMaybe <$> lookupSequences_IO x

-- | Similar to 'lookupSequence_IO', but return up to 10 results.
lookupSequences_IO :: SequenceData -> IO [OEISSequence]
lookupSequences_IO = getOEIS seqSearchURI

--------------------------------------------------------------------------------

{- $sample

Suppose we are interested in answering the question, \"how many distinct binary
trees are there with exactly 20 nodes?\" Some naive code to answer this
question might be as follows:

> import Data.List (genericLength)
>
> -- data-less binary trees.
> data BTree = Empty | Fork BTree BTree  deriving Show
>
> -- A list of all the binary trees with exactly n nodes.
> listTrees :: Int -> [BTree]
> listTrees 0 = [Empty]
> listTrees n = [Fork left right |
>                k <- [0..n-1],
>                left <- listTrees k,
>                right <- listTrees (n-1-k) ]
>
> countTrees :: Int -> Integer
> countTrees = genericLength . listTrees

The problem, of course, is that @countTrees@ is horribly inefficient:

@
*Main> :set +s
*Main> countTrees 5
42
(0.00 secs, 0 bytes)
*Main> countTrees 10
16796
(0.47 secs, 27513240 bytes)
*Main> countTrees 12
208012
(7.32 secs, 357487720 bytes)
*Main> countTrees 13
*** Exception: stack overflow
@

There's really no way we can evaluate @countTrees 20@. The solution? Cheat!

> import Math.OEIS
>
> -- countTrees works ok up to 10 nodes.
> -- [1,2,5,14,42,132,429,1430,4862,16796]
> smallTreeCounts = map countTrees [0..10]
>
> -- now, extend the sequence via the OEIS!
> treeCounts = extendSequence smallTreeCounts

Now we can answer the question:

> *Main> treeCounts !! 20
> 6564120420

Sweet.  Of course, to have any sort of confidence in our answer, more research
is required! Let's see what combinatorial goodness we have stumbled across.

@
*Main> description \`fmap\` lookupSequence smallTreeCounts
Just \"Catalan numbers: C(n) = binomial(2n,n)\/(n+1) = (2n)!\/(n!(n+1)!). Also called Segner numbers.\"
@

Catalan numbers, interesting.  And a nice formula we could use to code up a
/real/ solution! Hmm, where can we read more about these so-called \'Catalan
numbers\'?

@
*Main> (head . references) \`fmap\` lookupSequence smallTreeCounts
Just [\"A. Bernini, F. Disanto, R. Pinzani and S. Rinaldi, Permutations defining convex permutominoes, preprint, 2007.\"]
*Main> (head . links) \`fmap\` lookupSequence smallTreeCounts
Just [\"N. J. A. Sloane, \<a href=\\\"http:\/\/www.research.att.com\/~njas\/sequences\/b000108.txt\\\"\>The first 200 Catalan numbers\<\/a\>\"]
@

And so on. Reams of collected mathematical knowledge at your fingertips! You
must promise only to use this power for Good.
-}

--------------------------------------------------------------------------------

{-# ANN module "HLint: ignore Use camelCase" #-}
