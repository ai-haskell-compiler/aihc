{-# LANGUAGE ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Applicative.Permutation
-- Copyright   :  Ross Paterson 2009
-- License     :  BSD-style (see the LICENSE file in the distribution)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  existentials
--
-- Constructing an action as a choice between all the permutations of
-- some given actions (e.g. parsers), based on \"Parsing Permutation
-- Phrases\", by Arthur Baars, Andres Loeh and S. Doaitse Swierstra,
-- /Haskell Workshop 2001/.
--
-- This version has a slightly different interface from the paper.
-----------------------------------------------------------------------------

module Control.Applicative.Permutation (
    -- * Permutations of actions
    Perms,
    -- ** Primitive permutations
    atom,
    optAtom,
    maybeAtom,
    -- ** Extracting permutation actions
    runPerms,
    runPermsSep
    -- * Parsing example
    -- $example1

    -- * Other examples
    -- $example2
    ) where

import Control.Applicative (Applicative(..), Alternative(..), (<$>), (<$))

-- | A representation of a permutation of actions of an 'Alternative' type @p@.
-- The value type of the composite action is @a@.
--
-- Permutations are constructed from the primitives 'atom', 'optAtom'
-- and 'maybeAtom', and combined using the methods of 'Functor' and
-- 'Applicative'.  They are converted back to composite actions using
-- 'runPerms' and 'runPermsSep'.
--
-- The component actions of a permutation will be executed in each
-- possible order, but the values they produce are always assembled
-- in the order they occur in the program text, as in the following
-- permutations of one, two or three component actions:
--
-- * @'runPerms' (f '<$>' 'atom' a) = f '<$>' a@
--
-- * @'runPerms' (f '<$>' 'atom' a '<*>' 'atom' b) = (f '<$>' a '<*>' b) '<|>' ('flip' f '<$>' b '<*>' a)@
--
-- * @'runPerms' (f '<$>' 'atom' a '<*>' 'atom' b '<*>' 'atom' c) =
--   ((\\ x (y,z) -> f x y z) '<$>' a '<*>' ((,) '<$>' b '<*>' c) '<|>' ('flip' (,) '<$>' c '<*>' b)) '<|>'
--   ((\\ y (z,x) -> f x y z) '<$>' b '<*>' ((,) '<$>' a '<*>' c) '<|>' ('flip' (,) '<$>' c '<*>' a)) '<|>'
--   ((\\ z (x,y) -> f x y z) '<$>' c '<*>' ((,) '<$>' a '<*>' b) '<|>' ('flip' (,) '<$>' b '<*>' a))@
--
-- The permutation is encoded as a tree, with the first action
-- executed before the second selection is made.  Thus failing actions,
-- e.g. parsers, prune this tree.  The size of the tree is exponential
-- in the number of components, but it is constructed lazily.
data Perms p a = Choice (Maybe a) [Branch p a]
data Branch p a = forall b. Branch (Perms p (b -> a)) (p b)

instance Functor p => Functor (Perms p) where
    fmap f (Choice def bs) = Choice (fmap f def) (map (fmap f) bs)

instance Functor p => Functor (Branch p) where
    fmap f (Branch perm p) = Branch (fmap (f .) perm) p

-- These instances actually only need Applicative p, but we specify
-- Alternative for uniformity and to preserve the abstraction.

instance Alternative p => Applicative (Perms p) where
    pure v = Choice (Just v) []
    t1@(Choice d1 bs1) <*> t2@(Choice d2 bs2) =
        Choice (d1 <*> d2) (map ins2 bs1 ++ map ins1 bs2)
          where
            ins1 (Branch perm p) = Branch ((.) <$> t1 <*> perm) p
            ins2 (Branch perm p) = Branch (flip <$> perm <*> t2) p

instance Alternative p => Applicative (Branch p) where
    pure v = branch (pure v)
    Branch perm1 p1 <*> Branch perm2 p2 =
        Branch (lift_id <$> perm1 <*> perm2) ((,) <$> p1 <*> p2)

branch :: Alternative p => p a -> Branch p a
branch p = Branch (pure id) p

lift_id :: (a -> c -> r) -> (b -> c) -> (a,b) -> r
lift_id f g (x,y) = f x (g y)

-- Constructing primitive permutations.
-- The definitions of these three functions actually only require
-- Applicative p, but we specify Alternative for uniformity with
-- runPerms and runPermsSep, which are the only way to use the resulting
-- permutation.  Thus the interface hides the implementation detail of
-- where the Alternative methods are used.

-- | A primitive permutation consisting of a single action.
--
-- * @'runPerms' ('atom' a) = a@
--
-- When building permutation parsers, the argument parser should not match
-- the empty string: use 'optAtom' or 'maybeAtom' for optional elements.
atom :: Alternative p => p a -> Perms p a
atom p = Choice Nothing [branch p]

-- | Like 'atom', but the action may be omitted from the permutation.
--
-- * @'runPerms' ('optAtom' d p) = p '<|>' 'pure' d@
--
-- When building permutation parsers, the argument parser should not match
-- the empty string.
optAtom :: Alternative p => a -> p a -> Perms p a
optAtom def p = Choice (Just def) [branch p]

-- | Like 'atom', but the action may be omitted from the permutation.
--
-- * @'runPerms' ('maybeAtom' p) = 'Just' '<$>' p '<|>' 'pure' 'Nothing'@
--
-- When building permutation parsers, the argument parser should not match
-- the empty string.
maybeAtom :: Alternative p => p a -> Perms p (Maybe a)
maybeAtom p = optAtom Nothing (Just <$> p)

-- Extracting a permutation action from a permutation tree

-- | Construct a permutation action.
--
-- * @'runPerms' ('pure' x) = 'pure' x@
--
-- * @'runPerms' (f '<$>' p) = f '<$>' 'runPerms' p@
runPerms :: Alternative p => Perms p a -> p a
runPerms = foldChoice pars
  where pars (Branch t p) = flip ($) <$> p <*> runPerms t
-- NOT: pars (Branch t p) = runPerms t <*> p

-- | @'runPermsSep' sep p@ is similar to @'runPerms' p@, except that the
-- action @sep@ is interleaved between atomic actions in each permutation.
--
-- * @'runPermsSep' sep (f '<$>' 'atom' a) = f '<$>' a@
--
-- * @'runPermsSep' sep (f '<$>' 'atom' a '<*>' 'atom' b) = (f '<$>' a '<*' sep '<*>' b) '<|>' ('flip' f '<$>' b '<*' sep '<*>' a)@
--
-- It is particularly useful in constructing permutation parsers, where
-- @sep@ might be a parser for a comma or other separator.
runPermsSep :: Alternative p => p b -> Perms p a -> p a
runPermsSep sep = foldChoice (runBranchSep sep)

runPermsPref :: Alternative p => p b -> Perms p a -> p a
runPermsPref sep (Choice d bs) =
    (sep *> foldr ((<|>) . runBranchSep sep) empty bs) <|> maybe empty pure d

runBranchSep :: Alternative p => p b -> Branch p a -> p a
runBranchSep sep (Branch t p) = flip ($) <$> p <*> runPermsPref sep t

foldChoice :: Alternative p => (Branch p a -> p a) -> Perms p a -> p a
foldChoice f (Choice d bs) = foldr ((<|>) . f) (maybe empty pure d) bs

{- $example1

This example (based on the paper) involves parsing XHTML @img@ elements,
which have a number of attributes, some optional, that may occur in any
order, e.g.

> <img alt="Lambda" src="lambda.jpg" width=20 height=50/>

We assume a data type for XHTML elements, with a constructor @Img@ as one
alternative:

> data XHTML
>    = ...
>    | Img
>         { src :: URI
>         , alt :: Text
>         , longdesc :: Maybe URI
>         , height :: Maybe Length
>         , width :: Maybe Length
>         }

> type Text = String
> type URI = String
> type Length = Int

Suppose we have a parser type @Parser@ (an instance of 'Alternative')
with primitive parsers:

> pToken :: String -> Parser ()
> pSymbol :: Char -> Parser ()
> pText :: Parser Text
> pURI :: Parser URI
> pLength :: Parser Length

Then we can construct a parser for @img@ elements as follows:

> pImgTag :: Parser XHTML
> pImgTag = pToken "<" *> pToken "img" *> attrs <* pToken "/>"
>   where attrs = runPerms $ Img
>                         <$> atom (pField "src" pURI)
>                         <*> atom (pField "alt" pText)
>                         <*> maybeAtom (pField "longdesc" pURI)
>                         <*> maybeAtom (pField "height" pLength)
>                         <*> maybeAtom (pField "width" pLength)

> pField :: String -> Parser a -> Parser a
> pField f p = pToken f *> pSymbol '=' *> p

-}

{- $example2

Although permutations are particularly useful with parsers, they may
also be used with other instances of 'Alternative'.

For example, we can generate all the permutations of a list by permuting
@tell@ actions for the elements:

> import Control.Monad.Writer (execWriterT, tell)
> import Data.Foldable (sequenceA_)

> permutations :: [a] -> [[a]]
> permutations xs =
>     execWriterT $ runPerms $ sequenceA_ [atom (tell [x]) | x <- xs]

Note that if each atomic action simply returned an element on the list,
the result would be many copies of the original list, because the
combinators ensure that the results are re-assembled in the original order,
no matter what order the actions are executed.

We can also achieve a permutation of the integers 1 to @n@ by using a
permutation of effects that increment and return a state:

> import Control.Monad.State (evalStateT, get, put)
> import Data.Traversable (traverse)

> permuteN :: Int -> [[Int]]
> permuteN n = evalStateT (runPerms (traverse atom (replicate n incr))) 1
>   where incr = do { n <- get; put (n+1); return n }

A solution to the n-queens problem is such a permutation satisfying the
additional condition that no two positions are on the same diagonal.
We can adapt the previous example to implement this idea by changing
the state to a list of positions for the first @n@ rows.  Then when
adding a new position we need only check that it is not on the same
diagonal as the previous positions.  If this test fails, the partial
permutation will be discarded.  Thus the algorithm is

> import Control.Monad.State (evalStateT, get, put)
> import Data.Traversable (traverse)

> queens :: Int -> [[Int]]
> queens n = evalStateT (runPerms (traverse (atom . place) [1..n])) []

where the auxiliary function @place@ attempts to place a queen in a given
position on the current row, returning the row number.

> place :: Int -> StateT [Int] [] Int
> place n = do
>     ns <- get
>     guard (and [abs (m-n) /= k | (k, m) <- zip [1..] ns])
>     put (n:ns)
>     return (length ns + 1)

-}
