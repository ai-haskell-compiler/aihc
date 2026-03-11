{-# LANGUAGE EmptyCase #-}

{- | Uniform representation + handling of n-ary functions.

This module gives us types and functions (both value- and type-level) to work
with n-ary functions.
The following are all function types, yet have very different shapes:

@
f1 :: Int -> Int
f1 = undefined
f2 :: a -> (b, a) -> c
f2 = undefined
f3 :: a -> (a -> a) -> (a -> a -> a) -> a
f3 = undefined
@

However there are 2 ways we can "unify" these into a common structure.
Both ways involve diving the function type into arguments (e.g. @Int ->@,
@a -> (b, a) ->@ and @a -> (a -> a) -> (a -> a -> a) ->@), and result types
(e.g. @Int@, @c@ and @a@).

The first way is to see these functions as right-associative folds.
Imagine a type-level function directly corresponding to 'foldr':

@
type family Foldr cons xs nil where
  Foldr cons '[] nil = nil
  Foldr cons (x ': xs) nil = cons x (Foldr cons xs nil)
@

Then using the function arrow @(->)@ for @cons@, the result type of our function
for @nil@ and the list of arguments for @xs@:

@
f1_ :: Foldr (->) '[Int] Int
f1_ = f1
f2_ :: Foldr (->) '[a, (b, a)] c
f2_ = f2
f3_ :: Foldr (->) '[a, a -> a, a -> a -> a] a
f3_ = f3
@

The 'Chain' family does exactly that. Since GHC can unify these types, we
can use 'Chain' in our types signatures in "Generics.Case" and the user doesn't
have to think about SOP, generics etc.

@
f1__ :: Chain '[Int] Int
f1__ = f1_
f2__ :: Chain '[a, (b, a)] c
f2__ = f2_
f3__ :: Chain '[a, a -> a, a -> a -> a] a
f3__ = f3_
@

'Chains' iterates on this concept: it is a type-level family representing
a function of functions. This lets us represent "case analysis" functions like
'maybe' and 'either' nicely (see "Generics.Case"):

@
maybe' :: forall a r. Maybe a -> 'Chains' '[ '[], '[a]] r
maybe' m r f = 'maybe' r f m

either' :: forall a b r. Either a b -> 'Chains' '[ '[a], '[b]] r
either' e fa fb = 'either' fa fb e

bool' :: forall r. Bool -> 'Chains' '[ '[], '[]] r
bool' b f t = 'Data.Bool.bool' f t b
@

'ChainsR' does the same thing, but let us pass the datatype __after__ the functions,
exactly matching the shape of 'maybe' and 'either':

@
maybe'' :: forall a r. 'ChainsR' '[ '[], '[a]] (Maybe a) r
maybe'' = 'maybe'

either'' :: forall a b r. 'ChainsR' '[ '[a], '[b]] (Either a b) r
either'' = 'either'

bool'' :: forall r. 'ChainsR' '[ '[], '[]] Bool r
bool'' = 'Data.Bool.bool'
@

Both 'Chains' and 'ChainsR' are implemented using 'Chains'', and we can convert between them using
'toChainsR' and 'fromChainsR'.
-}
module Generics.Chain
  ( -- * Representation of n-ary functions
    Chain
  , toChain
  , fromChain

    -- * Functions of functions
  , Chains
  , applyChains
  , constChain

    -- * Flipped argument order
  , ChainsR
  , toChainsR
  , fromChainsR
  , Chains'
  )
where

import Data.SOP

{- | Type family representing an n-ary function. The first argument is a type-level list
that represent the arguments to the function; the second argument represents the result of
the function.

Isomorphic to @'NP' 'I' xs -> r@, as witnessed by 'fromChain' and 'toChain'.

@
Chain '[x, y, z] r
  ~ (x -> y -> z -> r)
@
-}
type family Chain xs r where
  Chain '[] r = r
  Chain (x ': xs) r = x -> Chain xs r

{- | Convert from type family 'Chain' to a function of a product 'NP'.

Inverse of 'toChain'.
-}
fromChain :: forall xs r. Chain xs r -> NP I xs -> r
fromChain c = \case
  Nil -> c
  I x :* xs -> fromChain (c x) xs

{- | Convert from a function of a product, to type family 'Chain'.

e.g.

@
productChain :: 'NP' 'I' '[Int, Maybe Char] -> String
productChain ('I' n :* 'I' mChar :* Nil) = show n <> " " <> show mChar

chain :: Int -> Maybe Char -> String
chain = toChain productChain
@
-}
toChain :: forall xs r. (SListI xs) => (NP I xs -> r) -> Chain xs r
toChain f = case sList @xs of
  SNil -> f Nil
  SCons -> \x -> toChain $ \xs -> f (I x :* xs)

{- | This type family generalises 'Chains' when we want the inner 'Chain's to have a different
return type to the overall function.

@
Chains' '[ '[x,y], '[z], '[]] ret final
  ~ Chain '[x,y] final -> Chain '[z] final -> Chain '[] final -> ret
  ~ (x -> y -> final)  -> (z -> final)     -> final           -> ret
@

It's used to implement both 'Chains' and 'ChainsR'.
-}
type family Chains' xss ret final where
  Chains' '[] ret final = ret
  Chains' (xs ': xss) ret final = Chain xs final -> Chains' xss ret final

{- | The next level up from 'Chain': now we represent a function of functions.

@
Chains '[ '[x,y], '[z], '[]] r
  ~ Chain '[x,y] r -> Chain '[z] r -> Chain '[] r -> r
  ~ (x -> y -> r)  -> (z -> r)     -> r           -> r
@

In an ideal world, we'd be able to write:

@
type Chains xss r = Chain (Map (\xs -> Chain xs r) xss) r
@
-}
type Chains xss r = Chains' xss r r

{- | Apply a series of chains. Used to implement 'Generics.Case.gcase'.

You can think of the signature and implementation of this function as being:

@
applyChains ::
  'NS' ('NP' 'I') '[xs1, xs2, ... , xsn] ->
  Chains xs1 r ->
  Chains xs2 r ->
  ... ->
  Chains xsn r ->
  r
applyChains (Z x1)                 f1 _  _ ... _  = fromChain f1 xs
applyChains (S (S x2)              _  f2 _ ... _  = fromChain f2 xs
...
applyChains (S (S (... (S xn)..))) _  _  _ ... fn = fromChain fn xs
@
-}
applyChains :: forall xss r. (SListI xss) => NS (NP I) xss -> Chains xss r
applyChains = go shape
  where
    go :: forall yss. Shape yss -> NS (NP I) yss -> Chains yss r
    go = \case
      ShapeNil -> \case {}
      ShapeCons (shp :: Shape xs) -> \case
        Z (npx :: NP I x) -> \cx -> constChain @_ @r (fromChain @x @r cx npx) shp
        S (s :: NS (NP I) xs) -> \_ -> go shp s

{- | Once we've hit the 'Z' and applied the correspond 'Chain', we've got our final answer and
we want to skip the rest of the functions and just return. This lets us do that.

You can think of the signature and implementation of this function (ignoring the 'Shape',
which just helps GHC understand the recursion) as being:

@
constChain ::
  r ->
  Chains xs1 r ->
  Chains xs2 r ->
  ... ->
  Chains xsn r ->
  r
constChain r _ _ ... _ = r
@
-}
constChain :: forall xss r. r -> Shape xss -> Chains xss r
constChain r = \case
  ShapeNil -> r
  ShapeCons s -> \_ -> constChain @_ @r r s

{- | Isomorphic to @a -> Chains xss r@, as witnessed by 'toChainsR' and 'fromChainsR'.

@
ChainsR '[ '[x,y], '[z], '[]] a r
  ~ Chain '[x,y] r -> Chain '[z] r -> Chain '[] r -> a -> r
  ~ (x -> y -> r)  -> (z -> r)     -> r           -> a -> r
@
-}
type ChainsR xss a r = Chains' xss (a -> r) r

{- | Convert between different representations of second-order n-ary functions. Should be inverse to 'fromChainsR'.

This will be used to convert from 'Generics.Case.Analysis' to 'Generics.Case.AnalysisR' in the implementation of
'Generics.Case.gcaseR'.

Morally we can think of this as:

@
toChainsR ::
  ( a -> Chains xs1 r -> ... -> Chains xsn r -> r) ->
  ( Chains xs1 r -> ... -> Chains xsn r -> a -> r)
toChainsR f c1 c2 ... cn a = f a c1 c2 ... cn
@

Effectively, it's an n-ary generalisation of 'flip'.
-}
toChainsR :: forall xss a r. (SListI xss) => (a -> Chains xss r) -> ChainsR xss a r
toChainsR = go $ shape @_ @xss
  where
    go :: forall yss. Shape yss -> (a -> Chains yss r) -> ChainsR yss a r
    go = \case
      ShapeNil -> id
      ShapeCons sc -> \f c -> go sc (`f` c)

{- | Convert between different representations of second-order n-ary functions. Should be inverse to 'toChainsR'.

Again, we can think of this as an n-ary generalisation of 'flip', in the other direction:

@
fromChainsR ::
  ( Chains xs1 r -> ... -> Chains xsn r -> a -> r) ->
  ( a -> Chains xs1 r -> ... -> Chains xsn r -> r)
fromChainsR f a c1 c2 ... cn = f c1 c2 ... cn a
@
-}
fromChainsR :: forall xss a r. (SListI xss) => ChainsR xss a r -> (a -> Chains xss r)
fromChainsR = go $ shape @_ @xss
  where
    go :: forall yss. Shape yss -> ChainsR yss a r -> (a -> Chains yss r)
    go = \case
      ShapeNil -> id
      ShapeCons (sc :: Shape xs) -> \f a c -> go sc (f c) a
