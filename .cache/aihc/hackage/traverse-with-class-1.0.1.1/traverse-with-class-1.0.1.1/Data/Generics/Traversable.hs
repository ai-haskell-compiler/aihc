-- | All of the functions below work only on «interesting» subterms.
-- It is up to the instance writer to decide which subterms are
-- interesting and which subterms should count as immediate. This can
-- also depend on the context @c@.
--
-- The context, denoted @c@, is a constraint (of kind @* -> Constraint@)
-- that provides additional facilities to work with the data.
-- In most cases, the context cannot be inferred automatically.
-- You need to provide it using the
-- <https://downloads.haskell.org/~ghc/8.0.2/docs/html/users_guide/glasgow_exts.html#visible-type-application type application syntax>:
--
-- > gmap @Show f x
-- > everywhere @Typeable f x
--
-- etc.
--
-- For more information, see:
--
-- [Scrap your boilerplate with class]
-- <https://www.microsoft.com/en-us/research/publication/scrap-your-boilerplate-with-class/>
--
-- [Generalizing generic fold]
-- <http://ro-che.info/articles/2013-03-11-generalizing-gfoldl>

module Data.Generics.Traversable
  (
    -- * Open recursion combinators

    GTraversable(..)
  , gmap
  , gmapM
  , gfoldMap
  , gfoldr
  , gfoldl'

    -- * Closed recursion combinators
  , Rec
  , everywhere
  , everywhere'
  , everywhereM
  , everything
  )
  where

import GHC.Exts (Constraint)

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Functor.Identity
import Data.Functor.Constant

import Data.Generics.Traversable.Core
import Data.Generics.Traversable.Instances ()

-- | 'Rec' enables \"deep traversals\".
--
-- It is satisfied automatically when its superclass constraints are
-- satisfied — you are not supposed to declare new instances of this class.
class    (GTraversable (Rec c) a, c a) => Rec (c :: * -> Constraint) a
instance (GTraversable (Rec c) a, c a) => Rec (c :: * -> Constraint) a

-- | Generic map over the immediate subterms
gmap
  :: forall c a . (GTraversable c a)
  => (forall d . (c d) => d -> d)
  -> a -> a
gmap f a = runIdentity (gtraverse @c (Identity . f) a)

-- | Generic monadic map over the immediate subterms
gmapM
  :: forall c m a . (Monad m, GTraversable c a)
  => (forall d . (c d) => d -> m d)
  -> a -> m a
gmapM f = unwrapMonad . gtraverse @c (WrapMonad . f)

-- | Generic monoidal fold over the immediate subterms (cf. 'Data.Foldable.foldMap')
gfoldMap
  :: forall c r a . (Monoid r, GTraversable c a)
  => (forall d . (c d) => d -> r)
  -> a -> r
gfoldMap f = getConstant . gtraverse @c (Constant . f)

-- | Generic right fold over the immediate subterms
gfoldr
  :: forall c a r . (GTraversable c a)
  => (forall d . (c d) => d -> r -> r)
  -> r -> a -> r
gfoldr f z t = appEndo (gfoldMap @c (Endo . f) t) z

-- | Generic strict left fold over the immediate subterms
gfoldl'
  :: forall c a r . (GTraversable c a)
  => (forall d . (c d) => r -> d -> r)
  -> r -> a -> r
gfoldl' f z0 xs = gfoldr @c f' id xs z0
  where f' x k z = k $! f z x

-- | Apply a transformation everywhere in bottom-up manner
everywhere
  :: forall c a .
     (Rec c a)
  => (forall d. (Rec c d) => d -> d)
  -> a -> a
everywhere f =
  let
    go :: forall b . Rec c b => b -> b
    go = f . gmap @(Rec c) go
  in go

-- | Apply a transformation everywhere in top-down manner
everywhere'
  :: forall c a .
     (Rec c a)
  => (forall d. (Rec c d) => d -> d)
  -> a -> a
everywhere' f =
  let
    go :: forall b . Rec c b => b -> b
    go = gmap @(Rec c) go . f
  in go

-- | Monadic variation on everywhere
everywhereM
  :: forall c m a .
     (Monad m, Rec c a)
  => (forall d. (Rec c d) => d -> m d)
  -> a -> m a
everywhereM f =
  let
    go :: forall b . Rec c b => b -> m b
    go = f <=< gmapM @(Rec c) go
  in go

-- | Strict left fold over all elements, top-down
everything
  :: forall c r a .
     (Rec c a)
  => (r -> r -> r)
  -> (forall d . (Rec c d) => d -> r)
  -> a -> r
everything combine f =
  let
    go :: forall b . Rec c b => b -> r
    go x = gfoldl' @(Rec c) (\a y -> combine a (go y)) (f x) x
  in go
