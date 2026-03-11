{-# LANGUAGE Trustworthy #-}
-- | Defunctionalized function combinators,
-- from [SKI](https://en.wikipedia.org/wiki/SKI_combinator_calculus)
-- and [BCKW](https://en.wikipedia.org/wiki/B,_C,_K,_W_system) combinator calculi.
--
-- These may be useful for writing anonymous functions in point-free style,
-- as pointful style would require extra defunctionalization symbols
-- (see e.g. t'DeFun.List.Map2' for an example).
--
module DeFun.Function (
    -- * Id, I
    Id, IdSym,
    id, idSym,
    -- * Const, K
    Const, ConstSym, ConstSym1,
    const, constSym, constSym1,
    -- * Flip, C
    Flip, FlipSym, FlipSym1, FlipSym2,
    flip, flipSym, flipSym1, flipSym2,
    -- * Comp, B
    Comp, CompSym, CompSym1, CompSym2,
    comp, compSym, compSym1, compSym2,
    -- * Ap, S
    Ap, ApSym, ApSym1, ApSym2,
    ap, apSym, apSym1, apSym2,
    -- * Join, W
    Join, JoinSym, JoinSym1,
    join, joinSym, joinSym1,
) where

import DeFun.Core

-- $setup
-- >>> import Prelude (Bool (..))
-- >>> import Data.Singletons.Bool (SBool (..))
-- >>> import DeFun

-- | Identity function. Combinator @I@ in https://en.wikipedia.org/wiki/SKI_combinator_calculus.
type Id :: a -> a
type family Id x where
    Id x = x

type IdSym :: a ~> a
data IdSym x
type instance App IdSym x = Id x

id :: a x -> a (Id x)
id x = x

idSym :: Lam a a IdSym
idSym = Lam id

--

-- | Constant function. Combinator @K@ in https://en.wikipedia.org/wiki/SKI_combinator_calculus and https://en.wikipedia.org/wiki/B,_C,_K,_W_system.

type Const :: a -> b -> a
type family Const x y where
    Const x y = x

type ConstSym :: a ~> b ~> a
data ConstSym x
type instance App ConstSym x = ConstSym1 x

type ConstSym1 :: a -> b ~> a
data ConstSym1 x y
type instance App (ConstSym1 x) y = Const x y

const :: a x -> b y -> a x
const x _ = x

constSym :: Lam2 a b a ConstSym
constSym = Lam constSym1

constSym1 :: a x -> Lam b a (ConstSym1 x)
constSym1 x = Lam (const x)

-- | Function flip. Combinator @C@ in https://en.wikipedia.org/wiki/B,_C,_K,_W_system.
type Flip :: (a ~> b ~> c) -> b -> a -> c
type family Flip f b a where
    Flip f b a = f @@ a @@ b

type FlipSym :: (a ~> b ~> c) ~> b ~> a ~> c
data FlipSym f
type instance App FlipSym f = FlipSym1 f

type FlipSym1 :: (a ~> b ~> c) -> b ~> a ~> c
data FlipSym1 f x
type instance App (FlipSym1 f) x = FlipSym2 f x

type FlipSym2 :: (a ~> b ~> c) -> b -> a ~> c
data FlipSym2 f b a
type instance App (FlipSym2 f b) a = Flip f b a

flip :: Lam2 a b c f -> b x -> a y -> c (Flip f x y)
flip f b a = f @@ a @@ b

flipSym :: Lam (a :~> b :~> c) (b :~> a :~> c) FlipSym
flipSym = Lam flipSym1

flipSym1 :: Lam2 a b c f -> Lam2 b a c (FlipSym1 f)
flipSym1 f = Lam (flipSym2 f)

flipSym2 :: Lam2 a b c f -> b x -> Lam a c (FlipSym2 f x)
flipSym2 f b = Lam (flip f b)


--

-- | Function composition. Combinator @B@ in https://en.wikipedia.org/wiki/B,_C,_K,_W_system.
type Comp :: (b ~> c) -> (a ~> b) -> a -> c
type family Comp f g x where
    Comp f g x = f @@ (g @@ x)

type CompSym :: (b ~> c) ~> (a ~> b) ~> a ~> c
data CompSym f
type instance App CompSym f = CompSym1 f

type CompSym1 :: (b ~> c) -> (a ~> b) ~> a ~> c
data CompSym1 f g
type instance App (CompSym1 f) g = CompSym2 f g

type CompSym2 :: (b ~> c) -> (a ~> b) -> a ~> c
data CompSym2 f g x
type instance App (CompSym2 f g) x = Comp f g x

comp :: Lam b c f -> Lam a b g -> a x -> c (Comp f g x)
comp f g x = f @@ (g @@ x)

compSym :: Lam (b :~> c) (Lam a b :~> Lam a c) CompSym
compSym = Lam compSym1

compSym1 :: Lam b c f -> Lam (a :~> b) (a :~> c) (CompSym1 f)
compSym1 f = Lam (compSym2 f)

compSym2 :: Lam b c f -> Lam a b g -> Lam a c (CompSym2 f g)
compSym2 f g = Lam (comp f g)

-- | Combinator 'S' in https://en.wikipedia.org/wiki/SKI_combinator_calculus.
type Ap :: (a ~> b ~> c) -> (a ~> b) -> a -> c
type family Ap f g x where
    Ap f g x = f @@ x @@ (g @@ x)

type ApSym :: (a ~> b ~> c) ~> (a ~> b) ~> a ~> c
data ApSym f
type instance App ApSym f = ApSym1 f

type ApSym1 :: (a ~> b ~> c) -> (a ~> b) ~> a ~> c
data ApSym1 f g
type instance App (ApSym1 f) g = ApSym2 f g

type ApSym2 :: (a ~> b ~> c) -> (a ~> b) -> a ~> c
data ApSym2 f g x
type instance App (ApSym2 f g) x = Ap f g x

ap :: Lam2 a b c f -> Lam a b g -> a x -> c (Ap f g x)
ap f g x = f @@ x @@ (g @@ x)

apSym :: Lam3 (a :~> b :~> c) (a :~> b) a c ApSym
apSym = Lam apSym1

apSym1 :: Lam2 a b c f -> Lam2 (a :~> b) a c (ApSym1 f)
apSym1 f = Lam (apSym2 f)

apSym2 :: Lam2 a b c f -> Lam a b g -> Lam a c (ApSym2 f g)
apSym2 f g = Lam (ap f g) 

-- | Combinator 'W' in https://en.wikipedia.org/wiki/B,_C,_K,_W_system
type Join :: (a ~> a ~> b) -> a -> b
type family Join f x where
    Join f x = f @@ x @@ x

type JoinSym :: (a ~> a ~> b) ~> a ~> b
data JoinSym f
type instance App JoinSym f = JoinSym1 f

type JoinSym1 :: (a ~> a ~> b) -> a ~> b
data JoinSym1 f x
type instance App (JoinSym1 f) x = Join f x

join :: Lam2 a a b f -> a x -> b (Join f x)
join f x = f @@ x @@ x

joinSym :: Lam2 (a :~> a :~> b) a b JoinSym
joinSym = Lam joinSym1

joinSym1 :: Lam2 a a b fun -> Lam a b (JoinSym1 fun)
joinSym1 f = Lam (join f)
