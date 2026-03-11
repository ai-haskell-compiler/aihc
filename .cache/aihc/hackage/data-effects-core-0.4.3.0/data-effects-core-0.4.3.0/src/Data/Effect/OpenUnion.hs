{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- SPDX-License-Identifier: MPL-2.0

{- |
Copyright   :  (c) 2024-2025 Sayo contributors
License     :  MPL-2.0 (see the file LICENSE)
Maintainer  :  ymdfield@outlook.jp
-}
module Data.Effect.OpenUnion where

import Control.Arrow ((&&&))
import Data.Coerce (coerce)
import Data.Data (Proxy (Proxy), (:~:) (Refl))
import Data.Effect (Effect, EffectForm (Exponential, Polynomial), EffectOrder (FirstOrder, HigherOrder), FirstOrder, FormCase, FormOf, LabelOf, OrderCase, OrderOf, PolyHFunctor, Weave, weave)
import Data.Effect.HFunctor (HFunctor, hfmap)
import Data.Effect.Tag (type (#))
import Data.Kind (Constraint, Type)
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:$$:), (:<>:)), KnownNat, Natural, Symbol, TypeError, natVal, type (+), type (-))
import Unsafe.Coerce (unsafeCoerce)

type role Union representational representational nominal
data Union (es :: [Effect]) (f :: Type -> Type) (a :: Type) where
    UnsafeUnion
        :: {-# UNPACK #-} !Int
        -- ^ A natural number tag to identify the element of the union.
        -> e g a
        -> {-# UNPACK #-} !EffectOrder
        -> (forall x. g x -> f x)
        -> Union es f a

instance HFunctor (Union es) where
    hfmap phi u@(UnsafeUnion n e order koi) =
        case order of
            FirstOrder -> unsafeCoerce u
            HigherOrder -> UnsafeUnion n e HigherOrder (phi . koi)
    {-# INLINE hfmap #-}

hfmapUnion :: (forall x. f x -> g x) -> Union es f a -> Union es g a
hfmapUnion phi u@(UnsafeUnion n e order koi) =
    case order of
        FirstOrder -> unsafeCoerce u
        HigherOrder -> UnsafeUnion n e HigherOrder (phi . koi)
{-# INLINE hfmapUnion #-}

-- | The list @es@ consists only of first-order effects.
class FOEs (es :: [Effect])

instance FOEs '[]
instance (FirstOrder e, FOEs es) => FOEs (e ': es)

-- | The list @es@ consists only of polynomial effects.
class PolyHFunctors (es :: [Effect])

instance PolyHFunctors '[]
instance (PolyHFunctor e, PolyHFunctors es) => PolyHFunctors (e ': es)

-- | The list @es@ consists only of weavable effects.
class (Weave (Union es)) => Weaves (es :: [Effect])

instance Weaves '[]
instance (Weave e, Weaves es, KnownOrder e) => Weaves (e ': es)

weaveUnion
    :: (Weaves es, Functor ctx)
    => ctx ()
    -> (forall x. ctx (m x) -> n (ctx x))
    -> (forall x. Union es n x -> (x -> ctx a) -> r)
    -> Union es m a
    -> r
weaveUnion = weave
{-# INLINE weaveUnion #-}

instance Weave (Union '[]) where
    weave _ _ _ = nil
    {-# INLINE weave #-}

instance (Weave e, Weave (Union es), KnownOrder e) => Weave (Union (e ': es)) where
    weave ctx distrib f =
        weave ctx distrib (f . inject Here)
            !: weave ctx distrib (f . weaken)
    {-# INLINE weave #-}

coerceFOEs :: (FOEs es) => Union es f a -> Union es g a
coerceFOEs = unsafeCoerce
{-# INLINE coerceFOEs #-}

type instance OrderOf (Union es) = 'HigherOrder

type role Membership representational representational
newtype Membership (e :: Effect) (es :: [Effect]) = UnsafeMembership {unMembership :: Int}
    deriving stock (Eq, Show)

pattern Here :: Membership e (e ': es)
pattern Here = UnsafeMembership 0
{-# INLINE Here #-}

pattern There :: Membership e es -> Membership e (e ': es)
pattern There i <- ((UnsafeMembership . (`subtract` 1) &&& (/= 0)) . unMembership -> (i, True))
    where
        There (UnsafeMembership n) = UnsafeMembership (n + 1)
{-# INLINE There #-}

infixr 5 !:>
(!:>) :: forall e e' es r. ((e :~: e') -> r) -> (Membership e es -> r) -> Membership e (e' ': es) -> r
(f !:> g) (UnsafeMembership n)
    | n == 0 = f $ unsafeCoerce Refl
    | otherwise = g $ UnsafeMembership $ n - 1
{-# INLINE (!:>) #-}

weakenFor :: Membership e es -> Membership e (e' ': es)
weakenFor (UnsafeMembership n) = UnsafeMembership $ n + 1
{-# INLINE weakenFor #-}

mapUnion
    :: forall es es' f a
     . (forall e. Membership e es -> Membership e es')
    -> Union es f a
    -> Union es' f a
mapUnion f (UnsafeUnion n e order koi) =
    UnsafeUnion (unMembership $ f $ UnsafeMembership n) e order koi
{-# INLINE mapUnion #-}

membershipAt :: forall i es. (KnownNat i) => Membership (At i es) es
membershipAt = UnsafeMembership $ intVal @i
{-# INLINE membershipAt #-}

compareMembership :: Membership e es -> Membership e' es -> Maybe (e :~: e')
compareMembership (UnsafeMembership m) (UnsafeMembership n)
    | m == n = Just $ unsafeCoerce Refl
    | otherwise = Nothing

type family At i es where
    At 0 (e ': es) = e
    At n (e ': es) = At (n - 1) es
    At _ '[] = TypeError ('Text "Effect index out of range")

intVal :: forall n. (KnownNat n) => Int
intVal = fromIntegral $ natVal @n Proxy
{-# INLINE intVal #-}

data LabelResolver
data KeyResolver
data IdentityResolver

data KeyDiscriminator key
data NoKeyDiscriminator

data IdentityDiscriminator (e :: Effect)

type family Discriminator resolver (e :: Effect)
type instance Discriminator LabelResolver e = LabelOf e
type instance Discriminator KeyResolver e = KeyOf e
type instance Discriminator IdentityResolver e = IdentityDiscriminator e

type family KeyOf e where
    KeyOf (e # key) = KeyDiscriminator key
    KeyOf e = NoKeyDiscriminator

type family ResolverName resolver :: Symbol
type instance ResolverName LabelResolver = "label"
type instance ResolverName KeyResolver = "key"
type instance ResolverName IdentityResolver = "identity"

infix 4 :>
infix 4 `In`

type e :> es = MemberBy LabelResolver (Discriminator LabelResolver e) e es
type Has key e es = MemberBy KeyResolver (KeyDiscriminator key) (e # key) es
type e `In` es = MemberBy IdentityResolver (IdentityDiscriminator e) e es
type KnownIndex i es = (KnownNat i, KnownOrder (At i es))

type FindByLabel label e es = MemberBy LabelResolver label e es

type MemberBy resolver dscr e es =
    ( FindBy resolver dscr (Discriminator resolver (HeadOf es)) e es
    , ErrorIfNotFound resolver dscr (Discriminator resolver (HeadOf es)) e es es
    , KnownOrder e
    )

class
    (dscr ~ Discriminator resolver e, dscr' ~ Discriminator resolver (HeadOf r)) =>
    FindBy resolver dscr dscr' e r
        | resolver dscr dscr' r -> e
    where
    findBy :: Membership e r

instance
    (dscr ~ Discriminator resolver e, dscr ~ Discriminator resolver e', e ~ e')
    => FindBy resolver dscr dscr e (e' ': r)
    where
    findBy = Here
    {-# INLINE findBy #-}

instance
    {-# OVERLAPPABLE #-}
    ( dscr ~ Discriminator resolver e
    , dscr' ~ Discriminator resolver e'
    , FindBy resolver dscr (Discriminator resolver (HeadOf r)) e r
    )
    => FindBy resolver dscr dscr' e (e' ': r)
    where
    findBy = weakenFor $ findBy @resolver @dscr @(Discriminator resolver (HeadOf r)) @e @r
    {-# INLINE findBy #-}

membership
    :: forall resolver dscr e es
     . (FindBy resolver dscr (Discriminator resolver (HeadOf es)) e es)
    => Membership e es
membership = findBy @resolver @dscr @(Discriminator resolver (HeadOf es)) @e @es
{-# INLINE membership #-}

labelMembership
    :: forall e es
     . (FindBy LabelResolver (LabelOf e) (LabelOf (HeadOf es)) e es)
    => Membership e es
labelMembership = membership @LabelResolver
{-# INLINE labelMembership #-}

keyMembership
    :: forall key e es
     . (FindBy KeyResolver (KeyDiscriminator key) (KeyOf (HeadOf es)) (e # key) es)
    => Membership (e # key) es
keyMembership = membership @KeyResolver
{-# INLINE keyMembership #-}

identityMembership
    :: forall e es
     . (FindBy IdentityResolver (IdentityDiscriminator e) (IdentityDiscriminator (HeadOf es)) e es)
    => Membership e es
identityMembership = membership @IdentityResolver
{-# INLINE identityMembership #-}
class
    (dscr ~ Discriminator resolver e, dscr' ~ Discriminator resolver (HeadOf r)) =>
    ErrorIfNotFound resolver dscr dscr' (e :: Effect) (r :: [Effect]) (w :: [Effect])

instance
    ( TypeError
        ( 'Text "The effect ‘"
            ':<>: 'ShowType e
            ':<>: 'Text "’ does not exist within the effect list"
            ':$$: 'Text "  ‘" ':<>: 'ShowType w ':<>: 'Text "’"
            ':$$: 'Text "Resolver: " ':<>: 'Text (ResolverName resolver)
            ':$$: 'Text "Discriminator: " ':<>: 'ShowType dscr
        )
    , dscr ~ Discriminator resolver e
    , dscr' ~ Discriminator resolver (HeadOf '[])
    )
    => ErrorIfNotFound resolver dscr dscr' e '[] w

instance
    (dscr ~ Discriminator resolver e, dscr ~ Discriminator resolver e', e ~ e')
    => ErrorIfNotFound resolver dscr dscr e (e' ': r) w

instance
    {-# OVERLAPPABLE #-}
    ( dscr ~ Discriminator resolver e
    , dscr' ~ Discriminator resolver e'
    , ErrorIfNotFound resolver dscr (Discriminator resolver (HeadOf r)) e r w
    )
    => ErrorIfNotFound resolver dscr dscr' e (e' ': r) w

instance
    {-# INCOHERENT #-}
    ( dscr ~ Discriminator resolver e
    , dscr' ~ Discriminator resolver (HeadOf r)
    )
    => ErrorIfNotFound resolver dscr dscr' e r w

type family HeadOf es where
    HeadOf (e ': es) = e

type KnownOrder e = Elem e (OrderOf e)

class (order ~ OrderOf e) => Elem e order where
    inject :: Membership e es -> e f a -> Union es f a
    project :: Membership e es -> Union es f a -> Maybe (e f a)
    (!:) :: (e f a -> r) -> (Union es f a -> r) -> Union (e ': es) f a -> r
    extract :: Union '[e] f a -> e f a

    infixr 5 !:

decomp :: (KnownOrder e, HFunctor e) => Union (e ': es) f a -> Either (e f a) (Union es f a)
decomp = Left !: Right
{-# INLINE decomp #-}

instance (FirstOrder e) => Elem e 'FirstOrder where
    inject :: forall es f a. Membership e es -> e f a -> Union es f a
    inject i = mkUnion i firstOrdership

    project
        :: forall es f a
         . Membership e es
        -> Union es f a
        -> Maybe (e f a)
    project i (UnsafeUnion n e _ _) =
        if n == unMembership i
            then Just $ unsafeCoerce e
            else Nothing

    (f !: g) (UnsafeUnion n e koi order) =
        if n == 0
            then f $ unsafeCoerce e
            else g $ UnsafeUnion (n - 1) e koi order

    extract (UnsafeUnion _ e _ _) = unsafeCoerce e

    {-# INLINE inject #-}
    {-# INLINE project #-}
    {-# INLINE (!:) #-}
    {-# INLINE extract #-}

instance (OrderOf e ~ 'HigherOrder, HFunctor e) => Elem e 'HigherOrder where
    inject :: forall es f a. Membership e es -> e f a -> Union es f a
    inject i = mkUnion i (higherOrdership id)

    project :: forall es f a. Membership e es -> Union es f a -> Maybe (e f a)
    project i (UnsafeUnion n e _ koi) =
        if n == unMembership i
            then Just $ hfmap koi (unsafeCoerce e)
            else Nothing

    (f !: g) (UnsafeUnion n e _ koi) =
        if n == 0
            then f $ hfmap koi (unsafeCoerce e)
            else g $ UnsafeUnion (n - 1) e HigherOrder koi

    extract (UnsafeUnion _ e _ koi) = hfmap koi (unsafeCoerce e)

    {-# INLINE inject #-}
    {-# INLINE project #-}
    {-# INLINE (!:) #-}
    {-# INLINE extract #-}

projectAnyOrder :: forall e es f a. (HFunctor e) => Membership e es -> Union es f a -> Maybe (e f a)
projectAnyOrder i (UnsafeUnion n e order koi) =
    if n == unMembership i
        then Just $ hfmapDynUnsafeCoerce order koi e
        else Nothing

infixr 5 `caseAnyOrder`

caseAnyOrder :: (HFunctor e) => (e f a -> r) -> (Union es f a -> r) -> Union (e ': es) f a -> r
(f `caseAnyOrder` g) (UnsafeUnion n e order koi) =
    if n == 0
        then f $ hfmapDynUnsafeCoerce order koi e
        else g $ UnsafeUnion (n - 1) e order koi

extractAnyOrder :: (HFunctor e) => Union es f a -> e f a
extractAnyOrder (UnsafeUnion _ e order koi) = hfmapDynUnsafeCoerce order koi e

{-# INLINE projectAnyOrder #-}
{-# INLINE caseAnyOrder #-}
{-# INLINE extractAnyOrder #-}

hfmapDynUnsafeCoerce :: (HFunctor e') => EffectOrder -> (forall x. f x -> g x) -> e f a -> e' g a
hfmapDynUnsafeCoerce order phi e = case order of
    FirstOrder -> unsafeCoerce e
    HigherOrder -> hfmap phi (unsafeCoerce e)
{-# INLINE hfmapDynUnsafeCoerce #-}

nil :: Union '[] f a -> r
nil _ = error "Effect system internal error: nil - An empty effect union, which should not be possible to create, has been created."

nilMembership :: Membership e '[] -> r
nilMembership _ = error "Effect system internal error: nilMmebership - An empty effect union membership, which should not be possible to create, has been created."

weakensFor :: forall es es' e. (Suffix es es') => Membership e es -> Membership e es'
weakensFor (UnsafeMembership n) = UnsafeMembership $ n + prefixLen @es @es'
{-# INLINE weakensFor #-}

class Suffix (es :: [Effect]) (es' :: [Effect]) where
    prefixLen :: Int

instance Suffix es es where
    prefixLen = 0
    {-# INLINE prefixLen #-}

instance {-# INCOHERENT #-} (Suffix es es') => Suffix es (e ': es') where
    prefixLen = prefixLen @es @es' + 1
    {-# INLINE prefixLen #-}

class SuffixUnder (es :: [Effect]) (es' :: [Effect]) where
    prefixLenUnder :: Int
    offset :: Int

instance (Suffix es es') => SuffixUnder es es' where
    prefixLenUnder = prefixLen @es @es'
    offset = 0

    {-# INLINE prefixLenUnder #-}
    {-# INLINE offset #-}

instance {-# INCOHERENT #-} (SuffixUnder es es') => SuffixUnder (e ': es) (e ': es') where
    prefixLenUnder = prefixLenUnder @es @es'
    offset = offset @es @es' + 1

    {-# INLINE prefixLenUnder #-}
    {-# INLINE offset #-}

weakensUnderFor :: forall es es' e. (SuffixUnder es es') => Membership e es -> Membership e es'
weakensUnderFor (UnsafeMembership n) =
    UnsafeMembership
        if n < offset @es @es'
            then n
            else n + prefixLenUnder @es @es'
{-# INLINE weakensUnderFor #-}

type family RemoveHOEs (es :: [Effect]) where
    RemoveHOEs '[] = '[]
    RemoveHOEs (e ': es) =
        OrderCase (OrderOf e) (e ': RemoveHOEs es) (RemoveHOEs es)

type WeakenHOEs es = (WeakenHOEs_ es 0 (OrderOf (HeadOf es)), FOEs (RemoveHOEs es))

class (orderOfHead ~ OrderOf (HeadOf es)) => WeakenHOEs_ es (countF :: Natural) orderOfHead where
    -- | Example for '[H,F,F,H,H,F,H,F]
    --
    -- +----+-------+--------+----------------------+
    -- | ix | order | countF | shifter accumulation |
    -- +====+=======+========+======================+
    -- |  0 |     H |      0 | 01234... -> 12345... |
    -- +----+-------+--------+----------------------+
    -- |  1 |     F |      0 | 01234... -> 12345... |
    -- +----+-------+--------+----------------------+
    -- |  2 |     F |      1 | 01234... -> 12345... |
    -- +----+-------+--------+----------------------+
    -- |  3 |     H |      2 | 01234... -> 12456... |
    -- +----+-------+--------+----------------------+
    -- |  4 |     H |      2 | 01234... -> 12567... |
    -- +----+-------+--------+----------------------+
    -- |  5 |     F |      2 | 01234... -> 12567... |
    -- +----+-------+--------+----------------------+
    -- |  6 |     H |      3 | 01234... -> 12578... |
    -- +----+-------+--------+----------------------+
    -- |  7 |     F |      3 | 01234... -> 12578... |
    -- +----+-------+--------+----------------------+
    foldHoeIndexShifter :: (Int -> Int) -> (Int -> Int)

instance (OrderOf (HeadOf '[]) ~ orderOfHead) => WeakenHOEs_ '[] countF orderOfHead where
    foldHoeIndexShifter = id
    {-# INLINE foldHoeIndexShifter #-}

instance (FirstOrder e, WeakenHOEs_ es (countF + 1) _orderOfHead) => WeakenHOEs_ (e ': es) countF 'FirstOrder where
    foldHoeIndexShifter = foldHoeIndexShifter @es @(countF + 1)
    {-# INLINE foldHoeIndexShifter #-}

instance
    (OrderOf e ~ 'HigherOrder, WeakenHOEs_ es countF _orderOfHead, KnownNat countF)
    => WeakenHOEs_ (e ': es) countF 'HigherOrder
    where
    foldHoeIndexShifter shifterAcc =
        foldHoeIndexShifter @es @countF \ix ->
            let shiftedIx = shifterAcc ix
             in if ix < intVal @countF
                    then shiftedIx
                    else shiftedIx + 1
    {-# INLINE foldHoeIndexShifter #-}

type family RemoveExps (es :: [Effect]) where
    RemoveExps '[] = '[]
    RemoveExps (e ': es) =
        FormCase (FormOf e) (e ': RemoveExps es) (RemoveExps es)

type WeakenExps es = (WeakenExps_ es 0 (FormOf (HeadOf es)), PolyHFunctors (RemoveExps es))

class (formOfHead ~ FormOf (HeadOf es)) => WeakenExps_ es (countP :: Natural) formOfHead where
    foldExpIndexShifter :: (Int -> Int) -> (Int -> Int)

instance (FormOf (HeadOf '[]) ~ formOfHead) => WeakenExps_ '[] countP formOfHead where
    foldExpIndexShifter = id
    {-# INLINE foldExpIndexShifter #-}

instance (PolyHFunctor e, WeakenExps_ es (countP + 1) _formOfHead) => WeakenExps_ (e ': es) countP 'Polynomial where
    foldExpIndexShifter = foldExpIndexShifter @es @(countP + 1)
    {-# INLINE foldExpIndexShifter #-}

instance
    (FormOf e ~ 'Exponential, WeakenExps_ es countP _formOfHead, KnownNat countP)
    => WeakenExps_ (e ': es) countP 'Exponential
    where
    foldExpIndexShifter shifterAcc =
        foldExpIndexShifter @es @countP \ix ->
            let shiftedIx = shifterAcc ix
             in if ix < intVal @countP
                    then shiftedIx
                    else shiftedIx + 1
    {-# INLINE foldExpIndexShifter #-}

weaken :: Union es f a -> Union (e ': es) f a
weaken = mapUnion weakenFor
{-# INLINE weaken #-}

weakens :: (Suffix es es') => Union es f a -> Union es' f a
weakens = mapUnion weakensFor
{-# INLINE weakens #-}

weakensUnder :: (SuffixUnder es es') => Union es f a -> Union es' f a
weakensUnder = mapUnion weakensUnderFor
{-# INLINE weakensUnder #-}

weakenHOEsFor :: forall es e. (WeakenHOEs es) => Membership e (RemoveHOEs es) -> Membership e es
weakenHOEsFor = UnsafeMembership . foldHoeIndexShifter @es @0 id . unMembership
{-# INLINE weakenHOEsFor #-}

weakenHOEs :: forall es f a. (WeakenHOEs es) => Union (RemoveHOEs es) f a -> Union es f a
weakenHOEs = mapUnion weakenHOEsFor
{-# INLINE weakenHOEs #-}

weakenExpsFor :: forall es e. (WeakenExps es) => Membership e (RemoveExps es) -> Membership e es
weakenExpsFor = UnsafeMembership . foldExpIndexShifter @es @0 id . unMembership
{-# INLINE weakenExpsFor #-}

weakenExps :: forall es f a. (WeakenExps es) => Union (RemoveExps es) f a -> Union es f a
weakenExps = mapUnion weakenExpsFor
{-# INLINE weakenExps #-}

type KnownLength :: forall {k}. [k] -> Constraint
class KnownLength xs where
    reifyLength :: Int

instance KnownLength '[] where
    reifyLength = 0
    {-# INLINE reifyLength #-}

instance (KnownLength xs) => KnownLength (x ': xs) where
    reifyLength = 1 + reifyLength @xs
    {-# INLINE reifyLength #-}

infixr 5 ++

type family (es :: [Effect]) ++ (es' :: [Effect]) where
    '[] ++ es = es
    (e ': es) ++ es' = e ': (es ++ es')

infixr 5 !++

(!++) :: forall es es' f a r. (KnownLength es) => (Union es f a -> r) -> (Union es' f a -> r) -> Union (es ++ es') f a -> r
(h !++ h') (UnsafeUnion n e o koi) =
    if n < reifyLength @es
        then h $ UnsafeUnion n e o koi
        else h' $ UnsafeUnion (n - reifyLength @es) e o koi
{-# INLINE (!++) #-}

bundleUnion
    :: forall es es' f a
     . (KnownLength es)
    => Union (es ++ es') f a
    -> Union (Union es ': es') f a
bundleUnion = union \i o e ->
    splitFor @es @es'
        (\j -> mkUnion Here (higherOrdership id) (mkUnion j o e))
        (\j -> mkUnion (weakenFor j) o e)
        i
{-# INLINE bundleUnion #-}

unbundleUnion
    :: forall es es' f a
     . (KnownLength es)
    => Union (Union es ': es') f a
    -> Union (es ++ es') f a
unbundleUnion = mapUnion (suffixFor @es') !: mapUnion (prefixFor @es)
{-# INLINE unbundleUnion #-}

splitUnion
    :: forall es es' es'' f a
     . (KnownLength es)
    => (forall e. Membership e es -> Membership e es'')
    -> (forall e. Membership e es' -> Membership e es'')
    -> Union (es ++ es') f a
    -> Union es'' f a
splitUnion injL injR = mapUnion $ splitFor @es @es' injL injR
{-# INLINE splitUnion #-}

mergeUnion
    :: forall es es' f a
     . (KnownLength es)
    => Either (Union es f a) (Union es' f a)
    -> Union (es ++ es') f a
mergeUnion = \case
    Left u -> mapUnion (suffixFor @es') u
    Right u -> mapUnion (prefixFor @es) u
{-# INLINE mergeUnion #-}

splitUnion1
    :: forall fs x es es' f a
     . (KnownLength fs)
    => (forall e. Membership e (Each fs x) -> Membership e es')
    -> (forall e. Membership e es -> Membership e es')
    -> Union (Each fs x ++ es) f a
    -> Union es' f a
splitUnion1 injL injR = mapUnion $ splitFor1 @fs @x @es injL injR
{-# INLINE splitUnion1 #-}

mergeUnion1
    :: forall fs x es f a
     . (KnownLength fs)
    => Either (Union (Each fs x) f a) (Union es f a)
    -> Union (Each fs x ++ es) f a
mergeUnion1 = \case
    Left u -> mapUnion (suffixFor @es) u
    Right u -> mapUnion (prefixFor1 @fs @x) u
{-# INLINE mergeUnion1 #-}

splitFor
    :: forall es es' e r
     . (KnownLength es)
    => (Membership e es -> r)
    -> (Membership e es' -> r)
    -> Membership e (es ++ es')
    -> r
splitFor f g (UnsafeMembership n)
    | n < l = f $ UnsafeMembership n
    | otherwise = g $ UnsafeMembership $ n - l
  where
    l = reifyLength @es
{-# INLINE splitFor #-}

splitFor1
    :: forall fs x es e r
     . (KnownLength fs)
    => (Membership e (Each fs x) -> r)
    -> (Membership e es -> r)
    -> Membership e (Each fs x ++ es)
    -> r
splitFor1 f g (UnsafeMembership n)
    | n < l = f $ UnsafeMembership n
    | otherwise = g $ UnsafeMembership $ n - l
  where
    l = reifyLength @fs
{-# INLINE splitFor1 #-}

suffixFor :: forall es' es e. Membership e es -> Membership e (es ++ es')
suffixFor = coerce
{-# INLINE suffixFor #-}

prefixFor :: forall es' es e. (KnownLength es') => Membership e es -> Membership e (es' ++ es)
prefixFor (UnsafeMembership n) = UnsafeMembership $ reifyLength @es' + n
{-# INLINE prefixFor #-}

type family Each (fs :: [k -> Effect]) x where
    Each (f ': fs) x = (f x ': Each fs x)
    Each '[] x = '[]

prefixFor1 :: forall fs x es e. (KnownLength fs) => Membership e es -> Membership e (Each fs x ++ es)
prefixFor1 (UnsafeMembership n) = UnsafeMembership $ reifyLength @fs + n
{-# INLINE prefixFor1 #-}

data Ordership (o :: EffectOrder) (e :: Effect) (f :: Type -> Type) (g :: Type -> Type) where
    UnsafeFirst :: Ordership 'FirstOrder e f g
    UnsafeHigher :: (forall x. g x -> f x) -> Ordership 'HigherOrder e f g

union :: (forall e o g. Membership e es -> Ordership o e f g -> e g a -> r) -> Union es f a -> r
union f (UnsafeUnion n e o koi) =
    let i = UnsafeMembership n
     in case o of
            FirstOrder -> f i UnsafeFirst e
            HigherOrder -> f i (UnsafeHigher koi) e
{-# INLINE union #-}

mkUnion :: Membership e es -> Ordership o e f g -> e g a -> Union es f a
mkUnion (UnsafeMembership i) o e = case o of
    UnsafeFirst -> UnsafeUnion i e FirstOrder undefined
    UnsafeHigher koi -> UnsafeUnion i e HigherOrder koi
{-# INLINE mkUnion #-}

firstOrdership :: (FirstOrder e) => Ordership 'FirstOrder e f g
firstOrdership = UnsafeFirst
{-# INLINE firstOrdership #-}

higherOrdership :: (OrderOf e ~ 'HigherOrder) => (forall x. g x -> f x) -> Ordership 'HigherOrder e f g
higherOrdership = UnsafeHigher
{-# INLINE higherOrdership #-}

shrinkOrdership :: Ordership o e f g -> Ordership (OrderOf e) e f g
shrinkOrdership = unsafeCoerce

continuationOfInterpretation :: Ordership 'HigherOrder e f g -> (forall x. g x -> f x)
continuationOfInterpretation (UnsafeHigher koi) = koi
{-# INLINE continuationOfInterpretation #-}
