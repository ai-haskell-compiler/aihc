{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{- |
Processes that use only the current and past data.
Essentially this is a data type for the 'Synthesizer.State.Signal.crochetL' function.
-}
{-
ToDo:
Causal process usually depend on the sample rate,
so we need a phantom type parameter of T for the rate.

Include ST monad for mutable arrays,
this can be useful for delay lines.
On the other hand, couldn't we also use the StorableVector.Cursor data structure
and avoid the ST monad here?
-}
module Synthesizer.Causal.Process (
   T(Cons),
   fromStateMaybe,
   fromState,
   fromSimpleModifier,
   fromInitializedModifier,

   id,
   map,
   first,
   second,
   compose,
   split,
   fanout,
   loop,

{-
   We don't re-export these identifiers
   because people could abuse them for other Arrows.

   (>>>), (***), (&&&),
   (Arrow.^<<), (Arrow.^>>), (Arrow.<<^), (Arrow.>>^),
-}

   apply,
   applyFst,
   applySnd,
   applySameType,
   applyConst,
   apply2,
   apply3,
   applyStorableChunk,

   feed,
   feedFst,
   feedSnd,
   feedGenericFst,
   feedGenericSnd,
   feedConstFst,
   feedConstSnd,

   crochetL,
   mapAccumL,
   scanL,
   scanL1,
   zipWith,
   consInit,
   chainControlled,
   replicateControlled,
   feedback,
   feedbackControlled,

   -- for testing
   applyFst',
   applySnd',
   ) where

import qualified Synthesizer.State.Signal as Sig
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Causal.Class as Class
import qualified Synthesizer.Causal.Utility as ArrowUtil

import qualified Synthesizer.Plain.Modifier as Modifier

import qualified Data.StorableVector as SV

import Foreign.Storable (Storable, )

import qualified Control.Category as Cat
import Control.Arrow
          (Arrow(..), returnA, (<<<), (>>>), (^>>), ArrowLoop(..),
           Kleisli(Kleisli), runKleisli, )
import Control.Monad.Trans.State
          (State, runState,
           StateT(StateT), runStateT, )
import Control.Monad (liftM, )
import Control.Applicative (Applicative, liftA2, pure, (<*>), )

import Data.Tuple.HT (mapSnd, )

import qualified Algebra.Field as Field
import qualified Algebra.Ring as Ring
import qualified Algebra.Additive as Additive

import qualified Prelude as P
import Prelude hiding (id, map, zipWith, )



-- | Cf. StreamFusion  'Synthesizer.State.Signal.T'
data T a b =
   forall s. -- Seq s =>
      Cons !(a -> StateT s Maybe b)  -- compute next value
           !s                        -- initial state



{-# INLINE fromStateMaybe #-}
fromStateMaybe :: (a -> StateT s Maybe b) -> s -> T a b
fromStateMaybe = Cons

{-# INLINE fromState #-}
fromState :: (a -> State s b) -> s -> T a b
fromState f =
   fromStateMaybe (\x -> StateT (Just . runState (f x)))

{-# INLINE fromSimpleModifier #-}
fromSimpleModifier ::
   Modifier.Simple s ctrl a b -> T (ctrl,a) b
fromSimpleModifier (Modifier.Simple s f) =
   fromState (uncurry f) s

{-# INLINE fromInitializedModifier #-}
fromInitializedModifier ::
   Modifier.Initialized s init ctrl a b -> init -> T (ctrl,a) b
fromInitializedModifier (Modifier.Initialized initF f) initS =
   fromState (uncurry f) (initF initS)


{-
It's almost a Kleisli Arrow,
but the hidden type of the state disturbs.
-}
instance Cat.Category T where
   {-# INLINE id #-}
   {-# INLINE (.) #-}

   id  = fromState return ()
   (.) = flip compose

instance Arrow T where
   {-# INLINE arr #-}
   {-# INLINE first #-}
   {-# INLINE second #-}
   {-# INLINE (***) #-}
   {-# INLINE (&&&) #-}

   arr    = map
   first  = liftKleisli first
   second = liftKleisli second
   (***)  = split
   (&&&)  = fanout

{-
I think we cannot define an ArrowApply instance,
because we must extract the initial state somehow
from the inner (T a b) which is not possible.

instance ArrowApply T where
--   app = Cons (runKleisli undefined) ()
   app = first (arr (flip Cons () . runKleisli)) >>> app
-}


instance ArrowLoop T where
   {-# INLINE loop #-}
   loop = liftKleisli loop


type instance Class.ProcessOf Sig.T = T

instance Class.C T where
   type SignalOf T = Sig.T
   toSignal = flip applyConst ()
   fromSignal sig = const () ^>> feed sig


instance Functor (T a) where
   fmap = ArrowUtil.map

instance Applicative (T a) where
   pure = ArrowUtil.pure
   (<*>) = ArrowUtil.apply


instance (Additive.C b) => Additive.C (T a b) where
   zero = pure Additive.zero
   negate = fmap Additive.negate
   (+) = liftA2 (Additive.+)
   (-) = liftA2 (Additive.-)

instance (Ring.C b) => Ring.C (T a b) where
   one = pure Ring.one
   (*) = liftA2 (Ring.*)
   x^n = fmap (Ring.^ n) x
   fromInteger = pure . Ring.fromInteger

instance (Field.C b) => Field.C (T a b) where
   (/) = liftA2 (Field./)
   recip = fmap Field.recip
   fromRational' = pure . Field.fromRational'


instance (P.Num b) => P.Num (T a b) where
   (+) = liftA2 (P.+)
   (-) = liftA2 (P.-)
   (*) = liftA2 (P.*)
   negate = fmap P.negate
   abs = fmap P.abs
   signum = fmap P.signum
   fromInteger = pure . P.fromInteger

instance (P.Fractional b) => P.Fractional (T a b) where
   (/) = liftA2 (P./)
   fromRational = pure . P.fromRational



{-# INLINE extendStateFstT #-}
extendStateFstT :: Monad m => StateT s m a -> StateT (t,s) m a
extendStateFstT st =
   StateT (\(t0,s0) -> liftM (mapSnd (\s1 -> (t0,s1))) (runStateT st s0))

{-# INLINE extendStateSndT #-}
extendStateSndT :: Monad m => StateT s m a -> StateT (s,t) m a
extendStateSndT st =
   StateT (\(s0,t0) -> liftM (mapSnd (\s1 -> (s1,t0))) (runStateT st s0))


{-# INLINE liftKleisli #-}
liftKleisli ::
   (forall s.
    Kleisli (StateT s Maybe) a0 a1 ->
    Kleisli (StateT s Maybe) b0 b1) ->
   T a0 a1 -> T b0 b1
liftKleisli op (Cons f s) =
   Cons (runKleisli $ op $ Kleisli f) s

{-# INLINE liftKleisli2 #-}
liftKleisli2 ::
   (forall s.
      Kleisli (StateT s Maybe) a0 a1 ->
      Kleisli (StateT s Maybe) b0 b1 ->
      Kleisli (StateT s Maybe) c0 c1) ->
   T a0 a1 -> T b0 b1 -> T c0 c1
liftKleisli2 op (Cons f s) (Cons g t) =
   Cons
      (runKleisli
         (Kleisli (extendStateSndT . f) `op`
          Kleisli (extendStateFstT . g)))
      (s,t)


{-# INLINE id #-}
id :: T a a
id = returnA

{-# INLINE map #-}
map :: (a -> b) -> T a b
map f = fromState (return . f) ()

{-# INLINE compose #-}
compose :: T a b -> T b c -> T a c
compose = liftKleisli2 (>>>)

{-# INLINE split #-}
split :: T a b -> T c d -> T (a,c) (b,d)
split = liftKleisli2 (***)

{-# INLINE fanout #-}
fanout :: T a b -> T a c -> T a (b,c)
fanout = liftKleisli2 (&&&)


{-# INLINE runViewL #-}
runViewL :: (SigG.Read sig a) =>
   sig a ->
   (forall s. StateT s Maybe a -> s -> x) ->
   x
runViewL sig cont =
   SigG.runViewL sig (\f s -> cont (StateT f) s)


{-# INLINE apply #-}
apply :: (SigG.Transform sig a, SigG.Transform sig b) =>
   T a b -> sig a -> sig b
apply (Cons f s) =
   SigG.crochetL (runStateT . f) s

{-# INLINE applySameType #-}
applySameType :: (SigG.Transform sig a) =>
   T a a -> sig a -> sig a
applySameType (Cons f s) =
   SigG.crochetL (runStateT . f) s


{- |
I think this function does too much.
Better use 'feedFst' and (>>>).
-}
{-# INLINE applyFst #-}
applyFst, applyFst' :: (SigG.Read sig a) =>
   T (a,b) c -> sig a -> T b c
applyFst c as =
   c <<< feedFst as

applyFst' (Cons f s) as =
   runViewL as (\getNext r ->
   Cons (\b ->
           do a <- extendStateFstT getNext
              extendStateSndT (f (a,b)))
        (s,r))

{- |
I think this function does too much.
Better use 'feedSnd' and (>>>).
-}
{-# INLINE applySnd #-}
applySnd, applySnd' :: (SigG.Read sig b) =>
   T (a,b) c -> sig b -> T a c
applySnd c as =
   c <<< feedSnd as

applySnd' (Cons f s) bs =
   runViewL bs (\getNext r ->
   Cons (\a ->
           do b <- extendStateFstT getNext
              extendStateSndT (f (a,b)))
        (s,r))

{- |
applyConst c x == apply c (repeat x)
-}
{-# INLINE applyConst #-}
applyConst :: T a b -> a -> Sig.T b
applyConst (Cons f s) a =
   Sig.unfoldR (runStateT (f a)) s

{-
Can be easily done by converting the result of applyConst to generic signal
{-# INLINE applyConstGeneric #-}
applyConstGeneric :: SigG.LazySize -> T a b -> a -> sig b
applyConstGeneric size (Cons f s) a =
   SigG.unfoldR size (runStateT (f a)) s
-}


{-# INLINE apply2 #-}
apply2 ::
   (SigG.Read sig a, SigG.Transform sig b, SigG.Transform sig c) =>
   T (a,b) c -> sig a -> sig b -> sig c
apply2 f x y =
   apply (applyFst f x) y

{-# INLINE apply3 #-}
apply3 ::
   (SigG.Read sig a, SigG.Read sig b, SigG.Transform sig c, SigG.Transform sig d) =>
   T (a,b,c) d -> sig a -> sig b -> sig c -> sig d
apply3 f x y z =
   apply2 (applyFst ((\(a,(b,c)) -> (a,b,c)) ^>> f) x) y z


{-
A generalized version could be of type

Transform sig a b => Causal.T a b -> Causal.T (sig a) (sig b)

but we cannot implement that,
since crochetL does not return the final state.
-}
applyStorableChunk ::
   (Storable a, Storable b) =>
   T a b -> T (SV.Vector a) (SV.Vector b)
applyStorableChunk (Cons next start) = Cons
   (\a -> StateT $ \ms ->
      flip fmap ms $ \s ->
         SV.crochetLResult (runStateT . next) s a)
   (Just start)


{-# INLINE feed #-}
feed :: (SigG.Read sig a) =>
   sig a -> T () a
feed proc =
   runViewL proc (\getNext ->
      fromStateMaybe (const getNext))

{-# INLINE feedFst #-}
feedFst :: (SigG.Read sig a) =>
   sig a -> T b (a,b)
feedFst proc =
   runViewL proc (\getNext ->
      fromStateMaybe (\b -> fmap (flip (,) b) getNext))

{-# INLINE feedSnd #-}
feedSnd :: (SigG.Read sig a) =>
   sig a -> T b (b,a)
feedSnd proc =
   runViewL proc (\getNext ->
      fromStateMaybe (\b -> fmap ((,) b) getNext))

{-# INLINE feedConstFst #-}
feedConstFst :: a -> T b (a,b)
feedConstFst a = map (\b -> (a,b))

{-# INLINE feedConstSnd #-}
feedConstSnd :: a -> T b (b,a)
feedConstSnd a = map (\b -> (b,a))

{-# INLINE feedGenericFst #-}
feedGenericFst :: (SigG.Read sig a) =>
   sig a -> T b (a,b)
feedGenericFst =
   feedFst . SigG.toState

{-# INLINE feedGenericSnd #-}
feedGenericSnd :: (SigG.Read sig a) =>
   sig a -> T b (b,a)
feedGenericSnd =
   feedSnd . SigG.toState



-- * list like functions

{-# INLINE crochetL #-}
crochetL :: (x -> acc -> Maybe (y, acc)) -> acc -> T x y
crochetL f s = fromStateMaybe (StateT . f) s

{-# INLINE mapAccumL #-}
mapAccumL :: (x -> acc -> (y, acc)) -> acc -> T x y
mapAccumL next = crochetL (\a s -> Just $ next a s)

{-# INLINE scanL #-}
scanL :: (acc -> x -> acc) -> acc -> T x acc
scanL f = mapAccumL (\x acc -> (acc, f acc x))

{-# INLINE scanL1 #-}
scanL1 :: (x -> x -> x) -> T x x
scanL1 f =
   mapAccumL (\x acc -> (x, Just $ maybe x (flip f x) acc)) Nothing

{-# INLINE zipWith #-}
zipWith :: (SigG.Read sig a) =>
   (a -> b -> c) -> sig a -> T b c
zipWith f = applyFst (map (uncurry f))

{- |
Prepend an element to a signal,
but keep the signal length,
i.e. drop the last element.
-}
{-# INLINE consInit #-}
consInit :: x -> T x x
consInit = mapAccumL (\x acc -> (acc, x))



{-# INLINE chainControlled #-}
chainControlled :: [T (c,x) x] -> T (c,x) x
chainControlled = Class.chainControlled

{- |
If @T@ would be the function type @->@
then @replicateControlled 3 f@ computes
@\(c,x) -> f(c, f(c, f(c, x)))@.
-}
{-# INLINE replicateControlled #-}
replicateControlled :: Int -> T (c,x) x -> T (c,x) x
replicateControlled = Class.replicateControlled


{-# INLINE feedback #-}
feedback :: T (a,c) b -> T b c -> T a b
feedback forth back =
   loop (forth >>>  id &&& back)

{-# INLINE feedbackControlled #-}
feedbackControlled :: T ((ctrl,a),c) b -> T (ctrl,b) c -> T (ctrl,a) b
feedbackControlled forth back =
   loop (map (fst.fst) &&& forth  >>>  map snd &&& back)

{-
{-# INLINE feedbackControlled #-}
feedbackControlled :: T (ctrl, (a,c)) b -> T (ctrl,b) c -> T (ctrl,a) b
feedbackControlled forth back =
   loop ((\((ctrl,a),c) -> (ctrl, (a,c)))  ^>>
         map fst &&& forth  >>>
         map snd &&& back)
-}
