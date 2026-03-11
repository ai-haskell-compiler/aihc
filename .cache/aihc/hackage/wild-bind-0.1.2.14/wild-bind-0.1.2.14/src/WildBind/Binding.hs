{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
-- |
-- Module: WildBind.Binding
-- Description: Functions to build Binding
-- Maintainer: Toshio Ito <debug.ito@gmail.com>
--
-- This module exports functions to build and manipulate 'Binding', an
-- object binding input symbols to actions.
--
module WildBind.Binding
       ( -- * Types
         Action(Action,actDescription,actDo),
         Binding,
         Binding',

         -- * Construction

         -- | Functions to create fundamental 'Binding's.
         --
         -- To create complex 'Binding's, use <#Condition Condition> functions
         -- described below and 'mappend' them together.

         noBinding,
         Binder,
         binds,
         binds',
         bindsF,
         bindsF',
         on,
         run,
         as,
         binding,
         binding',
         bindingF,
         bindingF',

         -- * Condition

         -- | #Condition# With these functions, you can create
         -- 'Binding's that behave differently for different front-end
         -- and/or back-end states.
         --
         -- If you call the condition functions multiple times, the
         -- conditions are combined with AND logic.
         ifFront,
         ifBack,
         ifBoth,
         whenFront,
         whenBack,
         whenBoth,
         -- * Conversion
         -- ** Stateful bindings
         startFrom,
         extend,
         -- ** Type conversion
         convFront,
         convInput,
         convBack,
         -- ** Action conversion
         advice,
         revise,
         revise',
         before,
         after,
         justBefore,
         justAfter,
         -- * Execution
         boundAction,
         boundAction',
         boundActions,
         boundActions',
         boundInputs,
         boundInputs'
       ) where

import           Control.Applicative        (Applicative, (*>), (<*))
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT (..), withReaderT)
import           Control.Monad.Trans.State  (StateT (..), mapStateT, runStateT)
import           Control.Monad.Trans.Writer (Writer, execWriter, mapWriter, tell)
import qualified Data.Map                   as M
import           Data.Monoid                (Endo (Endo, appEndo), Monoid (..))
import           Data.Semigroup             (Semigroup (..))

import           WildBind.Description       (ActionDescription)

-- | Action done by WildBind
data Action m a
  = Action
      { actDescription :: ActionDescription
        -- ^ Human-readable description of the action.
      , actDo          :: m a
        -- ^ The actual job.
      }

instance Show (Action m a) where
  show a = "Action " ++ show (actDescription a)

instance Functor m => Functor (Action m) where
  fmap f a = a { actDo = fmap f (actDo a) }

-- | Make an 'Action' that runs the given monadic action before the
-- original 'Action'.
before :: (Applicative m)
       => m b -- ^ the monadic action prepended
       -> Action m a -- ^ the original 'Action'.
       -> Action m a
before hook act = act { actDo = hook *> actDo act }

-- | Make an 'Action' that runs the given monadic action after the
-- original 'Action'.
after :: (Applicative m)
      => m b -- ^ the monadic action appended.
      -> Action m a -- ^ the original 'Action'.
      -> Action m a
after hook act = act { actDo = actDo act <* hook }

-- | Same as 'before', but it returns 'Just'.
--
-- @since 0.1.1.0
justBefore :: (Applicative m) => m b -> Action m a -> Maybe (Action m a)
justBefore m a = Just $ before m a

-- | Same as 'after', but it returns 'Just'.
--
-- @since 0.1.1.0
justAfter :: (Applicative m) => m b -> Action m a -> Maybe (Action m a)
justAfter m a = Just $ after m a

-- | State/Reader/IO Monad
type SRIM bs fs = StateT bs (ReaderT fs IO)

-- | WildBind back-end binding with both explicit and implicit
-- states. @bs@ is the explicit back-end state, @fs@ is the front-end
-- state, and @i@ is the input type.
--
-- You can make the explicit state @bs@ implicit by 'startFrom'
-- function.
newtype Binding' bs fs i
  = Binding' { unBinding' :: bs -> fs -> M.Map i (Action (SRIM bs fs) (Binding' bs fs i)) }

runSRIM :: bs -> fs -> SRIM bs fs a -> IO (a, bs)
runSRIM bs fs m = flip runReaderT fs $ flip runStateT bs m

redoSRIM :: IO (a, bs) -> SRIM bs fs a
redoSRIM m = StateT $ const $ lift m

mapActDo :: (m a -> n b) -> Action m a -> Action n b
mapActDo f act = act { actDo = f $ actDo act }

mapActResult :: Functor m => (a -> b) -> M.Map i (Action m a) -> M.Map i (Action m b)
mapActResult = fmap . fmap

liftActionR :: Monad m => Action m a -> Action (ReaderT fs m) a
liftActionR = mapActDo lift

withActionR :: (fs -> fs') -> Action (SRIM bs fs') a -> Action (SRIM bs fs) a
withActionR f = (mapActDo . mapStateT) (withReaderT f)

-- | WildBind back-end binding between inputs and actions. @s@ is the
-- front-end state type, and @i@ is the input type.
type Binding s i = Binding' () s i

-- | See "Monoid" instance.
instance Ord i => Semigroup (Binding' bs fs i) where
  abind <> bbind = Binding' $ \bs fs ->
    let amap = mapActResult (<> bbind) $ unBinding' abind bs fs
        bmap = mapActResult (abind <>) $ unBinding' bbind bs fs
    in M.unionWith (\_ b -> b) amap bmap

-- | 'mempty' returns a 'Binding' where no binding is
-- defined. 'mappend' combines two 'Binding's while preserving their
-- individual implicit states. The right-hand 'Binding' has precedence
-- over the left-hand one. That is, if the two 'Binding's both have a
-- binding to the same key in the same front-end and back-end state,
-- the binding from the right-hand one is used.
instance Ord i => Monoid (Binding' bs fs i) where
  mempty = noBinding
  mappend = (<>)

-- | A 'Binding'' with no bindings. It's the same as 'mempty', except
-- 'noBinding' requires no context.
noBinding :: Binding' bs fs i
noBinding = Binding' $ \_ _ -> M.empty

-- | Get the 'Action' bound to the specified state @s@ and input @i@.
boundAction :: (Ord i) => Binding s i -> s -> i -> Maybe (Action IO (Binding s i))
boundAction b state input = (fmap . fmap) fst $ boundAction' b () state input

-- | Get the 'Action' bound to the specified back-end state @bs@,
-- front-end state @fs@ and input @i@
boundAction' :: (Ord i) => Binding' bs fs i -> bs -> fs -> i -> Maybe (Action IO (Binding' bs fs i, bs))
boundAction' b bs fs input = (fmap . mapActDo) (runSRIM bs fs) $ M.lookup input $ unBinding' b bs fs

-- | Get the list of all bound inputs @i@ and their corresponding
-- actions for the specified front-end state @s@.
boundActions :: Binding s i -> s -> [(i, Action IO (Binding s i))]
boundActions b state = fmap (\(i, act) -> (i, fmap fst act)) $ boundActions' b () state

-- | Get the list of all bound inputs @i@ and their corresponding
-- actions for the specified back-end state @bs@ and front-end state
-- @fs@.
boundActions' :: Binding' bs fs i -> bs -> fs -> [(i, Action IO (Binding' bs fs i, bs))]
boundActions' b bs fs = map convertAction $ M.toList $ unBinding' b bs fs
  where
    convertAction (i, act) = (i, mapActDo (runSRIM bs fs) act)

-- | Get the list of all bound inputs @i@ for the specified front-end
-- state @s@.
boundInputs :: Binding s i -> s -> [i]
boundInputs b s = fmap fst $ boundActions b s

-- | Get the list of all bound inputs @i@ for the specified front-end
-- state @fs@ and the back-end state @bs@.
boundInputs' :: Binding' bs fs i -> bs -> fs -> [i]
boundInputs' b bs fs = fmap fst $ boundActions' b bs fs


-- | A monad to construct 'Binding''. @i@ is the input symbol, and @v@
-- is supposed to be the 'Action' bound to @i@.
newtype Binder i v a
  = Binder { unBinder :: Writer (Endo [(i, v)]) a }
  deriving (Applicative, Functor, Monad)

runBinder :: Binder i v a -> [(i, v)] -> [(i, v)]
runBinder = appEndo . execWriter . unBinder

-- | Build a 'Binding' with no explicit or implicit state. The bound
-- actions are activated regardless of the back-end or front-end
-- state.
--
-- If different actions are bound to the same input, the latter action
-- wins.
--
-- Result of action (@r@) is discarded.
binds :: Ord i => Binder i (Action IO r) a -> Binding' bs fs i
binds = binding . flip runBinder []

-- | Like 'binds', but this function allows actions to use the current
-- front-end state via 'ReaderT'.
--
-- @since 0.1.1.0
bindsF :: Ord i => Binder i (Action (ReaderT fs IO) r) a -> Binding' bs fs i
bindsF = bindingF . flip runBinder []

-- | Build a 'Binding'' with an explicit state (but no implicit
-- state). The bound actions are activated regardless of the back-end
-- or front-end state.
binds' :: Ord i => Binder i (Action (StateT bs IO) r) a -> Binding' bs fs i
binds' = binding' . flip runBinder []

-- | Like 'binds'', but this function allows actions to use the
-- current front-end state via 'ReaderT'.
--
-- @since 0.1.1.0
bindsF' :: Ord i => Binder i (Action (StateT bs (ReaderT fs IO)) r) a -> Binding' bs fs i
bindsF' = bindingF' . flip runBinder []

-- | Create a 'Binder' that binds the action @v@ to the input @i@.
on :: i -> v -> Binder i v ()
on i v = Binder $ tell $ Endo ((i,v) :)

-- | Transform the given action @m a@ into an 'Action' and apply the
-- continuation to it. It discards the result of action (type
-- @a@). Usually used as an operator.
run :: Functor m => (Action m () -> b) -> m a -> b
run cont raw_act = cont $ Action { actDescription = "", actDo = fmap (const ()) raw_act }

infixl 2 `run`

-- | Transform the given continuation so that the 'ActionDescription'
-- is set to the 'Action' passed to the continuation. Usually used as
-- an operator.
as :: (Action m a -> b) -> ActionDescription -> Action m a -> b
as cont desc act = cont $ act { actDescription = desc }

infixl 2 `as`

-- | Transform the actions in the given 'Binder'.
advice :: (v -> v') -> Binder i v a -> Binder i v' a
advice f = Binder . mapWriter f_writer . unBinder where
  f_writer (a, e) = (a, f_endo e)
  f_endo (Endo prepender) = Endo ((map f_pair $ prepender []) ++)
  f_pair (i, v) = (i, f v)

statelessBinding :: M.Map i (Action (ReaderT fs IO) r) -> Binding' bs fs i
statelessBinding bind_map = impl where
  impl = Binding' $ \_ _ -> (fmap . mapActDo) lift $ mapActResult (const impl) $ bind_map

-- | Non-monadic version of 'binds'.
binding :: Ord i => [(i, Action IO r)] -> Binding' bs fs i
binding = statelessBinding . fmap liftActionR . M.fromList

-- | Non-monadic version of 'bindsF'.
--
-- @since 0.1.1.0
bindingF :: Ord i => [(i, Action (ReaderT fs IO) r)] -> Binding' bs fs i
bindingF = statelessBinding . M.fromList

-- | Create a binding that behaves differently for different front-end
-- states @fs@.
ifFront :: (fs -> Bool) -- ^ The predicate
        -> Binding' bs fs i -- ^ Enabled if the predicate is 'True'
        -> Binding' bs fs i -- ^ Enabled if the predicate is 'False'
        -> Binding' bs fs i
ifFront p = ifBoth $ \_ fs -> p fs

-- | Create a binding that behaves differently for different back-end
-- states @bs@.
ifBack :: (bs -> Bool) -- ^ The predicate
       -> Binding' bs fs i -- ^ Enabled if the predicate is 'True'
       -> Binding' bs fs i -- ^ Enabled if the predicate is 'False'
       -> Binding' bs fs i
ifBack p = ifBoth $ \bs _ -> p bs

-- | Create a binding that behaves differently for different front-end
-- and back-end states, @fs@ and @bs@.
ifBoth :: (bs -> fs -> Bool) -- ^ The predicate
       -> Binding' bs fs i -- ^ Enabled if the predicate is 'True'
       -> Binding' bs fs i -- ^ Enabled if the predicate is 'False'
       -> Binding' bs fs i
ifBoth p thenb elseb = Binding' $ \bs fs ->
  if p bs fs
  then mapActResult (\nextb -> ifBoth p nextb elseb) $ unBinding' thenb bs fs
  else mapActResult (\nextb -> ifBoth p thenb nextb) $ unBinding' elseb bs fs

-- | Add a condition on the front-end state to 'Binding'.
whenFront :: (fs -> Bool) -- ^ The predicate.
          -> Binding' bs fs i -- ^ Enabled if the predicate is 'True'
          -> Binding' bs fs i
whenFront p = whenBoth $ \_ fs -> p fs

-- | Add a condition on the back-end state to 'Binding'.
whenBack :: (bs -> Bool) -- ^ The predicate.
         -> Binding' bs fs i -- ^ Enabled if the predicate is 'True'
         -> Binding' bs fs i
whenBack p = whenBoth $ \bs _ -> p bs

-- | Add a condition on the back-end and front-end states to
-- 'Binding'.
whenBoth :: (bs -> fs -> Bool) -- ^ The predicate.
         -> Binding' bs fs i -- ^ Enabled if the predicate is 'True'.
         -> Binding' bs fs i
whenBoth p b = ifBoth p b noBinding

-- | Contramap the front-end state.
convFront :: (fs -> fs') -> Binding' bs fs' i -> Binding' bs fs i
convFront cmapper orig_bind = Binding' $ \bs fs ->
  mapActResult (convFront cmapper) $ fmap (withActionR cmapper) $ unBinding' orig_bind bs (cmapper fs)

-- | Map the front-end input.
convInput :: Ord i' => (i -> i') -> Binding' bs fs i -> Binding' bs fs i'
convInput mapper orig_bind = Binding' $ \bs fs ->
  mapActResult (convInput mapper) $ M.mapKeys mapper $ unBinding' orig_bind bs fs

-- | Convert the back-end state. Intuitively, it converts a small
-- state type @bs@ into a bigger state type @bs'@, which includes
-- @bs@.
--
-- For example, if you have a 'Control.Lens.Lens'' @l@, you can do
--
-- > convBack (set l) (view l) b
convBack :: (bs -> bs' -> bs') -- ^ A setter. It's supposed to set
                               -- @bs@ into the original @bs'@ and
                               -- return the result.
         -> (bs' -> bs) -- ^ A getter. It's supposed to extract @bs@
                        -- from @bs'@.
         -> Binding' bs fs i
         -> Binding' bs' fs i
convBack setter getter orig_bind = Binding' $ \bs' fs ->
  (fmap . mapActDo) convState $ unBinding' orig_bind (getter bs') fs
  where
    convState ms = StateT $ \bs' -> fmap (convResult bs') $ runStateT ms $ getter bs'
    convResult bs' (next_b, bs) = (convBack setter getter next_b, setter bs bs')

-- | Convert 'Binding'' to 'Binding' by hiding the explicit state
-- @bs@.
startFrom :: bs -- ^ Initial state
          -> Binding' bs fs i -- ^ Binding' with explicit state
          -> Binding fs i -- ^ Binding containing the state inside
startFrom init_state b' = Binding' $ \() front_state ->
  mapActResult toB $ (fmap . mapActDo) (startSRIM init_state) $ unBinding' b' init_state front_state
  where
    toB (next_b', next_state) = startFrom next_state next_b'

startSRIM :: bs -> SRIM bs fs a -> SRIM () fs (a, bs)
startSRIM bs m = StateT $ \() -> fmap toState $ runStateT m bs
  where
    toState (a, result_bs) = ((a, result_bs), ())

-- | Extend 'Binding' to 'Binding''. In the result 'Binding'', the
-- explicit back-end state is just ignored and unmodified.
extend :: Binding fs i -> Binding' bs fs i
extend = convBack (const id) (const ())

-- | Non-monadic version of 'binds''.
binding' :: Ord i => [(i, Action (StateT bs IO) r)] -> Binding' bs fs i
binding' = statefulBinding . fmap addR . M.fromList where
  addR = mapActDo $ mapStateT lift

-- | Non-monadic version of 'bindsF''.
--
-- @since 0.1.1.0
bindingF' :: Ord i => [(i, Action (StateT bs (ReaderT fs IO)) r)] -> Binding' bs fs i
bindingF' = statefulBinding . M.fromList

statefulBinding :: M.Map i (Action (SRIM bs fs) r) -> Binding' bs fs i
statefulBinding bind_map = impl where
  impl = Binding' $ \_ _ -> mapActResult (const impl) bind_map

-- | Revise (modify) actions in the given 'Binding''.
--
-- @since 0.1.1.0
revise :: (forall a . bs -> fs -> i -> Action IO a -> Maybe (Action IO a))
       -- ^ A function to revise the action. If it returns 'Nothing',
       -- the action is unbound.
       -> Binding' bs fs i
       -- ^ original binding
       -> Binding' bs fs i
       -- ^ revised binding
revise f = reviseThis where
  reviseThis (Binding' orig) = Binding' $ \bs fs -> M.mapMaybeWithKey (f_to_map bs fs) (orig bs fs)
  f_to_map bs fs i orig_act = fmap convertResult $ f bs fs i $ mapActDo (runSRIM bs fs) orig_act
  convertResult = fmap reviseThis . mapActDo redoSRIM

-- | Like 'revise', but this function allows revising the back-end state.
--
-- @since 0.1.1.0
revise' :: (forall a . bs -> fs -> i -> Action (StateT bs IO) a -> Maybe (Action (StateT bs IO) a))
        -> Binding' bs fs i
        -> Binding' bs fs i
revise' f = reviseThis where
  reviseThis (Binding' orig) = Binding' $ \bs fs -> M.mapMaybeWithKey (f_to_map bs fs) (orig bs fs)
  f_to_map bs fs i orig_act = fmap convertResult $ f bs fs i $ mapActDo (runR fs) orig_act
  runR :: fs -> SRIM bs fs a -> StateT bs IO a
  runR fs m = mapStateT (flip runReaderT fs) m
  convertResult = fmap reviseThis . mapActDo toSRIM
  toSRIM :: StateT bs IO a -> SRIM bs fs a
  toSRIM m = mapStateT lift m
