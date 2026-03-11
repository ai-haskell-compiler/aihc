{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
module UniqueLogic.ST.TF.System (
   -- * Preparation
   Variable,
   globalVariable,
   -- * Handle duplicates
   C, update,
   simpleUpdate, -- should be private in future
   updateIfNew, -- should be private or with special type
   updateAndCheck,
   Fragile(break),
   Value, ValueConstraint, valueConstraint,
   -- * Posing statements
   T,
   localVariable,
   constant,
   assignment2,
   assignment3,
   Apply, arg, runApply, runApplyMaybe, runApplyM,
   -- * Solution
   solve, solveDepthFirst, solveBreadthFirst,
   query,
   ) where

import qualified Control.Monad.Trans.Except as ME
import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.Class  as MT
import qualified UniqueLogic.ST.TF.MonadTrans as UMT
import qualified Data.Sequence as Seq
import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.Ref as Ref
import Control.Monad.Trans.Writer (WriterT, )
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT, mapMaybeT, )
import Control.Monad.Trans.Identity (IdentityT, )
import Control.Monad (when, liftM, liftM2, ap, guard, join, )
import Control.Applicative (Applicative, liftA2, pure, (<*>), (<$>), )
import Data.Sequence (Seq, (|>), ViewL((:<)), )
import Data.Functor.Compose (Compose(Compose))

import Data.Maybe (isNothing, )
import Data.Monoid (Monoid, mempty, mappend, mconcat, )
import Data.Semigroup (Semigroup, (<>), )

import Prelude hiding (break)


data Variable w s a =
   Variable {
      varUpdate :: MaybeT s a -> Update w s,
      dependsRef :: Ref.T s (Updates w s),
      valueRef :: Ref.T s (Maybe a)
   }

type Update w s = UMT.Wrap w s (Updates w s)
newtype Updates w s = Updates {unpackUpdates :: Seq (Update w s)}

instance Semigroup (Updates w s) where
   Updates x <> Updates y = Updates $ x <> y

instance Monoid (Updates w s) where
   mempty = Updates Seq.empty
   mappend (Updates x) (Updates y) = Updates $ mappend x y

addUpdate :: Update w s -> Updates w s -> Updates w s
addUpdate x (Updates xs) = Updates $ xs |> x


type Updater w s a =
        Ref.T s (Updates w s) -> Ref.T s (Maybe a) ->
        MaybeT (UMT.Wrap w s) a -> Update w s

type SimpleUpdater w s a =
        Ref.T s (Updates w s) -> Ref.T s (Maybe a) ->
        MaybeT s a -> Update w s

newtype T w s a =
   Cons {run :: WriterT [Ref.T s (Updates w s)] s a}

instance (Ref.C s) => Functor (T w s) where
   fmap f (Cons x) = Cons (liftM f x)

instance (Ref.C s) => Applicative (T w s) where
   pure = Cons . return
   (<*>) = ap

instance (Ref.C s) => Monad (T w s) where
   return = Cons . return
   Cons x >>= k  = Cons $ run . k =<< x


lift :: (Monad s) => s a -> T w s a
lift = Cons . MT.lift

globalVariable ::
   (UMT.C w, Value w a, Ref.C s) =>
   SimpleUpdater w s a -> s (Variable w s a)
globalVariable triggerUpdate = object triggerUpdate Nothing

localVariable :: (C w, Value w a, Ref.C s) => T w s (Variable w s a)
localVariable = lift $ globalVariable simpleUpdate

constant ::
   (C w, Value w a, Ref.C s) =>
   a -> T w s (Variable w s a)
constant a =
   do v <- lift $ object simpleUpdate $ Just a
      Cons $ MW.tell [dependsRef v]
      return v

object ::
   (Ref.C s) =>
   SimpleUpdater w s a ->
   Maybe a -> s (Variable w s a)
object updater ma = do
   al <- Ref.new mempty
   av <- Ref.new ma
   return $ Variable (updater al av) al av


solve, solveDepthFirst, solveBreadthFirst ::
   (UMT.C w, Ref.C s) =>
   T w s a -> w s a
solve = solveDepthFirst

data Order = DepthFirst | BreadthFirst
   deriving (Eq, Enum)

solveDepthFirst   = solveOrder DepthFirst
solveBreadthFirst = solveOrder BreadthFirst

solveOrder ::
   (UMT.C w, Ref.C s) =>
   Order -> T w s a -> w s a
solveOrder order (Cons m) = UMT.unwrap $ do
   let resolve updates =
          case Seq.viewl updates of
             Seq.EmptyL -> return ()
             currentUpdate :< remUpdates -> do
                Updates newUpdates <- currentUpdate
                resolve $
                   case order of
                      DepthFirst -> mappend newUpdates remUpdates
                      BreadthFirst -> mappend remUpdates newUpdates

   (a, w) <- UMT.lift $ MW.runWriterT m
   resolve . unpackUpdates . mconcat =<< mapM (UMT.lift . Ref.read) w
   return a

query :: Variable w s a -> s (Maybe a)
query = Ref.read . valueRef


updateIfNew :: (C w, Ref.C s) => Updater w s a
updateIfNew al av act = do
   as <- UMT.lift $ Ref.read av
   fmap Fold.fold $ runMaybeT $ do
      guard $ isNothing as
      MT.lift . UMT.lift . Ref.write av . Just =<< act
      MT.lift $ UMT.lift $ Ref.read al


class Inconsistency e where
   inconsistency :: e

instance Inconsistency e => Fragile (ME.ExceptT e) where
   break = UMT.wrap $ ME.throwE inconsistency

class C t => Fragile t where
   break :: Monad m => UMT.Wrap t m a

updateAndCheck ::
   (UMT.C w, Ref.C s) =>
   (a -> a -> UMT.Wrap w s ()) ->
   Updater w s a
updateAndCheck customBreak al av act = do
   maold <- UMT.lift $ Ref.read av
   manew <- runMaybeT act
   case manew of
      Nothing -> return mempty
      Just anew -> do
         UMT.lift . Ref.write av . Just $ anew
         case maold of
            Just aold -> customBreak aold anew >> return mempty
            Nothing -> UMT.lift $ Ref.read al


class C w => Value w a where
   data ValueConstraint w a :: *
   valueConstraint ::
      Ref.T s (Updates w s) -> Ref.T s (Maybe a) -> ValueConstraint w a

class UMT.C w => C w where
   update :: (Value w a, Ref.C s) => Updater w s a

instance Value IdentityT a where
   data ValueConstraint IdentityT a = IdentityConstraint
   valueConstraint _ _ = IdentityConstraint

instance C IdentityT where
   update = updateIfNew

instance (Monoid w) => Value (MW.WriterT w) a where
   data ValueConstraint (MW.WriterT w) a = WriterConstraint
   valueConstraint _ _ = WriterConstraint

instance (Monoid w) => C (MW.WriterT w) where
   update = updateIfNew

instance (Inconsistency e, Eq a) => Value (ME.ExceptT e) a where
   data ValueConstraint (ME.ExceptT e) a =
           Eq a => ExceptionConstraint
   valueConstraint _ _ = ExceptionConstraint

instance (Inconsistency e) => C (ME.ExceptT e) where
   update al av act =
      case valueConstraint al av of
         ExceptionConstraint ->
            updateAndCheck (\aold anew -> when (aold /= anew) break) al av act

simpleUpdate :: (C w, Value w a, Ref.C s) => SimpleUpdater w s a
simpleUpdate al av = update al av . mapMaybeT UMT.lift


readSTRefM :: Ref.T s (Maybe a) -> MaybeT s a
readSTRefM = MaybeT . Ref.read


{- |
> assignment2 f a b = runApply (f <$> arg a) b
-}
assignment2, _assignment2 ::
   (UMT.C w, Ref.C s) =>
   (a -> b) ->
   Variable w s a -> Variable w s b ->
   T w s ()
assignment2 f (Variable _ al av) b =
   let triggerUpdate =
          varUpdate b $ liftM f $ readSTRefM av
   in  lift $
       Ref.modify al (addUpdate triggerUpdate)

{- |
> assignment3 f a b c = runApply (liftA2 f (arg a) (arg b)) c
-}
assignment3, _assignment3 ::
   (UMT.C w, Ref.C s) =>
   (a -> b -> c) ->
   Variable w s a -> Variable w s b -> Variable w s c ->
   T w s ()
assignment3 f (Variable _ al av) (Variable _ bl bv) c =
   let triggerUpdate =
          varUpdate c $
          liftM2 f (readSTRefM av) (readSTRefM bv)
   in  lift $
       Ref.modify al (addUpdate triggerUpdate) >>
       Ref.modify bl (addUpdate triggerUpdate)

_assignment2 f a b = runApply (f <$> arg a) b
_assignment3 f a b c = runApply (liftA2 f (arg a) (arg b)) c


newtype Apply w s a =
   Apply (Compose (MW.Writer [Ref.T s (Updates w s)]) (MaybeT s) a)


{- |
This function allows to generalize 'assignment2' and 'assignment3' to more arguments.
You could achieve the same with nested applications of @assignment3 (,)@.
-}
arg :: Variable w s a -> Apply w s a
arg (Variable _update al av) =
   Apply $ Compose $ MW.writer (readSTRefM av, [al])

{-
ToDo: reactivate when AMP has settled

instance (Ref.C s) => Functor (Apply w s) where
   fmap f (Apply a) = Apply $ fmap f a

instance (Ref.C s) => Applicative (Apply w s) where
   pure a = Apply $ pure a
   Apply f <*> Apply a = Apply $ f <*> a
-}

instance (Ref.C s) => Functor (Apply w s) where
   fmap f (Apply (Compose a)) = Apply $ Compose $ fmap (liftM f) a

instance (Ref.C s) => Applicative (Apply w s) where
   pure a = Apply $ Compose $ pure $ return a
   Apply (Compose f) <*> Apply (Compose a) = Apply $ Compose $ liftA2 ap f a


runApply ::
   (UMT.C w, Ref.C s) =>
   Apply w s a -> Variable w s a -> T w s ()
runApply (Apply (Compose w)) a =
   uncurry (runUpdate a) $ MW.runWriter w

runApplyMaybe ::
   (UMT.C w, Ref.C s) =>
   Apply w s (Maybe a) -> Variable w s a -> T w s ()
runApplyMaybe (Apply (Compose w)) a =
   case MW.runWriter w of
      (mf, refs) ->
         runUpdate a (MaybeT $ liftM join $ runMaybeT mf) refs

runApplyM ::
   (UMT.C w, Ref.C s) =>
   Apply w s (s a) -> Variable w s a -> T w s ()
runApplyM (Apply (Compose w)) a =
   case MW.runWriter w of
      (mf, refs) ->
         runUpdate a (MaybeT $ Trav.sequence =<< runMaybeT mf) refs

runUpdate ::
   (Ref.C s) =>
   Variable w s a -> MaybeT s a ->
   [Ref.T s (Updates w s)] -> T w s ()
runUpdate a f refs =
   lift $ Fold.forM_ refs $ flip Ref.modify (addUpdate $ varUpdate a f)
