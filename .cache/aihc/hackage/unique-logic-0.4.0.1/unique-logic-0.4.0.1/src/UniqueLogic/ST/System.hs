module UniqueLogic.ST.System (
   -- * Preparation
   Variable,
   globalVariable,
   -- * Handle duplicates
   C, doUpdate,
   simpleUpdate, -- should be private in future
   updateIfNew, -- should be private or with special type
   updateAndCheck,
   Fragile(break),
   -- * Posing statements
   T,
   localVariable,
   constant,
   assignment2,
   assignment3,
   Apply, arg, runApply,
   -- * Solution
   solve,
   query,
   queryForbid,
   queryIgnore,
   queryVerify,
   ) where

import qualified Control.Monad.Exception.Synchronous as E
import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.Class  as MT
import qualified UniqueLogic.ST.MonadTrans as UMT
import qualified UniqueLogic.ST.Duplicate as Duplicate
import qualified Data.Foldable as Fold
import Control.Monad.Trans.Writer (WriterT, )
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT, mapMaybeT, )
import Control.Monad.Trans.Identity (IdentityT, )
import Control.Monad.ST (ST, )
import Control.Monad.HT (void, (<=<), )
import Control.Monad (when, liftM2, ap, )
import Control.Applicative (Applicative, pure, (<*>), )
import Data.Functor.Compose (Compose(Compose))

import Data.STRef (STRef, newSTRef, modifySTRef, readSTRef, writeSTRef, )
import Data.Maybe (isNothing, )
import Data.Monoid (Monoid, )

import Prelude hiding (break)


data Variable w s a =
   Variable {
      varUpdate :: MaybeT (ST s) a -> Update w s,
      dependsRef :: STRef s [Update w s],
      valueRef :: STRef s (Maybe a)
   }

type Update w s = UMT.Wrap w (ST s) ()

type Updater w s a =
        STRef s [Update w s] -> STRef s (Maybe a) ->
        MaybeT (UMT.Wrap w (ST s)) a -> Update w s

type SimpleUpdater w s a =
        STRef s [Update w s] -> STRef s (Maybe a) ->
        MaybeT (ST s) a -> Update w s

newtype T w s a =
   Cons {run :: WriterT [STRef s [Update w s]] (ST s) a}

instance Functor (T w s) where
   fmap f (Cons x) = Cons (fmap f x)

instance Applicative (T w s) where
   pure = Cons . return
   (<*>) = ap

instance Monad (T w s) where
   return = pure
   Cons x >>= k  = Cons $ run . k =<< x


lift :: ST s a -> T w s a
lift = Cons . MT.lift

globalVariable ::
   (UMT.C w, Duplicate.C a) =>
   SimpleUpdater w s a -> ST s (Variable w s a)
globalVariable update = object update Nothing

localVariable :: (C w, Duplicate.C a) => T w s (Variable w s a)
localVariable = lift $ globalVariable simpleUpdate

constant ::
   (C w, Duplicate.C a) =>
   a -> T w s (Variable w s a)
constant a =
   do v <- lift $ object simpleUpdate $ Just a
      Cons $ MW.tell [dependsRef v]
      return v

object ::
   (STRef s [Update w s] -> STRef s (Maybe a) ->
    MaybeT (ST s) a -> Update w s) ->
   Maybe a -> ST s (Variable w s a)
object updater ma = do
   al <- newSTRef []
   av <- newSTRef ma
   return $ Variable (updater al av) al av

resolve ::
   UMT.C w =>
   STRef s [Update w s] -> Update w s
resolve =
   sequence_ <=< UMT.lift . readSTRef

solve ::
   UMT.C w =>
   T w s a -> w (ST s) a
solve (Cons m) = UMT.unwrap $ do
   (a,w) <- UMT.lift $ MW.runWriterT m
   mapM_ resolve w
   return a

query :: Variable w s a -> ST s (Maybe a)
query = readSTRef . valueRef

queryForbid :: Variable w s (Duplicate.Forbid a) -> ST s (Maybe a)
queryForbid = fmap (fmap (\(Duplicate.Forbid a) -> a)) . query

queryIgnore :: Variable w s (Duplicate.Ignore a) -> ST s (Maybe a)
queryIgnore = fmap (fmap (\(Duplicate.Ignore a) -> a)) . query

queryVerify :: Variable w s (Duplicate.Verify a) -> ST s (Maybe a)
queryVerify = fmap (fmap (\(Duplicate.Verify a) -> a)) . query


updateIfNew :: (C w, Duplicate.C a) => Updater w s a
updateIfNew al av act = do
   as <- UMT.lift $ readSTRef av
   when (isNothing as) $ void $ runMaybeT $ do
      MT.lift . UMT.lift . writeSTRef av . Just =<< act
      MT.lift $ resolve al


class Inconsistency e where
   inconsistency :: e

instance
   Inconsistency e =>
      Fragile (E.ExceptionalT e) where
   break =
      UMT.wrap $ E.throwT inconsistency

class C t => Fragile t where
   break :: Monad m => UMT.Wrap t m a

updateAndCheck ::
   (UMT.C w, Duplicate.C a) =>
   (a -> a -> UMT.Wrap w (ST s) ()) ->
   Updater w s a
updateAndCheck customBreak al av act = do
   maold <- UMT.lift $ readSTRef av
   manew <- runMaybeT act
   Fold.forM_ manew $ \anew -> do
      UMT.lift . writeSTRef av . Just $ anew
      case maold of
         Just aold ->
            when (not $ Duplicate.accept aold anew) $
               customBreak aold anew
         Nothing -> resolve al


class UMT.C w => C w where
   doUpdate :: (Duplicate.C a) => Updater w s a

instance C IdentityT where
   doUpdate = updateIfNew

instance (Monoid w) => C (MW.WriterT w) where
   doUpdate = updateIfNew

instance (Inconsistency e) => C (E.ExceptionalT e) where
   doUpdate = updateAndCheck $ \_ _ -> break

simpleUpdate :: (C w, Duplicate.C a) => SimpleUpdater w s a
simpleUpdate al av = doUpdate al av . mapMaybeT UMT.lift


readSTRefM :: STRef s (Maybe a) -> MaybeT (ST s) a
readSTRefM = MaybeT . readSTRef


assignment2 ::
   UMT.C w =>
   (a -> b) ->
   Variable w s a -> Variable w s b ->
   T w s ()
assignment2 f (Variable _ al av) b =
   let update =
          varUpdate b $ fmap f $ readSTRefM av
   in  lift $
       modifySTRef al (update :)

assignment3 ::
   UMT.C w =>
   (a -> b -> c) ->
   Variable w s a -> Variable w s b -> Variable w s c ->
   T w s ()
assignment3 f (Variable _ al av) (Variable _ bl bv) c =
   let update =
          varUpdate c $
          liftM2 f (readSTRefM av) (readSTRefM bv)
   in  lift $
       modifySTRef al (update :) >>
       modifySTRef bl (update :)


newtype Apply w s a =
   Apply (Compose (MW.Writer [STRef s [Update w s]]) (MaybeT (ST s)) a)


{- |
This function allows to generalize 'assignment2' and 'assignment3' to more arguments.
You could achieve the same with nested applications of @assignment3 (,)@.
-}
arg :: Variable w s a -> Apply w s a
arg (Variable _update al av) =
   Apply $ Compose $ MW.writer (MaybeT $ readSTRef av, [al])

instance Functor (Apply w s) where
   fmap f (Apply a) = Apply $ fmap f a

instance Applicative (Apply w s) where
   pure a = Apply $ pure a
   Apply f <*> Apply a = Apply $ f <*> a


runApply ::
   UMT.C w =>
   Apply w s a -> Variable w s a -> T w s ()
runApply (Apply (Compose w)) a =
   case MW.runWriter w of
      (f, refs) ->
         lift $ Fold.forM_ refs $ flip modifySTRef (varUpdate a f :)
