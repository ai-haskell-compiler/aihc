module Reactive.Banana.Bunch.Frameworks (
   RBF.MomentIO,
   RBF.Handler,
   AddHandler,
   newAddHandler,
   fromAddHandler,
   fromChanges,
   RBF.Future,
   RBF.compile,
   RBF.actuate,
   RBF.pause,
   RBF.liftIO,
   changes,
   newEvent,
   reactimate,
   reactimate',
   plainChanges,
   ) where

import qualified Reactive.Banana.Bunch.Combinators as RB
import Reactive.Banana.Bunch.Private (Event(Event))

import qualified Reactive.Banana.Frameworks as RBF

import Control.Monad (liftM)
import Control.Applicative ((<$>))

import qualified Data.NonEmpty as NonEmpty
import qualified Data.Foldable as Fold
import Data.Functor.Compose (Compose(Compose, getCompose))
import Data.Tuple.HT (mapPair)


changes :: RB.Behavior a -> RBF.MomentIO (Event (RBF.Future a))
changes = liftM (Event . fmap NonEmpty.singleton) . RBF.changes

reactimate :: Event (IO ()) -> RBF.MomentIO ()
reactimate (Event xs) = RBF.reactimate $ Fold.sequence_ <$> xs

reactimate' :: Event (RBF.Future (IO ())) -> RBF.MomentIO ()
reactimate' (Event xs) =
   RBF.reactimate' $ (getCompose . Fold.sequenceA_ . fmap Compose) <$> xs


newEvent :: RBF.MomentIO (Event a, RBF.Handler a)
newEvent = liftM (mapPair (Event, (. NonEmpty.singleton))) $ RBF.newEvent


{- |
This is a bit of a hack.
The events will occur slightly after the behavior changes.
-}
plainChanges :: RB.Behavior a -> RBF.MomentIO (Event a)
plainChanges x = do
   (evs, handle) <- RBF.newEvent
   xs <- RBF.changes x
   RBF.reactimate' $ fmap (fmap handle) xs
   return $ Event $ NonEmpty.singleton <$> evs


newtype AddHandler a = AddHandler (RBF.AddHandler (NonEmpty.T [] a))

fromAddHandler :: AddHandler a -> RBF.MomentIO (Event a)
fromAddHandler (AddHandler h) = liftM Event $ RBF.fromAddHandler h

fromChanges :: a -> AddHandler a -> RBF.MomentIO (RB.Behavior a)
fromChanges a h = RB.stepper a =<< fromAddHandler h

newAddHandler :: IO (AddHandler a, RBF.Handler a)
newAddHandler =
   liftM (mapPair (AddHandler, (. NonEmpty.singleton))) $ RBF.newAddHandler
